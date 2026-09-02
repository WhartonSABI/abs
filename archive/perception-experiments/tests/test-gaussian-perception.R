test_that("Gaussian perception blur preserves sigma zero and contracts toward 50 percent", {
  p <- c(0.001, 0.10, 0.50, 0.90, 0.999)
  unchanged <- perception_blur(p, sigma = 0, spatial_scale = 2)
  blurred <- perception_blur(p, sigma = 4, spatial_scale = 2)

  expect_equal(unchanged, p, tolerance = 1e-12)
  expect_true(all(abs(blurred - 0.5) <= abs(p - 0.5)))
  expect_equal(blurred[[3L]], 0.5, tolerance = 1e-12)
  expect_true(all(blurred >= 0 & blurred <= 1))
})

test_that("perception choices use the correct observer and remove preempted decisions", {
  opportunities <- data.table::data.table(
    game_pk = 1L,
    pitch_order = 1:4,
    initial_call = c("called_strike", "ball", "ball", "ball"),
    edge_distance_inches = c(0.2, -0.2, -0.3, 0.1),
    p_hat = c(0.6, 0.7, 0.8, 0.2),
    stake_G = 0.2,
    batter_id = 10:13,
    pitcher_id = 20:23,
    fielder_2 = 30:33,
    challenge_occurred = c(TRUE, TRUE, TRUE, FALSE),
    challenger_role = c("batter", "catcher", "pitcher", NA_character_),
    challenger_player_id = c(10L, 31L, 22L, NA_integer_),
    bat_team_challenges_before = c(2L, 2L, 2L, 0L),
    fld_team_challenges_before = c(2L, 2L, 2L, 0L),
    actual_wrong = c(TRUE, TRUE, TRUE, FALSE)
  )
  choices <- expand_perception_choices(opportunities)

  expect_equal(nrow(choices), 3L)
  expect_setequal(choices$role, c("batter", "catcher", "pitcher"))
  expect_true(all(choices$challenged == 1L))
  expect_false(any(choices$pitch_order == 4L))
  expect_equal(choices[role == "catcher", player_id], "31")
  expect_equal(choices[role == "pitcher", player_id], "22")
  expect_equal(
    anyDuplicated(choices[, .(game_pk, pitch_order, role, player_role_id)]),
    0L
  )

  policy_choices <- expand_perception_choices(
    opportunities, available_only = FALSE, exclude_preempted = FALSE
  )
  expect_equal(nrow(policy_choices), 7L)
  expect_true(any(policy_choices$inventory_before == 0L))
  policy_choices[, p_perceived := p_hat]
  pitch_probabilities <- perception_choices_to_pitch_probabilities(policy_choices)
  expect_equal(nrow(pitch_probabilities), 4L)
})

test_that("spatial scales recover known inch conversions for both call directions", {
  margin <- rep(seq(-4, 4, length.out = 81), 2L)
  call <- rep(c("ball", "called_strike"), each = 81L)
  scale <- ifelse(call == "ball", 2, 3)
  x <- data.table::data.table(
    p_hat = stats::pnorm(margin / scale),
    initial_call = call,
    edge_distance_inches = ifelse(call == "ball", -margin, margin)
  )
  result <- estimate_perception_spatial_scale(x)

  expect_equal(result[initial_call == "ball", spatial_scale], 2, tolerance = 1e-8)
  expect_equal(
    result[initial_call == "called_strike", spatial_scale], 3,
    tolerance = 1e-8
  )
  expect_true(all(result$slope_per_inch > 0))
})

test_that("Stan fitting data exclude overturn truth and retain the hierarchy", {
  choices <- data.table::data.table(
    game_pk = rep(1:6, each = 30),
    role = rep(c("batter", "catcher", "pitcher"), each = 60),
    player_id = rep(c("1", "2", "3"), each = 60),
    player_role_id = rep(c("batter:1", "catcher:2", "pitcher:3"), each = 60),
    challenged = rep(c(0L, 1L), 90),
    p_hat = rep(c(0.2, 0.8), 90),
    stake_G = 0.2,
    initial_call = rep(c("called_strike", "ball", "ball"), each = 60),
    edge_distance_inches = rep(seq(-2, 2, length.out = 60), 3),
    actual_wrong = rep(c(FALSE, TRUE), 90),
    challenge_outcome = "held_out_only"
  )
  scale <- data.table::data.table(
    initial_call = c("ball", "called_strike"),
    spatial_scale = c(2, 3)
  )
  bundle <- prepare_perception_stan_data(choices, scale)

  expect_false(any(c("actual_wrong", "challenge_outcome") %in% names(bundle$data)))
  expect_equal(bundle$data$R, 3L)
  expect_equal(bundle$data$P, 3L)
  expect_equal(length(bundle$data$role_of_player), 3L)
  expect_true(all(is.finite(bundle$data$tracking_z)))
})

test_that("perception folds are deterministic and never overlap", {
  one <- deterministic_perception_folds(1:30, folds = 5L, seed = 42L)
  two <- deterministic_perception_folds(1:30, folds = 5L, seed = 42L)

  expect_equal(one, two)
  expect_equal(sort(unique(one$fold)), 1:5)
  for (fold_id in 1:5) {
    expect_length(intersect(
      one[fold != fold_id, game_key__],
      one[fold == fold_id, game_key__]
    ), 0L)
  }
})

test_that("fast pilot keeps every challenge and corrects sampled passes", {
  set.seed(42)
  choices <- data.table::data.table(
    game_pk = sample(1:20, 6000, replace = TRUE),
    role = rep(c("batter", "catcher", "pitcher"), each = 2000),
    player_id = rep(rep(1:20, each = 100), 3),
    challenged = stats::rbinom(6000, 1, 0.04),
    p_hat = stats::runif(6000, 0.01, 0.99),
    stake_G = stats::runif(6000, 0.01, 0.5),
    initial_call = rep(c("called_strike", "ball", "ball"), each = 2000)
  )
  choices[, player_role_id := paste(role, player_id, sep = ":")]
  sampled <- subsample_perception_pilot(
    choices, pass_fraction = 0.05, min_passes_per_player = 10L, seed = 42L
  )

  expect_lt(nrow(sampled), nrow(choices))
  expect_equal(sum(sampled$challenged), sum(choices$challenged))
  expect_true(all(sampled$sampling_offset >= 0))
  expect_true(all(sampled$pass_inclusion_probability > 0))
  expect_false(is.null(attr(sampled, "sampling_manifest")))

  scale <- data.table::data.table(
    initial_call = c("ball", "called_strike"), spatial_scale = c(2, 3)
  )
  bundle <- prepare_perception_stan_data(sampled, scale)
  expect_equal(bundle$data$sampling_offset, sampled$sampling_offset)
})

test_that("the flat-55 validation gate accepts a truly flat held-out curve", {
  x <- data.table::CJ(
    midpoint = seq(-5.5, 5.5, by = 1),
    observation = 1:20
  )
  x[, `:=`(
    game_pk = observation,
    role = "batter",
    challenged = 1L,
    actual_wrong = observation <= 11L,
    edge_distance_inches = midpoint,
    challenge_probability = 0.5,
    challenge_probability_sigma0 = 0.5
  )]
  validation <- validate_flat55(
    x,
    min_bin_challenges = 10L,
    bootstrap_reps = 50L,
    seed = 42L
  )

  expect_true(validation$gate$bin_pass)
  expect_true(validation$gate$slope_pass)
  expect_true(validation$gate$log_loss_pass)
  expect_true(validation$gate$pass)
  expect_equal(validation$gate$max_absolute_bin_error, 0, tolerance = 1e-12)
})

test_that("sigma zero gives the same MDP action recommendations", {
  opportunities <- data.table::data.table(
    game_pk = rep(1:3, each = 3),
    team_id = 1L,
    pitch_order = rep(1:3, 3),
    inning = 1L,
    stage = 0L,
    role = "offense",
    state_key = "0|offense",
    raw_out_index = 0L,
    p_hat = rep(c(0.2, 0.6, 0.9), 3),
    stake_G = rep(c(0.1, 0.2, 0.4), 3),
    actual_wrong = rep(c(FALSE, TRUE, TRUE), 3)
  )
  fit <- fit_challenge_mdp(opportunities, tol = 1e-10, max_iter = 1000L)
  p_zero <- perception_blur(opportunities$p_hat, 0, 2)
  original <- mdp_action(fit, 0L, "offense", 2L, opportunities$p_hat, opportunities$stake_G)
  transformed <- mdp_action(fit, 0L, "offense", 2L, p_zero, opportunities$stake_G)

  expect_equal(original$recommended_action, transformed$recommended_action)
  expect_equal(original$challenge_advantage_re, transformed$challenge_advantage_re)
})

test_that("the hierarchical perception Stan program parses", {
  skip_if_not_installed("rstan")
  fixed <- rstan::stanc(
    file = default_perception_stan_file("fixed_slope"), allow_undefined = FALSE
  )
  sensitivity <- rstan::stanc(
    file = default_perception_stan_file("role_sensitivity"),
    allow_undefined = FALSE
  )
  expect_true(fixed$status)
  expect_true(sensitivity$status)
})

test_that("synthetic hierarchical sigma recovery is available for slow validation", {
  skip_if(Sys.getenv("RUN_SLOW_BAYES_TESTS") != "true")
  skip_if_not_installed("rstan")
  set.seed(42)
  roles <- perception_role_levels()
  true_median <- c(batter = 5, catcher = 2, pitcher = 3)
  players <- data.table::CJ(role = roles, player_number = 1:8)
  players[, `:=`(
    player_id = paste(role, player_number, sep = "_"),
    player_role_id = paste(role, player_id, sep = ":"),
    sigma = exp(log(true_median[role]) + 0.25 * stats::rnorm(.N)),
    alpha = -1.5 + 0.4 * stats::rnorm(.N)
  )]
  choices <- players[, {
    p <- seq(0.05, 0.95, length.out = 80)
    scale <- if (role == "batter") 3 else 2
    perceived <- perception_blur(p, sigma, scale)
    probability <- stats::plogis(alpha + log(perceived * 0.2 / 0.1))
    .(
      game_pk = seq_len(80),
      challenged = stats::rbinom(80, 1, probability),
      p_hat = p,
      stake_G = 0.2,
      initial_call = if (role == "batter") "called_strike" else "ball",
      edge_distance_inches = stats::qnorm(p) * scale
    )
  }, by = .(role, player_id, player_role_id)]
  scale <- data.table::data.table(
    initial_call = c("ball", "called_strike"),
    spatial_scale = c(2, 3)
  )
  fit <- fit_hierarchical_perception(
    choices, spatial_scale = scale,
    chains = 2L, cores = 2L, iter = 1200L, warmup = 600L,
    seed = 42L, refresh = 0L
  )
  summary <- summarize_perception_fit(fit)$population
  estimated <- stats::setNames(
    summary$population_median_sigma_median, summary$role
  )

  expect_true(all(is.finite(estimated)))
  expect_true(all(abs(log(estimated / true_median[names(estimated)])) < 0.8))
})
