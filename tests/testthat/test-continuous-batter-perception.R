synthetic_continuous_batter_choices <- function(rows = 180L) {
  index <- seq_len(rows)
  x_inches <- 14 * sin(index / 11)
  z_inches <- 13 * cos(index / 13)
  edge <- abs_edge_distance_inches(
    x_inches / 12, 2.5 + z_inches / 12, 3.5, 1.5
  )
  data.table::data.table(
    game_pk = 1L + index %% 20L,
    at_bat_number = index,
    pitch_number = 1L,
    batter_id = paste0("b", 1L + index %% 12L),
    swing = as.integer(edge + stats::rnorm(rows, 0, 2.5) < 0.5),
    plate_x = x_inches / 12,
    plate_z = 2.5 + z_inches / 12,
    sz_top = 3.5,
    sz_bot = 1.5,
    balls = index %% 4L,
    strikes = index %% 3L,
    pitch_family = c("fastball", "breaking", "offspeed")[1L + index %% 3L],
    matchup = c("L-L", "L-R", "R-L", "R-R")[1L + index %% 4L]
  )
}

test_that("continuous batter preparation uses exact geometry and no outcomes", {
  set.seed(1)
  choices <- synthetic_continuous_batter_choices()
  choices[, `:=`(
    challenged = rep(0:1, length.out = .N),
    challenge_success = rep(1:0, length.out = .N),
    abs_call = rep(c("ball", "called_strike"), length.out = .N)
  )]
  first <- prepare_continuous_batter_perception(choices)
  choices[, `:=`(
    challenged = 1L - challenged,
    challenge_success = 1L - challenge_success,
    abs_call = rev(abs_call)
  )]
  second <- prepare_continuous_batter_perception(choices)

  expect_equal(first$data, second$data)
  expect_false(any(c(
    "challenged", "challenge_success", "abs_call"
  ) %in% names(first$rows)))
  expect_equal(
    first$rows$edge_distance_inches,
    abs_edge_distance_inches(
      first$rows$plate_x, first$rows$plate_z,
      first$rows$sz_top, first$rows$sz_bot
    )
  )
  expect_equal(first$data$P, 12L)
  expect_true(all(first$data$strike_group %in% 1:3))
  expect_equal(first$data$estimate_anisotropy, 0L)
  expect_equal(
    first$data$normal_x^2 + first$data$normal_z^2,
    rep(1, first$data$N), tolerance = 1e-12
  )
})

test_that("rounded-boundary normals are exact on faces and corners", {
  root_two <- sqrt(2)
  normal <- continuous_abs_outward_normal(
    x_inches = c(10, 0, 11.5, -11.5),
    z_inches = c(0, 13, 15, -15),
    zone_half_height_inches = 12
  )

  expect_equal(normal$normal_x, c(1, 0, 1 / root_two, -1 / root_two))
  expect_equal(normal$normal_z, c(0, 1, 1 / root_two, -1 / root_two))
  expect_equal(normal$normal_x^2 + normal$normal_z^2, rep(1, 4L))
})

test_that("shared anisotropy preserves area and changes only normal width", {
  sigma <- 3
  ratio <- 2
  width <- continuous_normal_transition_sd(
    sigma_inches = sigma,
    normal_x = c(1, 0, 1 / sqrt(2)),
    normal_z = c(0, 1, 1 / sqrt(2)),
    anisotropy_ratio = ratio
  )

  expect_equal(width[[1L]], sigma * ratio)
  expect_equal(width[[2L]], sigma / ratio)
  expect_equal(
    width[[3L]],
    sigma * sqrt(0.5 * ratio^2 + 0.5 / ratio^2)
  )
  expect_equal((sigma * ratio) * (sigma / ratio), sigma^2)
})

test_that("shared anisotropy is an explicit nested candidate", {
  set.seed(11)
  choices <- synthetic_continuous_batter_choices()
  default <- prepare_continuous_batter_perception(choices)
  candidate <- prepare_continuous_batter_perception(
    choices, anisotropy = "shared"
  )

  expect_equal(default$anisotropy, "isotropic")
  expect_equal(candidate$anisotropy, "shared")
  expect_equal(default$data$estimate_anisotropy, 0L)
  expect_equal(candidate$data$estimate_anisotropy, 1L)
  expect_equal(default$data$normal_x, candidate$data$normal_x)
  expect_equal(default$data$normal_z, candidate$data$normal_z)
})

test_that("continuous sector terms are orthogonal to distance", {
  set.seed(2)
  choices <- synthetic_continuous_batter_choices(240L)
  bundle <- prepare_continuous_batter_perception(choices)
  expect_lt(max(abs(colMeans(bundle$data$sector_basis))), 1e-8)
  correlations <- apply(
    bundle$data$sector_basis, 2L, stats::cor,
    y = bundle$data$edge_distance
  )
  expect_lt(max(abs(correlations)), 1e-8)
})

test_that("continuous batter Stan program parses", {
  skip_if_not_installed("rstan")
  parsed <- rstan::stanc(file = default_continuous_batter_stan_file())
  expect_true(parsed$status)
})

test_that("continuous batter draw and scoring APIs preserve draw alignment", {
  set.seed(6)
  bundle <- prepare_continuous_batter_perception(
    synthetic_continuous_batter_choices(180L)
  )
  draw_count <- 3L
  columns <- list(
    mu_log_sigma = rep(log(3), draw_count),
    tau_log_sigma = rep(0.2, draw_count),
    mu_threshold = rep(0, draw_count),
    tau_threshold = rep(0.4, draw_count)
  )
  add_vector <- function(name, length, value) {
    if (!length) return()
    for (index in seq_len(length)) {
      columns[[paste0(name, "[", index, "]")]] <<- rep(value, draw_count)
    }
  }
  add_matrix <- function(name, rows, columns_count, value) {
    for (row in seq_len(rows)) for (column in seq_len(columns_count)) {
      columns[[paste0(name, "[", row, ",", column, "]")]] <<-
        rep(value, draw_count)
    }
  }
  add_vector("sigma_player", nrow(bundle$player_table), 3)
  add_vector("threshold_player", nrow(bundle$player_table), 0)
  add_vector("beta_context", bundle$data$K, 0)
  add_matrix("beta_sector", 3L, bundle$data$S, 0)
  add_vector("lower_swing", 3L, 0.1)
  add_vector("upper_swing", 3L, 0.9)
  draws <- do.call(cbind, columns)
  fit <- list(
    stan_fit = draws,
    player_table = bundle$player_table,
    context_specification = bundle$context_specification,
    sector_specification = bundle$sector_specification
  )
  class(fit) <- "continuous_batter_perception_fit"
  scoring_rows <- data.table::copy(bundle$rows[1:8])
  scoring_rows[1L, batter_id := "new-batter"]
  scored <- score_continuous_batter_perception(
    fit, scoring_rows, ndraws = 3L, return_draws = TRUE
  )

  expect_equal(scored$draw_id, 1:3)
  expect_equal(dim(scored$swing_probability), c(8L, 3L))
  expect_true(all(scored$swing_probability > 0 & scored$swing_probability < 1))
  expect_false(scored$summary$batter_seen_in_training[[1L]])
  expect_equal(scored$summary$sigma_inches_mean[-1L], rep(3, 7L))
  expect_equal(scored$anisotropy_ratio, rep(1, 3L))
  expect_equal(scored$normal_transition_sd_inches, scored$sigma_inches)

  candidate_fit <- fit
  candidate_fit$stan_fit <- cbind(
    draws,
    log_anisotropy = rep(log(2), draw_count),
    anisotropy_ratio = rep(2, draw_count)
  )
  candidate_fit$anisotropy <- "shared"
  candidate <- score_continuous_batter_perception(
    candidate_fit, scoring_rows, ndraws = 3L, return_draws = TRUE
  )
  expected_width <- vapply(seq_len(draw_count), function(draw) {
    continuous_normal_transition_sd(
      candidate$sigma_inches[, draw],
      candidate$summary$normal_x,
      candidate$summary$normal_z,
      2
    )
  }, numeric(nrow(scoring_rows)))
  expect_equal(candidate$anisotropy_ratio, rep(2, draw_count))
  expect_equal(candidate$normal_transition_sd_inches, expected_width)
  expect_equal(unique(candidate$summary$anisotropy_mode), "shared")
})

test_that("anisotropy simulation records the exact signal width", {
  choices <- synthetic_continuous_batter_choices(90L)
  simulated <- simulate_continuous_batter_anisotropy(
    choices, sigma_inches = 3, anisotropy_ratio = 1.5, seed = 12L
  )
  expected <- continuous_normal_transition_sd(
    3, simulated$normal_x, simulated$normal_z, 1.5
  )

  expect_equal(
    simulated$simulation_normal_transition_sd_inches, expected
  )
  expect_true(all(simulated$swing %in% 0:1))
  expect_true(all(simulated$swing_probability > 0 &
    simulated$swing_probability < 1))
  expect_equal(unique(simulated$simulation_anisotropy_ratio), 1.5)
})

test_that("anisotropy promotion requires recovery and one clustered SE", {
  games <- rep(1:5, each = 20L)
  observed <- rep(c(0L, 1L), 50L)
  isotropic <- data.table::data.table(
    game_pk = games,
    at_bat_number = seq_along(games),
    pitch_number = 1L,
    swing = observed,
    swing_probability_mean = ifelse(observed == 1L, 0.6, 0.4)
  )
  candidate <- data.table::copy(isotropic)
  candidate[, swing_probability_mean := ifelse(swing == 1L, 0.8, 0.2)]
  heldout <- compare_continuous_batter_anisotropy_heldout(
    isotropic, candidate
  )
  recovery <- validate_continuous_batter_anisotropy_recovery(
    seq(1.4, 1.6, length.out = 200L), true_anisotropy_ratio = 1.5
  )
  gate <- gate_continuous_batter_anisotropy(heldout, recovery)

  expect_true(heldout$metrics$one_clustered_se_pass)
  expect_true(recovery$recovery_pass)
  expect_true(gate$promote_shared_anisotropy)
  expect_equal(gate$selected_model, "shared")

  recovery[, recovery_pass := FALSE]
  failed <- gate_continuous_batter_anisotropy(heldout, recovery)
  expect_false(failed$promote_shared_anisotropy)
  expect_equal(failed$selected_model, "isotropic")
})

test_that("player sigma and decision thresholds recover in slow simulation", {
  skip_if(Sys.getenv("RUN_SLOW_BAYES_TESTS") != "true")
  skip_if_not_installed("cmdstanr")
  configure_continuous_cmdstan()

  choices <- synthetic_continuous_batter_choices(3600L)
  batters <- sort(unique(choices$batter_id))
  true_sigma <- stats::setNames(seq(2, 4.2, length.out = length(batters)), batters)
  true_threshold <- stats::setNames(
    seq(-1.2, 1.2, length.out = length(batters)), batters
  )
  simulated <- simulate_continuous_batter_anisotropy(
    choices,
    sigma_inches = true_sigma,
    threshold_inches = true_threshold,
    anisotropy_ratio = 1.4,
    seed = 20260825L
  )
  fit <- fit_continuous_batter_perception(
    simulated,
    anisotropy = "shared",
    backend = "cmdstanr",
    chains = 2L,
    parallel_chains = 2L,
    iter_warmup = 500L,
    iter_sampling = 500L,
    seed = 20260825L,
    refresh = 0L
  )
  draws <- draws_continuous_batter_perception(fit, ndraws = 800L)
  sigma_median <- apply(draws$sigma_player, 2L, stats::median)
  threshold_median <- apply(draws$threshold_player, 2L, stats::median)
  truth_sigma <- true_sigma[fit$player_table$batter_id]
  truth_threshold <- true_threshold[fit$player_table$batter_id]

  expect_gt(stats::cor(log(sigma_median), log(truth_sigma)), 0.7)
  expect_lt(sqrt(mean((log(sigma_median) - log(truth_sigma))^2)), 0.35)
  expect_gt(stats::cor(threshold_median, truth_threshold), 0.7)
  expect_lt(sqrt(mean((threshold_median - truth_threshold)^2)), 1)
  expect_lt(abs(stats::median(draws$anisotropy_ratio) - 1.4), 0.2)
})
