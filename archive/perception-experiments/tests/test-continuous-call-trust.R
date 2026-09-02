call_trust_test_inputs <- function(n = 12L, signals = 2L) {
  omega <- seq(0, 1.5, by = 0.25)
  q <- array(NA_real_, dim = c(n, signals, length(omega)))
  base <- outer(seq(0.15, 0.35, length.out = n), seq(0, 0.04, length.out = signals), "+")
  slope <- outer(seq(0.08, 0.16, length.out = n), seq(0, 0.02, length.out = signals), "+")
  for (g in seq_along(omega)) q[, , g] <- base + slope * omega[[g]]
  weights <- matrix(rep(seq_len(signals), each = n), nrow = n)
  decisions <- data.table::data.table(
    row_id = seq_len(n),
    game_pk = rep(seq_len(max(2L, n %/% 2L)), each = 2L, length.out = n),
    batter_id = rep(c("p1", "p2", "p3"), length.out = n),
    bat_team_id = rep(c("t1", "t2"), length.out = n),
    challenged = rep(0:1, length.out = n),
    stake_G = seq(0.1, 0.4, length.out = n),
    inventory_loss = seq(0.05, 0.2, length.out = n),
    stake_z = seq(-1, 1, length.out = n),
    sampling_offset = 0,
    actual_wrong = rep(c(TRUE, FALSE), length.out = n),
    challenge_outcome = rep(c("overturned", "upheld"), length.out = n)
  )
  list(decisions = decisions, q = q, omega = omega, weights = weights)
}

test_that("continuous omega interpolation preserves signals and endpoints", {
  input <- call_trust_test_inputs()
  at_zero <- continuous_call_trust_interpolate(input$q, input$omega, 0)
  at_end <- continuous_call_trust_interpolate(input$q, input$omega, 1.5)
  between <- continuous_call_trust_interpolate(input$q, input$omega, 0.375)

  expect_equal(dim(between), dim(input$q)[1:2])
  expect_equal(at_zero, input$q[, , 1L])
  expect_equal(at_end, input$q[, , length(input$omega)])
  expect_equal(
    between,
    0.5 * input$q[, , 2L] + 0.5 * input$q[, , 3L],
    tolerance = 1e-14
  )
  expect_error(
    continuous_call_trust_interpolate(input$q, input$omega, 1.51),
    "inside the precomputed grid"
  )
})

test_that("grid convergence detects interpolation error without fitting", {
  linear <- call_trust_test_inputs()
  exact <- continuous_call_trust_grid_convergence(
    linear$q, linear$omega, linear$weights,
    coarse_stride = 2L, mean_tolerance = 1e-12, p99_tolerance = 1e-12
  )
  expect_true(exact$summary$pass)
  expect_lt(exact$summary$maximum_weighted_mean_absolute_error, 1e-14)

  curved <- linear$q
  for (g in seq_along(linear$omega)) {
    curved[, , g] <- pmin(0.99, curved[, , g] + 0.12 * linear$omega[[g]]^2)
  }
  inaccurate <- continuous_call_trust_grid_convergence(
    curved, linear$omega, linear$weights,
    coarse_stride = 2L, mean_tolerance = 1e-8, p99_tolerance = 1e-8
  )
  expect_false(inaccurate$summary$pass)
  expect_gt(inaccurate$summary$maximum_weighted_mean_absolute_error, 0)
})

test_that("Stan data contain decisions but exclude call outcomes", {
  input <- call_trust_test_inputs()
  bundle <- prepare_continuous_call_trust_data(
    input$decisions, input$q, input$omega, input$weights,
    covariates = "stake_z"
  )

  expect_equal(bundle$data$N, nrow(input$decisions))
  expect_equal(bundle$data$S, dim(input$q)[[2L]])
  expect_equal(bundle$data$G, length(input$omega))
  expect_equal(bundle$data$estimate_omega, 1L)
  expect_equal(bundle$data$gain, input$decisions$stake_G)
  expect_equal(bundle$data$inventory_loss, input$decisions$inventory_loss)
  expect_equal(
    bundle$data$q_grid[, 3L],
    as.vector(input$q[, , 3L])
  )
  expect_equal(rowSums(bundle$data$signal_weight), rep(1, bundle$data$N))
  expect_setequal(
    bundle$excluded_outcome_columns,
    c("actual_wrong", "challenge_outcome")
  )
  expect_length(
    intersect(names(bundle$data), continuous_call_trust_forbidden_outcomes()),
    0L
  )
  expect_false(any(c("actual_wrong", "challenge_outcome") %in% names(bundle$data)))
  expect_false(any(
    c("actual_wrong", "challenge_outcome") %in% names(bundle$decisions)
  ))

  altered <- data.table::copy(input$decisions)
  altered[, `:=`(
    actual_wrong = !actual_wrong,
    challenge_outcome = rev(challenge_outcome)
  )]
  altered_bundle <- prepare_continuous_call_trust_data(
    altered, input$q, input$omega, input$weights,
    covariates = "stake_z"
  )
  expect_identical(bundle$data, altered_bundle$data)
  expect_identical(bundle$decisions, altered_bundle$decisions)
  expect_error(
    prepare_continuous_call_trust_data(
      input$decisions, input$q, input$omega, input$weights,
      covariates = "actual_wrong"
    ),
    "Outcome fields cannot enter the decision likelihood"
  )
  expect_error(
    prepare_continuous_call_trust_data(
      input$decisions, input$q, input$omega, input$weights,
      gain_col = "actual_wrong"
    ),
    "Outcome fields cannot enter the decision likelihood"
  )

  fixed <- prepare_continuous_call_trust_data(
    input$decisions, input$q, input$omega, input$weights,
    covariates = "stake_z", fixed_omega = 0.5
  )
  expect_equal(fixed$data$estimate_omega, 0L)
  expect_equal(fixed$data$omega_fixed, 0.5)
})

test_that("fixed omega crossfit stages are deterministic and leakage-free", {
  input <- call_trust_test_inputs(n = 30L, signals = 2L)
  one <- continuous_call_trust_crossfit_plan(input$decisions, folds = 5L, seed = 42L)
  two <- continuous_call_trust_crossfit_plan(input$decisions, folds = 5L, seed = 42L)

  expect_equal(one, two)
  expect_setequal(unique(one$stages$omega), c(0, 0.5, 1))
  expect_equal(nrow(one$stages), 15L)
  expect_equal(nrow(one$estimated_stages), 5L)
  expect_equal(nrow(one$all_stages), 20L)
  for (fold in 1:5) {
    train <- continuous_call_trust_fold_slice(
      input$decisions, input$q, input$omega, input$weights,
      one$assignments, fold, "train"
    )
    test <- continuous_call_trust_fold_slice(
      input$decisions, input$q, input$omega, input$weights,
      one$assignments, fold, "test"
    )
    expect_length(intersect(train$games, test$games), 0L)
    expect_setequal(c(train$row_index, test$row_index), seq_len(nrow(input$decisions)))

    bundle <- prepare_continuous_call_trust_fold_bundle(
      input$decisions, input$q, input$omega, input$weights,
      one$assignments, fold, fixed_omega = 0.5,
      covariates = "stake_z"
    )
    expect_length(
      intersect(bundle$decisions$game_pk, as.integer(test$games)),
      0L
    )
  }

  estimated_bundle <- prepare_continuous_call_trust_stage_bundle(
    input$decisions, input$q, input$omega, input$weights,
    one$assignments, one$estimated_stages[fold == 1L],
    covariates = "stake_z"
  )
  expect_equal(estimated_bundle$data$estimate_omega, 1L)
  expect_equal(attr(estimated_bundle, "call_trust_stage_id"), "omega_estimated")
})

test_that("held-out scoring preserves the existing expected-utility decision model", {
  input <- call_trust_test_inputs(n = 6L, signals = 2L)
  players <- sort(unique(input$decisions$batter_id))
  teams <- sort(unique(input$decisions$bat_team_id))
  draws <- matrix(c(
    -2, 0.3, 0.2, 3, 0.5,
    rep(-2, length(players)), rep(0, length(teams)), 0.1
  ), nrow = 1L)
  colnames(draws) <- c(
    "mu_player", "tau_player", "tau_team", "decision_slope", "omega_shared",
    paste0("alpha_player[", seq_along(players), "]"),
    paste0("alpha_team[", seq_along(teams), "]"), "gamma[1]"
  )
  fit <- list(
    fit = draws,
    player_table = data.table::data.table(
      player_id = players, player_index = seq_along(players)
    ),
    team_table = data.table::data.table(
      team_id = teams, team_index = seq_along(teams)
    ),
    covariate_names = "stake_z",
    player_col = "batter_id", team_col = "bat_team_id",
    gain_col = "stake_G", inventory_loss_col = "inventory_loss",
    cluster_col = "game_pk", offset_col = "sampling_offset",
    omega_grid = input$omega
  )
  class(fit) <- "continuous_call_trust_fit"

  scored <- score_continuous_call_trust(
    fit, input$decisions, input$q, input$weights, ndraws = 1L
  )
  q <- continuous_call_trust_interpolate(input$q, input$omega, 0.5)
  weight <- input$weights / rowSums(input$weights)
  utility <- q * input$decisions$stake_G -
    (1 - q) * input$decisions$inventory_loss
  base <- -2 + 0.1 * input$decisions$stake_z
  expected <- rowSums(weight * stats::plogis(base + 3 * utility))

  expect_equal(scored$p_challenge_call_trust, expected, tolerance = 1e-12)
  expect_true(all(scored$q_chosen_call_trust_mean > rowSums(weight * q)))
})

test_that("promotion requires prediction, information, and identification gates", {
  set.seed(2026)
  rows <- data.table::CJ(game_pk = 1:20, observation = 1:8)
  rows[, `:=`(
    row_id = .I,
    challenged = as.integer((game_pk + observation) %% 2L == 0L)
  )]
  baseline <- rows[, .(
    model = "omega_0", row_id, game_pk, challenged, p_challenge = 0.5
  )]
  estimated <- rows[, .(
    model = "omega_estimated", row_id, game_pk, challenged,
    p_challenge = ifelse(challenged == 1L, 0.88, 0.12)
  )]
  fixed_half <- rows[, .(
    model = "omega_0p5", row_id, game_pk, challenged,
    p_challenge = ifelse(challenged == 1L, 0.7, 0.3)
  )]
  metrics <- continuous_call_trust_crossfit_metrics(
    data.table::rbindlist(list(baseline, estimated, fixed_half))
  )
  interpolation_check <- continuous_call_trust_grid_convergence(
    call_trust_test_inputs()$q,
    call_trust_test_inputs()$omega,
    call_trust_test_inputs()$weights
  )

  draws <- cbind(
    omega_shared = pmin(1.05, pmax(0.35, stats::rnorm(2000, 0.7, 0.1))),
    mu_player = stats::rnorm(2000, -4, 0.2),
    tau_player = abs(stats::rnorm(2000, 0.4, 0.05)),
    tau_team = abs(stats::rnorm(2000, 0.2, 0.04)),
    decision_slope = exp(stats::rnorm(2000, 0, 0.1)),
    `gamma[1]` = stats::rnorm(2000),
    `alpha_player[1]` = stats::rnorm(2000, -4, 0.3),
    `alpha_team[1]` = stats::rnorm(2000, 0, 0.2)
  )
  gate <- continuous_call_trust_promotion_gate(
    metrics, draws, grid_convergence = interpolation_check
  )
  expect_true(gate$improvement_pass)
  expect_true(gate$informative_interval_pass)
  expect_true(gate$identifiability_pass)
  expect_true(gate$pass)
  expect_equal(gate$status, "estimated_omega_promoted")
  expect_gt(gate$selected_omega, 0)

  confounded <- draws
  confounded[, "decision_slope"] <- confounded[, "omega_shared"] +
    stats::rnorm(nrow(confounded), 0, 0.005)
  failed_identification <- continuous_call_trust_promotion_gate(
    metrics, confounded, grid_convergence = interpolation_check
  )
  expect_false(failed_identification$identifiability_pass)
  expect_false(failed_identification$pass)
  expect_equal(failed_identification$selected_omega, 0)
  expect_equal(failed_identification$status, "default_omega_zero")

  no_gain <- data.table::copy(estimated)
  no_gain[, p_challenge := 0.5]
  no_gain_metrics <- continuous_call_trust_crossfit_metrics(
    data.table::rbindlist(list(baseline, no_gain))
  )
  failed_prediction <- continuous_call_trust_promotion_gate(
    no_gain_metrics, draws, grid_convergence = interpolation_check
  )
  expect_false(failed_prediction$improvement_pass)
  expect_false(failed_prediction$pass)
  expect_equal(failed_prediction$selected_omega, 0)

  failed_interpolation <- continuous_call_trust_promotion_gate(
    metrics, draws, grid_convergence = NULL
  )
  expect_false(failed_interpolation$grid_convergence_pass)
  expect_equal(failed_interpolation$selected_omega, 0)
})

test_that("continuous call-trust Stan program passes semantic parsing", {
  skip_if_not_installed("rstan")
  stan_file <- default_continuous_call_trust_stan_file()
  expect_true(file.exists(stan_file))
  parsed <- rstan::stanc(file = stan_file, allow_undefined = FALSE)
  expect_true(parsed$status)

  source <- paste(readLines(stan_file, warn = FALSE), collapse = "\n")
  expect_match(
    source,
    "vector<lower=0, upper=1\\.5>\\[estimate_omega\\] omega_free"
  )
  expect_match(source, "log_sum_exp\\(log_component\\)")
  expect_false(grepl("actual_wrong|challenge_outcome|is_overturned", source))
})

test_that("shared trust and utility threshold recover in slow simulation", {
  skip_if(Sys.getenv("RUN_SLOW_BAYES_TESTS") != "true")
  skip_if_not_installed("cmdstanr")
  configure_continuous_cmdstan()

  set.seed(20260825L)
  n <- 2400L
  signals <- 3L
  omega_grid <- seq(0, 1.5, by = 0.05)
  rows <- data.table::data.table(
    game_pk = rep(seq_len(240L), each = 10L),
    batter_id = paste0("b", rep(seq_len(24L), length.out = n)),
    bat_team_id = paste0("t", rep(seq_len(12L), length.out = n)),
    stake_G = stats::runif(n, 0.08, 0.9),
    inventory_loss = stats::runif(n, 0.01, 0.25),
    sampling_offset = 0
  )
  base <- stats::plogis(stats::rnorm(n, -0.5, 1))
  signal_shift <- c(-0.07, 0, 0.07)
  q_grid <- array(NA_real_, dim = c(n, signals, length(omega_grid)))
  for (grid_index in seq_along(omega_grid)) {
    for (signal_index in seq_len(signals)) {
      q_grid[, signal_index, grid_index] <- pmin(
        0.995,
        pmax(
          0.005,
          base + signal_shift[[signal_index]] +
            omega_grid[[grid_index]] * (0.04 + 0.16 * base * (1 - base))
        )
      )
    }
  }
  signal_weight <- matrix(1 / signals, n, signals)
  true_omega <- 0.7
  true_alpha <- -2.8
  true_slope <- 7
  q_true <- continuous_call_trust_interpolate(
    q_grid, omega_grid, true_omega
  )
  utility <- q_true * rows$stake_G -
    (1 - q_true) * rows$inventory_loss
  probability <- rowSums(
    signal_weight * stats::plogis(true_alpha + true_slope * utility)
  )
  rows[, challenged := stats::rbinom(.N, 1L, probability)]

  bundle <- prepare_continuous_call_trust_data(
    rows, q_grid, omega_grid, signal_weight,
    omega_prior_mean = 0.75, omega_prior_sd = 0.4
  )
  fit <- fit_continuous_call_trust(
    bundle,
    backend = "cmdstanr",
    chains = 2L,
    parallel_chains = 2L,
    iter_warmup = 750L,
    iter_sampling = 750L,
    seed = 20260825L,
    refresh = 0L
  )
  draws <- continuous_call_trust_draw_matrix(fit, ndraws = 1000L)
  recovered_threshold <- -draws[, "mu_player"] / draws[, "decision_slope"]
  true_threshold <- -true_alpha / true_slope

  expect_lt(abs(stats::median(draws[, "omega_shared"]) - true_omega), 0.2)
  expect_lt(abs(stats::median(recovered_threshold) - true_threshold), 0.12)
  expect_lt(
    abs(stats::cor(draws[, "omega_shared"], draws[, "decision_slope"])),
    0.8
  )
})
