synthetic_challenge_discrimination_1d_rows <- function(n = 200L) {
  index <- seq_len(n)
  margin <- seq(-2.9, 2.9, length.out = n)
  probability <- stats::pnorm((margin - 2.5) / 2.5)
  data.table::data.table(
    game_pk = 1000L + (index - 1L) %% 20L,
    pitch_order = 1L + (index - 1L) %/% 20L,
    batter_id = paste0("b", 1L + (index - 1L) %% 12L),
    bat_team_id = paste0("t", 1L + (index - 1L) %% 4L),
    initial_call = "called_strike",
    challenged = as.integer(stats::runif(n) < probability),
    edge_distance_inches = margin,
    stake_G = 0.10 + (index %% 5L) / 50,
    inventory_loss = 0.08 + (index %% 3L) / 50,
    sampling_offset = 0,
    inning = 1L + index %% 9L,
    balls_before = index %% 4L,
    strikes_before = index %% 3L,
    adverse_challenges_before = 1L + index %% 2L,
    score_margin = (index %% 7L) - 3L,
    pitch_family = c("fastball", "breaking", "offspeed")[
      1L + index %% 3L
    ],
    matchup = c("L-L", "L-R", "R-L", "R-R")[1L + index %% 4L],
    umpire_id = paste0("u", 1L + index %% 5L),
    catcher_id = paste0("c", 1L + index %% 8L)
  )
}

fake_challenge_discrimination_1d_fit <- function(
  bundle, threshold = 1, sigma = 2, rho = 0.25,
  team_shift = 0, umpire_shift = 0, catcher_shift = 0,
  draws = 8L
) {
  threshold_sd <- stats::sd(threshold)
  sigma_sd <- stats::sd(log(sigma))
  if (!is.finite(threshold_sd)) threshold_sd <- 0
  if (!is.finite(sigma_sd)) sigma_sd <- 0
  columns <- list(
    mu_threshold = rep(mean(threshold), draws),
    mu_log_sigma = rep(log(mean(sigma)), draws),
    `tau_player[1]` = rep(threshold_sd, draws),
    `tau_player[2]` = rep(sigma_sd, draws),
    rho_threshold_log_sigma = rep(rho, draws)
  )
  add_vector <- function(name, length, value) {
    value <- rep_len(value, length)
    if (!length) return(invisible(NULL))
    for (index in seq_len(length)) {
      columns[[paste0(name, "[", index, "]")]] <<- rep(value[[index]], draws)
    }
    invisible(NULL)
  }
  add_vector("threshold_player", nrow(bundle$player_table), threshold)
  add_vector("sigma_player", nrow(bundle$player_table), sigma)
  add_vector("beta_context", ncol(bundle$data$X), 0)
  add_vector("team_shift", nrow(bundle$team_table), team_shift)
  add_vector("umpire_shift", nrow(bundle$umpire_table), umpire_shift)
  add_vector("catcher_shift", nrow(bundle$catcher_table), catcher_shift)
  fit <- list(
    stan_fit = do.call(cbind, columns),
    sigma_model = bundle$sigma_model,
    player_table = bundle$player_table,
    team_table = bundle$team_table,
    umpire_table = bundle$umpire_table,
    catcher_table = bundle$catcher_table,
    context_specification = bundle$context_specification,
    context_columns = bundle$context_columns,
    context_design_columns = bundle$context_design_columns,
    training_games = sort(unique(bundle$rows$game_pk)),
    training_rows = nrow(bundle$rows),
    eligibility = bundle$eligibility,
    margin_limit_inches = bundle$margin_limit_inches,
    stake_log_odds_epsilon = bundle$stake_log_odds_epsilon,
    tail_diagnostics = bundle$tail_diagnostics
  )
  class(fit) <- "challenge_discrimination_1d_fit"
  fit
}

test_that("1D preparation is outcome-free and retains every eligible action", {
  set.seed(1)
  rows <- synthetic_challenge_discrimination_1d_rows(240L)
  rows[, `:=`(
    abs_call = rep(c("ball", "called_strike"), length.out = .N),
    challenge_outcome = rep(c("overturned", "confirmed"), length.out = .N),
    official_success = rep(c(TRUE, FALSE), length.out = .N),
    description = rep(c("ball", "called_strike"), length.out = .N)
  )]
  first <- prepare_challenge_discrimination_1d(rows)
  rows[, `:=`(
    abs_call = rev(abs_call),
    challenge_outcome = rev(challenge_outcome),
    official_success = !official_success,
    description = rev(description)
  )]
  second <- prepare_challenge_discrimination_1d(rows)

  expect_equal(first$data, second$data)
  expect_equal(first$data$N, 240L)
  expect_equal(first$eligibility$eligible_rows, 240L)
  expect_false(any(
    challenge_discrimination_1d_outcome_columns() %in% names(first$rows)
  ))
  expect_true(all(first$rows$taken_called_strike))
  expect_equal(first$data$margin, first$rows$edge_distance_inches)
  expect_equal(sum(first$data$challenged), sum(rows$challenged))
  expect_setequal(
    first$context_columns$numeric,
    c("challenge_cost_log_odds", "total_stake", "inning", "score_margin")
  )
})

test_that("exact rounded geometry defines the signed margin", {
  rows <- synthetic_challenge_discrimination_1d_rows(4L)
  rows[, `:=`(
    plate_x = c(0, 8.5 / 12, 10 / 12, 10 / 12),
    plate_z = c(2.5, 2.5, 2.5, 3.5 + 1.5 / 12),
    sz_top = 3.5,
    sz_bot = 1.5
  )]
  rows[, edge_distance_inches := abs_edge_distance_inches(
    plate_x, plate_z, sz_top, sz_bot
  )]
  bundle <- prepare_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character()
  )

  expect_equal(
    bundle$rows$margin_inches,
    abs_edge_distance_inches(
      bundle$rows$plate_x, bundle$rows$plate_z,
      bundle$rows$sz_top, bundle$rows$sz_bot
    )
  )
  expect_true(all(bundle$rows$margin_source == "exact_rounded_geometry"))

  rows[1L, edge_distance_inches := edge_distance_inches + 0.1]
  expect_error(
    prepare_challenge_discrimination_1d(
      rows, categorical_context = character(), numeric_context = character()
    ),
    "disagree"
  )
})

test_that("common and hierarchical sigma are nested through one data flag", {
  set.seed(2)
  rows <- synthetic_challenge_discrimination_1d_rows(120L)
  common <- prepare_challenge_discrimination_1d(rows, sigma_model = "common")
  hierarchical <- prepare_challenge_discrimination_1d(
    rows, sigma_model = "hierarchical"
  )

  expect_equal(common$data$use_player_sigma, 0L)
  expect_equal(hierarchical$data$use_player_sigma, 1L)
  expect_equal(common$data$margin, hierarchical$data$margin)
  expect_equal(common$data$challenged, hierarchical$data$challenged)
  expect_true(all(common$player_table$training_opportunities >= 1L))
  expect_true(any(common$player_table$reliability_tier == "limited"))
  expect_true(all(c(
    "fallback_flag", "fallback_reason", "exposure_reliability_weight"
  ) %in% names(common$player_table)))

  fake <- fake_challenge_discrimination_1d_fit(
    common, threshold = 2, sigma = 3
  )
  summary <- summarize_challenge_discrimination_1d(fake)
  expect_equal(unique(summary$players$sigma_inches_median), 3)
  expect_true(all(summary$players$estimate_source == "league_common_sigma"))
})

test_that("cache identities bind data games backend seed and Stan source", {
  rows <- synthetic_challenge_discrimination_1d_rows(120L)
  bundle <- prepare_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character()
  )
  stan_file <- default_challenge_discrimination_1d_stan_file()
  identity <- challenge_discrimination_1d_cache_identity(
    bundle, backend = "cmdstanr", seed = 123L, stan_file = stan_file,
    sampling_controls = challenge_discrimination_1d_sampling_identity(
      4L, 1000L, 1000L, 0.97, 12L
    )
  )
  shuffled <- prepare_challenge_discrimination_1d(
    rows[sample(.N)], categorical_context = character(),
    numeric_context = character()
  )
  expect_identical(
    challenge_discrimination_1d_training_fingerprint(bundle),
    challenge_discrimination_1d_training_fingerprint(shuffled)
  )

  cache_file <- tempfile(fileext = ".rds")
  cached <- structure(
    list(cache_identity = identity, cache_marker = "safe-reuse"),
    class = "challenge_discrimination_1d_fit"
  )
  saveRDS(cached, cache_file)
  reused <- fit_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character(),
    backend = "cmdstanr", seed = 123L, stan_file = stan_file,
    file = cache_file
  )
  expect_equal(reused$cache_marker, "safe-reuse")

  changed <- data.table::copy(rows)
  changed[1L, challenged := 1L - challenged]
  expect_error(
    fit_challenge_discrimination_1d(
      changed, categorical_context = character(), numeric_context = character(),
      backend = "cmdstanr", seed = 123L, stan_file = stan_file,
      file = cache_file
    ),
    "cache identity mismatch"
  )
  expect_error(
    fit_challenge_discrimination_1d(
      rows, categorical_context = character(), numeric_context = character(),
      backend = "cmdstanr", seed = 124L, stan_file = stan_file,
      file = cache_file
    ),
    "cache identity mismatch"
  )
  expect_error(
    fit_challenge_discrimination_1d(
      rows, categorical_context = character(), numeric_context = character(),
      backend = "cmdstanr", seed = 123L, stan_file = stan_file,
      iter_sampling = 999L, file = cache_file
    ),
    "cache identity mismatch"
  )
  expect_error(
    fit_challenge_discrimination_1d(
      rows, categorical_context = character(), numeric_context = character(),
      backend = "rstan", seed = 123L, stan_file = stan_file,
      file = cache_file
    ),
    "cache identity mismatch"
  )

  changed_stan <- tempfile(fileext = ".stan")
  expect_true(file.copy(stan_file, changed_stan))
  writeLines(
    c(readLines(changed_stan), "// cache identity test"), changed_stan
  )
  expect_false(identical(
    challenge_discrimination_1d_stan_fingerprint(stan_file),
    challenge_discrimination_1d_stan_fingerprint(changed_stan)
  ))
})

test_that("thinning preserves source posterior draw IDs", {
  rows <- synthetic_challenge_discrimination_1d_rows(60L)
  bundle <- prepare_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character()
  )
  fit <- fake_challenge_discrimination_1d_fit(bundle, draws = 20L)
  source_ids <- 1001:1020
  attr(fit$stan_fit, "source_draw_id") <- source_ids
  set.seed(44L)
  expected <- source_ids[sort(sample.int(20L, 5L))]

  posterior <- draws_challenge_discrimination_1d(
    fit, ndraws = 5L, seed = 44L
  )
  scored <- score_challenge_discrimination_1d(
    fit, rows, ndraws = 5L, seed = 44L,
    return_draws = TRUE, allow_in_sample = TRUE
  )
  expect_identical(posterior$draw_id, expected)
  expect_identical(scored$draw_id, expected)
  expect_false(identical(posterior$draw_id, seq_len(5L)))
})

test_that("the primary likelihood is explicitly local and tails stay diagnostic", {
  rows <- synthetic_challenge_discrimination_1d_rows(7L)
  rows[, edge_distance_inches := c(-5, -3, -2, 0, 3, 3.1, 6)]
  local <- prepare_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character()
  )
  full <- prepare_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character(),
    margin_limit_inches = Inf
  )

  expect_equal(local$margin_limit_inches, 3)
  expect_equal(local$data$N, 4L)
  expect_equal(nrow(local$tail_rows), 3L)
  expect_equal(sum(local$tail_diagnostics$tail_opportunities), 3L)
  expect_true(all(abs(local$rows$margin_inches) <= 3))
  expect_equal(local$eligibility$model_rows, 4L)
  expect_equal(local$eligibility$tail_diagnostic_rows, 3L)
  expect_equal(full$margin_limit_inches, Inf)
  expect_equal(full$data$N, 7L)

  fit <- fake_challenge_discrimination_1d_fit(local)
  scored <- score_challenge_discrimination_1d(
    fit, rows, allow_in_sample = TRUE
  )
  expect_equal(nrow(scored), 4L)
  expect_true(all(scored$local_discrimination_domain))
  expect_equal(unique(scored$margin_limit_inches), 3)
  expect_equal(sum(attr(scored, "tail_diagnostics")$tail_opportunities), 3L)
})

test_that("break-even challenge cost is coherent and log-odds clipping is recorded", {
  rows <- synthetic_challenge_discrimination_1d_rows(4L)
  rows[, `:=`(
    edge_distance_inches = 0,
    stake_G = c(1, 0.8, 0.2, 0),
    inventory_loss = c(0, 0.2, 0.8, 1)
  )]
  normalized <- normalize_challenge_discrimination_1d_rows(rows)

  expect_equal(normalized$q_star, c(0, 0.2, 0.8, 1))
  expect_equal(normalized$total_stake, rep(1, 4L))
  expect_true(all(normalized$q_star_log_odds_clipped[c(1L, 4L)]))
  expect_false(any(normalized$q_star_log_odds_clipped[c(2L, 3L)]))
  expect_true(all(is.finite(normalized$challenge_cost_log_odds)))
  expect_equal(
    unique(normalized$q_star_log_odds_epsilon),
    challenge_discrimination_1d_stake_epsilon()
  )

  simulated <- simulate_challenge_discrimination_1d(
    rows[2:3], threshold_player = 0, sigma_player = 2,
    context_beta = 1, categorical_context = character(),
    numeric_context = "challenge_cost_log_odds", seed = 9L
  )
  ordered <- simulated[order(q_star)]
  expect_gt(
    ordered$simulation_context_shift_inches[[2L]],
    ordered$simulation_context_shift_inches[[1L]]
  )
  expect_lt(
    ordered$challenge_probability[[2L]],
    ordered$challenge_probability[[1L]]
  )
})

test_that("case-control offsets and outcome contexts are rejected", {
  rows <- synthetic_challenge_discrimination_1d_rows(100L)
  rows[1L, sampling_offset := 1]
  expect_error(prepare_challenge_discrimination_1d(rows), "all eligible")

  rows[, sampling_offset := 0]
  rows[, official_success := challenged == 1L]
  expect_error(
    prepare_challenge_discrimination_1d(
      rows, categorical_context = "official_success",
      numeric_context = character()
    ),
    "Invalid or outcome-bearing"
  )
})

test_that("scoring is the fixed-slope probit with additive threshold shifts", {
  rows <- synthetic_challenge_discrimination_1d_rows(24L)
  bundle <- prepare_challenge_discrimination_1d(
    rows, sigma_model = "hierarchical",
    categorical_context = character(), numeric_context = character()
  )
  fit <- fake_challenge_discrimination_1d_fit(
    bundle, threshold = 1, sigma = 2,
    team_shift = 0.5, umpire_shift = 0.25, catcher_shift = -0.25
  )
  scored <- score_challenge_discrimination_1d(
    fit, rows, ndraws = 8L, return_draws = TRUE,
    allow_in_sample = TRUE
  )
  expected <- stats::pnorm((scored$summary$margin_inches - 1.5) / 2)

  expect_equal(
    scored$summary$challenge_probability_mean, expected,
    tolerance = 1e-12
  )
  expect_equal(scored$sigma_inches, matrix(2, nrow(rows), 8L))
  expect_equal(scored$total_threshold_inches, matrix(1.5, nrow(rows), 8L))
  expect_true(all(scored$summary$batter_seen_in_training))
  expect_false(any(scored$summary$fallback_flag))
})

test_that("unseen players and groups use explicit population fallbacks", {
  rows <- synthetic_challenge_discrimination_1d_rows(40L)
  bundle <- prepare_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character()
  )
  fit <- fake_challenge_discrimination_1d_fit(
    bundle, threshold = 1, sigma = 2,
    team_shift = 3, umpire_shift = 3, catcher_shift = 3
  )
  heldout <- data.table::copy(rows[1L])
  heldout[, `:=`(
    game_pk = "new-game", batter_id = "new-batter", bat_team_id = "new-team",
    umpire_id = "new-umpire", catcher_id = "new-catcher"
  )]
  scored <- score_challenge_discrimination_1d(fit, heldout, ndraws = 8L)

  expect_false(scored$batter_seen_in_training)
  expect_false(scored$team_seen_in_training)
  expect_true(scored$fallback_flag)
  expect_equal(scored$fallback_source, "population_mean")
  expect_equal(
    scored$challenge_probability_mean,
    stats::pnorm((scored$margin_inches - 1) / 2), tolerance = 1e-12
  )
})

test_that("simulation uses the same physical-inch probit and carries truth", {
  rows <- synthetic_challenge_discrimination_1d_rows(300L)
  simulated <- simulate_challenge_discrimination_1d(
    rows, threshold_player = 0.5, sigma_player = 2,
    context_beta = 0, categorical_context = character(),
    numeric_context = character(), seed = 11L
  )
  expected <- stats::pnorm((simulated$margin_inches - 0.5) / 2)
  truth <- attr(simulated, "challenge_discrimination_1d_truth")

  expect_equal(simulated$challenge_probability, expected)
  expect_true(all(simulated$challenged %in% 0:1))
  expect_true(all(diff(
    simulated$challenge_probability[order(simulated$margin_inches)]
  ) > 0))
  expect_equal(unique(truth$players$threshold_inches), 0.5)
  expect_equal(unique(truth$players$sigma_inches), 2)
})

test_that("recovery validation checks threshold sigma and correlation", {
  rows <- synthetic_challenge_discrimination_1d_rows(160L)
  bundle <- prepare_challenge_discrimination_1d(
    rows, sigma_model = "hierarchical",
    categorical_context = character(), numeric_context = character()
  )
  player_count <- nrow(bundle$player_table)
  threshold <- seq(1, 4, length.out = player_count)
  sigma <- seq(1.5, 3.5, length.out = player_count)
  fit <- fake_challenge_discrimination_1d_fit(
    bundle, threshold = threshold, sigma = sigma, rho = 0.3, draws = 20L
  )
  truth <- list(
    players = data.table::data.table(
      batter_id = bundle$player_table$batter_id,
      threshold_inches = threshold,
      sigma_inches = sigma
    ),
    hyperparameters = list(rho_threshold_log_sigma = 0.3)
  )
  recovery <- validate_challenge_discrimination_1d_recovery(fit, truth)

  expect_true(recovery$metrics$threshold_recovery_pass)
  expect_true(recovery$metrics$sigma_recovery_pass)
  expect_true(recovery$metrics$correlation_recovery_pass)
  expect_true(recovery$metrics$recovery_pass)
  expect_equal(recovery$metrics$threshold_correlation, 1)
  expect_equal(recovery$metrics$log_sigma_correlation, 1)
})

test_that("five-fold helpers separate games and cover every eligible pitch", {
  rows <- synthetic_challenge_discrimination_1d_rows(400L)
  folds <- make_challenge_discrimination_1d_game_folds(
    rows, folds = 5L, seed = 42L
  )
  again <- make_challenge_discrimination_1d_game_folds(
    rows, folds = 5L, seed = 42L
  )
  split <- split_challenge_discrimination_1d_fold(rows, folds, 3L)

  expect_equal(folds, again)
  expect_equal(nrow(folds), data.table::uniqueN(rows$game_pk))
  expect_setequal(unique(folds$fold), 1:5)
  expect_equal(unique(folds$margin_limit_inches), 3)
  expect_equal(
    unique(folds$stake_log_odds_epsilon),
    challenge_discrimination_1d_stake_epsilon()
  )
  expect_equal(sum(folds$eligible_margin_rows), nrow(rows))
  expect_equal(sum(folds$local_model_rows), nrow(rows))
  expect_equal(sum(folds$tail_diagnostic_rows), 0L)
  expect_length(
    intersect(unique(split$train$game_pk), unique(split$heldout$game_pk)), 0L
  )
  expect_equal(nrow(split$train) + nrow(split$heldout), nrow(rows))
  expect_error(
    split_challenge_discrimination_1d_fold(
      rows, folds, 3L, margin_limit_inches = 2
    ),
    "recorded"
  )
})

test_that("cross-fit splits retain full tails and partition them downstream", {
  rows <- synthetic_challenge_discrimination_1d_rows(400L)
  tail_only_game <- as.character(rows$game_pk[[1L]])
  rows[as.character(game_pk) == tail_only_game, edge_distance_inches := 5]
  rows[as.character(game_pk) != tail_only_game & pitch_order %% 7L == 0L,
    edge_distance_inches := -5]
  eligible <- normalize_challenge_discrimination_1d_rows(rows)
  folds <- make_challenge_discrimination_1d_game_folds(
    rows, folds = 5L, seed = 47L
  )
  heldout_fold <- folds[game_pk == tail_only_game, fold][[1L]]
  split <- split_challenge_discrimination_1d_fold(
    rows, folds, heldout_fold
  )

  expect_true(tail_only_game %in% folds$game_pk)
  expect_equal(sum(folds$eligible_margin_rows), nrow(eligible))
  expect_equal(
    sum(folds$local_model_rows + folds$tail_diagnostic_rows),
    nrow(eligible)
  )
  expect_gt(sum(folds$tail_diagnostic_rows), 0L)
  expect_equal(
    nrow(split$train) + nrow(split$heldout), nrow(eligible)
  )
  expect_true(any(abs(split$train$margin_inches) > 3))
  expect_true(any(abs(split$heldout$margin_inches) > 3))
  expect_equal(
    split$split_summary$eligible_margin_rows,
    split$split_summary$local_model_rows +
      split$split_summary$tail_diagnostic_rows
  )
  expect_equal(
    sum(split$heldout_tail_diagnostics$tail_opportunities),
    split$split_summary[split == "heldout", tail_diagnostic_rows]
  )

  training_bundle <- prepare_challenge_discrimination_1d(
    split$train, categorical_context = character(),
    numeric_context = character()
  )
  expect_equal(
    training_bundle$eligibility$tail_diagnostic_rows,
    split$split_summary[split == "training", tail_diagnostic_rows]
  )
  fit <- fake_challenge_discrimination_1d_fit(training_bundle)
  scored <- score_challenge_discrimination_1d(
    fit, split$heldout, ndraws = 8L
  )
  expect_equal(
    nrow(scored),
    split$split_summary[split == "heldout", local_model_rows]
  )
  expect_true(all(abs(scored$margin_inches) <= 3))
  expect_equal(
    sum(attr(scored, "tail_diagnostics")$tail_opportunities),
    split$split_summary[split == "heldout", tail_diagnostic_rows]
  )
})

test_that("held-out scoring rejects overlapping training games", {
  rows <- synthetic_challenge_discrimination_1d_rows(80L)
  bundle <- prepare_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character()
  )
  fit <- fake_challenge_discrimination_1d_fit(bundle)
  expect_error(
    score_challenge_discrimination_1d(fit, rows),
    "overlap"
  )
  in_sample <- score_challenge_discrimination_1d(
    fit, rows, allow_in_sample = TRUE
  )
  expect_true(all(
    in_sample$scoring_regime == "in_sample_explicit_opt_out"
  ))
})

test_that("held-out promotion requires one game-clustered SE and recovery", {
  games <- rep(1:10, each = 20L)
  observed <- rep(c(0L, 1L), 100L)
  common <- data.table::data.table(
    game_pk = games,
    pitch_order = ave(seq_along(games), games, FUN = seq_along),
    challenged = observed,
    challenge_probability_mean = ifelse(observed == 1L, 0.60, 0.40)
  )
  hierarchical <- data.table::copy(common)
  hierarchical[, challenge_probability_mean := ifelse(
    challenged == 1L, 0.90, 0.10
  )]
  comparison <- compare_challenge_discrimination_1d_heldout(
    common, hierarchical
  )
  recovery <- data.table::data.table(recovery_pass = TRUE)
  gate <- gate_challenge_discrimination_1d(comparison, recovery)

  expect_true(comparison$metrics$one_game_clustered_se_pass)
  expect_true(gate$predictive_pass)
  expect_true(gate$recovery_pass)
  expect_true(gate$promote_hierarchical_sigma)
  expect_equal(gate$selected_sigma_model, "hierarchical")

  recovery[, recovery_pass := FALSE]
  failed <- gate_challenge_discrimination_1d(comparison, recovery)
  expect_false(failed$promote_hierarchical_sigma)
  expect_equal(failed$selected_sigma_model, "common")

  common[, `:=`(margin_limit_inches = 3, margin_inches = 0)]
  hierarchical[, `:=`(margin_limit_inches = 3, margin_inches = 0)]
  hierarchical[1L, margin_inches := 4]
  expect_error(
    compare_challenge_discrimination_1d_heldout(common, hierarchical),
    "local margin"
  )
})

test_that("1D Stan program parses and has no free slope or lapse", {
  program <- readLines(default_challenge_discrimination_1d_stan_file())
  expect_true(any(grepl("use_player_sigma", program, fixed = TRUE)))
  expect_false(any(grepl("decision_slope", program, fixed = TRUE)))
  expect_false(any(grepl("lapse", program, ignore.case = TRUE)))
  expect_false(any(grepl("generated quantities", program, fixed = TRUE)))
  expect_false(any(grepl("vector[N] log_lik", program, fixed = TRUE)))
  expect_false(any(grepl(
    "vector[N] challenge_probability", program, fixed = TRUE
  )))
  expect_true(any(grepl(
    "use_player_sigma * L_player[2, 1]", program, fixed = TRUE
  )))
  expect_false(any(grepl(
    "multiply_lower_tri_self_transpose", program, fixed = TRUE
  )))
  expect_false(any(grepl("corr_matrix[2]", program, fixed = TRUE)))

  skip_if_not_installed("rstan")
  parsed <- rstan::stanc(
    file = default_challenge_discrimination_1d_stan_file(),
    allow_undefined = FALSE
  )
  expect_true(parsed$status)
})

test_that("hierarchical simulation recovery runs in slow Bayesian tests", {
  skip_if(Sys.getenv("RUN_SLOW_BAYES_TESTS") != "true")
  skip_if_not_installed("cmdstanr")
  configure_continuous_cmdstan()

  rows <- synthetic_challenge_discrimination_1d_rows(6000L)
  simulated <- simulate_challenge_discrimination_1d(
    rows, mu_threshold = 0, mu_log_sigma = log(1.5),
    tau_threshold = 0.5, tau_log_sigma = 0.15,
    rho_threshold_log_sigma = 0.3, context_beta = 0,
    categorical_context = character(), numeric_context = character(),
    seed = 20260825L
  )
  fit <- fit_challenge_discrimination_1d(
    simulated, sigma_model = "hierarchical",
    categorical_context = character(), numeric_context = character(),
    backend = "cmdstanr", chains = 2L, parallel_chains = 2L,
    iter_warmup = 500L, iter_sampling = 500L, refresh = 0L,
    seed = 20260825L
  )
  recovery <- validate_challenge_discrimination_1d_recovery(fit, simulated)
  expect_true(recovery$metrics$threshold_recovery_pass)
  expect_true(recovery$metrics$sigma_recovery_pass)
})
