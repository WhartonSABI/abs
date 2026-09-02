test_that("pitch mixture uses the smallest candidate within one SE", {
  scores <- data.table::rbindlist(lapply(c(1L, 3L, 6L), function(k) {
    data.table::data.table(
      components = k,
      game_pk = 1:20,
      log_score = -3 + c(`1` = 0, `3` = 0.02, `6` = 0.021)[as.character(k)] +
        rep(c(-0.1, 0.1), 10)
    )
  }))
  chosen <- select_continuous_pitch_mixture_one_se(scores)
  expect_equal(chosen$components, 1L)
  expect_equal(chosen$best_components, 6L)
})

test_that("fold helpers enforce game separation", {
  folds <- continuous_game_folds(1:30, folds = 5, seed = 9)
  train <- continuous_training_games(folds, 2)
  test <- continuous_heldout_games(folds, 2)
  expect_true(assert_continuous_fold_separation(train, test))
  expect_error(assert_continuous_fold_separation(train, train[[1L]]), "overlap")
})

test_that("publication gates fail closed when metrics are absent", {
  gates <- build_continuous_validation_gates(list(fold_separation = TRUE))
  expect_true(gates[gate == "game_fold_separation", passed])
  expect_false(any(gates[gate != "game_fold_separation", passed]))
})

test_that("pilot sampling is deterministic and stratified", {
  x <- data.table::data.table(
    batter_id = rep(letters[1:3], each = 10), row = 1:30
  )
  one <- continuous_pilot_sample(x, 0.2, seed = 10, strata = "batter_id")
  two <- continuous_pilot_sample(x, 0.2, seed = 10, strata = "batter_id")
  expect_identical(one, two)
  expect_equal(one[, .N, by = batter_id]$N, rep(2L, 3L))
})

test_that("outcomes enter only in the final evaluation join", {
  scores <- data.table::data.table(
    game_pk = rep(1L, 3), pitch_order = rep(1L, 3), fold = rep(1L, 3),
    omega = c(0, 0.5, 1), challenged = rep(1L, 3),
    q_model_mean = c(0.6, 0.7, 0.8), p_challenge_mean = c(0.1, 0.2, 0.3),
    q_chosen_mean = c(0.65, 0.75, 0.85),
    q_chosen_lower_95 = c(0.5, 0.6, 0.7),
    q_chosen_median = c(0.65, 0.75, 0.85),
    q_chosen_upper_95 = c(0.8, 0.9, 0.95),
    p_challenge_lower_95 = c(0.05, 0.1, 0.2),
    p_challenge_median = c(0.1, 0.2, 0.3),
    p_challenge_upper_95 = c(0.2, 0.3, 0.4)
  )
  gate <- data.table::data.table(primary_omega = 0, reason = "fail closed")
  formatted <- format_continuous_human_decision_posteriors(scores, gate)
  expect_false("official_success" %in% names(formatted))
  labels <- data.table::data.table(
    game_pk = 1L, pitch_order = 1L,
    official_success = TRUE, geometry_success = TRUE
  )
  attached <- attach_continuous_outcome_evaluation(formatted, labels)
  expect_true(all(attached$official_success))
  expect_equal(attached$q_private_mean, rep(0.6, 3))
})

test_that("mixture Rosenblatt transform retains correlated component shape", {
  prior <- list(
    weights = 1, means = matrix(c(0, 0), 1, 2),
    scale = matrix(c(2, 3), 1, 2), correlation = 0.6
  )
  center <- continuous_pitch_prior_rosenblatt(prior, c(0, 0))
  expect_equal(center[["u_x"]], 0.5, tolerance = 1e-12)
  expect_equal(center[["u_z_given_x"]], 0.5, tolerance = 1e-12)
  shifted <- continuous_pitch_prior_rosenblatt(prior, c(2, 1.8))
  expect_gt(shifted[["u_x"]], 0.5)
  expect_equal(shifted[["u_z_given_x"]], 0.5, tolerance = 1e-10)
})

test_that("case-control decision sampling carries the population offset", {
  rows <- data.table::data.table(
    game_pk = 1:100, pitch_order = 1:100,
    batter_id = "b", bat_team_id = "t", initial_call = "called_strike",
    challenged = c(rep(1L, 10), rep(0L, 90)), stake_G = 1,
    inventory_loss = 1
  )
  sampled <- continuous_case_control_decision_sample(
    rows, zero_fraction = 0.25, seed = 2
  )
  expect_equal(sum(sampled$challenged), 10L)
  expect_true(all(sampled$sampling_offset == log(4)))
  expect_lt(nrow(sampled), nrow(rows))
})

workflow_trust_scores <- function(games = 20L, pitches = 4L) {
  rows <- data.table::CJ(game_pk = seq_len(games), pitch_order = seq_len(pitches))
  rows[, challenged := as.integer((game_pk + pitch_order) %% 2L == 0L)]
  make_stage <- function(omega, quality = c("baseline", "good", "neutral")) {
    quality <- match.arg(quality)
    probability <- switch(
      quality,
      baseline = rep(0.5, nrow(rows)),
      good = ifelse(rows$challenged == 1L, 0.82, 0.18),
      neutral = rep(0.5, nrow(rows))
    )
    rows[, .(
      game_pk, pitch_order,
      fold = (game_pk %% 2L) + 1L,
      omega = omega,
      challenged,
      q_model_mean = 0.55 + 0.1 * omega,
      q_model_sd = 0.04,
      q_model_lower_95 = 0.45 + 0.1 * omega,
      q_model_median = 0.55 + 0.1 * omega,
      q_model_upper_95 = 0.65 + 0.1 * omega,
      p_challenge_mean = probability,
      p_challenge_lower_95 = pmax(0.01, probability - 0.05),
      p_challenge_median = probability,
      p_challenge_upper_95 = pmin(0.99, probability + 0.05),
      q_chosen_mean = 0.65 + 0.05 * omega,
      q_chosen_lower_95 = 0.55 + 0.05 * omega,
      q_chosen_median = 0.65 + 0.05 * omega,
      q_chosen_upper_95 = 0.75 + 0.05 * omega
    )]
  }
  data.table::rbindlist(list(
    make_stage(0, "baseline"),
    make_stage(0.5, "good"),
    make_stage(1, "neutral")
  ))
}

workflow_estimated_trust_fold <- function(fold, fixed_scores, seed = fold) {
  set.seed(seed)
  fold_value <- as.integer(fold)
  scores <- fixed_scores[fold == fold_value & abs(omega) < 1e-12]
  scores[, `:=`(
    p_challenge_mean = ifelse(challenged == 1L, 0.9, 0.1),
    q_model_mean = 0.7,
    q_model_sd = 0.03,
    q_model_lower_95 = 0.64,
    q_model_median = 0.7,
    q_model_upper_95 = 0.76,
    q_chosen_mean = 0.76,
    q_chosen_lower_95 = 0.7,
    q_chosen_median = 0.76,
    q_chosen_upper_95 = 0.82,
    p_challenge_lower_95 = pmax(0.01, p_challenge_mean - 0.04),
    p_challenge_median = p_challenge_mean,
    p_challenge_upper_95 = pmin(0.99, p_challenge_mean + 0.04),
    omega = 0.7,
    trust_variant = "omega_estimated"
  )]
  draws <- cbind(
    omega_shared = pmin(1.05, pmax(0.35, stats::rnorm(600, 0.7, 0.08))),
    mu_player = stats::rnorm(600, -4, 0.2),
    tau_player = abs(stats::rnorm(600, 0.4, 0.05)),
    tau_team = abs(stats::rnorm(600, 0.2, 0.04)),
    decision_slope = exp(stats::rnorm(600, 0, 0.1)),
    `gamma[1]` = stats::rnorm(600),
    `alpha_player[1]` = stats::rnorm(600, -4, 0.3),
    `alpha_team[1]` = stats::rnorm(600, 0, 0.2)
  )
  structure(list(
    fold = as.integer(fold),
    status = "estimated_omega_fit_and_scored",
    scores = scores,
    posterior_draws = draws,
    grid_convergence = data.table::data.table(
      fold = fold, split = c("training", "heldout"), pass = TRUE
    ),
    diagnostics = data.table::data.table()
  ), class = "continuous_estimated_trust_fold")
}

test_that("fixed q uncertainty is summarized by aligned global draw blocks", {
  draw_map <- data.table::data.table(
    global_draw_id = 1:2,
    signal_draw_id = c(4L, 2L),
    prior_draw_id = c(3L, 1L),
    call_draw_id = c(7L, 5L)
  )
  q <- rbind(
    c(0.1, 0.3, 0.7, 0.9),
    c(0.2, 0.4, 0.4, 0.6)
  )
  weight <- matrix(0.25, nrow = 2L, ncol = 4L)
  summary <- summarize_continuous_signal_q_draw_blocks(
    q, weight, draw_map, nodes_per_draw = 2L
  )

  expect_equal(
    unname(summary$q_by_global_draw),
    rbind(c(0.2, 0.8), c(0.3, 0.5))
  )
  expect_equal(summary$mean, c(0.5, 0.4))
  expect_equal(summary$sd, apply(summary$q_by_global_draw, 1L, stats::sd))
  expect_identical(summary$draw_map, draw_map)

  unequal <- weight
  unequal[, 1:2] <- 0.2
  unequal[, 3:4] <- 0.3
  expect_error(
    summarize_continuous_signal_q_draw_blocks(
      q, unequal, draw_map, nodes_per_draw = 2L
    ),
    "equal posterior mass"
  )
})

test_that("fixed omega variants require identical training and heldout draw maps", {
  map <- data.table::data.table(
    global_draw_id = 1:2,
    signal_draw_id = c(2L, 1L),
    prior_draw_id = c(1L, 2L),
    call_draw_id = c(2L, 1L)
  )
  result <- list(
    decision_fits = lapply(c(0, 0.5, 1), function(omega) {
      list(omega = omega, draw_map = data.table::copy(map))
    }),
    sigma_zero_fit = list(draw_map = data.table::copy(map)),
    scoring_draw_maps = rep(list(data.table::copy(map)), 3L),
    sigma_zero_scoring_draw_map = data.table::copy(map)
  )
  class(result) <- "continuous_fixed_trust_fold"
  expect_true(assert_continuous_fixed_trust_draw_alignment(result))

  result$decision_fits[[2L]]$draw_map$signal_draw_id[[1L]] <- 99L
  expect_error(
    assert_continuous_fixed_trust_draw_alignment(result),
    "training draw maps are not aligned"
  )
})

test_that("fixed OOF trust scores gate estimated fitting by paired clustered SE", {
  scores <- workflow_trust_scores()
  gate <- continuous_fixed_trust_pre_gate(scores)
  expect_true(gate$summary$pass)
  expect_equal(gate$summary$best_fixed_omega, 0.5)
  expect_gte(gate$summary$improvement_in_se, 1)

  no_gain <- data.table::copy(scores)
  no_gain[omega > 0, p_challenge_mean := 0.5]
  failed <- continuous_fixed_trust_pre_gate(no_gain)
  expect_false(failed$summary$pass)

  fold_fit <- list(fold = 1L)
  skipped <- run_continuous_estimated_trust_fold(
    fold_fit,
    decision_rows = data.table::data.table(),
    folds = data.table::data.table(),
    profile = list(),
    pre_gate = failed,
    grid_function = function(...) stop("grid must not run"),
    fit_function = function(...) stop("fit must not run")
  )
  expect_s3_class(skipped, "continuous_estimated_trust_fold_skip")
  expect_equal(skipped$status, "skipped_fixed_pre_gate")
  expect_null(skipped$fit)
})

test_that("estimated trust promotion appends OOF rows and retains sensitivities", {
  fixed <- workflow_trust_scores()
  pre_gate <- continuous_fixed_trust_pre_gate(fixed)
  results <- list(
    workflow_estimated_trust_fold(1L, fixed, seed = 11L),
    workflow_estimated_trust_fold(2L, fixed, seed = 12L)
  )
  aggregated <- aggregate_continuous_estimated_trust(
    results, fixed, pre_gate
  )
  expect_true(aggregated$gate$pass)
  expect_true(aggregated$gate$informative_interval_pass)
  expect_true(aggregated$gate$identifiability_pass)
  expect_true(aggregated$gate$grid_convergence_pass)
  expect_equal(aggregated$gate$primary_stage_id, "omega_estimated")

  formatted <- format_continuous_human_decision_posteriors(
    fixed, aggregated$gate, aggregated$scores
  )
  expect_setequal(
    unique(formatted$trust_variant),
    c("omega_0", "omega_0p5", "omega_1", "omega_estimated")
  )
  expect_true(all(
    formatted[trust_variant == "omega_estimated", primary_trust_variant]
  ))
  expect_true(all(c(
    "q_private_sd", "q_private_q025", "q_private_q50", "q_private_q975",
    "q_call_sd", "q_call_q025", "q_call_q50", "q_call_q975"
  ) %in% names(formatted)))

  confounded <- results
  for (index in seq_along(confounded)) {
    confounded[[index]]$posterior_draws[, "decision_slope"] <-
      confounded[[index]]$posterior_draws[, "omega_shared"] +
      stats::rnorm(nrow(confounded[[index]]$posterior_draws), 0, 0.002)
  }
  failed <- aggregate_continuous_estimated_trust(confounded, fixed, pre_gate)
  expect_false(failed$gate$identifiability_pass)
  expect_equal(failed$gate$primary_stage_id, "omega_0")
  fallback <- format_continuous_human_decision_posteriors(
    fixed, failed$gate, failed$scores
  )
  expect_false("omega_estimated" %in% fallback$trust_variant)
  expect_true(all(fallback[trust_variant == "omega_0", primary_trust_variant]))
})

test_that("estimated trust grid failure defaults to omega zero", {
  fixed <- workflow_trust_scores()
  pre_gate <- continuous_fixed_trust_pre_gate(fixed)
  results <- list(
    workflow_estimated_trust_fold(1L, fixed, seed = 21L),
    workflow_estimated_trust_fold(2L, fixed, seed = 22L)
  )
  results[[2L]]$grid_convergence[split == "heldout", pass := FALSE]
  aggregated <- aggregate_continuous_estimated_trust(results, fixed, pre_gate)
  expect_false(aggregated$gate$grid_convergence_pass)
  expect_false(aggregated$gate$pass)
  expect_equal(aggregated$gate$selected_omega, 0)
})

test_that("prior manifest is explicit and target graph wires staged trust", {
  specification <- continuous_perception_prior_specifications(
    continuous_perception_profile("pilot")
  )
  expect_equal(
    specification$pitch_location_mixture$component_candidates,
    c(1L, 3L, 6L)
  )
  expect_match(specification$batter_perception$sigma$population_log_mean, "log\\(3")
  expect_match(specification$initial_call$edge_spline$basis, "natural cubic")
  expect_match(specification$challenge_decision$positive_utility_slope, "lognormal")
  expect_equal(specification$shared_call_trust$failure_default, "omega 0")

  target_file <- file.path(
    rprojroot::find_root(rprojroot::has_file("_targets.R")),
    "_targets_perception.R"
  )
  expect_silent(parse(file = target_file))
  source <- paste(readLines(target_file, warn = FALSE), collapse = "\n")
  expect_match(source, "perception_trust_pre_gate")
  expect_match(source, "run_continuous_estimated_trust_fold")
  expect_match(source, "aggregate_continuous_estimated_trust")
  expect_match(source, "continuous_global_draw_alignment_manifest")
  expect_match(source, "prior_specifications")
  expect_match(source, "swing_rows = perception_swing_rows")
})

test_that("season batter exposure is unique rather than summed over folds", {
  rows <- data.table::data.table(
    game_pk = c(1L, 1L, 2L),
    at_bat_number = c(1L, 1L, 2L),
    pitch_number = c(1L, 1L, 1L),
    batter_id = c("b1", "b1", "b2"),
    swing = c(1L, 1L, 0L),
    plate_x = 0,
    plate_z = 2.5,
    sz_top = 3.5,
    sz_bot = 1.5,
    balls = 0L,
    strikes = 0L,
    pitch_family = "fastball",
    matchup = "R-R"
  )
  exposure <- continuous_batter_season_exposure(rows)
  expect_equal(exposure[batter_id == "b1", training_opportunities], 1L)
  expect_equal(exposure[batter_id == "b1", training_swings], 1L)
  expect_equal(exposure[batter_id == "b2", training_opportunities], 1L)
})

test_that("pitch-family sensitivity aggregates heldout log-score differences", {
  validation <- list(list(
    swing_by_game = data.table::data.table(
      game_pk = 1:4, swing_improvement = rep(0.1, 4)
    ),
    call_by_game = data.table::data.table(
      game_pk = 1:4, call_improvement = rep(0.1, 4)
    ),
    call_calibration = data.table::data.table(fold = 1L, ece = 0.01),
    location_pit = data.table::data.table(
      fold = 1L, game_pk = 1:4,
      u_x = c(0.1, 0.35, 0.65, 0.9),
      u_z_given_x = c(0.15, 0.4, 0.6, 0.85)
    ),
    pitch_family_by_game = data.table::data.table(
      game_pk = 1:4,
      with_minus_without_pitch_family = c(0.02, 0.04, 0.06, 0.08)
    )
  ))
  result <- aggregate_continuous_component_validation(validation)
  expect_equal(result$pitch_family_log_score_difference, 0.05)
  expect_equal(
    result$pitch_family_log_score_difference_se,
    stats::sd(c(0.02, 0.04, 0.06, 0.08)) / 2
  )
  expect_equal(
    result$by_game$pitch_family_sensitivity$with_minus_without_pitch_family,
    c(0.02, 0.04, 0.06, 0.08)
  )
})
