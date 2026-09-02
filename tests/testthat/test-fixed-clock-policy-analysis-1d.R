fixed_clock_analysis_test_clock_1d <- function(prefix = "g", games = 4L) {
  data.table::rbindlist(lapply(seq_len(games), function(index) {
    data.table::data.table(
      game_pk = paste0(prefix, index),
      team_id = paste0("t", index),
      pitch_order = 1:6,
      inning = c(1L, 2L, 4L, 6L, 8L, 10L),
      stage = c(0L, 7L, 19L, 31L, 43L, 55L),
      role = rep(c("offense", "defense"), 3L),
      count_state = rep(c("0-0", "1-1"), 3L),
      stake_G = c(0.2, 0.3, 0.25, 0.35, 0.15, 0.4),
      decision_mode = "structural"
    )
  }))
}

fixed_clock_analysis_test_prior_1d <- function(role, games) {
  structure(list(
    components = 1L,
    weight = 1,
    mean = if (role == "offense") -0.1 else 0.1,
    sd = 1.1,
    context_column = "count_state",
    context_weights = matrix(
      1, nrow = 2L,
      dimnames = list(c("0-0", "1-1"), "component_1")
    ),
    fold_id = paste0("analysis_", role),
    global_draw_id = 1L,
    training_games = sort(unique(as.character(games))),
    training_fingerprint = paste0("analysis_", role)
  ), class = "challenge_margin_prior_1d_fit")
}

fixed_clock_analysis_test_priors_1d <- function(clock) {
  list(
    offense = fixed_clock_analysis_test_prior_1d("offense", clock$game_pk),
    defense = fixed_clock_analysis_test_prior_1d("defense", clock$game_pk)
  )
}

fixed_clock_analysis_test_truth_1d <- function(clock) {
  clock[, .(
    game_pk, team_id, pitch_order, role,
    role_margin_inches = rep(c(0.5, -0.4, 0.7), length.out = .N)
  )]
}

test_that("public benchmark is location-free and separates decision/evaluation G", {
  training <- fixed_clock_analysis_test_clock_1d("train", 3L)
  fit <- fit_fixed_clock_public_policy_1d(
    training,
    fixed_clock_analysis_test_priors_1d(training),
    stage_df = 2L,
    optimizer_control = list(maxit = 1L, reltol = 1e-5)
  )
  frozen <- freeze_fixed_clock_policy_1d(fit, fit$training_games)
  confirmation <- fixed_clock_analysis_test_clock_1d("confirm", 2L)
  truth <- fixed_clock_analysis_test_truth_1d(confirmation)
  primary <- evaluate_fixed_clock_public_policy_1d(
    frozen, confirmation, truth
  )
  gains <- confirmation[, .(
    game_pk, team_id, pitch_order, G_evaluation = 2 * stake_G
  )]
  rescored <- evaluate_fixed_clock_public_policy_1d(
    frozen, confirmation, truth, evaluation_gain = gains
  )

  expect_s3_class(fit, "fixed_clock_public_policy_1d")
  expect_equal(
    primary$replay$expected_challenges,
    rescored$replay$expected_challenges
  )
  expect_equal(
    rescored$replay$expected_captured_re,
    2 * primary$replay$expected_captured_re,
    tolerance = 1e-12
  )
  expect_false(any(primary$replay$truth_used_by_decision_rule))
  expect_false(any(primary$replay$geometry_used_for_signal_integration))
})

test_that("game bootstrap expansion duplicates complete chronological games", {
  rows <- fixed_clock_analysis_test_clock_1d("g", 2L)
  expanded <- expand_fixed_clock_game_bootstrap_1d(
    rows,
    data.table::data.table(
      game_pk = c("g1", "g2"), bootstrap_weight = c(2L, 0L)
    )
  )
  expect_equal(data.table::uniqueN(expanded$game_pk), 2L)
  expect_equal(nrow(expanded), 2L * nrow(rows[game_pk == "g1"]))
  expect_equal(
    expanded[, .N, by = game_pk]$N,
    rep(nrow(rows[game_pk == "g1"]), 2L)
  )
  expect_true(all(expanded[, all(diff(pitch_order) > 0), by = game_pk]$V1))
  expect_error(
    expand_fixed_clock_game_bootstrap_1d(
      rows,
      data.table::data.table(
        game_pk = c("g1", "g2"), bootstrap_weight = c(1.5, 0.5)
      )
    ),
    "finite non-negative integers"
  )
})

test_that("observed fixed-clock actions fail closed on missing values", {
  rows <- fixed_clock_analysis_test_clock_1d("observed", 1L)
  rows[, observed_team_challenge := c(TRUE, FALSE, NA, FALSE, FALSE, FALSE)]
  expect_error(
    fixed_clock_policy_observed_actions_1d(rows),
    "non-missing and binary"
  )
})

test_that("direct smoothing selection uses held-out development games", {
  clock <- fixed_clock_analysis_test_clock_1d("cv", 4L)
  folds <- data.table::data.table(
    game_pk = sort(unique(clock$game_pk)), fold = rep(1:2, each = 2L)
  )
  priors <- lapply(1:2, function(index) {
    fixed_clock_analysis_test_priors_1d(clock[!game_pk %in%
      folds[fold == index, game_pk]])
  })
  names(priors) <- as.character(1:2)
  widths <- data.table::CJ(fold = 1:2, role = c("offense", "defense"))
  widths[, sigma_inches := ifelse(role == "offense", 1.1, 1.2)]
  scenario <- data.table::data.table(
    scenario_id = "middle",
    offense_kappa = 0.5,
    defense_kappa = 0.5
  )
  checkpoint_dir <- tempfile("fixed-clock-direct-cv-")
  on.exit(unlink(checkpoint_dir, recursive = TRUE), add = TRUE)
  cv <- cross_validate_fixed_clock_direct_policy_1d(
    clock,
    folds,
    fold_prior_fits = priors,
    width_estimates = widths,
    scenarios = scenario,
    tuning_grid = data.table::data.table(stage_df = 1L, ridge = 1e-3),
    lookup_grid_step = 0.5,
    optimizer_control = list(maxit = 1L, reltol = 1e-5),
    checkpoint_dir = checkpoint_dir,
    checkpoint_key = "synthetic",
    progress = FALSE
  )
  expect_equal(cv$selected$stage_df, 1L)
  expect_equal(nrow(cv$fold_scores), 2L)
  expect_equal(cv$scenario_scores$fold, 1:2)
  expect_true(all(is.finite(
    cv$scenario_scores$expected_re_per_team_game
  )))
  expect_equal(length(list.files(
    file.path(checkpoint_dir, "synthetic"), pattern = "[.]rds$"
  )), 2L)
  resumed <- cross_validate_fixed_clock_direct_policy_1d(
    clock,
    folds,
    fold_prior_fits = priors,
    width_estimates = widths,
    scenarios = scenario,
    tuning_grid = data.table::data.table(stage_df = 1L, ridge = 1e-3),
    lookup_grid_step = 0.5,
    optimizer_control = list(maxit = 1L, reltol = 1e-5),
    checkpoint_dir = checkpoint_dir,
    checkpoint_key = "synthetic",
    progress = FALSE
  )
  expect_equal(resumed$scenario_scores, cv$scenario_scores)
})

test_that("persistent bias is integrated before inventory paths are averaged", {
  training <- fixed_clock_analysis_test_clock_1d("bias_train", 2L)
  scenario <- data.table::data.table(
    scenario_id = "middle",
    offense_kappa = 0.5,
    defense_kappa = 0.5
  )
  fit <- fit_fixed_clock_direct_policy_1d(
    training,
    fixed_clock_analysis_test_priors_1d(training),
    scenarios = scenario,
    effective_width = c(offense = 1.1, defense = 1.2),
    stage_df = 1L,
    lookup_grid_step = 0.5,
    optimizer_control = list(maxit = 1L, reltol = 1e-5),
    progress = FALSE
  )
  frozen <- freeze_fixed_clock_policy_1d(fit, fit$training_games)
  confirmation <- fixed_clock_analysis_test_clock_1d("bias_confirm", 1L)
  truth <- fixed_clock_analysis_test_truth_1d(confirmation)
  sensitivity <- evaluate_fixed_clock_policy_bias_sensitivity_1d(
    frozen,
    confirmation,
    truth,
    bias_sd_inches = c(offense = 0, defense = 0),
    scenario_ids = "middle",
    quadrature_nodes = 3L
  )
  expect_equal(sum(sensitivity$quadrature$quadrature_weight), 1)
  expect_equal(
    sum(sensitivity$game_role$captured_re),
    sum(sensitivity$baseline$game_role$captured_re),
    tolerance = 1e-10
  )
  expect_match(sensitivity$information_regime, "before")
})

test_that("bootstrap_fixed_clock_policy_1d coordinates all three sources", {
  sources <- list(
    historical_re = paste0("h", 1:3),
    development = paste0("d", 1:2),
    confirmation = paste0("c", 1:2)
  )
  history <- function(weights, replicate_id, seed, context) {
    list(weight = sum(weights$bootstrap_weight))
  }
  development <- function(
    weights, historical_re_fit, replicate_id, seed, context
  ) {
    list(weight = sum(weights$bootstrap_weight) + historical_re_fit$weight)
  }
  score <- function(
    weights, historical_re_fit, development_fit,
    replicate_id, seed, context
  ) {
    data.table::data.table(
      policy = "frozen",
      score = sum(weights$bootstrap_weight) + development_fit$weight
    )
  }
  result <- bootstrap_fixed_clock_policy_1d(
    sources, history, development, score,
    reps = 2L, seed = 44L, workers = 1L
  )
  expect_equal(result$replicate_ids, 1:2)
  expect_equal(nrow(result$results), 2L)
  expect_equal(
    result$plan[, data.table::uniqueN(source), by = replicate]$V1,
    c(3L, 3L)
  )
  expect_equal(result$results$score, c(7, 7))
})

test_that("simultaneous and worst-scenario bounds use paired scenario draws", {
  point <- data.table::data.table(
    policy = "robust", role = "combined",
    scenario_id = c("a", "b"),
    gain_over_observed_re = c(1, 2)
  )
  draws <- data.table::rbindlist(lapply(1:3, function(replicate) {
    value <- data.table::copy(point)
    value[, `:=`(
      replicate = replicate,
      gain_over_observed_re = gain_over_observed_re + c(-0.1, 0.2) * replicate
    )]
    value
  }))
  expect_error(
    summarize_fixed_clock_simultaneous_intervals_1d(
      point, draws, confidence = 1
    ),
    "strictly"
  )
  # confidence must be open; use the largest empirical quantile just below one.
  simultaneous <- summarize_fixed_clock_simultaneous_intervals_1d(
    point, draws, confidence = 0.999,
    expected_scenario_ids = c("a", "b")
  )
  expect_equal(
    unique(round(simultaneous$simultaneous_critical_value, 3)),
    0.6
  )
  lower <- fixed_clock_worst_scenario_lower_bound_1d(
    draws, confidence = 2 / 3,
    expected_scenario_ids = c("a", "b")
  )
  expect_equal(lower$bootstrap_replicates, 3L)
  expect_true(is.finite(lower$one_sided_lower_bound))
  expect_true(all(simultaneous$scenario_grid_validated))
  expect_true(lower$scenario_grid_validated)

  incomplete <- draws[!(replicate == 2L & scenario_id == "b")]
  expect_error(
    summarize_fixed_clock_simultaneous_intervals_1d(
      point, incomplete, expected_scenario_ids = c("a", "b")
    ),
    "complete common scenario grid"
  )
  expect_error(
    fixed_clock_worst_scenario_lower_bound_1d(
      incomplete, expected_scenario_ids = c("a", "b")
    ),
    "complete common scenario grid"
  )
})
