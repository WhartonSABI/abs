synthetic_revealed_perception_prior_1d <- function(
  fold = 1L, role = "offense", training_games = character()
) {
  context_weights <- rbind(
    `0-0` = c(0.72, 0.28),
    `1-1` = c(0.58, 0.42)
  )
  fit <- list(
    components = 2L,
    weight = c(0.65, 0.35),
    mean = if (role == "offense") c(-4, -0.5) else c(-7, -1),
    sd = if (role == "offense") c(2.2, 1.4) else c(3.5, 1.6),
    fold_id = paste0("outer_", fold, "_", role),
    global_draw_id = 1L,
    context_weights = context_weights,
    context_levels = rownames(context_weights),
    training_games = as.character(training_games),
    training_fingerprint = paste0("fingerprint_", fold, "_", role)
  )
  class(fit) <- "challenge_margin_prior_1d_fit"
  fit
}

synthetic_revealed_perception_inputs_1d <- function(
  games = 12L, rows_per_game_role = 15L, folds = 3L,
  seed = 20260826L
) {
  set.seed(seed)
  game_table <- data.table::data.table(
    game_pk = as.character(9100L + seq_len(games)),
    fold = rep(seq_len(folds), length.out = games)
  )
  rows <- data.table::rbindlist(lapply(c("offense", "defense"), function(role) {
    out <- data.table::CJ(
      game_pk = game_table$game_pk,
      within_role_game = seq_len(rows_per_game_role),
      sorted = TRUE
    )
    out[, `:=`(
      role = role,
      fold = game_table$fold[match(game_pk, game_table$game_pk)],
      pitch_order = within_role_game + if (role == "defense") 100L else 0L,
      stage = (within_role_game - 1L) %% 4L,
      count_state = ifelse(within_role_game %% 2L, "0-0", "1-1"),
      role_margin_inches = stats::runif(.N, -5, 5),
      stake_G = 0.08 + 0.03 * ((within_role_game - 1L) %% 4L),
      inventory_before = 1L + within_role_game %% 2L
    )]
    threshold <- if (role == "offense") 0.7 else 0.3
    width <- if (role == "offense") 2.2 else 1.6
    out[, challenged := stats::rbinom(
      .N, 1L, stats::pnorm((role_margin_inches - threshold) / width)
    )]
    out[, row_id := paste(role, game_pk, pitch_order, sep = "|")]
    out[, within_role_game := NULL]
    out[]
  }))
  states <- data.table::CJ(
    fold = seq_len(folds),
    role = c("offense", "defense"),
    stage = 0:3,
    sorted = TRUE
  )
  states[, `:=`(
    marginal_re_1_to_0 = 0.20 - 0.025 * stage +
      ifelse(role == "offense", 0.01, 0),
    marginal_re_2_to_1 = 0.10 - 0.012 * stage +
      ifelse(role == "offense", 0.005, 0)
  )]
  widths <- data.table::CJ(
    fold = seq_len(folds), role = c("offense", "defense"), sorted = TRUE
  )
  widths[, sigma_inches := ifelse(role == "offense", 2.2, 1.6)]
  priors <- lapply(seq_len(folds), function(fold) {
    training_games <- game_table[game_table$fold != fold, game_pk]
    list(
      offense = synthetic_revealed_perception_prior_1d(
        fold, "offense", training_games
      ),
      defense = synthetic_revealed_perception_prior_1d(
        fold, "defense", training_games
      )
    )
  })
  list(
    rows = rows[], states = states[], widths = widths[],
    priors = priors, folds = game_table[]
  )
}

test_that("kappa decomposition preserves the effective variance", {
  grid <- revealed_perception_kappa_grid_1d()
  decomposition <- decompose_revealed_perception_width_1d(3, grid)

  expect_equal(grid, c(0, 0.25, 0.5, 0.75, 1))
  expect_equal(decomposition$sensory_sigma_inches[[1L]], 0)
  expect_equal(decomposition$action_sigma_inches[[5L]], 0)
  expect_equal(decomposition$sensory_sigma_inches[[2L]], 0.75)
  expect_equal(
    decomposition$action_sigma_inches[[2L]],
    3 * sqrt(1 - 0.25^2)
  )
  expect_equal(decomposition$sensory_variance_share, grid^2)
  expect_equal(
    decomposition$reconstructed_effective_sigma_inches,
    rep(3, 5L), tolerance = 1e-12
  )
  expect_true(all(abs(decomposition$decomposition_error) < 1e-12))
  expect_error(
    decompose_revealed_perception_width_1d(0, 0.5),
    "positive widths"
  )
})

test_that("zero sensory sigma and payoff edge cases are analytic", {
  prior <- synthetic_revealed_perception_prior_1d()
  rows <- data.table::data.table(
    row_id = paste0("r", 1:3), game_pk = "g1", pitch_order = 1:3,
    role = "offense", count_state = "0-0",
    role_margin_inches = c(1, -1, 1), challenged = c(0L, 1L, 1L),
    stake_G = c(-0.1, 0.2, 0.2), inventory_loss = c(0.1, 0, 0.1)
  )
  profile <- revealed_perception_profile_thresholds_1d(
    rows, prior, effective_sigma_inches = 2, kappa = 0
  )

  expect_equal(profile$signal_threshold_inches, c(Inf, -Inf, 0))
  expect_true(is.na(profile$q_target[[1L]]))
  expect_equal(profile$q_target[[2L]], 0)
  expect_equal(profile$q_target[[3L]], 1 / 3)
  probability <- revealed_perception_action_probability_1d(
    profile$role_margin_inches,
    profile$signal_threshold_inches,
    profile$effective_sigma_inches
  )
  expect_equal(probability[1:2], c(0, 1))
  expect_equal(probability[[3L]], stats::pnorm(0.5))
})

test_that("contextual GMM inversion recovers q target", {
  prior <- synthetic_revealed_perception_prior_1d()
  rows <- data.table::data.table(
    row_id = c("a", "b"), game_pk = "g1", pitch_order = 1:2,
    role = "offense", count_state = c("0-0", "1-1"),
    role_margin_inches = 0, challenged = 0L,
    stake_G = 0.15, inventory_loss = 0.10
  )
  profile <- revealed_perception_profile_thresholds_1d(
    rows, prior, effective_sigma_inches = 2, kappa = 0.5,
    lookup_grid_step = 0.005
  )

  expect_true(all(is.finite(profile$signal_threshold_inches)))
  expect_true(all(profile$threshold_status == "interior_lookup_inverse"))
  expect_lt(max(profile$inversion_absolute_error), 1e-4)
  expect_false(isTRUE(all.equal(
    profile$signal_threshold_inches[[1L]],
    profile$signal_threshold_inches[[2L]]
  )))
})

test_that("pooled threshold bias is recovered from challenge actions", {
  set.seed(91)
  n <- 3000L
  true_bias <- 0.65
  profile <- data.table::data.table(
    game_pk = paste0("g", 1L + (seq_len(n) - 1L) %% 100L),
    role_margin_inches = stats::runif(n, -5, 5),
    signal_threshold_inches = 0.4,
    effective_sigma_inches = 1.8
  )
  profile[, challenged := stats::rbinom(
    .N, 1L,
    revealed_perception_action_probability_1d(
      role_margin_inches, signal_threshold_inches,
      effective_sigma_inches, true_bias
    )
  )]
  fit <- fit_revealed_perception_threshold_bias_1d(profile)

  expect_equal(fit$optimization_status, "interior_optimum")
  expect_equal(fit$threshold_bias_inches, true_bias, tolerance = 0.15)
  expect_true(is.finite(fit$threshold_bias_std_error))
  expect_equal(fit$training_games, 100L)
  expect_error(
    fit_revealed_perception_threshold_bias_1d(
      profile, probability_epsilon = 0
    ),
    "probability_epsilon"
  )
})

test_that("sensory and action decomposition is not uniquely identified", {
  set.seed(415)
  n <- 5000L
  effective_width <- 1.7
  total_threshold <- 0.6
  margin <- stats::runif(n, -5, 5)
  action <- stats::rbinom(
    n, 1L, stats::pnorm((margin - total_threshold) / effective_width)
  )
  make_profile <- function(structural_threshold) {
    data.table::data.table(
      game_pk = paste0("nonid_", 1L + (seq_len(n) - 1L) %% 100L),
      role_margin_inches = margin,
      signal_threshold_inches = structural_threshold,
      effective_sigma_inches = effective_width,
      challenged = action
    )
  }
  low_sensory <- make_profile(-0.8)
  high_sensory <- make_profile(1.4)
  fit_low <- fit_revealed_perception_threshold_bias_1d(low_sensory)
  fit_high <- fit_revealed_perception_threshold_bias_1d(high_sensory)
  scored_low <- score_revealed_perception_profile_1d(
    low_sensory, fit_low$threshold_bias_inches
  )
  scored_high <- score_revealed_perception_profile_1d(
    high_sensory, fit_high$threshold_bias_inches
  )

  expect_equal(
    fit_low$threshold_bias_inches - fit_high$threshold_bias_inches,
    2.2,
    tolerance = 1e-4
  )
  expect_equal(
    scored_low$challenge_probability,
    scored_high$challenge_probability,
    tolerance = 2e-6
  )
  expect_equal(mean(scored_low$log_loss), mean(scored_high$log_loss),
    tolerance = 1e-10
  )
})

test_that("profile-row adapter ignores Bellman truth and outcome columns", {
  offense <- data.table::data.table(
    game_pk = c("g1", "g2"), pitch_order = 1L,
    batter_id = c("b1", "b2"), bat_team_id = c("t1", "t2"),
    initial_call = "called_strike", challenged = c(1L, 0L),
    edge_distance_inches = c(1, -1), stake_G = 0.2,
    inventory_loss = 0.1, adverse_challenges_before = c(2L, 1L),
    balls_before = 0L, strikes_before = 0L,
    umpire_id = "u", catcher_id = "c"
  )
  defense <- data.table::data.table(
    game_pk = c("g1", "g2"), pitch_order = 2L,
    defensive_team_id = c("t2", "t1"),
    opponent_team_id = c("t1", "t2"),
    initial_call = "ball", challenged = c(0L, 1L),
    physical_edge_distance_inches = c(1, -1), stake_G = 0.2,
    inventory_loss = 0.1, defense_inventory_before = c(1L, 2L),
    balls_before = 0L, strikes_before = 0L,
    umpire_id = "u", catcher_id = "c"
  )
  bellman <- data.table::rbindlist(list(
    offense[, .(game_pk, pitch_order, role = "offense")],
    defense[, .(game_pk, pitch_order, role = "defense")]
  ))
  bellman[, `:=`(
    stage = rep(0:1, 2L), stake_G = 0.2,
    fold = ifelse(game_pk == "g1", 1L, 2L),
    actual_wrong = TRUE,
    official_success = FALSE,
    challenge_outcome = "upheld"
  )]
  first <- prepare_revealed_perception_profile_rows_1d(
    offense, defense, bellman
  )
  bellman[, `:=`(
    actual_wrong = !actual_wrong,
    official_success = !official_success,
    challenge_outcome = "overturned"
  )]
  second <- prepare_revealed_perception_profile_rows_1d(
    offense, defense, bellman
  )

  expect_equal(first, second)
  expect_equal(nrow(first), 4L)
  expect_false(any(
    revealed_perception_profile_outcome_columns_1d() %in% names(first)
  ))
  expect_equal(first[role == "offense", inventory_before], c(2L, 1L))
  expect_equal(first[role == "defense", inventory_before], c(1L, 2L))
})

test_that("cross-fitted profile returns 25 joint scenarios and OOF scores", {
  source <- synthetic_revealed_perception_inputs_1d()
  result <- crossfit_revealed_perception_profile_1d(
    source$rows,
    fold_prior_fits = source$priors,
    width_estimates = source$widths,
    state_values = source$states,
    lookup_grid_step = 0.1,
    progress = FALSE
  )

  expect_s3_class(result, "revealed_perception_profile_1d")
  expect_equal(nrow(result$scenario_metrics), 25L)
  expect_equal(nrow(result$scenario_game_scores), 25L * 12L)
  expect_equal(
    nrow(result$oof_profiles),
    nrow(source$rows) * length(revealed_perception_kappa_grid_1d())
  )
  expect_equal(nrow(result$threshold_bias), 3L * 2L * 5L)
  expect_equal(nrow(result$width_decomposition), 3L * 2L * 5L)
  expect_equal(sum(result$scenario_metrics$best_scenario), 1L)
  expect_true(any(result$scenario_metrics$one_se_accepted))
  expect_true(all(is.finite(result$scenario_metrics$mean_log_loss)))
  expect_true(all(result$oof_profiles$challenge_probability >= 0 &
    result$oof_profiles$challenge_probability <= 1))
  expect_true(all(result$oof_profiles$feature_outer_fold ==
    result$oof_profiles$fold))
  expect_false(any(
    revealed_perception_profile_outcome_columns_1d() %in%
      names(result$oof_profiles)
  ))
  expect_equal(result$manifest$joint_scenarios, 25L)
  expect_false(result$manifest$outcomes_used)
  expect_match(result$manifest$accepted_set_rule, "game-clustered")
})

test_that("profile priors containing held-out games fail closed", {
  source <- synthetic_revealed_perception_inputs_1d()
  source$priors[[1L]]$offense$training_games <- c(
    source$priors[[1L]]$offense$training_games,
    source$folds[fold == 1L, game_pk][[1L]]
  )
  expect_error(
    crossfit_revealed_perception_profile_1d(
      source$rows, source$priors, source$widths, source$states,
      lookup_grid_step = 0.1, progress = FALSE
    ),
    "held-out games"
  )
})
