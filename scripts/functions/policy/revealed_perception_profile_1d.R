# Partial-identification profile for the one-dimensional challenge model.
#
# A role/fold effective width w is identified from the reduced-form action
# curve.  For a sensitivity value kappa in [0, 1], decompose its variance as
#
#   sensory_sigma = kappa * w
#   action_sigma  = w * sqrt(1 - kappa^2).
#
# The contextual posterior q(r) and payoff threshold r* use sensory_sigma.
# Conditional on true margin, sensory and action noise combine back to w, so
#
#   P(challenge | margin) = Phi((margin - r* - pooled_bias) / w).
#
# Challenge outcomes and geometric truth are never admitted to this likelihood.

revealed_perception_kappa_grid_1d <- function() {
  c(0, 0.25, 0.5, 0.75, 1)
}

revealed_perception_profile_outcome_columns_1d <- function() {
  unique(c(
    challenge_discrimination_1d_outcome_columns(),
    defense_challenge_discrimination_1d_outcome_columns(),
    "actual_wrong", "geometry_wrong", "observed_success", "official_success",
    "expected_policy_success", "expected_policy_failure",
    "expected_captured_re", "challenge_outcome"
  ))
}

.revealed_perception_scalar_1d <- function(
  value, name, lower = -Inf, upper = Inf, integer = FALSE
) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (
    length(numeric_value) != 1L || !is.finite(numeric_value) ||
      numeric_value < lower || numeric_value > upper ||
      (isTRUE(integer) && numeric_value != as.integer(numeric_value))
  ) {
    stop(
      name, " must be one finite ", if (isTRUE(integer)) "integer" else "number",
      " in [", lower, ", ", upper, "]",
      call. = FALSE
    )
  }
  if (isTRUE(integer)) as.integer(numeric_value) else numeric_value
}

validate_revealed_perception_kappa_grid_1d <- function(
  kappa_grid = revealed_perception_kappa_grid_1d()
) {
  kappa <- sort(unique(as.numeric(kappa_grid)))
  if (!length(kappa) || anyNA(kappa) || any(!is.finite(kappa)) ||
      any(kappa < 0 | kappa > 1)) {
    stop("kappa_grid must contain unique finite values in [0, 1]")
  }
  kappa
}

decompose_revealed_perception_width_1d <- function(
  effective_sigma_inches, kappa
) {
  effective <- as.numeric(effective_sigma_inches)
  kappa <- as.numeric(kappa)
  size <- max(length(effective), length(kappa))
  if (any(c(length(effective), length(kappa)) != 1L &
      c(length(effective), length(kappa)) != size)) {
    stop("effective_sigma_inches and kappa must be scalar or row-aligned")
  }
  effective <- rep_len(effective, size)
  kappa <- rep_len(kappa, size)
  if (anyNA(effective) || any(!is.finite(effective)) || any(effective <= 0) ||
      anyNA(kappa) || any(!is.finite(kappa)) || any(kappa < 0 | kappa > 1)) {
    stop("Width decomposition requires positive widths and kappa in [0, 1]")
  }
  sensory <- effective * kappa
  action <- effective * sqrt(1 - kappa^2)
  data.table::data.table(
    effective_sigma_inches = effective,
    kappa = kappa,
    sensory_standard_deviation_share = kappa,
    sensory_variance_share = kappa^2,
    sensory_sigma_inches = sensory,
    action_sigma_inches = action,
    reconstructed_effective_sigma_inches = sqrt(sensory^2 + action^2),
    decomposition_error = sqrt(sensory^2 + action^2) - effective
  )
}

.revealed_perception_assert_no_outcomes_1d <- function(rows, label) {
  leaked <- intersect(
    names(data.table::as.data.table(rows)),
    revealed_perception_profile_outcome_columns_1d()
  )
  if (length(leaked)) {
    stop(
      label, " contains outcome/truth columns: ",
      paste(sort(leaked), collapse = ", "), call. = FALSE
    )
  }
  invisible(TRUE)
}

.revealed_perception_fold_table_1d <- function(fold_assignment, games) {
  folds <- data.table::copy(data.table::as.data.table(fold_assignment))
  stop_if_missing_columns(
    folds, c("game_pk", "fold"), "revealed-perception fold assignment"
  )
  folds <- folds[, .(game_pk = as.character(game_pk), fold = as.integer(fold))]
  if (!nrow(folds) || anyNA(folds) || any(!nzchar(folds$game_pk)) ||
      any(folds$fold < 1L) || anyDuplicated(folds$game_pk)) {
    stop("Each game must have exactly one valid revealed-perception fold")
  }
  ids <- sort(unique(folds$fold))
  if (length(ids) < 2L || !identical(ids, seq_len(max(ids)))) {
    stop("Revealed-perception folds must be contiguous 1,...,K")
  }
  missing <- setdiff(unique(as.character(games)), folds$game_pk)
  if (length(missing)) stop("Fold assignment omits eligible action games")
  data.table::setorder(folds, game_pk)
  folds[]
}

prepare_revealed_perception_profile_rows_1d <- function(
  offense_rows, defense_rows, bellman_rows, fold_assignment = NULL,
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon(),
  stake_tolerance = 1e-8
) {
  offense <- normalize_challenge_discrimination_1d_rows(
    offense_rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  defense <- normalize_defense_challenge_discrimination_1d_rows(
    defense_rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  offense_inventory <- if ("adverse_challenges_before" %in% names(offense)) {
    as.integer(offense$adverse_challenges_before)
  } else {
    rep(NA_integer_, nrow(offense))
  }
  offense_canonical <- offense[, .(
    game_pk = as.character(game_pk), pitch_order,
    role = "offense", count_state = as.character(count_state),
    role_margin_inches = as.numeric(margin_inches),
    challenged = as.integer(challenged),
    stake_G_action = as.numeric(stake_G),
    inventory_before = offense_inventory
  )]
  defense_canonical <- defense[, .(
    game_pk = as.character(game_pk), pitch_order,
    role = "defense", count_state = as.character(count_state),
    role_margin_inches = as.numeric(defense_margin_inches),
    challenged = as.integer(challenged),
    stake_G_action = as.numeric(stake_G),
    inventory_before = as.integer(defense_inventory_before)
  )]
  actions <- data.table::rbindlist(
    list(offense_canonical, defense_canonical), use.names = TRUE
  )
  if (anyNA(actions$inventory_before) || any(actions$inventory_before < 1L)) {
    stop("Revealed-perception actions require positive observed inventory")
  }
  actions[, inventory_before := pmin(2L, inventory_before)]
  actions[, row_id := paste(role, game_pk, pitch_order, sep = "|")]
  if (anyDuplicated(actions$row_id)) {
    stop("Revealed-perception action keys are duplicated")
  }

  bellman <- data.table::copy(data.table::as.data.table(bellman_rows))
  stop_if_missing_columns(
    bellman,
    c("game_pk", "pitch_order", "role", "stage", "stake_G"),
    "Bellman rows for revealed-perception profiling"
  )
  # This allowlist intentionally drops geometry truth and official outcomes.
  keep <- intersect(
    c("game_pk", "pitch_order", "role", "stage", "stake_G", "fold"),
    names(bellman)
  )
  bellman <- bellman[, ..keep]
  bellman[, `:=`(
    game_pk = as.character(game_pk),
    role = as.character(role),
    stage = as.integer(stage),
    stake_G_bellman = as.numeric(stake_G)
  )]
  bellman[, stake_G := NULL]
  if (anyDuplicated(bellman[, .(game_pk, pitch_order, role)])) {
    stop("Bellman rows have duplicate role/pitch keys")
  }
  x <- merge(
    actions, bellman,
    by = c("game_pk", "pitch_order", "role"),
    all.x = TRUE, sort = FALSE
  )
  if (anyNA(x$stage) || any(!is.finite(x$stake_G_bellman))) {
    stop("Every action row must match one Bellman stage and gain")
  }
  disagreement <- abs(x$stake_G_action - x$stake_G_bellman) > stake_tolerance
  if (any(disagreement)) {
    stop("Action and Bellman correction gains disagree")
  }
  x[, `:=`(stake_G = stake_G_bellman)]
  x[, c("stake_G_action", "stake_G_bellman") := NULL]

  if (!"fold" %in% names(x)) {
    if (is.null(fold_assignment)) {
      stop("Supply fold_assignment when Bellman rows do not contain fold")
    }
    folds <- .revealed_perception_fold_table_1d(
      fold_assignment, x$game_pk
    )
    x[, fold := folds$fold[match(game_pk, folds$game_pk)]]
  } else {
    x[, fold := as.integer(fold)]
    if (!is.null(fold_assignment)) {
      folds <- .revealed_perception_fold_table_1d(
        fold_assignment, x$game_pk
      )
      expected <- folds$fold[match(x$game_pk, folds$game_pk)]
      if (!identical(x$fold, expected)) {
        stop("Bellman rows and fold_assignment disagree")
      }
    }
  }
  if (anyNA(x$fold) || any(x$fold < 1L)) {
    stop("Revealed-perception rows have invalid folds")
  }
  data.table::setorder(x, game_pk, pitch_order, role)
  out <- x[, .(
    row_id, game_pk, pitch_order, fold, role, stage, count_state,
    role_margin_inches, challenged, stake_G, inventory_before
  )]
  .revealed_perception_assert_no_outcomes_1d(
    out, "Prepared revealed-perception rows"
  )
  out[]
}

.revealed_perception_prior_for_fold_1d <- function(
  fold_prior_fits, fold, role
) {
  if (!is.list(fold_prior_fits) || length(fold_prior_fits) < fold) {
    stop("fold_prior_fits lacks outer fold ", fold)
  }
  value <- fold_prior_fits[[fold]]
  if (!is.list(value) || is.null(value[[role]])) {
    stop("Outer fold ", fold, " lacks the ", role, " prior")
  }
  .challenge_margin_validate_fit(value[[role]])
  value[[role]]
}

.revealed_perception_width_for_fold_1d <- function(
  width_estimates, fold, role
) {
  widths <- data.table::as.data.table(width_estimates)
  stop_if_missing_columns(
    widths, c("fold", "role", "sigma_inches"),
    "revealed-perception width estimates"
  )
  fold_id <- as.integer(fold)
  role_name <- as.character(role)
  value <- widths[
    as.integer(widths$fold) == fold_id &
      as.character(widths$role) == role_name,
    sigma_inches
  ]
  if (length(value) != 1L || !is.finite(value) || value <= 0) {
    stop("Need one positive effective width for fold ", fold, " and ", role)
  }
  as.numeric(value)
}

.revealed_perception_losses_for_fold_1d <- function(
  rows, state_values, fold, role
) {
  states <- data.table::copy(data.table::as.data.table(state_values))
  stop_if_missing_columns(
    states,
    c(
      "fold", "role", "stage", "marginal_re_1_to_0",
      "marginal_re_2_to_1"
    ),
    "revealed-perception Bellman state values"
  )
  fold_id <- as.integer(fold)
  role_name <- as.character(role)
  states <- states[
    as.integer(states$fold) == fold_id &
      as.character(states$role) == role_name,
    .(
      stage = as.integer(stage),
      loss_k1 = as.numeric(marginal_re_1_to_0),
      loss_k2 = as.numeric(marginal_re_2_to_1)
    )
  ]
  if (!nrow(states) || anyDuplicated(states$stage) ||
      anyNA(states) || any(as.matrix(states[, .(loss_k1, loss_k2)]) < 0)) {
    stop("Invalid Bellman losses for fold ", fold, " and ", role)
  }
  index <- match(as.integer(rows$stage), states$stage)
  if (anyNA(index)) stop("Bellman state values omit action stages")
  ifelse(
    as.integer(rows$inventory_before) == 1L,
    states$loss_k1[index], states$loss_k2[index]
  )
}

score_revealed_perception_context_q_1d <- function(
  prior_fit, private_signal_inches, sensory_sigma_inches, context
) {
  challenge_margin_subjective_ball_probability_1d(
    prior_fit,
    private_margin_signal = private_signal_inches,
    perception_sigma = sensory_sigma_inches,
    context = context
  )
}

revealed_perception_profile_thresholds_1d <- function(
  rows, prior_fit, effective_sigma_inches, kappa,
  lookup_grid_step = 0.01
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  stop_if_missing_columns(
    x,
    c(
      "row_id", "game_pk", "pitch_order", "role", "count_state",
      "role_margin_inches", "challenged", "stake_G", "inventory_loss"
    ),
    "revealed-perception threshold rows"
  )
  .revealed_perception_assert_no_outcomes_1d(
    x, "Revealed-perception threshold rows"
  )
  decomposition <- decompose_revealed_perception_width_1d(
    effective_sigma_inches, kappa
  )
  if (nrow(decomposition) != 1L) {
    stop("Threshold profiling requires scalar width and kappa")
  }
  sensory <- decomposition$sensory_sigma_inches[[1L]]
  gain <- as.numeric(x$stake_G)
  loss <- as.numeric(x$inventory_loss)
  if (anyNA(gain) || any(!is.finite(gain)) || anyNA(loss) ||
      any(!is.finite(loss)) || any(loss < 0)) {
    stop("Profile gains/losses must be finite and losses non-negative")
  }
  q_target <- threshold <- rep(NA_real_, nrow(x))
  status <- rep(NA_character_, nrow(x))
  context_fallback <- rep(FALSE, nrow(x))
  positive <- gain > 0
  zero_loss <- positive & loss == 0
  never <- !positive
  interior <- positive & loss > 0
  q_target[zero_loss] <- 0
  q_target[interior] <- loss[interior] / (gain[interior] + loss[interior])
  threshold[zero_loss] <- -Inf
  threshold[never] <- Inf
  status[zero_loss] <- "analytic_zero_failure_cost_always"
  status[never] <- "analytic_nonpositive_gain_never"
  if (sensory == 0) {
    threshold[interior] <- 0
    status[interior] <- "analytic_zero_sensory_sigma_generalized_inverse"
  } else if (any(interior)) {
    lookup <- build_challenge_signal_lookup_1d(
      prior_fit,
      perception_sigma = sensory,
      context = unique(as.character(x$count_state)),
      grid_step = lookup_grid_step
    )
    solved <- challenge_signal_payoff_terms_1d(
      lookup,
      gain = gain[interior],
      inventory_loss = loss[interior],
      context = x$count_state[interior]
    )
    threshold[interior] <- solved$threshold_inches
    q_target[interior] <- solved$q_target
    status[interior] <- ifelse(
      solved$exact_root_fallback,
      "interior_exact_root_fallback", "interior_lookup_inverse"
    )
    context_fallback[interior] <- solved$context_fallback
  }
  q_at_threshold <- inversion_error <- rep(NA_real_, nrow(x))
  finite <- is.finite(threshold) & sensory > 0
  if (any(finite)) {
    q_at_threshold[finite] <- score_revealed_perception_context_q_1d(
      prior_fit,
      private_signal_inches = threshold[finite],
      sensory_sigma_inches = sensory,
      context = x$count_state[finite]
    )
    inversion_error[finite] <- abs(q_at_threshold[finite] - q_target[finite])
  }
  x[, `:=`(
    effective_sigma_inches = decomposition$effective_sigma_inches[[1L]],
    kappa = decomposition$kappa[[1L]],
    sensory_sigma_inches = sensory,
    action_sigma_inches = decomposition$action_sigma_inches[[1L]],
    q_target = q_target,
    signal_threshold_inches = threshold,
    q_at_signal_threshold = q_at_threshold,
    inversion_absolute_error = inversion_error,
    threshold_status = status,
    context_fallback = context_fallback
  )]
  if (anyNA(x$signal_threshold_inches) || anyNA(x$threshold_status)) {
    stop("Revealed-perception threshold construction was incomplete")
  }
  x[]
}

revealed_perception_action_probability_1d <- function(
  margin_inches, signal_threshold_inches, effective_sigma_inches,
  threshold_bias_inches = 0
) {
  margin <- as.numeric(margin_inches)
  threshold <- as.numeric(signal_threshold_inches)
  width <- as.numeric(effective_sigma_inches)
  bias <- as.numeric(threshold_bias_inches)
  size <- max(length(margin), length(threshold), length(width), length(bias))
  lengths <- c(length(margin), length(threshold), length(width), length(bias))
  if (any(lengths != 1L & lengths != size)) {
    stop("Action-probability inputs must be scalar or row-aligned")
  }
  margin <- rep_len(margin, size)
  threshold <- rep_len(threshold, size)
  width <- rep_len(width, size)
  bias <- rep_len(bias, size)
  if (anyNA(margin) || any(!is.finite(margin)) || anyNA(threshold) ||
      anyNA(width) || any(!is.finite(width)) || any(width <= 0) ||
      anyNA(bias) || any(!is.finite(bias))) {
    stop("Invalid revealed-perception action-probability inputs")
  }
  probability <- stats::pnorm((margin - threshold - bias) / width)
  probability[threshold == -Inf] <- 1
  probability[threshold == Inf] <- 0
  pmin(1, pmax(0, probability))
}

.revealed_perception_log_loss_1d <- function(action, probability, epsilon) {
  p <- pmin(1 - epsilon, pmax(epsilon, probability))
  -(action * log(p) + (1 - action) * log1p(-p))
}

fit_revealed_perception_threshold_bias_1d <- function(
  training_profile, bias_bounds_inches = c(-20, 20),
  probability_epsilon = 1e-12
) {
  x <- data.table::as.data.table(training_profile)
  .revealed_perception_assert_no_outcomes_1d(
    x, "Revealed-perception bias training data"
  )
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "challenged", "role_margin_inches",
      "signal_threshold_inches", "effective_sigma_inches"
    ),
    "revealed-perception bias training data"
  )
  bounds <- as.numeric(bias_bounds_inches)
  if (length(bounds) != 2L || anyNA(bounds) || any(!is.finite(bounds)) ||
      bounds[[1L]] >= bounds[[2L]]) {
    stop("bias_bounds_inches must be two increasing finite values")
  }
  epsilon <- .revealed_perception_scalar_1d(
    probability_epsilon, "probability_epsilon", .Machine$double.xmin, 0.1
  )
  if (!nrow(x) || anyNA(x$challenged) || any(!x$challenged %in% 0:1)) {
    stop("Bias training needs non-empty binary challenge actions")
  }
  informative <- is.finite(x$signal_threshold_inches)
  objective <- function(bias) {
    probability <- revealed_perception_action_probability_1d(
      x$role_margin_inches,
      x$signal_threshold_inches,
      x$effective_sigma_inches,
      bias
    )
    sum(.revealed_perception_log_loss_1d(x$challenged, probability, epsilon))
  }
  if (!any(informative)) {
    estimate <- 0
    convergence <- "no_finite_threshold_rows_bias_fixed_zero"
    objective_value <- objective(estimate)
    standard_error <- NA_real_
  } else {
    optimized <- stats::optimize(objective, interval = bounds)
    estimate <- optimized$minimum
    objective_value <- optimized$objective
    convergence <- if (
      abs(estimate - bounds[[1L]]) < 1e-5 ||
        abs(estimate - bounds[[2L]]) < 1e-5
    ) {
      "boundary_optimum"
    } else {
      "interior_optimum"
    }
    curvature <- tryCatch(
      as.numeric(stats::optimHess(estimate, function(value) objective(value))),
      error = function(error) NA_real_
    )
    standard_error <- if (length(curvature) && is.finite(curvature) &&
      curvature > 0) sqrt(1 / curvature) else NA_real_
  }
  data.table::data.table(
    threshold_bias_inches = estimate,
    threshold_bias_std_error = standard_error,
    threshold_bias_lower_95 = estimate - stats::qnorm(0.975) * standard_error,
    threshold_bias_upper_95 = estimate + stats::qnorm(0.975) * standard_error,
    training_rows = nrow(x),
    informative_threshold_rows = sum(informative),
    training_games = data.table::uniqueN(x$game_pk),
    training_challenges = sum(x$challenged),
    training_log_loss = objective_value / nrow(x),
    optimization_status = convergence,
    bias_lower_bound_inches = bounds[[1L]],
    bias_upper_bound_inches = bounds[[2L]],
    probability_epsilon = epsilon
  )
}

score_revealed_perception_profile_1d <- function(
  profile, threshold_bias_inches, probability_epsilon = 1e-12
) {
  x <- data.table::copy(data.table::as.data.table(profile))
  .revealed_perception_assert_no_outcomes_1d(
    x, "Revealed-perception scoring data"
  )
  epsilon <- .revealed_perception_scalar_1d(
    probability_epsilon, "probability_epsilon", .Machine$double.xmin, 0.1
  )
  bias <- .revealed_perception_scalar_1d(
    threshold_bias_inches, "threshold_bias_inches"
  )
  probability <- revealed_perception_action_probability_1d(
    x$role_margin_inches,
    x$signal_threshold_inches,
    x$effective_sigma_inches,
    bias
  )
  x[, `:=`(
    threshold_bias_inches = bias,
    challenge_probability = probability,
    log_loss = .revealed_perception_log_loss_1d(
      challenged, probability, epsilon
    )
  )]
  x[]
}

.revealed_perception_cluster_metric_1d <- function(
  game_scores, loss_sum = "loss_sum", row_count = "rows"
) {
  x <- data.table::as.data.table(game_scores)
  losses <- as.numeric(x[[loss_sum]])
  counts <- as.integer(x[[row_count]])
  total_rows <- sum(counts)
  games <- nrow(x)
  estimate <- sum(losses) / total_rows
  standard_error <- if (games > 1L) {
    residual <- losses - estimate * counts
    sqrt(games / (games - 1) * sum(residual^2) / total_rows^2)
  } else {
    NA_real_
  }
  c(
    rows = total_rows, games = games,
    mean_log_loss = estimate, game_clustered_se = standard_error
  )
}

.revealed_perception_role_metrics_1d <- function(oof_profiles) {
  game <- oof_profiles[, .(
    loss_sum = sum(log_loss), rows = .N
  ), by = .(role, kappa, game_pk)]
  data.table::rbindlist(lapply(
    split(game, interaction(game$role, game$kappa, drop = TRUE)),
    function(block) {
      metric <- .revealed_perception_cluster_metric_1d(block)
      data.table::data.table(
        role = block$role[[1L]], kappa = block$kappa[[1L]],
        rows = as.integer(metric[["rows"]]),
        games = as.integer(metric[["games"]]),
        mean_log_loss = metric[["mean_log_loss"]],
        game_clustered_se = metric[["game_clustered_se"]]
      )
    }
  ))[order(role, kappa)]
}

.revealed_perception_scenario_id_1d <- function(offense_kappa, defense_kappa) {
  sprintf(
    "offense_kappa_%.2f__defense_kappa_%.2f",
    offense_kappa, defense_kappa
  )
}

.revealed_perception_scenario_metrics_1d <- function(oof_profiles, kappa_grid) {
  games <- sort(unique(oof_profiles$game_pk))
  scenarios <- data.table::CJ(
    offense_kappa = kappa_grid,
    defense_kappa = kappa_grid,
    sorted = TRUE
  )
  game_parts <- vector("list", nrow(scenarios))
  metric_parts <- vector("list", nrow(scenarios))
  for (index in seq_len(nrow(scenarios))) {
    offense_kappa <- scenarios$offense_kappa[[index]]
    defense_kappa <- scenarios$defense_kappa[[index]]
    scenario_id <- .revealed_perception_scenario_id_1d(
      offense_kappa, defense_kappa
    )
    selected <- oof_profiles[
      (role == "offense" & abs(kappa - offense_kappa) < 1e-12) |
        (role == "defense" & abs(kappa - defense_kappa) < 1e-12)
    ]
    game <- selected[, .(
      loss_sum = sum(log_loss), rows = .N,
      challenges = sum(challenged)
    ), by = game_pk]
    game <- merge(
      data.table::data.table(game_pk = games), game,
      by = "game_pk", all.x = TRUE, sort = FALSE
    )
    game[is.na(rows), `:=`(loss_sum = 0, rows = 0L, challenges = 0L)]
    game[, `:=`(
      scenario_id = scenario_id,
      offense_kappa = offense_kappa,
      defense_kappa = defense_kappa
    )]
    metric <- .revealed_perception_cluster_metric_1d(game)
    metric_parts[[index]] <- data.table::data.table(
      scenario_id = scenario_id,
      offense_kappa = offense_kappa,
      defense_kappa = defense_kappa,
      rows = as.integer(metric[["rows"]]),
      games = as.integer(metric[["games"]]),
      mean_log_loss = metric[["mean_log_loss"]],
      game_clustered_se = metric[["game_clustered_se"]]
    )
    game_parts[[index]] <- game
  }
  metrics <- data.table::rbindlist(metric_parts)
  game_scores <- data.table::rbindlist(game_parts)
  best_index <- which.min(metrics$mean_log_loss)
  best_id <- metrics$scenario_id[[best_index]]
  best_games <- game_scores[scenario_id == best_id, .(
    game_pk, best_loss_sum = loss_sum, best_rows = rows
  )]
  differences <- merge(
    game_scores, best_games, by = "game_pk", all.x = TRUE, sort = FALSE
  )
  differences[, `:=`(
    difference_sum = loss_sum - best_loss_sum,
    difference_rows = rows
  )]
  paired <- differences[, {
    total_rows <- sum(difference_rows)
    estimate <- sum(difference_sum) / total_rows
    game_count <- .N
    residual <- difference_sum - estimate * difference_rows
    standard_error <- if (game_count > 1L) {
      sqrt(
        game_count / (game_count - 1) * sum(residual^2) / total_rows^2
      )
    } else {
      NA_real_
    }
    .(
      difference_from_best = estimate,
      difference_game_clustered_se = standard_error
    )
  }, by = scenario_id]
  metrics <- merge(metrics, paired, by = "scenario_id", sort = FALSE)
  metrics[, `:=`(
    best_scenario = scenario_id == best_id,
    one_se_accepted = scenario_id == best_id |
      difference_from_best <= difference_game_clustered_se + 1e-15,
    comparison_rule =
      "paired game-clustered difference from best <= one SE"
  )]
  data.table::setorder(metrics, mean_log_loss, offense_kappa, defense_kappa)
  data.table::setorder(game_scores, scenario_id, game_pk)
  list(metrics = metrics[], game_scores = game_scores[])
}

revealed_perception_profile_manifest_1d <- function(result) {
  if (!inherits(result, "revealed_perception_profile_1d")) {
    stop("result must be a revealed_perception_profile_1d")
  }
  accepted <- result$scenario_metrics[one_se_accepted == TRUE]
  data.table::data.table(
    model = "revealed_perception_partial_identification_profile_1d",
    status = "cross-validated exploratory partial-identification profile",
    kappa_definition =
      "sensory_standard_deviation / effective_action_curve_width",
    kappa_grid = paste(format(result$kappa_grid, trim = TRUE), collapse = ","),
    joint_scenarios = nrow(result$scenario_metrics),
    accepted_scenarios = nrow(accepted),
    best_scenario = result$scenario_metrics[best_scenario == TRUE, scenario_id],
    accepted_scenario_ids = paste(accepted$scenario_id, collapse = ","),
    sensory_action_identity =
      "sensory_sigma^2 + action_sigma^2 = effective_sigma^2",
    threshold_rule = "q(r*) = L / (G + L)",
    threshold_bias =
      "one pooled additive signal-inch bias per outer fold, role, and kappa",
    bias_information = "challenge/pass actions only",
    outcomes_used = FALSE,
    score = "row-mean OOF Bernoulli log loss with game-clustered SE",
    accepted_set_rule =
      "paired game-clustered loss difference from best within one SE",
    folds = data.table::uniqueN(result$oof_profiles$fold),
    games = data.table::uniqueN(result$oof_profiles$game_pk),
    scored_rows_per_kappa = nrow(result$rows),
    effective_width_source = "supplied role-by-outer-fold estimates",
    inventory_loss_source =
      "supplied outer-fold Bellman marginal continuation values",
    prior_source = "supplied role-by-outer-fold contextual GMM fits",
    special_cases = paste(
      "kappa=0 uses exact-sensory generalized inverse r*=0;",
      "G<=0 never challenges; G>0,L=0 always challenges"
    )
  )
}

crossfit_revealed_perception_profile_1d <- function(
  rows, fold_prior_fits, width_estimates, state_values,
  kappa_grid = revealed_perception_kappa_grid_1d(),
  lookup_grid_step = 0.01, bias_bounds_inches = c(-20, 20),
  probability_epsilon = 1e-12, progress = interactive()
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  .revealed_perception_assert_no_outcomes_1d(
    x, "Revealed-perception profile input"
  )
  stop_if_missing_columns(
    x,
    c(
      "row_id", "game_pk", "pitch_order", "fold", "role", "stage",
      "count_state", "role_margin_inches", "challenged", "stake_G",
      "inventory_before"
    ),
    "revealed-perception profile input"
  )
  if (anyDuplicated(x$row_id) || anyNA(x[, .(
    row_id, game_pk, pitch_order, fold, role, stage, count_state,
    role_margin_inches, challenged, stake_G, inventory_before
  )]) || any(!x$role %in% c("offense", "defense")) ||
      any(!x$challenged %in% 0:1) || any(!x$inventory_before %in% 1:2)) {
    stop("Revealed-perception profile rows are invalid")
  }
  kappa <- validate_revealed_perception_kappa_grid_1d(kappa_grid)
  if (!identical(kappa, revealed_perception_kappa_grid_1d())) {
    warning(
      "A non-primary kappa grid was requested; the canonical grid yields 25 scenarios",
      call. = FALSE
    )
  }
  lookup_grid_step <- .revealed_perception_scalar_1d(
    lookup_grid_step, "lookup_grid_step", .Machine$double.eps
  )
  folds <- sort(unique(as.integer(x$fold)))
  if (length(folds) < 2L || !identical(folds, seq_len(max(folds)))) {
    stop("Profile rows must contain contiguous outer folds 1,...,K")
  }
  profile_parts <- bias_parts <- list()
  index <- 0L
  for (fold_id in folds) {
    heldout_games <- unique(x[fold == fold_id, game_pk])
    training_games <- unique(x[fold != fold_id, game_pk])
    if (length(intersect(heldout_games, training_games))) {
      stop("Revealed-perception training and held-out games overlap")
    }
    for (role_name in c("offense", "defense")) {
      if (isTRUE(progress)) {
        message(
          "revealed-perception fold ", fold_id, "/", max(folds), ": ",
          role_name
        )
      }
      role_rows <- x[role == role_name]
      if (!nrow(role_rows[fold == fold_id]) ||
          !nrow(role_rows[fold != fold_id])) {
        stop("Each fold needs training and held-out actions for both roles")
      }
      prior <- .revealed_perception_prior_for_fold_1d(
        fold_prior_fits, fold_id, role_name
      )
      prior_games <- as.character(prior$training_games %||% character())
      if (length(intersect(prior_games, heldout_games))) {
        stop("A profile prior contains held-out games")
      }
      width <- .revealed_perception_width_for_fold_1d(
        width_estimates, fold_id, role_name
      )
      role_rows[, inventory_loss :=
        .revealed_perception_losses_for_fold_1d(
          role_rows, state_values, fold_id, role_name
        )]
      for (kappa_value in kappa) {
        profile <- revealed_perception_profile_thresholds_1d(
          role_rows, prior, width, kappa_value,
          lookup_grid_step = lookup_grid_step
        )
        training <- profile[profile$fold != fold_id]
        heldout <- profile[profile$fold == fold_id]
        if (length(intersect(unique(training$game_pk), unique(heldout$game_pk)))) {
          stop("Threshold-bias training and held-out games overlap")
        }
        bias <- fit_revealed_perception_threshold_bias_1d(
          training,
          bias_bounds_inches = bias_bounds_inches,
          probability_epsilon = probability_epsilon
        )
        scored <- score_revealed_perception_profile_1d(
          heldout,
          threshold_bias_inches = bias$threshold_bias_inches,
          probability_epsilon = probability_epsilon
        )
        scored[, feature_outer_fold := fold_id]
        index <- index + 1L
        profile_parts[[index]] <- scored
        bias[, `:=`(
          fold = fold_id, role = role_name, kappa = kappa_value,
          effective_sigma_inches = width,
          sensory_sigma_inches = width * kappa_value,
          action_sigma_inches = width * sqrt(1 - kappa_value^2),
          heldout_rows = nrow(heldout),
          heldout_games = data.table::uniqueN(heldout$game_pk)
        )]
        bias_parts[[index]] <- bias
      }
    }
  }
  oof <- data.table::rbindlist(profile_parts, use.names = TRUE, fill = TRUE)
  bias <- data.table::rbindlist(bias_parts, use.names = TRUE, fill = TRUE)
  data.table::setorder(oof, fold, role, kappa, game_pk, pitch_order)
  data.table::setorder(bias, fold, role, kappa)
  expected_rows <- nrow(x) * length(kappa)
  if (nrow(oof) != expected_rows || anyDuplicated(oof[, .(row_id, kappa)])) {
    stop("Every action must receive exactly one OOF score per role kappa")
  }
  role_metrics <- .revealed_perception_role_metrics_1d(oof)
  scenarios <- .revealed_perception_scenario_metrics_1d(oof, kappa)
  decomposition <- data.table::rbindlist(lapply(
    seq_len(nrow(data.table::as.data.table(width_estimates))),
    function(row) {
      width_row <- data.table::as.data.table(width_estimates)[row]
      data.table::rbindlist(lapply(kappa, function(value) {
        out <- decompose_revealed_perception_width_1d(
          width_row$sigma_inches, value
        )
        out[, `:=`(
          fold = as.integer(width_row$fold),
          role = as.character(width_row$role)
        )]
        out
      }))
    }
  ))
  data.table::setcolorder(decomposition, c("fold", "role", "kappa"))
  result <- list(
    rows = x[],
    kappa_grid = kappa,
    width_decomposition = decomposition[],
    threshold_bias = bias[],
    oof_profiles = oof[],
    role_metrics = role_metrics[],
    scenario_metrics = scenarios$metrics[],
    scenario_game_scores = scenarios$game_scores[],
    information_set = c(
      "challenge_or_pass", "role_oriented_true_margin", "current_gain_G",
      "outer-fold Bellman inventory loss_L", "count_context",
      "outer-fold contextual margin prior", "outer-fold effective width"
    ),
    excluded_information = revealed_perception_profile_outcome_columns_1d(),
    profile_definition = paste(
      "sensory/effective SD ratio kappa; contextual q inversion;",
      "pooled role/fold/kappa signal-threshold bias"
    )
  )
  class(result) <- "revealed_perception_profile_1d"
  result$manifest <- revealed_perception_profile_manifest_1d(result)
  result
}
