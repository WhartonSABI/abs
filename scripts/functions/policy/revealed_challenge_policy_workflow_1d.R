# End-to-end revealed-selection, partial-identification, and shared-inventory
# policy workflow.  The workflow is deliberately separate from the dashboard
# and from the earlier continuous-perception artifacts.

revealed_challenge_policy_profile_1d <- function(
  profile = c("pilot", "full", "report")
) {
  profile <- match.arg(profile)
  if (profile == "pilot") {
    return(list(
      name = profile,
      folds = 5L,
      active_scenarios = .revealed_perception_scenario_id_1d(0.5, 0.5),
      inner_folds = 2L,
      bootstrap_reps = 0L,
      lookup_grid_step = 0.02,
      density_grid_step = 0.05,
      nthreads = 1L,
      seed = 20260826L
    ))
  }
  list(
    name = profile,
    folds = 5L,
    active_scenarios = NULL,
    inner_folds = 3L,
    bootstrap_reps = if (profile == "full") 2000L else 0L,
    lookup_grid_step = 0.01,
    density_grid_step = 0.025,
    nthreads = 1L,
    seed = 20260826L
  )
}

prepare_revealed_challenge_policy_opportunities_1d <- function(
  pitch_ledger, re_model
) {
  clean_ledger <- data.table::copy(data.table::as.data.table(pitch_ledger))
  # Fail closed before delegating to the legacy joint-clock constructor: the
  # primary workflow cannot use an official disposition even transiently.
  if ("challenge_outcome" %in% names(clean_ledger)) {
    clean_ledger[, challenge_outcome := NA_character_]
  }
  x <- prepare_joint_challenge_signal_mdp_opportunities_1d(
    clean_ledger, re_model, include_passive_rows = TRUE
  )
  if (!"pitch_family" %in% names(x)) {
    x[, pitch_family := if ("pitch_type" %in% names(x)) {
      revealed_challenge_selection_pitch_family_1d(pitch_type)
    } else {
      "other"
    }]
  }
  x[, `:=`(
    raw_count_state = as.character(count_state),
    pitch_family_coarse = data.table::fcoalesce(
      as.character(pitch_family), "other"
    )
  )]
  x[, prior_context := paste(
    raw_count_state, pitch_family_coarse, sep = "__"
  )]
  # Existing Bellman code names its prior-context column count_state.  Preserve
  # the baseball count explicitly and route the composite context through that
  # internal interface.
  x[, count_state := prior_context]
  # Official dispositions are a quarantined evaluation leaf. Structural
  # geometry truth remains available for held-out replay, but the Bellman and
  # prior interfaces cannot even see official challenge results.
  official_columns <- intersect(
    c(
      "challenge_outcome", "official_success", "truth_source",
      "official_geometry_mismatch"
    ),
    names(x)
  )
  if (length(official_columns)) x[, (official_columns) := NULL]
  data.table::setorder(x, game_pk, team_id, pitch_order)
  if (anyDuplicated(x[, .(game_pk, pitch_order)])) {
    stop("Revealed-policy opportunities contain duplicate pitches")
  }
  data.table::setattr(x, "information_regime", list(
    true_margin = "evaluation and signal integration only",
    decision_context = "count x coarse pitch family adverse-call prior",
    inventory = "one shared chronological offense/defense team inventory",
    official_outcomes = "absent; joined only after all policies are frozen"
  ))
  x[]
}

prepare_revealed_perception_action_rows_1d <- function(
  selection_rows, policy_opportunities, fold_assignment,
  stake_tolerance = 1e-8
) {
  action <- data.table::copy(data.table::as.data.table(selection_rows))
  assert_revealed_challenge_selection_1d_outcome_free(
    action, "revealed-perception action rows"
  )
  stop_if_missing_columns(
    action,
    c(
      "game_pk", "pitch_order", "role", "challenged", "inventory_before",
      "role_margin_inches", "stake_G"
    ),
    "revealed-perception action rows"
  )
  if (any(action$inventory_before < 1L) ||
      any(!action$challenged %in% 0:1)) {
    stop("Perception-profile likelihood rows need available inventory")
  }
  bellman <- data.table::copy(data.table::as.data.table(policy_opportunities))
  bellman <- bellman[decision_mode == "structural", .(
    game_pk = as.character(game_pk),
    pitch_order = as.integer(pitch_order),
    role = as.character(role),
    stage = as.integer(stage),
    prior_context = as.character(prior_context),
    stake_G_bellman = as.numeric(stake_G)
  )]
  if (anyDuplicated(bellman[, .(game_pk, pitch_order, role)])) {
    stop("Structural Bellman keys are duplicated")
  }
  action[, `:=`(
    game_pk = as.character(game_pk),
    pitch_order = as.integer(pitch_order),
    role = as.character(role),
    stake_G_action = as.numeric(stake_G)
  )]
  x <- merge(
    action,
    bellman,
    by = c("game_pk", "pitch_order", "role"),
    all.x = TRUE,
    sort = FALSE
  )
  if (anyNA(x$stage) || anyNA(x$prior_context) ||
      any(!is.finite(x$stake_G_bellman)) ||
      any(abs(x$stake_G_action - x$stake_G_bellman) > stake_tolerance)) {
    stop("Selection and Bellman opportunity rows do not align")
  }
  folds <- normalize_joint_common_width_fold_assignment_1d(
    fold_assignment, unique(x$game_pk)
  )
  x[, fold := folds$fold[match(game_pk, folds$game_pk)]]
  x[, row_id := paste(role, game_pk, pitch_order, sep = "|")]
  out <- x[, .(
    row_id,
    game_pk,
    pitch_order,
    fold,
    role,
    stage,
    count_state = prior_context,
    raw_count_state = as.character(count_state),
    role_margin_inches = as.numeric(role_margin_inches),
    challenged = as.integer(challenged),
    stake_G = stake_G_bellman,
    inventory_before = pmin(2L, as.integer(inventory_before))
  )]
  if (anyDuplicated(out$row_id) || anyNA(out)) {
    stop("Prepared perception-profile action rows are incomplete")
  }
  .revealed_perception_assert_no_outcomes_1d(
    out, "prepared joint perception-profile rows"
  )
  out[]
}

revealed_perception_joint_scenarios_1d <- function(
  kappa_grid = revealed_perception_kappa_grid_1d()
) {
  kappa <- validate_revealed_perception_kappa_grid_1d(kappa_grid)
  out <- data.table::CJ(
    offense_kappa = kappa,
    defense_kappa = kappa,
    sorted = TRUE
  )
  out[, scenario_id := .revealed_perception_scenario_id_1d(
    offense_kappa, defense_kappa
  )]
  data.table::setcolorder(
    out, c("scenario_id", "offense_kappa", "defense_kappa")
  )
  out[]
}

.revealed_policy_fold_sigmas_1d <- function(
  width_estimates, scenario, folds
) {
  widths <- data.table::as.data.table(width_estimates)
  stop_if_missing_columns(
    widths, c("fold", "role", "sigma_inches"),
    "revealed-policy effective widths"
  )
  lapply(seq_len(folds), function(fold_id) {
    role_width <- widths[fold == fold_id]
    if (nrow(role_width) != 2L ||
        !setequal(role_width$role, c("offense", "defense"))) {
      stop("Every policy fold needs one effective width per role")
    }
    c(
      offense = role_width[role == "offense", sigma_inches] *
        scenario$offense_kappa[[1L]],
      defense = role_width[role == "defense", sigma_inches] *
        scenario$defense_kappa[[1L]]
    )
  })
}

.revealed_policy_state_values_1d <- function(result, scenario) {
  data.table::rbindlist(lapply(seq_along(result$fold_fits), function(fold_id) {
    out <- data.table::copy(result$fold_fits[[fold_id]]$state_values)
    out[, `:=`(
      fold = fold_id,
      scenario_id = scenario$scenario_id[[1L]],
      offense_kappa = scenario$offense_kappa[[1L]],
      defense_kappa = scenario$defense_kappa[[1L]]
    )]
    out
  }), use.names = TRUE, fill = TRUE)
}

crossfit_revealed_policy_bellman_scenarios_1d <- function(
  opportunities,
  fold_assignment,
  fold_prior_fits,
  width_estimates,
  scenarios = revealed_perception_joint_scenarios_1d(),
  prior_n = 30,
  tol = 1e-7,
  max_iter = 10000L,
  lookup_grid_step = 0.01,
  keep_replays = FALSE,
  progress = interactive()
) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  scenarios <- data.table::copy(data.table::as.data.table(scenarios))
  stop_if_missing_columns(
    scenarios, c("scenario_id", "offense_kappa", "defense_kappa"),
    "revealed-policy scenarios"
  )
  folds <- max(as.integer(data.table::as.data.table(fold_assignment)$fold))
  states <- diagnostics <- replays <- vector("list", nrow(scenarios))
  for (scenario_index in seq_len(nrow(scenarios))) {
    scenario <- scenarios[scenario_index]
    if (isTRUE(progress)) {
      message(
        "revealed-policy Bellman scenario ", scenario_index, "/",
        nrow(scenarios), ": ", scenario$scenario_id
      )
    }
    fold_sigmas <- .revealed_policy_fold_sigmas_1d(
      width_estimates, scenario, folds
    )
    result <- crossfit_joint_challenge_signal_mdp_1d(
      x,
      perception_sigma = c(offense = 1, defense = 1),
      fold_assignment = fold_assignment,
      folds = folds,
      prior_components = c(offense = 1L, defense = 1L),
      fold_prior_fits = fold_prior_fits,
      fold_perception_sigma = fold_sigmas,
      prior_n = prior_n,
      tol = tol,
      max_iter = max_iter,
      lookup_grid_step = lookup_grid_step,
      bootstrap_reps = 0L,
      progress = FALSE
    )
    states[[scenario_index]] <- .revealed_policy_state_values_1d(
      result, scenario
    )
    diagnostic <- data.table::copy(result$fold_diagnostics)
    diagnostic[, `:=`(
      scenario_id = scenario$scenario_id[[1L]],
      offense_kappa = scenario$offense_kappa[[1L]],
      defense_kappa = scenario$defense_kappa[[1L]]
    )]
    diagnostics[[scenario_index]] <- diagnostic
    if (isTRUE(keep_replays)) {
      replay <- data.table::copy(result$replay)
      retained <- c(
        "game_pk", "team_id", "pitch_order", "fold", "role", "stage",
        "decision_mode", "stake_G", "count_state", "role_margin_inches",
        "signal_threshold_k1_inches", "signal_threshold_k2_inches",
        "marginal_inventory_re_k1", "marginal_inventory_re_k2",
        "context_fallback_k1", "context_fallback_k2",
        "exact_root_fallback_k1", "exact_root_fallback_k2"
      )
      stop_if_missing_columns(
        replay, retained, "accepted normative Bellman replay"
      )
      replay <- replay[, ..retained]
      replay[, `:=`(
        scenario_id = scenario$scenario_id[[1L]],
        offense_kappa = scenario$offense_kappa[[1L]],
        defense_kappa = scenario$defense_kappa[[1L]]
      )]
      replays[[scenario_index]] <- replay
    }
  }
  list(
    scenarios = scenarios[],
    state_values = data.table::rbindlist(states, use.names = TRUE, fill = TRUE),
    diagnostics = data.table::rbindlist(
      diagnostics, use.names = TRUE, fill = TRUE
    ),
    replays = if (isTRUE(keep_replays)) {
      data.table::rbindlist(replays, use.names = TRUE, fill = TRUE)
    } else {
      data.table::data.table()
    },
    information_regime = paste(
      "each perception scenario solved on outer-training opportunity",
      "transitions with one shared offense/defense inventory"
    )
  )
}

.revealed_policy_scenario_losses_1d <- function(
  rows, state_values, scenario_id, fold, role
) {
  scenario_value <- scenario_id
  fold_value <- as.integer(fold)
  role_value <- as.character(role)
  states <- data.table::as.data.table(state_values)[
    scenario_id == scenario_value & fold == fold_value & role == role_value
  ]
  if (!nrow(states) || anyDuplicated(states$stage)) {
    stop("Scenario Bellman state values are missing or duplicated")
  }
  index <- match(as.integer(rows$stage), as.integer(states$stage))
  if (anyNA(index)) stop("Scenario Bellman states omit an action stage")
  loss <- ifelse(
    as.integer(rows$inventory_before) == 1L,
    states$marginal_re_1_to_0[index],
    states$marginal_re_2_to_1[index]
  )
  if (anyNA(loss) || any(!is.finite(loss)) || any(loss < -1e-10)) {
    stop("Scenario Bellman inventory losses are invalid")
  }
  pmax(0, loss)
}

.revealed_policy_clustered_loss_1d <- function(game_scores) {
  metric <- .revealed_perception_cluster_metric_1d(game_scores)
  data.table::data.table(
    rows = as.integer(metric[["rows"]]),
    games = as.integer(metric[["games"]]),
    mean_log_loss = as.numeric(metric[["mean_log_loss"]]),
    game_clustered_se = as.numeric(metric[["game_clustered_se"]])
  )
}

.revealed_policy_accept_scenarios_1d <- function(game_scores, scenarios) {
  metrics <- game_scores[, .(
    loss_sum = sum(loss_sum), rows = sum(rows)
  ), by = .(scenario_id, game_pk)]
  summary <- metrics[, .revealed_policy_clustered_loss_1d(.SD),
    by = scenario_id
  ]
  best_id <- summary[which.min(mean_log_loss), scenario_id]
  best <- metrics[scenario_id == best_id, .(
    game_pk, best_loss_sum = loss_sum, best_rows = rows
  )]
  paired <- merge(metrics, best, by = "game_pk", all.x = TRUE, sort = FALSE)
  difference <- paired[, {
    difference_sum <- loss_sum - best_loss_sum
    total_rows <- sum(rows)
    estimate <- sum(difference_sum) / total_rows
    residual <- difference_sum - estimate * rows
    se <- if (.N > 1L) {
      sqrt(.N / (.N - 1L) * sum(residual^2) / total_rows^2)
    } else {
      NA_real_
    }
    .(
      difference_from_best = estimate,
      difference_game_clustered_se = se
    )
  }, by = scenario_id]
  out <- merge(summary, difference, by = "scenario_id", sort = FALSE)
  out <- merge(out, scenarios, by = "scenario_id", all.x = TRUE, sort = FALSE)
  out[, `:=`(
    best_scenario = scenario_id == best_id,
    one_se_accepted = scenario_id == best_id |
      difference_from_best <= difference_game_clustered_se + 1e-15,
    comparison_rule =
      "paired game-clustered loss difference from best <= one SE"
  )]
  data.table::setorder(out, mean_log_loss, offense_kappa, defense_kappa)
  out[]
}

.revealed_policy_direct_benchmark_gate_1d <- function(
  profile_game_scores, direct_selection_oof, best_scenario_id
) {
  direct <- data.table::copy(data.table::as.data.table(direct_selection_oof))
  stop_if_missing_columns(
    direct, c("game_pk", "challenged", "challenge_probability"),
    "direct selection benchmark"
  )
  probability <- pmin(
    1 - 1e-12, pmax(1e-12, as.numeric(direct$challenge_probability))
  )
  direct[, direct_loss := -(
    challenged * log(probability) + (1 - challenged) * log1p(-probability)
  )]
  direct_game <- direct[, .(
    direct_loss_sum = sum(direct_loss), direct_rows = .N
  ), by = game_pk]
  structural <- profile_game_scores[scenario_id == best_scenario_id, .(
    structural_loss_sum = sum(loss_sum), structural_rows = sum(rows)
  ), by = game_pk]
  paired <- merge(structural, direct_game, by = "game_pk", all = TRUE)
  if (anyNA(paired) || any(paired$structural_rows != paired$direct_rows)) {
    stop("Structural profile and direct selection benchmark rows do not align")
  }
  paired[, difference_sum := structural_loss_sum - direct_loss_sum]
  total_rows <- sum(paired$structural_rows)
  estimate <- sum(paired$difference_sum) / total_rows
  residual <- paired$difference_sum - estimate * paired$structural_rows
  se <- if (nrow(paired) > 1L) {
    sqrt(
      nrow(paired) / (nrow(paired) - 1L) *
        sum(residual^2) / total_rows^2
    )
  } else {
    NA_real_
  }
  data.table::data.table(
    best_scenario_id = best_scenario_id,
    structural_minus_direct_log_loss = estimate,
    difference_game_clustered_se = se,
    structurally_noninferior_to_direct_by_one_se =
      is.finite(se) && estimate <= se,
    interpretation = if (is.finite(se) && estimate <= se) {
      "behaviorally supported partial-identification profile"
    } else {
      "assumption-driven sensitivity analysis"
    }
  )
}

crossfit_revealed_joint_perception_profile_1d <- function(
  action_rows,
  fold_prior_fits,
  width_estimates,
  scenario_state_values,
  scenarios = revealed_perception_joint_scenarios_1d(),
  direct_selection_oof = NULL,
  lookup_grid_step = 0.01,
  bias_bounds_inches = c(-20, 20),
  probability_epsilon = 1e-12,
  keep_oof_profiles = FALSE,
  progress = interactive()
) {
  x <- data.table::copy(data.table::as.data.table(action_rows))
  .revealed_perception_assert_no_outcomes_1d(
    x, "joint perception-profile action rows"
  )
  scenarios <- data.table::copy(data.table::as.data.table(scenarios))
  fold_ids <- sort(unique(as.integer(x$fold)))
  if (length(fold_prior_fits) < max(fold_ids)) {
    stop("Every profile fold needs one role-specific training prior")
  }
  for (fold_id in fold_ids) {
    for (role_value in revealed_challenge_selection_1d_roles()) {
      prior <- fold_prior_fits[[fold_id]][[role_value]]
      if (is.null(prior)) {
        stop("A fold/role profile prior is missing")
      }
      expected_training_games <- sort(unique(x[
        fold != fold_id & role == role_value, as.character(game_pk)
      ]))
      prior_training_games <- sort(as.character(
        prior$training_games %||% character()
      ))
      if (!length(prior_training_games) ||
          !identical(prior_training_games, expected_training_games)) {
        stop(
          "Profile prior games do not exactly match the outer-training ",
          "games for fold ", fold_id, " ", role_value
        )
      }
    }
  }
  profile_games <- bias_rows <- role_metrics <- threshold_diagnostics <- list()
  oof_parts <- if (isTRUE(keep_oof_profiles)) list() else NULL
  index <- 0L
  for (scenario_index in seq_len(nrow(scenarios))) {
    scenario <- scenarios[scenario_index]
    for (fold_id in sort(unique(x$fold))) {
      for (role_value in c("offense", "defense")) {
        index <- index + 1L
        if (isTRUE(progress)) {
          message(
            "revealed profile ", scenario_index, "/", nrow(scenarios),
            " fold ", fold_id, " ", role_value
          )
        }
        kappa <- if (role_value == "offense") {
          scenario$offense_kappa[[1L]]
        } else {
          scenario$defense_kappa[[1L]]
        }
        width <- .revealed_perception_width_for_fold_1d(
          width_estimates, fold_id, role_value
        )
        prior <- .revealed_perception_prior_for_fold_1d(
          fold_prior_fits, fold_id, role_value
        )
        role_rows <- data.table::copy(x[role == role_value])
        role_rows[, inventory_loss := .revealed_policy_scenario_losses_1d(
          role_rows,
          scenario_state_values,
          scenario$scenario_id[[1L]],
          fold_id,
          role_value
        )]
        profile <- revealed_perception_profile_thresholds_1d(
          role_rows,
          prior,
          effective_sigma_inches = width,
          kappa = kappa,
          lookup_grid_step = lookup_grid_step
        )
        training <- profile[fold != fold_id]
        heldout <- profile[fold == fold_id]
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
        scored[, `:=`(
          scenario_id = scenario$scenario_id[[1L]],
          offense_kappa = scenario$offense_kappa[[1L]],
          defense_kappa = scenario$defense_kappa[[1L]],
          feature_outer_fold = fold_id
        )]
        profile_games[[index]] <- scored[, .(
          loss_sum = sum(log_loss), rows = .N,
          challenges = sum(challenged),
          predicted_challenges = sum(challenge_probability)
        ), by = .(scenario_id, game_pk, role)]
        bias[, `:=`(
          scenario_id = scenario$scenario_id[[1L]],
          offense_kappa = scenario$offense_kappa[[1L]],
          defense_kappa = scenario$defense_kappa[[1L]],
          fold = fold_id,
          role = role_value,
          kappa = kappa,
          effective_sigma_inches = width,
          sensory_sigma_inches = width * kappa,
          action_sigma_inches = width * sqrt(1 - kappa^2)
        )]
        bias_rows[[index]] <- bias
        role_metrics[[index]] <- scored[, .(
          loss_sum = sum(log_loss), rows = .N
        ), by = .(scenario_id, role, game_pk)]
        threshold_diagnostics[[index]] <- scored[, .(
          scenario_id = scenario$scenario_id[[1L]],
          fold = fold_id,
          role = role_value,
          kappa = kappa,
          threshold_rows = .N,
          finite_threshold_rows = sum(is.finite(signal_threshold_inches)),
          always_challenge_rows = sum(signal_threshold_inches == -Inf),
          never_challenge_rows = sum(signal_threshold_inches == Inf),
          context_fallback_rows = sum(context_fallback),
          maximum_inversion_absolute_error = if (
            any(is.finite(inversion_absolute_error))
          ) max(inversion_absolute_error, na.rm = TRUE) else NA_real_
        )]
        if (isTRUE(keep_oof_profiles)) oof_parts[[index]] <- scored
      }
    }
  }
  game_scores <- data.table::rbindlist(profile_games, use.names = TRUE)
  scenario_metrics <- .revealed_policy_accept_scenarios_1d(
    game_scores, scenarios
  )
  role_game <- data.table::rbindlist(role_metrics, use.names = TRUE)
  role_summary <- role_game[, .revealed_policy_clustered_loss_1d(.SD),
    by = .(scenario_id, role)
  ]
  benchmark_gate <- data.table::data.table(
    best_scenario_id = scenario_metrics[best_scenario == TRUE, scenario_id],
    structural_minus_direct_log_loss = NA_real_,
    difference_game_clustered_se = NA_real_,
    structurally_noninferior_to_direct_by_one_se = FALSE,
    interpretation = "direct selection benchmark not supplied"
  )
  if (!is.null(direct_selection_oof)) {
    benchmark_gate <- .revealed_policy_direct_benchmark_gate_1d(
      game_scores,
      direct_selection_oof,
      scenario_metrics[best_scenario == TRUE, scenario_id]
    )
  }
  scenario_metrics[, `:=`(
    structurally_noninferior_to_direct_by_one_se =
      benchmark_gate$structurally_noninferior_to_direct_by_one_se[[1L]],
    interpretation = benchmark_gate$interpretation[[1L]]
  )]
  result <- list(
    scenarios = scenarios[],
    scenario_metrics = scenario_metrics[],
    role_metrics = role_summary[],
    scenario_game_scores = game_scores[],
    threshold_bias = data.table::rbindlist(bias_rows, fill = TRUE),
    threshold_diagnostics = data.table::rbindlist(
      threshold_diagnostics, fill = TRUE
    ),
    direct_benchmark_gate = benchmark_gate[],
    oof_profiles = if (isTRUE(keep_oof_profiles)) {
      data.table::rbindlist(oof_parts, fill = TRUE)
    } else {
      data.table::data.table()
    },
    information_regime = paste(
      "25 joint sensory/action decompositions; scenario-specific",
      "outer-training Bellman L; challenge/pass likelihood only"
    )
  )
  class(result) <- "revealed_joint_perception_profile_1d"
  result
}

.revealed_policy_threshold_probability_1d <- function(
  margin, threshold, sensory_sigma
) {
  revealed_policy_signal_integration_probability_1d(
    role_margin_inches = margin,
    signal_threshold_inches = threshold,
    sensory_sigma_inches = sensory_sigma
  )
}

build_revealed_policy_thresholds_1d <- function(
  accepted_replays, width_estimates
) {
  x <- data.table::copy(data.table::as.data.table(accepted_replays))
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "team_id", "pitch_order", "fold", "role", "stage",
      "scenario_id", "offense_kappa", "defense_kappa", "stake_G",
      "count_state", "decision_mode", "signal_threshold_k1_inches",
      "signal_threshold_k2_inches", "marginal_inventory_re_k1",
      "marginal_inventory_re_k2"
    ),
    "accepted normative replay"
  )
  x <- x[decision_mode == "structural"]
  widths <- data.table::as.data.table(width_estimates)[, .(
    fold = as.integer(fold), role = as.character(role),
    effective_width_inches = as.numeric(sigma_inches)
  )]
  x <- merge(x, widths, by = c("fold", "role"), all.x = TRUE, sort = FALSE)
  x[, kappa := ifelse(role == "offense", offense_kappa, defense_kappa)]
  x[, `:=`(
    sensory_sigma_inches = effective_width_inches * kappa,
    action_sigma_inches = effective_width_inches * sqrt(1 - kappa^2)
  )]
  make_inventory <- function(inventory) {
    loss_column <- paste0("marginal_inventory_re_k", inventory)
    threshold_column <- paste0("signal_threshold_k", inventory, "_inches")
    out <- x[, .(
      game_pk,
      team_id,
      pitch_order,
      fold,
      scenario_id,
      offense_kappa,
      defense_kappa,
      role,
      stage,
      inventory = as.integer(inventory),
      prior_context = as.character(count_state),
      G = as.numeric(stake_G),
      L = as.numeric(get(loss_column)),
      model_implied_r_star_inches = as.numeric(get(threshold_column)),
      effective_width_inches,
      sensory_sigma_inches,
      action_sigma_inches,
      context_fallback = if (inventory == 1L) {
        as.logical(context_fallback_k1)
      } else {
        as.logical(context_fallback_k2)
      },
      numerical_root_fallback = if (inventory == 1L) {
        as.logical(exact_root_fallback_k1)
      } else {
        as.logical(exact_root_fallback_k2)
      }
    )]
    out[, q_star := data.table::fcase(
      G <= 0, 1,
      L == 0 & G > 0, 0,
      default = L / (G + L)
    )]
    out[, threshold_status := data.table::fcase(
      G <= 0, "nonpositive_gain_never_challenge",
      L == 0 & G > 0, "zero_continuation_loss_always_challenge",
      is.finite(model_implied_r_star_inches), "interior_q_inverse",
      default = "nonfinite_policy_threshold"
    )]
    out
  }
  out <- data.table::rbindlist(
    list(make_inventory(1L), make_inventory(2L)),
    use.names = TRUE,
    fill = TRUE
  )
  if (anyNA(out[, .(G, L, q_star)]) || any(out$L < 0) ||
      any(out$q_star < 0 | out$q_star > 1) ||
      any(abs(
        out[G > 0 & L > 0, q_star] -
          out[G > 0 & L > 0, L / (G + L)]
      ) > 1e-12)) {
    stop("Policy threshold payoff identities failed")
  }
  # Thresholds are functions of fold/scenario/state/context/payoffs, not pitch
  # identity. Persist one row per distinct threshold with its held-out exposure
  # rather than repeating identical rows millions of times.
  out <- out[, .(
    opportunity_rows = .N,
    context_fallback = any(context_fallback),
    context_fallback_rows = sum(context_fallback),
    numerical_root_fallback = any(numerical_root_fallback),
    numerical_root_fallback_rows = sum(numerical_root_fallback)
  ), by = .(
    fold,
    scenario_id,
    offense_kappa,
    defense_kappa,
    role,
    stage,
    inventory,
    prior_context,
    G,
    L,
    q_star,
    model_implied_r_star_inches,
    effective_width_inches,
    sensory_sigma_inches,
    action_sigma_inches,
    threshold_status
  )]
  data.table::setorder(
    out, scenario_id, fold, role, stage, inventory, prior_context, G
  )
  out[]
}

build_revealed_normative_probability_rows_1d <- function(
  accepted_replays, width_estimates
) {
  x <- data.table::copy(data.table::as.data.table(accepted_replays))
  x <- x[decision_mode == "structural"]
  widths <- data.table::as.data.table(width_estimates)[, .(
    fold = as.integer(fold), role = as.character(role),
    effective_width_inches = as.numeric(sigma_inches)
  )]
  x <- merge(x, widths, by = c("fold", "role"), all.x = TRUE, sort = FALSE)
  x[, kappa := ifelse(role == "offense", offense_kappa, defense_kappa)]
  x[, sensory_sigma_inches := effective_width_inches * kappa]
  probability_k1 <- .revealed_policy_threshold_probability_1d(
    x$role_margin_inches,
    x$signal_threshold_k1_inches,
    x$sensory_sigma_inches
  )
  probability_k2 <- .revealed_policy_threshold_probability_1d(
    x$role_margin_inches,
    x$signal_threshold_k2_inches,
    x$sensory_sigma_inches
  )
  out <- x[, .(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    scenario_id,
    policy = paste0("normative__", scenario_id),
    probability_k1 = probability_k1,
    probability_k2 = probability_k2
  )]
  if (any(!is.finite(out$probability_k1)) ||
      any(!is.finite(out$probability_k2)) ||
      any(out$probability_k1 < 0 | out$probability_k1 > 1) ||
      any(out$probability_k2 < 0 | out$probability_k2 > 1)) {
    stop("Normative held-out action probabilities are invalid")
  }
  out[]
}

.revealed_policy_smoothed_propensity_1d <- function(
  margin, probability, grid
) {
  margin <- as.numeric(margin)
  probability <- pmin(1 - 1e-8, pmax(1e-8, as.numeric(probability)))
  keep <- is.finite(margin) & is.finite(probability)
  margin <- margin[keep]
  probability <- probability[keep]
  if (length(margin) < 12L || data.table::uniqueN(margin) < 6L ||
      stats::sd(stats::qnorm(probability)) < 1e-8) {
    return(rep(mean(probability), length(grid)))
  }
  fitted <- tryCatch(
    stats::smooth.spline(
      x = margin,
      y = stats::qnorm(probability),
      spar = 0.65
    ),
    error = function(error) NULL
  )
  if (is.null(fitted)) return(rep(mean(probability), length(grid)))
  pmin(
    1 - 1e-8,
    pmax(1e-8, stats::pnorm(stats::predict(fitted, grid)$y))
  )
}

build_revealed_challenge_margin_distributions_1d <- function(
  prior_crossfit,
  selection_oof,
  grid_step = 0.025,
  tail_standard_deviations = 12,
  direct_truth_tolerance = 0.05
) {
  if (!inherits(prior_crossfit, "revealed_challenge_prior_1d_crossfit")) {
    stop("prior_crossfit must be a revealed challenge prior cross-fit")
  }
  x <- data.table::copy(data.table::as.data.table(selection_oof))
  assert_revealed_challenge_selection_1d_outcome_free(
    x, "margin-distribution selection rows"
  )
  stop_if_missing_columns(
    x,
    c(
      "fold", "role", "count_state", "pitch_family_coarse",
      "role_margin_inches", "challenge_probability", "selected_model"
    ),
    "margin-distribution selection rows"
  )
  step <- as.numeric(grid_step)
  tail_sd <- as.numeric(tail_standard_deviations)
  truth_tolerance <- as.numeric(direct_truth_tolerance)
  if (!is.finite(step) || step <= 0 || !is.finite(tail_sd) || tail_sd < 8) {
    stop("Invalid selected-margin density grid controls")
  }
  if (length(truth_tolerance) != 1L || !is.finite(truth_tolerance) ||
      truth_tolerance <= 0 || truth_tolerance >= 1) {
    stop("direct_truth_tolerance must be one number in (0, 1)")
  }
  x[, prior_context := paste(
    as.character(count_state), as.character(pitch_family_coarse), sep = "__"
  )]
  groups <- unique(x[, .(fold, role, prior_context)])
  pieces <- vector("list", nrow(groups))
  for (group_index in seq_len(nrow(groups))) {
    group <- groups[group_index]
    fold_id <- as.integer(group$fold[[1L]])
    role_value <- as.character(group$role[[1L]])
    context_value <- as.character(group$prior_context[[1L]])
    fit <- prior_crossfit$primary_fits[[fold_id]][[role_value]]
    rows <- x[
      fold == fold_id & role == role_value & prior_context == context_value
    ]
    if (!nrow(rows)) stop("A selected-margin context has no held-out rows")
    component_scale <- sqrt(fit$sd^2)
    lower <- min(
      rows$role_margin_inches,
      fit$mean - tail_sd * component_scale,
      -12
    )
    upper <- max(
      rows$role_margin_inches,
      fit$mean + tail_sd * component_scale,
      12
    )
    lower <- floor(lower / step) * step
    upper <- ceiling(upper / step) * step
    grid <- seq(lower, upper, by = step)
    if (tail(grid, 1L) < upper) grid <- c(grid, upper)
    grid <- sort(unique(round(grid / step) * step))
    density <- challenge_margin_prior_density_1d(
      fit, grid, context = context_value
    )
    propensity <- .revealed_policy_smoothed_propensity_1d(
      rows$role_margin_inches, rows$challenge_probability, grid
    )
    opportunity_mass <- .revealed_challenge_trapezoid_1d(grid, density)
    opportunity <- density / opportunity_mass
    selected_unnormalized <- opportunity * propensity
    action_mass <- .revealed_challenge_trapezoid_1d(
      grid, selected_unnormalized
    )
    selected <- selected_unnormalized / action_mass
    selected_success <- .revealed_challenge_trapezoid_above_zero_1d(
      grid, selected
    )
    opportunity_success_numeric <-
      .revealed_challenge_trapezoid_above_zero_1d(grid, opportunity)
    opportunity_success_analytic <- challenge_margin_prior_ball_rate_1d(
      fit, context_value
    )
    pieces[[group_index]] <- data.table::data.table(
      fold = fold_id,
      role = role_value,
      prior_context = context_value,
      margin_inches = grid,
      opportunity_density = opportunity,
      challenge_probability = propensity,
      selected_density = selected,
      density_ratio = selected / opportunity,
      selected_action_mass = action_mass,
      opportunity_wrong_call_mass_analytic = opportunity_success_analytic,
      opportunity_wrong_call_mass_numeric = opportunity_success_numeric,
      opportunity_wrong_call_mass_absolute_error = abs(
        opportunity_success_analytic - opportunity_success_numeric
      ),
      selected_wrong_call_mass = selected_success,
      selected_model = unique(rows$selected_model)[[1L]],
      heldout_context_rows = nrow(rows),
      heldout_context_challenges = sum(rows$challenged)
    )
  }
  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  split_context <- data.table::tstrsplit(
    out$prior_context, "__", fixed = TRUE, keep = 1:2
  )
  out[, `:=`(
    count_state = split_context[[1L]],
    pitch_family_coarse = split_context[[2L]]
  )]
  checks <- out[, .(
    opportunity_mass = .revealed_challenge_trapezoid_1d(
      margin_inches, opportunity_density
    ),
    selected_mass = .revealed_challenge_trapezoid_1d(
      margin_inches, selected_density
    ),
    analytic_numeric_wrong_mass_error = max(
      opportunity_wrong_call_mass_absolute_error
    )
  ), by = .(fold, role, prior_context)]
  if (any(abs(checks$opportunity_mass - 1) > 1e-6) ||
      any(abs(checks$selected_mass - 1) > 1e-6) ||
      any(checks$analytic_numeric_wrong_mass_error > 0.001) ||
      any(!is.finite(out$challenge_probability)) ||
      any(out$challenge_probability < 0 | out$challenge_probability > 1)) {
    stop("Selected-margin density validation failed")
  }
  x[, geometry_wrong__ := revealed_challenge_geometry_success_1d(
    role, role_margin_inches
  )]
  direct_validation <- x[, .(
    direct_propensity_weighted_success = sum(
      challenge_probability * geometry_wrong__
    ) / sum(challenge_probability),
    direct_mean_propensity = mean(challenge_probability)
  ), by = .(fold, role)]
  derived_validation <- unique(out[, .(
    fold, role, prior_context, selected_action_mass,
    selected_wrong_call_mass, heldout_context_rows
  )])[, .(
    derived_propensity_weighted_success = sum(
      heldout_context_rows * selected_action_mass * selected_wrong_call_mass
    ) / sum(heldout_context_rows * selected_action_mass),
    derived_mean_propensity = sum(
      heldout_context_rows * selected_action_mass
    ) / sum(heldout_context_rows)
  ), by = .(fold, role)]
  validation <- merge(
    direct_validation, derived_validation,
    by = c("fold", "role"), all = TRUE, sort = FALSE
  )
  if (anyNA(validation)) {
    stop("Direct and derived challenged-margin validations do not align")
  }
  validation[, `:=`(
    direct_derived_success_absolute_error = abs(
      direct_propensity_weighted_success -
        derived_propensity_weighted_success
    ),
    direct_derived_propensity_absolute_error = abs(
      direct_mean_propensity - derived_mean_propensity
    ),
    direct_truth_tolerance = truth_tolerance
  )]
  if (any(validation$direct_derived_success_absolute_error > truth_tolerance)) {
    stop(
      "Derived challenged-margin success does not match direct OOF ",
      "propensity-weighted geometry within tolerance"
    )
  }
  out <- merge(out, validation, by = c("fold", "role"), sort = FALSE)
  data.table::setcolorder(
    out,
    c(
      "fold", "role", "count_state", "pitch_family_coarse",
      "prior_context", setdiff(
        names(out),
        c("fold", "role", "count_state", "pitch_family_coarse", "prior_context")
      )
    )
  )
  out[]
}

build_revealed_perception_profile_table_1d <- function(
  profile, width_estimates
) {
  if (!inherits(profile, "revealed_joint_perception_profile_1d")) {
    stop("profile must be a joint revealed-perception profile")
  }
  widths <- data.table::as.data.table(width_estimates)
  scenario_role <- data.table::rbindlist(lapply(
    seq_len(nrow(profile$scenario_metrics)),
    function(index) {
      scenario <- profile$scenario_metrics[index]
      data.table::rbindlist(lapply(c("offense", "defense"), function(role) {
        kappa <- if (role == "offense") {
          scenario$offense_kappa[[1L]]
        } else {
          scenario$defense_kappa[[1L]]
        }
        value <- data.table::copy(widths[as.character(widths$role) == role])
        value[, `:=`(
          scenario_id = scenario$scenario_id[[1L]],
          offense_kappa = scenario$offense_kappa[[1L]],
          defense_kappa = scenario$defense_kappa[[1L]],
          kappa = kappa,
          effective_width_inches = as.numeric(sigma_inches),
          sensory_sigma_inches = as.numeric(sigma_inches) * kappa,
          action_sigma_inches = as.numeric(sigma_inches) * sqrt(1 - kappa^2),
          joint_heldout_log_loss = scenario$mean_log_loss[[1L]],
          joint_game_clustered_se = scenario$game_clustered_se[[1L]],
          difference_from_best = scenario$difference_from_best[[1L]],
          difference_game_clustered_se =
            scenario$difference_game_clustered_se[[1L]],
          best_scenario = scenario$best_scenario[[1L]],
          one_se_membership = scenario$one_se_accepted[[1L]],
          behaviorally_supported =
            scenario$structurally_noninferior_to_direct_by_one_se[[1L]],
          interpretation = scenario$interpretation[[1L]]
        )]
        value
      }))
    }
  ), use.names = TRUE, fill = TRUE)
  bias <- profile$threshold_bias[, .(
    scenario_id, fold, role, threshold_bias_inches,
    threshold_bias_std_error, optimization_status
  )]
  scenario_role <- merge(
    scenario_role, bias,
    by = c("scenario_id", "fold", "role"),
    all.x = TRUE,
    sort = FALSE
  )
  scenario_role[, sigma_inches := NULL]
  data.table::setorder(scenario_role, scenario_id, fold, role)
  scenario_role[]
}

build_revealed_subjective_belief_envelope_1d <- function(
  fold_prior_fits,
  width_estimates,
  accepted_scenarios,
  signal_grid = seq(-20, 20, by = 0.1)
) {
  scenarios <- data.table::copy(data.table::as.data.table(accepted_scenarios))
  stop_if_missing_columns(
    scenarios, c("scenario_id", "offense_kappa", "defense_kappa"),
    "accepted subjective-belief scenarios"
  )
  signal <- sort(unique(as.numeric(signal_grid)))
  if (length(signal) < 3L || anyNA(signal) || any(!is.finite(signal)) ||
      any(diff(signal) <= 0)) {
    stop("signal_grid must contain at least three increasing finite values")
  }
  widths <- data.table::as.data.table(width_estimates)
  stop_if_missing_columns(
    widths, c("fold", "role", "sigma_inches"),
    "subjective-belief effective widths"
  )
  pieces <- list()
  index <- 0L
  for (scenario_index in seq_len(nrow(scenarios))) {
    scenario <- scenarios[scenario_index]
    for (fold_id in sort(unique(as.integer(widths$fold)))) {
      for (role_value in revealed_challenge_selection_1d_roles()) {
        fit <- fold_prior_fits[[fold_id]][[role_value]]
        if (is.null(fit)) stop("A subjective-belief fold prior is missing")
        width <- widths[
          fold == fold_id & role == role_value, as.numeric(sigma_inches)
        ]
        if (length(width) != 1L || !is.finite(width) || width <= 0) {
          stop("Every subjective-belief fold/role needs one positive width")
        }
        kappa <- if (role_value == "offense") {
          scenario$offense_kappa[[1L]]
        } else {
          scenario$defense_kappa[[1L]]
        }
        sensory <- width * kappa
        contexts <- rownames(fit$context_weights %||% matrix(nrow = 0L))
        if (!length(contexts)) contexts <- "league"
        exposure <- as.numeric(fit$context_exposure[contexts])
        if (length(exposure) != length(contexts) || anyNA(exposure) ||
            any(exposure <= 0)) {
          exposure <- rep(1, length(contexts))
        }
        for (context_index in seq_along(contexts)) {
          context_value <- contexts[[context_index]]
          context_argument <- if (context_value == "league") {
            NULL
          } else {
            context_value
          }
          q <- challenge_margin_subjective_ball_probability_1d(
            fit,
            private_margin_signal = signal,
            perception_sigma = sensory,
            context = context_argument
          )
          if (any(diff(q) < -1e-10)) {
            stop("A contextual subjective-belief curve is not monotone")
          }
          index <- index + 1L
          pieces[[index]] <- data.table::data.table(
            scenario_id = scenario$scenario_id[[1L]],
            fold = fold_id,
            role = role_value,
            offense_kappa = scenario$offense_kappa[[1L]],
            defense_kappa = scenario$defense_kappa[[1L]],
            kappa = kappa,
            effective_width_inches = width,
            sensory_sigma_inches = sensory,
            prior_context = context_value,
            count_state = sub("__.*$", "", context_value),
            context_exposure = exposure[[context_index]],
            perceived_margin_inches = signal,
            subjective_wrong_call_probability = q,
            prior_wrong_call_probability = challenge_margin_prior_ball_rate_1d(
              fit, context = context_argument
            )
          )
        }
      }
    }
  }
  raw <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  all_counts <- data.table::copy(raw)
  all_counts[, count_state := "all_counts"]
  raw <- data.table::rbindlist(list(raw, all_counts), use.names = TRUE)
  envelope <- raw[, .(
    subjective_q_mean = stats::weighted.mean(
      subjective_wrong_call_probability, context_exposure
    ),
    subjective_q_min = min(subjective_wrong_call_probability),
    subjective_q_max = max(subjective_wrong_call_probability),
    prior_wrong_call_probability_mean = stats::weighted.mean(
      prior_wrong_call_probability, context_exposure
    ),
    sensory_sigma_min = min(sensory_sigma_inches),
    sensory_sigma_max = max(sensory_sigma_inches),
    accepted_scenarios = data.table::uniqueN(scenario_id),
    outer_folds = data.table::uniqueN(fold),
    contextual_curves = .N
  ), by = .(role, count_state, perceived_margin_inches)]
  monotone <- envelope[order(perceived_margin_inches), .(
    mean_monotone = all(diff(subjective_q_mean) >= -1e-10),
    lower_monotone = all(diff(subjective_q_min) >= -1e-10),
    upper_monotone = all(diff(subjective_q_max) >= -1e-10)
  ), by = .(role, count_state)]
  if (any(!monotone$mean_monotone) || any(!monotone$lower_monotone) ||
      any(!monotone$upper_monotone) ||
      any(envelope$subjective_q_min < 0 | envelope$subjective_q_max > 1) ||
      any(envelope$subjective_q_min > envelope$subjective_q_max)) {
    stop("The accepted-set subjective-belief envelope is invalid")
  }
  data.table::setorder(envelope, role, count_state, perceived_margin_inches)
  envelope[]
}

build_revealed_policy_value_comparison_1d <- function(
  policy_clock_predictions,
  normative_probabilities,
  bootstrap_reps = 2000L,
  seed = 20260826L
) {
  scored <- data.table::copy(
    data.table::as.data.table(policy_clock_predictions)
  )
  assert_revealed_policy_action_outcome_free_1d(
    scored, "policy-value frozen selection clock"
  )
  stop_if_missing_columns(
    scored,
    c(
      "game_pk", "pitch_order", "team_id", "role", "inning", "stake_G",
      "challenged", "role_margin_inches", "probability_k1", "probability_k2"
    ),
    "policy clock predictions"
  )
  clock <- scored[, .(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    inning = as.integer(inning),
    role = as.character(role),
    stake_G = as.numeric(stake_G),
    challenged = as.integer(challenged),
    fitted_probability_k1 = as.numeric(probability_k1),
    fitted_probability_k2 = as.numeric(probability_k2)
  )]
  truth <- scored[, .(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    role = as.character(role),
    role_margin_inches = as.numeric(role_margin_inches)
  )]
  normative <- data.table::copy(
    data.table::as.data.table(normative_probabilities)
  )
  assert_revealed_policy_action_outcome_free_1d(
    normative, "frozen normative policy probabilities"
  )
  stop_if_missing_columns(
    normative,
    c(
      "game_pk", "team_id", "pitch_order", "policy",
      "probability_k1", "probability_k2"
    ),
    "frozen normative policy probabilities"
  )
  keys <- revealed_policy_value_key_columns_1d()
  base <- clock[, ..keys]
  base_key <- do.call(paste, c(base, sep = "\r"))
  truth_normalized <- normalize_revealed_policy_truth_1d(truth)
  truth_key <- do.call(paste, c(truth_normalized[, ..keys], sep = "\r"))
  truth_normalized <- truth_normalized[match(base_key, truth_key)]
  if (anyNA(truth_normalized)) {
    stop("Geometry truth does not align with the policy clock")
  }
  make_probability <- function(
    policy_name, policy_family, probability_k1, probability_k2,
    truth_used = FALSE, geometry_integrated = FALSE
  ) {
    if (!length(probability_k1) %in% c(1L, nrow(base)) ||
        !length(probability_k2) %in% c(1L, nrow(base))) {
      stop("A policy probability vector does not cover the complete clock")
    }
    probability_k1 <- rep_len(probability_k1, nrow(base))
    probability_k2 <- rep_len(probability_k2, nrow(base))
    out <- data.table::copy(base)
    out[, `:=`(
      policy = policy_name,
      policy_family = policy_family,
      probability_k1 = as.numeric(probability_k1),
      probability_k2 = as.numeric(probability_k2),
      truth_used_by_decision_rule = truth_used,
      geometry_used_for_signal_integration = geometry_integrated,
      truth_used_by_action_rule = truth_used
    )]
    out
  }
  run_one <- function(probability_rows) {
    replay_revealed_policy_value_1d(
      clock,
      probability_rows,
      truth,
      initial_inventory = 2L
    )
  }

  observed_probability <- make_probability(
    "observed", "observed", clock$challenged, clock$challenged
  )
  observed_replay <- run_one(observed_probability)
  game_parts <- list(
    .revealed_policy_game_role_summary_1d(observed_replay)
  )
  fitted_replay <- run_one(make_probability(
    "fitted_human_selection", "fitted_human_selection",
    clock$fitted_probability_k1, clock$fitted_probability_k2,
    geometry_integrated = TRUE
  ))
  game_parts[[length(game_parts) + 1L]] <-
    .revealed_policy_game_role_summary_1d(fitted_replay)
  rm(fitted_replay)
  no_replay <- run_one(make_probability(
    "no_challenges", "no_challenges", 0, 0
  ))
  game_parts[[length(game_parts) + 1L]] <-
    .revealed_policy_game_role_summary_1d(no_replay)
  rm(no_replay)
  oracle_probability <- as.numeric(
    truth_normalized$geometry_success & clock$stake_G > 0
  )
  oracle_replay <- run_one(make_probability(
    "exact_location_oracle", "exact_location_oracle",
    oracle_probability, oracle_probability, truth_used = TRUE
  ))
  game_parts[[length(game_parts) + 1L]] <-
    .revealed_policy_game_role_summary_1d(oracle_replay)
  rm(oracle_replay)

  normative_names <- sort(unique(as.character(normative$policy)))
  for (policy_name in normative_names) {
    value <- normative[policy == policy_name, c(
      keys, "probability_k1", "probability_k2"
    ), with = FALSE]
    if (anyDuplicated(value[, ..keys])) {
      stop("A normative policy has duplicate opportunity keys")
    }
    value_key <- do.call(paste, c(value[, ..keys], sep = "\r"))
    index <- match(base_key, value_key)
    if (anyNA(index) || nrow(value) != nrow(base)) {
      stop("A normative policy does not cover the complete clock")
    }
    value <- value[index]
    replay <- run_one(make_probability(
      policy_name, "normative",
      value$probability_k1, value$probability_k2,
      geometry_integrated = TRUE
    ))
    game_parts[[length(game_parts) + 1L]] <-
      .revealed_policy_game_role_summary_1d(replay)
    rm(replay)
  }
  game_role <- data.table::rbindlist(game_parts, use.names = TRUE, fill = TRUE)
  summary <- summarize_revealed_policy_game_role_1d(
    game_role,
    bootstrap_reps = bootstrap_reps,
    seed = seed,
    observed_policy = "observed"
  )
  list(
    replay = observed_replay[],
    game_team_role = game_role[],
    summary = summary,
    memory_regime = paste(
      "policies replayed sequentially; only observed pitch-level replay and",
      "game-team-role summaries retained"
    )
  )
}

revealed_policy_snapshot_id_1d <- function(target_metadata, games) {
  metadata <- data.table::copy(data.table::as.data.table(target_metadata))
  stop_if_missing_columns(metadata, c("name", "data"), "target metadata")
  if (anyNA(metadata[, .(name, data)]) || anyDuplicated(metadata$name)) {
    stop("Target hashes are incomplete or duplicated")
  }
  digest::digest(
    list(
      schema = "revealed_selection_perception_policy_snapshot_v1",
      target_hashes = metadata[order(name), .(name, data)],
      games = sort(unique(as.character(games)))
    ),
    algo = "sha256",
    serialize = TRUE
  )
}

attach_revealed_policy_snapshot_1d <- function(
  rows, snapshot_id, pitch_ledger_target_hash
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  x[, `:=`(
    snapshot_id = as.character(snapshot_id),
    pitch_ledger_target_hash = as.character(pitch_ledger_target_hash)
  )]
  x[]
}
