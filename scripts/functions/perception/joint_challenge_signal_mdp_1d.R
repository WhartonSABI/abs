# Joint offense/defense one-dimensional challenge policy.
#
# A team owns one challenge inventory.  Batter called-strike opportunities and
# catcher/pitcher called-ball opportunities therefore enter one chronological
# stream.  Each role has its own signed-margin prior and private-signal width,
# but both roles face the same continuation-value loss from spending the team's
# next challenge.

joint_challenge_signal_mdp_roles_1d <- function() c("offense", "defense")

joint_challenge_signal_mdp_required_columns_1d <- function() {
  c(
    "game_pk", "team_id", "pitch_order", "inning", "stage", "role",
    "state_key", "raw_out_index", "count_state", "stake_G",
    "role_margin_inches", "actual_wrong", "decision_mode",
    "observed_team_challenge", "observed_success",
    "exogenous_action_probability",
    "exogenous_success_and_action_probability"
  )
}

.joint_challenge_signal_named_numeric_1d <- function(
  value, roles, label, allow_zero = FALSE
) {
  value <- unlist(value, use.names = TRUE)
  if (is.null(names(value)) || anyDuplicated(names(value)) ||
      !all(roles %in% names(value))) {
    stop(label, " must be named for every structural role", call. = FALSE)
  }
  value <- as.numeric(value[roles])
  names(value) <- roles
  invalid <- if (isTRUE(allow_zero)) value < 0 else value <= 0
  if (anyNA(value) || any(!is.finite(value)) || any(invalid)) {
    qualifier <- if (isTRUE(allow_zero)) "non-negative" else "positive"
    stop(label, " must contain finite ", qualifier, " role-specific values",
      call. = FALSE
    )
  }
  value
}

.joint_challenge_signal_prior_list_1d <- function(prior_fits, roles) {
  if (!is.list(prior_fits) || is.null(names(prior_fits)) ||
      anyDuplicated(names(prior_fits)) || !all(roles %in% names(prior_fits))) {
    stop("prior_fits must be a named list for every structural role",
      call. = FALSE
    )
  }
  out <- prior_fits[roles]
  invisible(lapply(out, .challenge_margin_validate_fit))
  out
}

prepare_joint_challenge_signal_mdp_opportunities_1d <- function(
  pitch_ledger, re_model, include_passive_rows = TRUE
) {
  x <- data.table::copy(data.table::as.data.table(pitch_ledger))
  required <- c(
    "game_pk", "pitch_order", "inning", "half", "outs_before",
    "balls_before", "strikes_before", "initial_call", "adverse_team_id",
    "adverse_role", "bat_team_id", "tracking_available",
    "edge_distance_inches", "challenge_occurred", "challenger_role"
  )
  stop_if_missing_columns(x, required, "joint challenge-signal pitch ledger")
  optional_int <- c(
    "on_1b", "on_2b", "on_3b", "concurrent_remove_1b",
    "concurrent_remove_2b", "concurrent_remove_3b", "concurrent_add_1b",
    "concurrent_add_2b", "concurrent_add_3b", "concurrent_runs",
    "concurrent_outs"
  )
  for (column in optional_int) {
    if (!column %in% names(x)) {
      x[, (column) := if (startsWith(column, "on_")) NA_integer_ else 0L]
    }
  }
  if (!"challenger_team_id" %in% names(x)) {
    x[, challenger_team_id := NA_integer_]
  }
  if (!"challenge_outcome" %in% names(x)) {
    x[, challenge_outcome := NA_character_]
  }
  if (!"abs_eligible" %in% names(x)) x[, abs_eligible := TRUE]

  roles <- joint_challenge_signal_mdp_roles_1d()
  base <- x$initial_call %in% c("called_strike", "ball") &
    !is.na(x$adverse_team_id) & x$adverse_role %in% roles &
    x$balls_before %in% 0:3 & x$strikes_before %in% 0:2 &
    x$outs_before %in% 0:2 & !is.na(x$inning) &
    tolower(as.character(x$half)) %in% c("top", "bottom")
  rows <- continuous_context_fields(x[base])
  if (!nrow(rows)) stop("No joint adverse-call opportunities are available")

  incompatible_call <-
    (rows$adverse_role == "offense" & rows$initial_call != "called_strike") |
    (rows$adverse_role == "defense" & rows$initial_call != "ball")
  if (any(incompatible_call)) {
    stop("Adverse role and initial call disagree in the joint stream",
      call. = FALSE
    )
  }
  challenged_by_other_team <- rows$challenge_occurred %in% TRUE &
    !is.na(rows$challenger_team_id) &
    rows$challenger_team_id != rows$adverse_team_id
  if (any(challenged_by_other_team)) {
    stop("A challenge is attributed to a team other than the adverse team",
      call. = FALSE
    )
  }

  re_table <- if (is.list(re_model) && !is.null(re_model$table)) {
    re_model$table
  } else {
    re_model
  }
  stop_if_missing_columns(
    re_table, c("outs", "base_state", "balls", "strikes", "re"),
    "run-expectancy model"
  )
  stands <- vectorized_call_branch(rows, rows$initial_call)
  flipped <- vectorized_call_branch(rows, opposite_call(rows$initial_call))
  batting_gain <- vectorized_branch_re(re_table, flipped) -
    vectorized_branch_re(re_table, stands)
  stake <- ifelse(
    rows$adverse_team_id == rows$bat_team_id,
    batting_gain,
    -batting_gain
  )

  tracked <- rows$tracking_available %in% TRUE &
    is.finite(rows$edge_distance_inches)
  structural <- tracked & rows$abs_eligible %in% TRUE & is.finite(stake)
  observed_team_challenge <- rows$challenge_occurred %in% TRUE
  mode <- ifelse(
    structural,
    "structural",
    ifelse(observed_team_challenge, "exogenous", "passive")
  )
  role_margin <- ifelse(
    rows$adverse_role == "offense",
    rows$edge_distance_inches,
    -rows$edge_distance_inches
  )
  geometry_wrong <- rep(NA, nrow(rows))
  geometry_wrong[tracked & rows$adverse_role == "offense"] <-
    rows$edge_distance_inches[tracked & rows$adverse_role == "offense"] > 0
  # The ABS boundary belongs to the strike region.  Thus a called ball exactly
  # on the boundary is wrong even though its role-oriented margin is zero.
  geometry_wrong[tracked & rows$adverse_role == "defense"] <-
    rows$edge_distance_inches[tracked & rows$adverse_role == "defense"] <= 0
  official_resolved <- observed_team_challenge &
    rows$challenge_outcome %in% c("overturned", "upheld")
  official_success <- rep(NA, nrow(rows))
  official_success[official_resolved] <-
    rows$challenge_outcome[official_resolved] == "overturned"
  actual_wrong <- geometry_wrong
  actual_wrong[is.na(actual_wrong) & official_resolved] <-
    official_success[is.na(actual_wrong) & official_resolved]
  observed_success <- observed_team_challenge & actual_wrong %in% TRUE

  # Unsupported observed challenges remain on the chronological trace as an
  # explicit factual/exogenous transition.  The primary joint fit refuses to
  # consume these outcome-derived probabilities unless the caller opts in.
  exogenous_action <- ifelse(mode == "exogenous", 1, 0)
  exogenous_success <- ifelse(
    mode == "exogenous" & !is.na(actual_wrong),
    as.numeric(actual_wrong),
    ifelse(mode == "exogenous", NA_real_, 0)
  )
  if (any(mode == "exogenous" & !is.finite(exogenous_success))) {
    stop(
      "Unsupported observed challenges need a resolved geometry or official outcome",
      call. = FALSE
    )
  }

  rows[, `:=`(
    team_id = as.integer(adverse_team_id),
    role = as.character(adverse_role),
    decision_agent = ifelse(
      adverse_role == "offense", "batter", "catcher_or_pitcher"
    ),
    raw_out_index = mdp_out_index(inning, half, outs_before),
    role_margin_inches = as.numeric(role_margin),
    stake_G = as.numeric(ifelse(is.finite(stake), stake, 0)),
    actual_wrong = as.logical(actual_wrong),
    geometry_wrong = as.logical(geometry_wrong),
    official_success = as.logical(official_success),
    truth_source = ifelse(
      !is.na(geometry_wrong), "geometry",
      ifelse(!is.na(official_success), "official_fallback", "unavailable")
    ),
    official_geometry_mismatch = observed_team_challenge &
      !is.na(geometry_wrong) & !is.na(official_success) &
      geometry_wrong != official_success,
    decision_mode = mode,
    observed_team_challenge = observed_team_challenge,
    observed_success = observed_success,
    exogenous_action_probability = as.numeric(exogenous_action),
    exogenous_success_and_action_probability =
      as.numeric(exogenous_success),
    exogenous_probability_source = ifelse(
      mode == "exogenous", "factual_observed_outcome", "none"
    )
  )]
  rows[, stage := collapse_mdp_stage(raw_out_index)]
  rows[, state_key := mdp_state_key(stage, role)]
  if (!isTRUE(include_passive_rows)) rows <- rows[decision_mode != "passive"]

  keep <- unique(c(
    joint_challenge_signal_mdp_required_columns_1d(),
    "initial_call", "edge_distance_inches", "tracking_available",
    "balls_before", "strikes_before", "decision_agent",
    "challenger_role", "challenge_outcome", "geometry_wrong",
    "official_success", "truth_source", "official_geometry_mismatch",
    "exogenous_probability_source", "pitch_type", "pitch_family",
    "matchup", "release_speed", "batter_id", "pitcher_id", "fielder_2",
    "bat_team_id", "fld_team_id", "umpire_id", "stand", "p_throws",
    "adverse_challenges_before", "bat_team_challenges_before",
    "fld_team_challenges_before", "bat_score", "fld_score"
  ))
  keep <- intersect(keep, names(rows))
  out <- rows[, ..keep]
  data.table::setorder(out, game_pk, team_id, pitch_order)
  if (!nrow(out) || anyDuplicated(out[, .(game_pk, pitch_order)])) {
    stop("Joint challenge opportunities have invalid or duplicate pitch keys",
      call. = FALSE
    )
  }
  out[]
}

joint_challenge_margin_prior_input_1d <- function(opportunities, role) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  role_value <- match.arg(role, joint_challenge_signal_mdp_roles_1d())
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "pitch_order", "role", "decision_mode",
      "role_margin_inches", "count_state"
    ),
    "joint margin-prior opportunities"
  )
  x <- x[
    role == role_value & decision_mode == "structural" &
      is.finite(role_margin_inches)
  ]
  if (!nrow(x)) stop("No structural ", role_value, " margins are available")
  x[, `:=`(
    initial_call = "called_strike",
    tracking_available = TRUE,
    edge_distance_inches = as.numeric(role_margin_inches)
  )]
  x[]
}

fit_joint_challenge_margin_priors_1d <- function(
  opportunities, components = c(offense = 3L, defense = 3L),
  training_games = NULL, fold_id = "full", global_draw_id = 1L, ...
) {
  roles <- joint_challenge_signal_mdp_roles_1d()
  if (length(components) == 1L) {
    components <- stats::setNames(rep(as.integer(components), length(roles)), roles)
  }
  if (is.null(names(components)) || !all(roles %in% names(components))) {
    stop("components must be scalar or named by offense and defense",
      call. = FALSE
    )
  }
  components <- as.integer(components[roles])
  names(components) <- roles
  fits <- lapply(roles, function(role) {
    input <- joint_challenge_margin_prior_input_1d(opportunities, role)
    fit <- fit_challenge_margin_prior_1d(
      input,
      components = components[[role]],
      training_games = training_games,
      fold_id = paste0(fold_id, "_", role),
      global_draw_id = global_draw_id,
      ...
    )
    fit$joint_role <- role
    fit$conditioning <- c(
      "taken", if (role == "offense") "called_strike" else "called_ball"
    )
    fit$success_event <- if (role == "offense") {
      "true signed ABS distance > 0 (called strike should be ball)"
    } else {
      "true signed ABS distance <= 0 (called ball should be strike)"
    }
    fit
  })
  names(fits) <- roles
  fits
}

.joint_challenge_signal_fit_rows_1d <- function(opportunities) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  required <- setdiff(
    joint_challenge_signal_mdp_required_columns_1d(),
    c("role_margin_inches", "actual_wrong", "observed_team_challenge",
      "observed_success")
  )
  stop_if_missing_columns(x, required, "joint challenge-signal opportunities")
  x <- x[, ..required]
  x[, `:=`(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    inning = as.integer(inning),
    stage = as.integer(stage),
    role = as.character(role),
    state_key = as.character(state_key),
    raw_out_index = as.integer(raw_out_index),
    count_state = as.character(count_state),
    stake_G = as.numeric(stake_G),
    decision_mode = as.character(decision_mode),
    exogenous_action_probability = as.numeric(exogenous_action_probability),
    exogenous_success_and_action_probability =
      as.numeric(exogenous_success_and_action_probability),
    p_hat = 0.5
  )]
  if (!nrow(x) || anyNA(x[, .(
    game_pk, team_id, pitch_order, inning, stage, role, state_key,
    raw_out_index, count_state, stake_G, decision_mode
  )]) || any(!is.finite(x$stake_G)) ||
      any(!x$role %in% joint_challenge_signal_mdp_roles_1d()) ||
      any(!x$decision_mode %in% c("structural", "passive", "exogenous")) ||
      anyDuplicated(x[, .(game_pk, pitch_order)])) {
    stop("Joint challenge-signal opportunities are invalid", call. = FALSE)
  }
  exogenous <- x$decision_mode == "exogenous"
  action <- x$exogenous_action_probability
  success <- x$exogenous_success_and_action_probability
  if (any(exogenous & (
    !is.finite(action) | action < 0 | action > 1 |
      !is.finite(success) | success < 0 | success > action
  ))) {
    stop("Exogenous rows need valid action and joint-success probabilities",
      call. = FALSE
    )
  }
  data.table::setorder(x, game_pk, team_id, pitch_order)
  add_mdp_transitions(x)
}

.joint_challenge_signal_opportunity_value_1d <- function(
  rows, inventory, wait, loss, lookups
) {
  if (!nrow(rows) || inventory == 0L) return(wait)
  row_weight <- if ("row_weight" %in% names(rows)) {
    as.numeric(rows$row_weight)
  } else {
    rep(1, nrow(rows))
  }
  if (anyNA(row_weight) || any(!is.finite(row_weight)) ||
      any(row_weight <= 0)) {
    stop("Joint opportunity-profile weights are invalid")
  }
  increment <- numeric(nrow(rows))
  structural <- which(rows$decision_mode == "structural")
  if (length(structural)) {
    for (role in unique(rows$role[structural])) {
      index <- structural[rows$role[structural] == role]
      terms <- challenge_signal_payoff_terms_1d(
        lookups[[role]],
        gain = rows$stake_G[index],
        inventory_loss = loss,
        context = rows$count_state[index]
      )
      increment[index] <- terms$prior_expected_advantage_re
    }
  }
  exogenous <- which(rows$decision_mode == "exogenous")
  if (length(exogenous)) {
    action <- rows$exogenous_action_probability[exogenous]
    success <- rows$exogenous_success_and_action_probability[exogenous]
    failure <- action - success
    increment[exogenous] <- success * rows$stake_G[exogenous] - failure * loss
  }
  wait + stats::weighted.mean(increment, row_weight)
}

.joint_challenge_signal_mdp_operator_1d <- function(fit_data, arrival_values) {
  states <- fit_data$states
  continuation <- matrix(
    0, nrow = nrow(states), ncol = 3L,
    dimnames = list(states$state_key, as.character(0:2))
  )
  updated <- arrival_values
  state_order <- order(states$stage, decreasing = TRUE)
  for (i in state_order) {
    exact <- fit_data$exact_rows[[i]]
    parent <- fit_data$parent_rows[[i]]
    weight <- states$shrink_weight[[i]]
    for (inventory in 0:2) {
      exact_continuation <- mdp_transition_mean(
        exact, states$stage[[i]], inventory, updated,
        fit_data$state_index, relative = FALSE
      )
      parent_continuation <- mdp_transition_mean(
        parent, states$stage[[i]], inventory, updated,
        fit_data$state_index, relative = TRUE
      )
      continuation[i, inventory + 1L] <-
        weight * exact_continuation + (1 - weight) * parent_continuation
    }
    for (inventory in 0:2) {
      wait <- continuation[i, inventory + 1L]
      if (inventory == 0L) {
        updated[i, inventory + 1L] <- wait
        next
      }
      lower <- continuation[i, inventory]
      loss <- wait - lower
      if (loss < -1e-8) {
        stop("Joint Bellman step produced negative shared-inventory value")
      }
      loss <- pmax(0, loss)
      exact_value <- .joint_challenge_signal_opportunity_value_1d(
        fit_data$exact_policy_rows[[i]], inventory, wait, loss,
        fit_data$lookups
      )
      parent_value <- .joint_challenge_signal_opportunity_value_1d(
        fit_data$parent_policy_rows[[i]], inventory, wait, loss,
        fit_data$lookups
      )
      updated[i, inventory + 1L] <-
        weight * exact_value + (1 - weight) * parent_value
    }
  }
  list(arrival = updated, continuation = continuation)
}

fit_joint_challenge_signal_mdp_1d <- function(
  opportunities, prior_fits, perception_sigma, prior_n = 30,
  tol = 1e-9, max_iter = 10000L, lookup_grid_step = 0.0025,
  allow_factual_exogenous = FALSE
) {
  raw <- data.table::as.data.table(opportunities)
  x <- .joint_challenge_signal_fit_rows_1d(raw)
  roles <- sort(unique(x[decision_mode == "structural", role]))
  expected_roles <- joint_challenge_signal_mdp_roles_1d()
  if (!setequal(roles, expected_roles)) {
    stop("A joint fit requires structural offense and defense opportunities",
      call. = FALSE
    )
  }
  priors <- .joint_challenge_signal_prior_list_1d(prior_fits, roles)
  sigma <- .joint_challenge_signal_named_numeric_1d(
    perception_sigma, roles, "perception_sigma", allow_zero = TRUE
  )
  if (!isTRUE(allow_factual_exogenous) &&
      "exogenous_probability_source" %in% names(raw) &&
      any(raw$decision_mode == "exogenous" &
        raw$exogenous_probability_source == "factual_observed_outcome")) {
    stop(
      "Factual exogenous outcomes are disabled in the primary Bellman fit",
      call. = FALSE
    )
  }
  training_games <- sort(unique(x$game_pk))
  for (role in roles) {
    role_value <- role
    role_games <- sort(unique(x[
      role == role_value & decision_mode == "structural", game_pk
    ]))
    prior_games <- sort(as.character(priors[[role]]$training_games %||% character()))
    if (length(prior_games) && !identical(role_games, prior_games)) {
      stop("The ", role, " prior and joint Bellman fit use different games",
        call. = FALSE
      )
    }
  }
  contexts <- lapply(roles, function(role) {
    role_value <- role
    unique(x[
      role == role_value & decision_mode == "structural", count_state
    ])
  })
  names(contexts) <- roles
  lookups <- lapply(roles, function(role) {
    build_challenge_signal_lookup_1d(
      priors[[role]], sigma[[role]], contexts[[role]],
      grid_step = lookup_grid_step
    )
  })
  names(lookups) <- roles

  prior_n <- as.numeric(prior_n)
  tol <- as.numeric(tol)
  max_iter <- as.integer(max_iter)
  if (!is.finite(prior_n) || prior_n < 0 || !is.finite(tol) || tol <= 0 ||
      is.na(max_iter) || max_iter < 1L) {
    stop("Invalid joint challenge-signal Bellman controls", call. = FALSE)
  }
  states <- unique(x[, .(stage = as.integer(stage), role = as.character(role))])
  states[, `:=`(
    state_key = mdp_state_key(stage, role),
    parent_key = mdp_parent_key(stage, role)
  )]
  data.table::setorder(states, stage, role)
  state_index <- stats::setNames(seq_len(nrow(states)), states$state_key)
  attr(state_index, "states") <- states
  exact_rows <- lapply(states$state_key, function(key) x[state_key == key])
  parent_rows <- lapply(states$parent_key, function(key) x[parent_key == key])
  compress_policy_rows <- function(rows) {
    rows[, .(row_weight = .N), by = .(
      role, decision_mode, stake_G, count_state,
      exogenous_action_probability,
      exogenous_success_and_action_probability
    )]
  }
  exact_policy_rows <- lapply(exact_rows, compress_policy_rows)
  parent_policy_rows <- lapply(parent_rows, compress_policy_rows)
  states[, `:=`(
    n = vapply(exact_rows, nrow, integer(1)),
    parent_n = vapply(parent_rows, nrow, integer(1))
  )]
  if (any(states$parent_n == 0L)) {
    stop("Every joint Bellman state needs empirical parent rows")
  }
  states[, shrink_weight := if (prior_n == 0) {
    as.numeric(n > 0L)
  } else {
    n / (n + prior_n)
  }]
  fit_data <- list(
    states = states,
    exact_rows = exact_rows,
    parent_rows = parent_rows,
    exact_policy_rows = exact_policy_rows,
    parent_policy_rows = parent_policy_rows,
    state_index = state_index,
    lookups = lookups
  )
  arrival <- matrix(
    0, nrow = nrow(states), ncol = 3L,
    dimnames = list(states$state_key, as.character(0:2))
  )
  residual <- Inf
  iteration <- 0L
  while (iteration < max_iter && residual > tol) {
    iteration <- iteration + 1L
    step <- .joint_challenge_signal_mdp_operator_1d(fit_data, arrival)
    residual <- max(abs(step$arrival - arrival))
    arrival <- step$arrival
  }
  if (!is.finite(residual) || residual > tol) {
    stop(sprintf(
      "Joint challenge-signal MDP did not converge after %s iterations (%.3g)",
      iteration, residual
    ))
  }
  final <- .joint_challenge_signal_mdp_operator_1d(fit_data, arrival)
  bellman_residual <- max(abs(final$arrival - arrival))
  if (!is.finite(bellman_residual) || bellman_residual > tol) {
    stop(sprintf(
      "Final joint Bellman residual %.3g exceeds tolerance %.3g",
      bellman_residual, tol
    ))
  }
  continuation <- final$continuation
  marginal_1 <- continuation[, "1"] - continuation[, "0"]
  marginal_2 <- continuation[, "2"] - continuation[, "1"]
  marginal_1[abs(marginal_1) < 1e-10] <- 0
  marginal_2[abs(marginal_2) < 1e-10] <- 0
  if (any(marginal_1 < -1e-7) || any(marginal_2 < -1e-7)) {
    stop("Joint challenge-signal MDP produced negative inventory values")
  }
  marginal_1 <- pmax(0, marginal_1)
  marginal_2 <- pmax(0, marginal_2)
  values <- data.table::copy(states)
  values[, `:=`(
    arrival_re_k0 = arrival[, "0"],
    arrival_re_k1 = arrival[, "1"],
    arrival_re_k2 = arrival[, "2"],
    continuation_re_k0 = continuation[, "0"],
    continuation_re_k1 = continuation[, "1"],
    continuation_re_k2 = continuation[, "2"],
    marginal_re_1_to_0 = marginal_1,
    marginal_re_2_to_1 = marginal_2
  )]
  fit <- list(
    state_values = values[],
    arrival = arrival,
    continuation = continuation,
    states = states,
    state_index = state_index,
    lookups = lookups,
    prior_fits = priors,
    perception_sigma = sigma,
    residual = bellman_residual,
    iterations = iteration,
    prior_n = prior_n,
    training_games = training_games,
    roles = roles,
    inventory_scope = "one shared team inventory across offense and defense",
    information_regime = paste(
      "role-specific private margin signals integrated before action;",
      "one shared team inventory and empirical game stage persist"
    )
  )
  class(fit) <- "joint_challenge_signal_mdp_1d"
  fit
}

joint_challenge_signal_mdp_action_1d <- function(
  fit, stage, role, inventory, gain, context, true_margin = NULL
) {
  if (!inherits(fit, "joint_challenge_signal_mdp_1d")) {
    stop("fit must be a joint_challenge_signal_mdp_1d", call. = FALSE)
  }
  lengths <- c(
    length(stage), length(role), length(inventory), length(gain),
    length(context), if (is.null(true_margin)) 1L else length(true_margin)
  )
  size <- max(lengths)
  if (any(lengths != 1L & lengths != size)) {
    stop("Joint action inputs must be scalar or row-aligned")
  }
  stage <- rep_len(collapse_mdp_stage(stage), size)
  role <- rep_len(as.character(role), size)
  inventory <- rep_len(as.integer(inventory), size)
  gain <- rep_len(as.numeric(gain), size)
  context <- rep_len(as.character(context), size)
  if (anyNA(stage) || anyNA(role) || any(!role %in% fit$roles) ||
      anyNA(inventory) || any(!inventory %in% 0:2) || anyNA(gain) ||
      any(!is.finite(gain)) || anyNA(context)) {
    stop("Invalid joint action inputs", call. = FALSE)
  }
  index <- resolve_mdp_state_index(stage, role, fit$states, fit$state_index)
  wait <- fit$continuation[cbind(index, inventory + 1L)]
  lower <- fit$continuation[
    cbind(index, pmax(inventory - 1L, 0L) + 1L)
  ]
  loss <- pmax(0, wait - lower)
  pieces <- lapply(fit$roles, function(level) {
    rows <- which(role == level)
    if (!length(rows)) return(NULL)
    terms <- challenge_signal_payoff_terms_1d(
      fit$lookups[[level]], gain[rows], loss[rows], context[rows]
    )
    data.table::data.table(
      row_id = rows,
      q_threshold = terms$q_target,
      signal_threshold_inches = terms$threshold_inches,
      prior_challenge_probability = terms$prior_challenge_probability,
      prior_success_and_challenge_probability =
        terms$prior_success_and_challenge_probability,
      prior_failure_and_challenge_probability =
        terms$prior_failure_and_challenge_probability,
      prior_q_chosen = terms$prior_q_chosen,
      prior_expected_advantage_re = terms$prior_expected_advantage_re,
      context_fallback = terms$context_fallback,
      exact_root_fallback = terms$exact_root_fallback
    )
  })
  terms <- data.table::rbindlist(pieces)[order(row_id)]
  unavailable <- inventory == 0L
  terms[unavailable, `:=`(
    q_threshold = NA_real_,
    signal_threshold_inches = NA_real_,
    prior_challenge_probability = 0,
    prior_success_and_challenge_probability = 0,
    prior_failure_and_challenge_probability = 0,
    prior_q_chosen = NA_real_,
    prior_expected_advantage_re = 0,
    context_fallback = FALSE,
    exact_root_fallback = FALSE
  )]
  conditional_probability <- rep(NA_real_, size)
  if (!is.null(true_margin)) {
    margin <- rep_len(as.numeric(true_margin), size)
    if (anyNA(margin) || any(!is.finite(margin))) {
      stop("true_margin must be finite when supplied")
    }
    role_sigma <- fit$perception_sigma[role]
    conditional_probability <- ifelse(
      role_sigma == 0,
      as.numeric(margin > terms$signal_threshold_inches),
      stats::pnorm(
        (margin - terms$signal_threshold_inches) / role_sigma
      )
    )
    conditional_probability[terms$signal_threshold_inches == -Inf] <- 1
    conditional_probability[terms$signal_threshold_inches == Inf] <- 0
    conditional_probability[unavailable] <- 0
  }
  data.table::data.table(
    stage = stage,
    role = role,
    inventory = inventory,
    gain = gain,
    continuation_re = wait,
    lower_continuation_re = lower,
    marginal_inventory_re = ifelse(unavailable, NA_real_, loss),
    q_threshold = terms$q_threshold,
    signal_threshold_inches = terms$signal_threshold_inches,
    prior_challenge_probability = terms$prior_challenge_probability,
    prior_success_and_challenge_probability =
      terms$prior_success_and_challenge_probability,
    prior_failure_and_challenge_probability =
      terms$prior_failure_and_challenge_probability,
    prior_q_chosen = terms$prior_q_chosen,
    prior_expected_advantage_re = terms$prior_expected_advantage_re,
    challenge_probability_given_true_margin = conditional_probability,
    context = context,
    context_fallback = terms$context_fallback,
    exact_root_fallback = terms$exact_root_fallback
  )
}

replay_joint_challenge_signal_mdp_1d <- function(
  opportunities, fit, initial_inventory = 2L,
  require_game_separation = TRUE, allow_factual_exogenous = FALSE
) {
  if (!inherits(fit, "joint_challenge_signal_mdp_1d")) {
    stop("fit must be a joint_challenge_signal_mdp_1d")
  }
  x <- data.table::copy(data.table::as.data.table(opportunities))
  stop_if_missing_columns(
    x, joint_challenge_signal_mdp_required_columns_1d(),
    "joint challenge-signal replay"
  )
  if (isTRUE(require_game_separation) &&
      length(intersect(as.character(unique(x$game_pk)), fit$training_games))) {
    stop("Joint challenge-signal replay overlaps its training games")
  }
  if (!isTRUE(allow_factual_exogenous) &&
      "exogenous_probability_source" %in% names(x) &&
      any(x$decision_mode == "exogenous" &
        x$exogenous_probability_source == "factual_observed_outcome")) {
    stop("Factual exogenous outcomes require an explicit replay opt-in")
  }
  initial_inventory <- as.integer(initial_inventory)
  if (length(initial_inventory) != 1L || is.na(initial_inventory) ||
      !initial_inventory %in% 0:2) {
    stop("initial_inventory must be 0, 1, or 2")
  }
  data.table::setorder(x, game_pk, team_id, pitch_order)
  structural <- x$decision_mode == "structural"
  if (any(structural & (
    !is.finite(x$role_margin_inches) | is.na(x$actual_wrong)
  ))) {
    stop("Structural replay rows need finite role margins and known truth")
  }
  n <- nrow(x)
  action <- success <- failure <- prior_action <- prior_success <-
    matrix(0, nrow = n, ncol = 3L, dimnames = list(NULL, as.character(0:2)))
  threshold <- marginal <- matrix(
    NA_real_, nrow = n, ncol = 3L,
    dimnames = list(NULL, as.character(0:2))
  )
  context_fallback <- exact_root_fallback <- matrix(
    FALSE, nrow = n, ncol = 3L,
    dimnames = list(NULL, as.character(0:2))
  )
  prior_advantage <- matrix(0, nrow = n, ncol = 3L)
  for (inventory in 1:2) {
    if (any(structural)) {
      decision <- joint_challenge_signal_mdp_action_1d(
        fit,
        stage = x$stage[structural],
        role = x$role[structural],
        inventory = inventory,
        gain = x$stake_G[structural],
        context = x$count_state[structural],
        true_margin = x$role_margin_inches[structural]
      )
      a <- decision$challenge_probability_given_true_margin
      action[structural, inventory + 1L] <- a
      success[structural, inventory + 1L] <-
        a * as.numeric(x$actual_wrong[structural])
      failure[structural, inventory + 1L] <-
        a * as.numeric(!x$actual_wrong[structural])
      prior_action[structural, inventory + 1L] <-
        decision$prior_challenge_probability
      prior_success[structural, inventory + 1L] <-
        decision$prior_success_and_challenge_probability
      threshold[structural, inventory + 1L] <-
        decision$signal_threshold_inches
      marginal[structural, inventory + 1L] <-
        decision$marginal_inventory_re
      prior_advantage[structural, inventory + 1L] <-
        decision$prior_expected_advantage_re
      context_fallback[structural, inventory + 1L] <-
        decision$context_fallback
      exact_root_fallback[structural, inventory + 1L] <-
        decision$exact_root_fallback
    }
    exogenous <- x$decision_mode == "exogenous"
    if (any(exogenous)) {
      a <- x$exogenous_action_probability[exogenous]
      s <- x$exogenous_success_and_action_probability[exogenous]
      action[exogenous, inventory + 1L] <- a
      success[exogenous, inventory + 1L] <- s
      failure[exogenous, inventory + 1L] <- a - s
      prior_action[exogenous, inventory + 1L] <- a
      prior_success[exogenous, inventory + 1L] <- s
    }
  }
  if (any(!is.finite(action)) || any(action < 0 | action > 1) ||
      any(success < 0 | failure < 0) ||
      any(abs(action - success - failure) > 1e-10)) {
    stop("Joint replay action masses are invalid")
  }

  inventory_before <- matrix(NA_real_, nrow = n, ncol = 3L)
  policy_challenge <- expected_success <- expected_failure <-
    expected_captured_re <- expected_subjective_advantage <-
      prior_policy_challenge <- prior_policy_success <- numeric(n)
  groups <- split(seq_len(n), interaction(x$game_pk, x$team_id, drop = TRUE))
  for (indices in groups) {
    mass <- c(`0` = 0, `1` = 0, `2` = 0)
    mass[[as.character(initial_inventory)]] <- 1
    previous_inning <- NA_integer_
    for (row in indices) {
      inning_now <- as.integer(x$inning[[row]])
      extra_entry <- (is.na(previous_inning) && inning_now > 9L) ||
        (!is.na(previous_inning) && inning_now > previous_inning &&
          inning_now > 9L)
      if (extra_entry) {
        mass[["1"]] <- mass[["1"]] + mass[["0"]]
        mass[["0"]] <- 0
      }
      inventory_before[row, ] <- mass
      row_action <- action[row, ]
      row_success <- success[row, ]
      row_failure <- failure[row, ]
      policy_challenge[[row]] <- sum(mass * row_action)
      expected_success[[row]] <- sum(mass * row_success)
      expected_failure[[row]] <- sum(mass * row_failure)
      expected_captured_re[[row]] <-
        expected_success[[row]] * x$stake_G[[row]]
      expected_subjective_advantage[[row]] <-
        sum(mass * prior_advantage[row, ])
      prior_policy_challenge[[row]] <-
        sum(mass * prior_action[row, ])
      prior_policy_success[[row]] <-
        sum(mass * prior_success[row, ])

      old <- mass
      mass <- old * (1 - row_failure)
      mass[[1L]] <- mass[[1L]] + old[[2L]] * row_failure[[2L]]
      mass[[2L]] <- mass[[2L]] + old[[3L]] * row_failure[[3L]]
      if (abs(sum(mass) - 1) > 1e-10 || any(mass < -1e-12)) {
        stop("Joint shared-inventory probability mass failed to close")
      }
      mass <- stats::setNames(
        pmax(0, mass) / sum(mass), as.character(0:2)
      )
      previous_inning <- inning_now
    }
  }
  x[, `:=`(
    inventory_probability_0 = inventory_before[, 1L],
    inventory_probability_1 = inventory_before[, 2L],
    inventory_probability_2 = inventory_before[, 3L],
    signal_threshold_k1_inches = threshold[, 2L],
    signal_threshold_k2_inches = threshold[, 3L],
    marginal_inventory_re_k1 = marginal[, 2L],
    marginal_inventory_re_k2 = marginal[, 3L],
    context_fallback_k1 = context_fallback[, 2L],
    context_fallback_k2 = context_fallback[, 3L],
    exact_root_fallback_k1 = exact_root_fallback[, 2L],
    exact_root_fallback_k2 = exact_root_fallback[, 3L],
    policy_challenge_probability = policy_challenge,
    expected_policy_success = expected_success,
    expected_policy_failure = expected_failure,
    expected_captured_re = expected_captured_re,
    expected_subjective_advantage_re = expected_subjective_advantage,
    prior_policy_challenge_probability = prior_policy_challenge,
    prior_policy_success_probability = prior_policy_success,
    prior_policy_selected_success_probability = ifelse(
      prior_policy_challenge > 0,
      prior_policy_success / prior_policy_challenge,
      NA_real_
    )
  )]
  x[]
}

bootstrap_joint_challenge_signal_mdp_1d <- function(
  replay, reps = 1000L, seed = 20260826L
) {
  x <- data.table::as.data.table(replay)
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "role", "expected_captured_re", "stake_G",
      "observed_team_challenge", "observed_success"
    ),
    "joint challenge-signal bootstrap"
  )
  reps <- as.integer(reps)
  if (length(reps) != 1L || is.na(reps) || reps < 0L) {
    stop("reps must be one non-negative integer")
  }
  empty_draws <- data.table::data.table(
    replicate = integer(), role = character(), model_re = numeric(),
    observed_re = numeric(), difference_re = numeric()
  )
  empty_interval <- data.table::data.table(
    role = character(), difference_re_lower_95 = numeric(),
    difference_re_upper_95 = numeric()
  )
  if (reps == 0L) {
    return(list(draws = empty_draws, interval = empty_interval))
  }
  game_role <- x[, .(
    model_re = sum(expected_captured_re),
    observed_re = sum(stake_G[observed_team_challenge & observed_success])
  ), by = .(game_pk, role)]
  games <- sort(unique(as.character(game_role$game_pk)))
  if (!length(games)) {
    return(list(draws = empty_draws, interval = empty_interval))
  }
  game_role[, game_pk := as.character(game_pk)]
  game_role <- merge(
    data.table::CJ(
      game_pk = games,
      role = joint_challenge_signal_mdp_roles_1d(),
      unique = TRUE
    ),
    game_role,
    by = c("game_pk", "role"),
    all.x = TRUE
  )
  game_role[, `:=`(
    model_re = data.table::fcoalesce(model_re, 0),
    observed_re = data.table::fcoalesce(observed_re, 0)
  )]
  set.seed(seed)
  draws <- data.table::rbindlist(lapply(seq_len(reps), function(index) {
    weights <- data.table::data.table(
      game_pk = sample(games, length(games), replace = TRUE)
    )[, .(weight = .N), by = game_pk]
    value <- merge(game_role, weights, by = "game_pk", all.y = TRUE)
    by_role <- value[, .(
      model_re = sum(model_re * weight),
      observed_re = sum(observed_re * weight)
    ), by = role]
    total <- by_role[, .(
      role = "total",
      model_re = sum(model_re),
      observed_re = sum(observed_re)
    )]
    out <- data.table::rbindlist(list(by_role, total), use.names = TRUE)
    out[, `:=`(
      replicate = index,
      difference_re = model_re - observed_re
    )]
    data.table::setcolorder(
      out, c("replicate", "role", "model_re", "observed_re", "difference_re")
    )
    out
  }))
  interval <- draws[, .(
    difference_re_lower_95 = stats::quantile(
      difference_re, 0.025, names = FALSE
    ),
    difference_re_upper_95 = stats::quantile(
      difference_re, 0.975, names = FALSE
    )
  ), by = role]
  list(draws = draws[], interval = interval[])
}

summarize_joint_challenge_signal_mdp_1d <- function(
  replay, bootstrap_reps = 0L, seed = 20260826L
) {
  x <- data.table::as.data.table(replay)
  required <- c(
    "game_pk", "team_id", "role", "decision_mode", "stake_G",
    "actual_wrong", "observed_team_challenge", "observed_success",
    "policy_challenge_probability", "expected_policy_success",
    "expected_policy_failure", "expected_captured_re",
    "inventory_probability_0"
  )
  stop_if_missing_columns(x, required, "joint challenge-signal summary")
  team_game_role <- x[, .(
    model_re = sum(expected_captured_re),
    model_attempts = sum(policy_challenge_probability),
    model_successes = sum(expected_policy_success),
    model_failures = sum(expected_policy_failure),
    observed_re = sum(stake_G[observed_team_challenge & observed_success]),
    observed_attempts = sum(observed_team_challenge),
    observed_successes = sum(observed_success),
    zero_inventory_exposure = sum(
      data.table::fcoalesce(inventory_probability_0, 0)
    ),
    opportunity_exposure = .N,
    structural_zero_inventory_exposure = sum(
      data.table::fcoalesce(inventory_probability_0, 0) *
        (decision_mode == "structural")
    ),
    structural_opportunity_exposure = sum(decision_mode == "structural"),
    oracle_re = sum(stake_G[
      decision_mode == "structural" & actual_wrong %in% TRUE & stake_G > 0
    ]),
    oracle_attempts = sum(
      decision_mode == "structural" & actual_wrong %in% TRUE & stake_G > 0
    )
  ), by = .(game_pk, team_id, role)]
  by_role <- team_game_role[, lapply(.SD, sum), by = role, .SDcols = c(
    "model_re", "model_attempts", "model_successes", "model_failures",
    "observed_re", "observed_attempts", "observed_successes",
    "zero_inventory_exposure", "opportunity_exposure",
    "structural_zero_inventory_exposure", "structural_opportunity_exposure",
    "oracle_re", "oracle_attempts"
  )]
  total <- by_role[, lapply(.SD, sum), .SDcols = setdiff(names(by_role), "role")]
  total[, role := "total"]
  data.table::setcolorder(total, names(by_role))
  season <- data.table::rbindlist(list(by_role, total), use.names = TRUE)
  season[, `:=`(
    additional_re_vs_observed = model_re - observed_re,
    model_success_rate = ifelse(model_attempts > 0,
      model_successes / model_attempts, NA_real_),
    observed_success_rate = ifelse(observed_attempts > 0,
      observed_successes / observed_attempts, NA_real_),
    zero_inventory_rate = ifelse(opportunity_exposure > 0,
      zero_inventory_exposure / opportunity_exposure, NA_real_),
    structural_zero_inventory_rate = ifelse(structural_opportunity_exposure > 0,
      structural_zero_inventory_exposure / structural_opportunity_exposure,
      NA_real_),
    model_share_of_oracle = ifelse(oracle_re > 0, model_re / oracle_re, NA_real_),
    observed_share_of_oracle = ifelse(oracle_re > 0,
      observed_re / oracle_re, NA_real_)
  )]
  comparators <- data.table::rbindlist(lapply(
    c("model", "observed", "no_challenge", "exact_location_oracle"),
    function(policy) {
      data.table::data.table(
        role = season$role,
        policy = policy,
        captured_re = switch(policy,
          model = season$model_re,
          observed = season$observed_re,
          no_challenge = rep(0, nrow(season)),
          exact_location_oracle = season$oracle_re
        ),
        attempts = switch(policy,
          model = season$model_attempts,
          observed = season$observed_attempts,
          no_challenge = rep(0, nrow(season)),
          exact_location_oracle = season$oracle_attempts
        )
      )
    }
  ))
  bootstrap <- bootstrap_joint_challenge_signal_mdp_1d(
    x, reps = bootstrap_reps, seed = seed
  )
  if (nrow(bootstrap$interval)) {
    season <- merge(season, bootstrap$interval, by = "role", all.x = TRUE)
  } else {
    season[, `:=`(
      difference_re_lower_95 = NA_real_,
      difference_re_upper_95 = NA_real_
    )]
  }
  truth_diagnostics <- if (all(c(
    "geometry_wrong", "official_success", "official_geometry_mismatch"
  ) %in% names(x))) {
    x[observed_team_challenge == TRUE, .(
      observed_challenges = .N,
      geometry_available = sum(!is.na(geometry_wrong)),
      official_available = sum(!is.na(official_success)),
      both_available = sum(!is.na(geometry_wrong) & !is.na(official_success)),
      official_geometry_mismatches = sum(
        official_geometry_mismatch %in% TRUE
      ),
      official_geometry_mismatch_rate = {
        denominator <- sum(!is.na(geometry_wrong) & !is.na(official_success))
        if (denominator) {
          sum(official_geometry_mismatch %in% TRUE) / denominator
        } else {
          NA_real_
        }
      }
    ), by = role]
  } else {
    data.table::data.table()
  }
  list(
    season = season[],
    team_game_role = team_game_role[],
    comparators = comparators[],
    bootstrap = bootstrap$draws[],
    bootstrap_interval = bootstrap$interval[],
    truth_diagnostics = truth_diagnostics[]
  )
}

crossfit_joint_challenge_signal_mdp_1d <- function(
  opportunities, perception_sigma, fold_assignment = NULL, folds = 5L,
  seed = 20260826L, prior_components = 3L, fold_prior_fits = NULL,
  fold_perception_sigma = NULL, prior_n = 30, tol = 1e-7,
  max_iter = 10000L, lookup_grid_step = 0.005, bootstrap_reps = 0L,
  progress = interactive()
) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  stop_if_missing_columns(
    x, joint_challenge_signal_mdp_required_columns_1d(),
    "joint challenge-signal cross-fit"
  )
  games <- sort(unique(as.character(x$game_pk)))
  folds <- as.integer(folds)
  if (is.null(fold_assignment)) {
    fold_assignment <- continuous_game_folds(games, folds = folds, seed = seed)
  } else {
    fold_assignment <- data.table::copy(data.table::as.data.table(fold_assignment))
    stop_if_missing_columns(
      fold_assignment, c("game_pk", "fold"), "joint Bellman folds"
    )
    fold_assignment[, game_pk := as.character(game_pk)]
  }
  if (anyDuplicated(fold_assignment$game_pk) ||
      !setequal(fold_assignment$game_pk, games) ||
      !setequal(unique(fold_assignment$fold), seq_len(folds))) {
    stop("Joint Bellman folds must assign every game exactly once")
  }
  if (!is.null(fold_prior_fits) && length(fold_prior_fits) != folds) {
    stop("fold_prior_fits must contain one named prior list per fold")
  }
  if (!is.null(fold_perception_sigma) &&
      length(fold_perception_sigma) != folds) {
    stop("fold_perception_sigma must contain one role vector per fold")
  }
  x[, game_pk := as.character(game_pk)]
  x <- merge(x, fold_assignment, by = "game_pk", all.x = TRUE, sort = FALSE)
  data.table::setorder(x, game_pk, team_id, pitch_order)
  fold_fits <- prior_fits <- replay_parts <- diagnostics <- vector("list", folds)
  for (fold_id in seq_len(folds)) {
    training <- x[fold != fold_id]
    heldout <- x[fold == fold_id]
    training_games <- sort(unique(training$game_pk))
    heldout_games <- sort(unique(heldout$game_pk))
    if (isTRUE(progress)) {
      message(sprintf(
        "joint challenge-signal fold %s/%s: %s training, %s heldout rows",
        fold_id, folds, nrow(training), nrow(heldout)
      ))
    }
    priors <- if (is.null(fold_prior_fits)) {
      fit_joint_challenge_margin_priors_1d(
        training,
        components = prior_components,
        training_games = training_games,
        fold_id = paste0("joint_bellman_fold_", fold_id),
        global_draw_id = 1L
      )
    } else {
      fold_prior_fits[[fold_id]]
    }
    sigma <- if (is.null(fold_perception_sigma)) {
      perception_sigma
    } else {
      fold_perception_sigma[[fold_id]]
    }
    bellman <- fit_joint_challenge_signal_mdp_1d(
      training,
      prior_fits = priors,
      perception_sigma = sigma,
      prior_n = prior_n,
      tol = tol,
      max_iter = max_iter,
      lookup_grid_step = lookup_grid_step
    )
    if (length(intersect(bellman$training_games, heldout_games))) {
      stop("Heldout games entered a joint Bellman fit")
    }
    replay <- replay_joint_challenge_signal_mdp_1d(
      heldout, bellman, require_game_separation = TRUE
    )
    replay[, fold := fold_id]
    fold_fits[[fold_id]] <- bellman
    prior_fits[[fold_id]] <- priors
    replay_parts[[fold_id]] <- replay
    diagnostics[[fold_id]] <- data.table::data.table(
      fold = fold_id,
      training_games = length(training_games),
      heldout_games = length(heldout_games),
      training_rows = nrow(training),
      heldout_rows = nrow(heldout),
      bellman_iterations = bellman$iterations,
      bellman_residual = bellman$residual,
      offense_sigma = bellman$perception_sigma[["offense"]],
      defense_sigma = bellman$perception_sigma[["defense"]],
      offense_prior_components = priors$offense$components,
      defense_prior_components = priors$defense$components
    )
  }
  replay <- data.table::rbindlist(replay_parts, fill = TRUE)
  summary <- summarize_joint_challenge_signal_mdp_1d(
    replay, bootstrap_reps = bootstrap_reps, seed = seed
  )
  list(
    fold_assignment = fold_assignment[order(game_pk)],
    prior_fits = prior_fits,
    fold_fits = fold_fits,
    fold_diagnostics = data.table::rbindlist(diagnostics),
    replay = replay[],
    season = summary$season,
    team_game_role = summary$team_game_role,
    comparators = summary$comparators,
    bootstrap = summary$bootstrap,
    bootstrap_interval = summary$bootstrap_interval,
    truth_diagnostics = summary$truth_diagnostics,
    information_regime = paste(
      "game-heldout role-specific priors and joint Bellman;",
      "one shared team inventory across offense and defense"
    )
  )
}
