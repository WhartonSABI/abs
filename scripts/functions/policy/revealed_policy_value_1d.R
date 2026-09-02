# Policy-value replay for revealed offense/defense challenge behavior.
#
# The decision threshold/rule is frozen before held-out geometry is joined.
# For a non-oracle latent-signal policy, its pitch-conditional evaluation
# probability may nevertheless depend on held-out margin through the permitted
# integral P(action | M) = integral P(action | R) dP(R | M).  That evaluation
# integral is not the object available to the decision maker.  The replay then
# propagates one shared 0/1/2 challenge inventory for each team and game.  A
# successful challenge retains inventory; only failure removes one.  Exhausted
# teams receive one challenge on entry to every extra inning, matching
# replay_joint_challenge_signal_mdp_1d().

revealed_policy_value_key_columns_1d <- function() {
  c("game_pk", "team_id", "pitch_order")
}

assert_revealed_policy_action_outcome_free_1d <- function(
  rows_or_columns, label = "revealed policy action input"
) {
  assert_revealed_challenge_propensity_outcome_free_1d(
    rows_or_columns, label = label
  )
}

revealed_policy_decision_rule_truth_columns_1d <- function() {
  unique(c(
    revealed_challenge_margin_outcome_columns_1d(),
    "role_margin_inches", "edge_distance_inches", "plate_x", "plate_z",
    "sz_top", "sz_bot", "p_x", "p_z", "call_wrong"
  ))
}

assert_revealed_policy_decision_rule_truth_free_1d <- function(
  rows_or_columns, label = "frozen revealed policy decision rule"
) {
  columns <- if (is.character(rows_or_columns)) {
    as.character(rows_or_columns)
  } else {
    names(data.table::as.data.table(rows_or_columns))
  }
  leaked <- intersect(
    columns, revealed_policy_decision_rule_truth_columns_1d()
  )
  if (length(leaked)) {
    stop(
      label, " contains held-out truth/geometry columns: ",
      paste(sort(leaked), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.revealed_policy_assert_probability_payload_geometry_free_1d <- function(
  rows_or_columns
) {
  columns <- if (is.character(rows_or_columns)) {
    as.character(rows_or_columns)
  } else {
    names(data.table::as.data.table(rows_or_columns))
  }
  leaked <- intersect(
    columns, revealed_policy_decision_rule_truth_columns_1d()
  )
  if (length(leaked)) {
    stop(
      "Revealed policy probability rows must contain only the completed ",
      "evaluation probabilities, not raw held-out truth/geometry: ",
      paste(sort(leaked), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.revealed_policy_logical_provenance_1d <- function(value, name) {
  out <- as.logical(value)
  if (anyNA(out)) {
    stop(name, " must contain only TRUE/FALSE values", call. = FALSE)
  }
  out
}

revealed_policy_signal_integration_probability_1d <- function(
  role_margin_inches,
  signal_threshold_inches,
  sensory_sigma_inches
) {
  lengths <- c(
    length(role_margin_inches), length(signal_threshold_inches),
    length(sensory_sigma_inches)
  )
  size <- max(lengths)
  if (!size || any(lengths != 1L & lengths != size)) {
    stop(
      "Margin, frozen threshold, and sensory sigma must be scalar or aligned",
      call. = FALSE
    )
  }
  margin <- rep_len(as.numeric(role_margin_inches), size)
  threshold <- rep_len(as.numeric(signal_threshold_inches), size)
  sigma <- rep_len(as.numeric(sensory_sigma_inches), size)
  if (anyNA(margin) || any(!is.finite(margin)) || anyNA(threshold) ||
      anyNA(sigma) || any(!is.finite(sigma)) || any(sigma < 0)) {
    stop(
      "Signal integration needs finite margins, non-missing thresholds, and ",
      "finite non-negative sensory sigmas",
      call. = FALSE
    )
  }

  probability <- numeric(size)
  finite_threshold <- is.finite(threshold)
  deterministic <- finite_threshold & sigma == 0
  noisy <- finite_threshold & sigma > 0
  probability[deterministic] <- as.numeric(
    margin[deterministic] > threshold[deterministic]
  )
  probability[noisy] <- stats::pnorm(
    (margin[noisy] - threshold[noisy]) / sigma[noisy]
  )
  probability[threshold == -Inf] <- 1
  probability[threshold == Inf] <- 0
  pmin(1, pmax(0, probability))
}

.revealed_policy_scalar_integer_1d <- function(
  value, name, minimum = 0L
) {
  numeric_value <- suppressWarnings(as.numeric(value))
  integer_value <- suppressWarnings(as.integer(numeric_value))
  if (length(numeric_value) != 1L || !is.finite(numeric_value) ||
      numeric_value != integer_value || integer_value < minimum) {
    stop(name, " must be one integer at least ", minimum, call. = FALSE)
  }
  integer_value
}

normalize_revealed_policy_clock_1d <- function(rows) {
  x <- data.table::copy(data.table::as.data.table(rows))
  assert_revealed_policy_action_outcome_free_1d(
    x, "revealed policy opportunity clock"
  )
  required <- c(
    revealed_policy_value_key_columns_1d(), "inning", "role", "stake_G"
  )
  stop_if_missing_columns(x, required, "revealed policy opportunity clock")
  x[, `:=`(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    inning = as.integer(inning),
    role = as.character(role),
    stake_G = as.numeric(stake_G)
  )]
  if (!nrow(x) || anyNA(x[, .(
    game_pk, team_id, pitch_order, inning, role, stake_G
  )]) || any(!nzchar(x$game_pk)) || any(!nzchar(x$team_id)) ||
      any(x$pitch_order < 1L) || any(x$inning < 1L) ||
      any(!x$role %in% c("offense", "defense")) ||
      any(!is.finite(x$stake_G)) ||
      anyDuplicated(x[, .(game_pk, team_id, pitch_order)])) {
    stop("Revealed policy opportunity clock is invalid", call. = FALSE)
  }
  data.table::setorder(x, game_pk, team_id, pitch_order)
  chronology <- x[, any(diff(pitch_order) <= 0), by = .(game_pk, team_id)]
  if (any(chronology$V1)) {
    stop("Pitch order must increase within every team-game clock",
      call. = FALSE
    )
  }
  x[]
}

normalize_revealed_policy_truth_1d <- function(truth_rows) {
  x <- data.table::copy(data.table::as.data.table(truth_rows))
  # Official outcomes are never accepted by the primary geometry-truth table.
  official <- intersect(
    names(x),
    c(
      "official_success", "challenge_outcome", "official_outcome",
      "is_overturned", "overturned"
    )
  )
  if (length(official)) {
    stop(
      "Revealed policy truth contains evaluation-only official outcomes: ",
      paste(sort(official), collapse = ", "),
      call. = FALSE
    )
  }
  required <- c(
    revealed_policy_value_key_columns_1d(), "role", "role_margin_inches"
  )
  stop_if_missing_columns(x, required, "revealed policy geometry truth")
  retained <- required
  unexpected_truth <- intersect(
    setdiff(names(x), retained),
    revealed_challenge_margin_outcome_columns_1d()
  )
  if (length(unexpected_truth)) {
    stop(
      "Geometry truth must be derived from role_margin_inches; unexpected: ",
      paste(sort(unexpected_truth), collapse = ", "),
      call. = FALSE
    )
  }
  x <- x[, ..retained]
  x[, `:=`(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    role = as.character(role),
    role_margin_inches = as.numeric(role_margin_inches)
  )]
  if (!nrow(x) || anyNA(x) || any(!nzchar(x$game_pk)) ||
      any(!nzchar(x$team_id)) || any(x$pitch_order < 1L) ||
      any(!x$role %in% c("offense", "defense")) ||
      any(!is.finite(x$role_margin_inches)) ||
      anyDuplicated(x[, .(game_pk, team_id, pitch_order)])) {
    stop("Revealed policy geometry truth is invalid", call. = FALSE)
  }
  x[, geometry_success := revealed_challenge_geometry_success_1d(
    role, role_margin_inches
  )]
  x[]
}

normalize_revealed_policy_probabilities_1d <- function(probability_rows) {
  x <- data.table::copy(data.table::as.data.table(probability_rows))
  assert_revealed_policy_action_outcome_free_1d(
    x, "revealed policy probability rows"
  )
  .revealed_policy_assert_probability_payload_geometry_free_1d(x)
  required <- c(
    revealed_policy_value_key_columns_1d(), "policy", "policy_family",
    "probability_k1", "probability_k2"
  )
  stop_if_missing_columns(x, required, "revealed policy probability rows")
  x[, `:=`(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    policy = as.character(policy),
    policy_family = as.character(policy_family),
    probability_k1 = as.numeric(probability_k1),
    probability_k2 = as.numeric(probability_k2)
  )]

  has_decision_truth <- "truth_used_by_decision_rule" %in% names(x)
  has_legacy_truth <- "truth_used_by_action_rule" %in% names(x)
  if (!has_decision_truth && !has_legacy_truth) {
    stop(
      "Revealed policy probability rows must declare ",
      "truth_used_by_decision_rule (or legacy truth_used_by_action_rule)",
      call. = FALSE
    )
  }
  decision_truth <- if (has_decision_truth) {
    .revealed_policy_logical_provenance_1d(
      x$truth_used_by_decision_rule, "truth_used_by_decision_rule"
    )
  } else {
    .revealed_policy_logical_provenance_1d(
      x$truth_used_by_action_rule, "truth_used_by_action_rule"
    )
  }
  if (has_decision_truth && has_legacy_truth) {
    legacy_truth <- .revealed_policy_logical_provenance_1d(
      x$truth_used_by_action_rule, "truth_used_by_action_rule"
    )
    if (any(decision_truth != legacy_truth)) {
      stop(
        "truth_used_by_decision_rule conflicts with legacy ",
        "truth_used_by_action_rule",
        call. = FALSE
      )
    }
  }

  has_signal_integration <-
    "geometry_used_for_signal_integration" %in% names(x)
  geometry_integration <- if (has_signal_integration) {
    .revealed_policy_logical_provenance_1d(
      x$geometry_used_for_signal_integration,
      "geometry_used_for_signal_integration"
    )
  } else {
    # Backward compatibility for v1 probability artifacts.  These two legacy
    # families store pitch-conditional evaluation probabilities.  Their
    # held-out margin dependence is an integration over an unobserved decision
    # signal, not an input to the frozen decision threshold/rule.
    !decision_truth &
      x$policy_family %in% c("fitted_human_selection", "normative")
  }
  x[, `:=`(
    truth_used_by_decision_rule = decision_truth,
    geometry_used_for_signal_integration = geometry_integration,
    # Preserve the v1 name as a strict alias for downstream readers.  Its
    # wording is deprecated because an evaluation probability is not itself
    # the frozen action rule.
    truth_used_by_action_rule = decision_truth,
    provenance_schema = if (
      has_decision_truth && has_signal_integration
    ) {
      "decision_rule_v2_explicit"
    } else {
      "decision_rule_v2_legacy_compatible"
    }
  )]
  if (!nrow(x) || anyNA(x) || any(!nzchar(x$game_pk)) ||
      any(!nzchar(x$team_id)) || any(!nzchar(x$policy)) ||
      any(!nzchar(x$policy_family)) || any(x$pitch_order < 1L) ||
      any(!is.finite(x$probability_k1)) ||
      any(!is.finite(x$probability_k2)) ||
      any(x$probability_k1 < 0 | x$probability_k1 > 1) ||
      any(x$probability_k2 < 0 | x$probability_k2 > 1) ||
      anyDuplicated(x[, .(policy, game_pk, team_id, pitch_order)])) {
    stop("Revealed policy probability rows are invalid", call. = FALSE)
  }
  direct_truth <- x[truth_used_by_decision_rule == TRUE]
  if (nrow(direct_truth) && any(
    direct_truth$policy != "exact_location_oracle" |
      direct_truth$policy_family != "exact_location_oracle"
  )) {
    stop(
      "Only the explicitly labeled exact-location oracle may use truth ",
      "directly in its decision rule",
      call. = FALSE
    )
  }
  oracle <- x[
    policy == "exact_location_oracle" |
      policy_family == "exact_location_oracle"
  ]
  if (nrow(oracle) && any(
    oracle$policy != "exact_location_oracle" |
      oracle$policy_family != "exact_location_oracle" |
      !oracle$truth_used_by_decision_rule
  )) {
    stop(
      "The exact-location oracle must be labeled consistently and declare ",
      "truth_used_by_decision_rule = TRUE",
      call. = FALSE
    )
  }
  if (any(
    x$truth_used_by_decision_rule &
      x$geometry_used_for_signal_integration
  )) {
    stop(
      "Direct truth use and geometry-based signal integration are distinct ",
      "and cannot both be TRUE",
      call. = FALSE
    )
  }
  if (any(
    x$policy_family %in% c("observed", "no_challenges") &
      x$geometry_used_for_signal_integration
  )) {
    stop(
      "Observed and no-challenge comparators cannot claim geometry-based ",
      "signal integration",
      call. = FALSE
    )
  }
  policy_metadata <- unique(x[, .(
    policy, policy_family, truth_used_by_decision_rule,
    geometry_used_for_signal_integration
  )])
  if (anyDuplicated(policy_metadata$policy)) {
    stop("Each policy must have one family and one provenance declaration",
      call. = FALSE
    )
  }
  x[]
}

.revealed_policy_assert_complete_probabilities_1d <- function(clock, probability) {
  keys <- revealed_policy_value_key_columns_1d()
  clock_key <- do.call(paste, c(clock[, ..keys], sep = "\r"))
  policies <- unique(probability$policy)
  for (policy_name in policies) {
    value <- probability[policy == policy_name]
    probability_key <- do.call(paste, c(value[, ..keys], sep = "\r"))
    if (length(probability_key) != length(clock_key) ||
        !setequal(probability_key, clock_key)) {
      stop(
        "Policy ", policy_name,
        " does not assign k=1/k=2 probabilities to the complete clock",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

replay_revealed_policy_value_1d <- function(
  clock_rows,
  probability_rows,
  truth_rows,
  initial_inventory = 2L
) {
  clock <- normalize_revealed_policy_clock_1d(clock_rows)
  probability <- normalize_revealed_policy_probabilities_1d(probability_rows)
  truth <- normalize_revealed_policy_truth_1d(truth_rows)
  initial_inventory <- .revealed_policy_scalar_integer_1d(
    initial_inventory, "initial_inventory", minimum = 0L
  )
  if (!initial_inventory %in% 0:2) {
    stop("initial_inventory must be 0, 1, or 2", call. = FALSE)
  }
  keys <- revealed_policy_value_key_columns_1d()
  .revealed_policy_assert_complete_probabilities_1d(clock, probability)
  truth_key <- do.call(paste, c(truth[, ..keys], sep = "\r"))
  clock_key <- do.call(paste, c(clock[, ..keys], sep = "\r"))
  if (length(truth_key) != length(clock_key) ||
      !setequal(truth_key, clock_key)) {
    stop("Geometry truth must cover the complete opportunity clock exactly",
      call. = FALSE
    )
  }

  x <- merge(clock, truth, by = keys, all.x = TRUE, sort = FALSE)
  if (any(x$role.x != x$role.y)) {
    stop("Clock and geometry truth disagree on role", call. = FALSE)
  }
  x[, `:=`(role = role.x, role.x = NULL, role.y = NULL)]
  if (all(c(
    "role_margin_inches.x", "role_margin_inches.y"
  ) %in% names(x))) {
    if (any(abs(
      as.numeric(x$role_margin_inches.x) -
        as.numeric(x$role_margin_inches.y)
    ) > 1e-10, na.rm = TRUE)) {
      stop("Clock and geometry truth disagree on role margin", call. = FALSE)
    }
    x[, `:=`(
      role_margin_inches = as.numeric(role_margin_inches.y),
      role_margin_inches.x = NULL,
      role_margin_inches.y = NULL
    )]
  }
  x <- merge(x, probability, by = keys, allow.cartesian = TRUE, sort = FALSE)
  data.table::setorder(x, policy, game_pk, team_id, pitch_order)

  oracle <- x$truth_used_by_decision_rule %in% TRUE
  if (any(oracle)) {
    expected <- as.numeric(x$geometry_success & x$stake_G > 0)
    if (any(abs(x$probability_k1[oracle] - expected[oracle]) > 1e-12) ||
        any(abs(x$probability_k2[oracle] - expected[oracle]) > 1e-12)) {
      stop(
        paste(
          "Exact-location oracle probabilities must challenge exactly",
          "positive-gain wrong calls"
        ),
        call. = FALSE
      )
    }
  }

  n <- nrow(x)
  inventory_probability_0 <- inventory_probability_1 <-
    inventory_probability_2 <- expected_challenges <- expected_successes <-
      expected_failures <- expected_captured_re <- numeric(n)
  extra_inning_replenishment <- logical(n)
  groups <- split(
    seq_len(n), interaction(x$policy, x$game_pk, x$team_id, drop = TRUE)
  )
  for (indices in groups) {
    mass <- c(`0` = 0, `1` = 0, `2` = 0)
    mass[[as.character(initial_inventory)]] <- 1
    previous_inning <- NA_integer_
    for (row in indices) {
      inning_now <- x$inning[[row]]
      extra_entry <- (is.na(previous_inning) && inning_now > 9L) ||
        (!is.na(previous_inning) && inning_now > previous_inning &&
          inning_now > 9L)
      if (extra_entry) {
        mass[["1"]] <- mass[["1"]] + mass[["0"]]
        mass[["0"]] <- 0
        extra_inning_replenishment[[row]] <- TRUE
      }
      inventory_probability_0[[row]] <- mass[["0"]]
      inventory_probability_1[[row]] <- mass[["1"]]
      inventory_probability_2[[row]] <- mass[["2"]]

      action_by_inventory <- c(
        `0` = 0,
        `1` = x$probability_k1[[row]],
        `2` = x$probability_k2[[row]]
      )
      challenge <- sum(mass * action_by_inventory)
      success <- challenge * as.numeric(x$geometry_success[[row]])
      failure <- challenge * as.numeric(!x$geometry_success[[row]])
      expected_challenges[[row]] <- challenge
      expected_successes[[row]] <- success
      expected_failures[[row]] <- failure
      expected_captured_re[[row]] <- success * x$stake_G[[row]]

      # Passing and successful challenges retain inventory.  Failure alone
      # moves probability mass down by one challenge.
      failure_by_inventory <- action_by_inventory *
        as.numeric(!x$geometry_success[[row]])
      old <- mass
      mass <- old * (1 - failure_by_inventory)
      mass[["0"]] <- mass[["0"]] + old[["1"]] *
        failure_by_inventory[["1"]]
      mass[["1"]] <- mass[["1"]] + old[["2"]] *
        failure_by_inventory[["2"]]
      if (any(mass < -1e-12) || abs(sum(mass) - 1) > 1e-10) {
        stop("Revealed-policy inventory probability mass failed to close",
          call. = FALSE
        )
      }
      mass <- stats::setNames(pmax(0, mass) / sum(mass), as.character(0:2))
      previous_inning <- inning_now
    }
  }
  x[, `:=`(
    inventory_probability_0 = inventory_probability_0,
    inventory_probability_1 = inventory_probability_1,
    inventory_probability_2 = inventory_probability_2,
    inventory_available_probability =
      inventory_probability_1 + inventory_probability_2,
    extra_inning_replenishment = extra_inning_replenishment,
    expected_challenges = expected_challenges,
    expected_successes = expected_successes,
    expected_failures = expected_failures,
    expected_captured_re = expected_captured_re
  )]
  if (any(abs(
    x$expected_challenges - x$expected_successes - x$expected_failures
  ) > 1e-12)) {
    stop("Challenge mass does not equal success plus failure mass",
      call. = FALSE
    )
  }
  data.table::setorder(x, policy, game_pk, team_id, pitch_order)
  data.table::setattr(x, "information_regime", list(
    frozen_object = "OOF decision threshold/rule, not its evaluation probability",
    decision_rule = paste(
      "held-out geometry and official outcomes excluded from every frozen",
      "decision rule except the labeled exact-location oracle"
    ),
    pitch_conditional_evaluation_probability = paste(
      "non-oracle held-out margin may enter only through declared signal",
      "integration P(action | M) = integral P(action | R) dP(R | M)"
    ),
    evaluation_truth = "exact role-oriented signed-margin geometry",
    official_outcomes = "not used",
    inventory = "one chronological offense/defense inventory per team-game",
    truth_used_by_decision_rule = paste(
      "explicit v2 provenance; truth_used_by_action_rule retained as a",
      "deprecated exact alias"
    ),
    geometry_used_for_signal_integration = paste(
      "explicit v2 provenance, inferred by policy family only for legacy",
      "probability artifacts"
    )
  ))
  x[]
}

.revealed_policy_probability_spec_1d <- function(
  rows, specification, policy_name
) {
  columns <- as.character(specification)
  if (!length(columns) || length(columns) > 2L || anyNA(columns) ||
      any(!nzchar(columns))) {
    stop(
      "Policy ", policy_name,
      " must map to one shared or two k-specific probability columns",
      call. = FALSE
    )
  }
  if (length(columns) == 1L) columns <- rep(columns, 2L)
  stop_if_missing_columns(rows, unique(columns), paste(policy_name, "policy"))
  list(
    k1 = as.numeric(rows[[columns[[1L]]]]),
    k2 = as.numeric(rows[[columns[[2L]]]]),
    source_columns = columns
  )
}

build_revealed_policy_comparators_1d <- function(
  clock_rows,
  truth_rows,
  observed_action_column = "challenged",
  fitted_policies = list(fitted_human_selection = "challenge_probability"),
  normative_policies = list(),
  initial_inventory = 2L
) {
  clock <- normalize_revealed_policy_clock_1d(clock_rows)
  truth <- normalize_revealed_policy_truth_1d(truth_rows)
  stop_if_missing_columns(
    clock, observed_action_column, "revealed observed-decision clock"
  )
  observed_action <- suppressWarnings(as.integer(clock[[observed_action_column]]))
  if (anyNA(observed_action) || any(!observed_action %in% 0:1)) {
    stop("Observed challenge actions must be binary", call. = FALSE)
  }
  valid_policy_list <- function(value) {
    is.list(value) &&
      (!length(value) || (!is.null(names(value)) &&
        !anyDuplicated(names(value))))
  }
  if (!valid_policy_list(fitted_policies) ||
      !valid_policy_list(normative_policies)) {
    stop("Fitted and normative policies must be uniquely named lists",
      call. = FALSE
    )
  }
  reserved <- c("observed", "no_challenges", "exact_location_oracle")
  supplied_names <- c(names(fitted_policies), names(normative_policies))
  if (anyDuplicated(supplied_names) || any(supplied_names %in% reserved) ||
      any(!nzchar(supplied_names))) {
    stop("Comparator policy names must be unique and non-reserved",
      call. = FALSE
    )
  }
  keys <- revealed_policy_value_key_columns_1d()
  base <- clock[, ..keys]
  make_probability <- function(
    policy, family, k1, k2, truth_used = FALSE,
    geometry_integrated = FALSE
  ) {
    out <- data.table::copy(base)
    out[, `:=`(
      policy = policy,
      policy_family = family,
      probability_k1 = as.numeric(k1),
      probability_k2 = as.numeric(k2),
      truth_used_by_decision_rule = truth_used,
      geometry_used_for_signal_integration = geometry_integrated,
      truth_used_by_action_rule = truth_used
    )]
    out
  }
  pieces <- list(
    make_probability("observed", "observed", observed_action, observed_action),
    make_probability("no_challenges", "no_challenges", 0, 0)
  )
  for (policy_name in names(fitted_policies)) {
    value <- .revealed_policy_probability_spec_1d(
      clock, fitted_policies[[policy_name]], policy_name
    )
    pieces[[length(pieces) + 1L]] <- make_probability(
      policy_name, "fitted_human_selection", value$k1, value$k2,
      geometry_integrated = TRUE
    )
  }
  for (policy_name in names(normative_policies)) {
    value <- .revealed_policy_probability_spec_1d(
      clock, normative_policies[[policy_name]], policy_name
    )
    pieces[[length(pieces) + 1L]] <- make_probability(
      policy_name, "normative", value$k1, value$k2,
      geometry_integrated = TRUE
    )
  }

  oracle_truth <- merge(
    clock[, c(keys, "stake_G"), with = FALSE],
    truth[, c(keys, "geometry_success"), with = FALSE],
    by = keys, all.x = TRUE, sort = FALSE
  )
  base_key <- do.call(paste, c(base[, ..keys], sep = "\r"))
  oracle_key <- do.call(paste, c(oracle_truth[, ..keys], sep = "\r"))
  oracle_truth <- oracle_truth[match(base_key, oracle_key)]
  oracle_probability <- as.numeric(
    oracle_truth$geometry_success & oracle_truth$stake_G > 0
  )
  pieces[[length(pieces) + 1L]] <- make_probability(
    "exact_location_oracle", "exact_location_oracle",
    oracle_probability, oracle_probability, truth_used = TRUE,
    geometry_integrated = FALSE
  )
  probability <- data.table::rbindlist(pieces, use.names = TRUE)
  replay_revealed_policy_value_1d(
    clock,
    probability,
    truth[, c(keys, "role", "role_margin_inches"), with = FALSE],
    initial_inventory = initial_inventory
  )
}

.revealed_policy_game_role_summary_1d <- function(replay) {
  x <- data.table::as.data.table(replay)
  required <- c(
    "game_pk", "team_id", "policy", "policy_family", "role",
    "expected_captured_re", "expected_challenges", "expected_successes",
    "expected_failures", "inventory_probability_0"
  )
  stop_if_missing_columns(x, required, "revealed policy replay summary")
  x[, .(
    captured_re = sum(expected_captured_re),
    attempts = sum(expected_challenges),
    successes = sum(expected_successes),
    failures = sum(expected_failures),
    exhausted_opportunity_mass = sum(inventory_probability_0),
    opportunity_exposure = .N
  ), by = .(game_pk, team_id, policy, policy_family, role)]
}

.revealed_policy_add_combined_1d <- function(values, group_columns) {
  x <- data.table::as.data.table(values)
  measure_columns <- c(
    "captured_re", "attempts", "successes", "failures",
    "exhausted_opportunity_mass", "opportunity_exposure"
  )
  combined <- x[, lapply(.SD, sum), by = group_columns,
    .SDcols = measure_columns
  ]
  combined[, role := "combined"]
  data.table::rbindlist(list(x, combined), use.names = TRUE, fill = TRUE)
}

.revealed_policy_finalize_summary_1d <- function(
  values, observed_policy = "observed"
) {
  x <- data.table::copy(data.table::as.data.table(values))
  x[, `:=`(
    success_rate = ifelse(attempts > 0, successes / attempts, NA_real_),
    inventory_exhaustion_rate = ifelse(
      opportunity_exposure > 0,
      exhausted_opportunity_mass / opportunity_exposure,
      NA_real_
    )
  )]
  reference <- x[policy == observed_policy, .(
    role,
    observed_captured_re = captured_re,
    observed_attempts = attempts,
    observed_successes = successes,
    observed_success_rate = success_rate,
    observed_inventory_exhaustion_rate = inventory_exhaustion_rate
  )]
  if (!nrow(reference) || anyDuplicated(reference$role)) {
    stop("Exactly one observed reference is required for every role",
      call. = FALSE
    )
  }
  x <- merge(x, reference, by = "role", all.x = TRUE, sort = FALSE)
  x[, `:=`(
    gain_over_observed_re = captured_re - observed_captured_re,
    attempts_over_observed = attempts - observed_attempts
  )]
  x[, role_order__ := match(role, c("offense", "defense", "combined"))]
  data.table::setorder(x, policy, role_order__)
  x[, role_order__ := NULL]
  x[]
}

.revealed_policy_finite_quantile_1d <- function(value, probability) {
  value <- as.numeric(value)
  value <- value[is.finite(value)]
  if (!length(value)) return(NA_real_)
  stats::quantile(value, probability, names = FALSE)
}

bootstrap_revealed_policy_game_role_1d <- function(
  game_role,
  reps = 2000L,
  seed = 20260826L,
  observed_policy = "observed"
) {
  game_role <- data.table::copy(data.table::as.data.table(game_role))
  stop_if_missing_columns(
    game_role,
    c(
      "game_pk", "policy", "policy_family", "role", "captured_re",
      "attempts", "successes", "failures", "exhausted_opportunity_mass",
      "opportunity_exposure"
    ),
    "revealed policy game-role summary"
  )
  reps <- .revealed_policy_scalar_integer_1d(reps, "reps", minimum = 0L)
  seed <- .revealed_policy_scalar_integer_1d(seed, "seed", minimum = 1L)
  games <- sort(unique(as.character(game_role$game_pk)))
  if (!length(games)) stop("No games are available for policy bootstrap")
  empty_draws <- data.table::data.table()
  empty_intervals <- data.table::data.table()
  if (reps == 0L) {
    return(list(draws = empty_draws, intervals = empty_intervals))
  }
  set.seed(seed)
  draws <- data.table::rbindlist(lapply(seq_len(reps), function(index) {
    weights <- data.table::data.table(
      game_pk = sample(games, length(games), replace = TRUE)
    )[, .(bootstrap_weight = .N), by = game_pk]
    value <- merge(game_role, weights, by = "game_pk", all.y = TRUE)
    by_role <- value[, .(
      captured_re = sum(captured_re * bootstrap_weight),
      attempts = sum(attempts * bootstrap_weight),
      successes = sum(successes * bootstrap_weight),
      failures = sum(failures * bootstrap_weight),
      exhausted_opportunity_mass = sum(
        exhausted_opportunity_mass * bootstrap_weight
      ),
      opportunity_exposure = sum(opportunity_exposure * bootstrap_weight)
    ), by = .(policy, policy_family, role)]
    all_roles <- .revealed_policy_add_combined_1d(
      by_role, c("policy", "policy_family")
    )
    out <- .revealed_policy_finalize_summary_1d(
      all_roles, observed_policy = observed_policy
    )
    out[, replicate := index]
    out
  }), use.names = TRUE, fill = TRUE)
  intervals <- draws[, .(
    captured_re_lower_95 = stats::quantile(
      captured_re, 0.025, names = FALSE
    ),
    captured_re_upper_95 = stats::quantile(
      captured_re, 0.975, names = FALSE
    ),
    attempts_lower_95 = stats::quantile(attempts, 0.025, names = FALSE),
    attempts_upper_95 = stats::quantile(attempts, 0.975, names = FALSE),
    success_rate_lower_95 = .revealed_policy_finite_quantile_1d(
      success_rate, 0.025
    ),
    success_rate_upper_95 = .revealed_policy_finite_quantile_1d(
      success_rate, 0.975
    ),
    inventory_exhaustion_rate_lower_95 =
      .revealed_policy_finite_quantile_1d(
        inventory_exhaustion_rate, 0.025
      ),
    inventory_exhaustion_rate_upper_95 =
      .revealed_policy_finite_quantile_1d(
        inventory_exhaustion_rate, 0.975
      ),
    gain_over_observed_re_lower_95 = stats::quantile(
      gain_over_observed_re, 0.025, names = FALSE
    ),
    gain_over_observed_re_upper_95 = stats::quantile(
      gain_over_observed_re, 0.975, names = FALSE
    )
  ), by = .(policy, policy_family, role)]
  list(draws = draws[], intervals = intervals[])
}

bootstrap_revealed_policy_value_1d <- function(
  replay,
  reps = 2000L,
  seed = 20260826L,
  observed_policy = "observed"
) {
  game_role <- .revealed_policy_game_role_summary_1d(replay)
  bootstrap_revealed_policy_game_role_1d(
    game_role,
    reps = reps,
    seed = seed,
    observed_policy = observed_policy
  )
}

summarize_revealed_policy_game_role_1d <- function(
  game_role,
  bootstrap_reps = 2000L,
  seed = 20260826L,
  observed_policy = "observed"
) {
  game_role <- data.table::copy(data.table::as.data.table(game_role))
  stop_if_missing_columns(
    game_role,
    c(
      "game_pk", "team_id", "policy", "policy_family", "role",
      "captured_re", "attempts", "successes", "failures",
      "exhausted_opportunity_mass", "opportunity_exposure"
    ),
    "revealed policy game-role values"
  )
  by_role <- game_role[, .(
    captured_re = sum(captured_re),
    attempts = sum(attempts),
    successes = sum(successes),
    failures = sum(failures),
    exhausted_opportunity_mass = sum(exhausted_opportunity_mass),
    opportunity_exposure = sum(opportunity_exposure)
  ), by = .(policy, policy_family, role)]
  season <- .revealed_policy_add_combined_1d(
    by_role, c("policy", "policy_family")
  )
  season <- .revealed_policy_finalize_summary_1d(
    season, observed_policy = observed_policy
  )
  bootstrap <- bootstrap_revealed_policy_game_role_1d(
    game_role,
    reps = bootstrap_reps,
    seed = seed,
    observed_policy = observed_policy
  )
  if (nrow(bootstrap$intervals)) {
    season <- merge(
      season, bootstrap$intervals,
      by = c("policy", "policy_family", "role"),
      all.x = TRUE, sort = FALSE
    )
  }
  list(
    season = season[],
    game_team_role = game_role[],
    bootstrap = bootstrap$draws[],
    bootstrap_intervals = bootstrap$intervals[],
    observed_policy = observed_policy,
    information_regime = paste(
      "shared-inventory expected replay with frozen decision rules; held-out",
      "geometry may form pitch-conditional probabilities only through",
      "declared signal integration and otherwise enters success/failure",
      "valuation; official outcomes excluded"
    )
  )
}

summarize_revealed_policy_value_1d <- function(
  replay,
  bootstrap_reps = 2000L,
  seed = 20260826L,
  observed_policy = "observed"
) {
  game_role <- .revealed_policy_game_role_summary_1d(replay)
  summarize_revealed_policy_game_role_1d(
    game_role,
    bootstrap_reps = bootstrap_reps,
    seed = seed,
    observed_policy = observed_policy
  )
}

evaluate_revealed_policy_official_outcomes_1d <- function(
  replay,
  official_labels,
  observed_policy = "observed"
) {
  x <- data.table::copy(data.table::as.data.table(replay))
  required_replay <- c(
    revealed_policy_value_key_columns_1d(), "policy", "role",
    "probability_k1", "probability_k2", "geometry_success"
  )
  stop_if_missing_columns(x, required_replay, "revealed policy replay")
  x <- x[policy == observed_policy]
  if (!nrow(x)) stop("Observed policy is absent from replay", call. = FALSE)
  labels <- data.table::copy(data.table::as.data.table(official_labels))
  allowed <- c(
    revealed_policy_value_key_columns_1d(), "role", "official_success"
  )
  stop_if_missing_columns(labels, allowed, "revealed policy official labels")
  unexpected <- setdiff(names(labels), allowed)
  if (length(unexpected)) {
    stop(
      "Official policy labels must use the evaluation-only allowlist; unexpected: ",
      paste(sort(unexpected), collapse = ", "), call. = FALSE
    )
  }
  labels[, `:=`(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    role = as.character(role),
    official_success = as.logical(official_success)
  )]
  keys <- revealed_policy_value_key_columns_1d()
  if (anyNA(labels[, c(keys, "role"), with = FALSE]) ||
      anyDuplicated(labels[, ..keys])) {
    stop("Official policy labels have invalid keys", call. = FALSE)
  }
  x <- merge(x, labels, by = keys, all.x = TRUE, sort = FALSE)
  if (any(!is.na(x$role.y) & x$role.x != x$role.y)) {
    stop("Official policy labels disagree on role", call. = FALSE)
  }
  x[, observed_action__ := probability_k1 == 1 & probability_k2 == 1]
  by_role <- x[, {
    local_labeled <- observed_action__ & !is.na(official_success)
    attempts <- sum(local_labeled)
    mismatch <- sum(
      official_success[local_labeled] != geometry_success[local_labeled]
    )
    list(
      official_labeled_challenges = attempts,
      official_successes = sum(official_success[local_labeled]),
      official_success_rate = if (attempts > 0) {
        sum(official_success[local_labeled]) / attempts
      } else {
        NA_real_
      },
      geometry_successes = sum(geometry_success[local_labeled]),
      official_geometry_mismatches = mismatch,
      official_geometry_mismatch_rate = if (attempts > 0) {
        mismatch / attempts
      } else {
        NA_real_
      }
    )
  }, by = .(role = role.x)]
  combined <- by_role[, .(
    role = "combined",
    official_labeled_challenges = sum(official_labeled_challenges),
    official_successes = sum(official_successes),
    official_success_rate = if (sum(official_labeled_challenges) > 0) {
      sum(official_successes) / sum(official_labeled_challenges)
    } else {
      NA_real_
    },
    geometry_successes = sum(geometry_successes),
    official_geometry_mismatches = sum(official_geometry_mismatches),
    official_geometry_mismatch_rate = if (
      sum(official_labeled_challenges) > 0
    ) {
      sum(official_geometry_mismatches) /
        sum(official_labeled_challenges)
    } else {
      NA_real_
    }
  )]
  result <- data.table::rbindlist(list(by_role, combined), use.names = TRUE)
  data.table::setattr(result, "evaluation_only", TRUE)
  data.table::setattr(result, "official_outcomes_used_for_policy", FALSE)
  result[]
}
