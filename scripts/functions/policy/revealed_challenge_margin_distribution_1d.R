# Revealed challenge-margin distributions.
#
# These utilities do not fit challenge wins.  They combine an opportunity
# density f(M | z) with a held-out challenge propensity pi(M, z) and derive
#
#   f(M | A = 1, z) = f(M | z) pi(M, z) / E[pi(M, z) | z].
#
# Official outcomes are deliberately accepted only by the leaf evaluation
# function.  Geometry truth is reconstructed from the signed role margin so it
# cannot enter a propensity fit as a separate response or predictor.

revealed_challenge_margin_outcome_columns_1d <- function() {
  unique(c(
    "abs_call", "final_call", "description", "is_overturned",
    "challenge_outcome", "official_abs_call", "official_outcome",
    "official_success", "geometry_success", "geometry_wrong",
    "actual_wrong", "observed_success", "overturned",
    "challenge_success", "outcome"
  ))
}

assert_revealed_challenge_propensity_outcome_free_1d <- function(
  rows_or_columns, label = "revealed challenge propensity input"
) {
  columns <- if (is.character(rows_or_columns)) {
    as.character(rows_or_columns)
  } else {
    names(data.table::as.data.table(rows_or_columns))
  }
  leaked <- intersect(columns, revealed_challenge_margin_outcome_columns_1d())
  if (length(leaked)) {
    stop(
      label, " contains evaluation-only outcome columns: ",
      paste(sort(leaked), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.revealed_challenge_scalar_number_1d <- function(
  value, name, lower = -Inf, upper = Inf, lower_open = FALSE,
  upper_open = FALSE
) {
  value <- suppressWarnings(as.numeric(value))
  bad_lower <- if (lower_open) value <= lower else value < lower
  bad_upper <- if (upper_open) value >= upper else value > upper
  if (length(value) != 1L || !is.finite(value) || bad_lower || bad_upper) {
    left <- if (lower_open) "(" else "["
    right <- if (upper_open) ")" else "]"
    stop(
      name, " must be one finite number in ", left, lower, ", ", upper,
      right, call. = FALSE
    )
  }
  value
}

.revealed_challenge_trapezoid_1d <- function(x, y) {
  if (length(x) < 2L) return(0)
  sum(diff(x) * (y[-length(y)] + y[-1L]) / 2)
}

.revealed_challenge_trapezoid_above_zero_1d <- function(x, y) {
  if (max(x) <= 0) return(0)
  if (min(x) >= 0) return(.revealed_challenge_trapezoid_1d(x, y))
  if (!any(x == 0)) {
    zero_y <- stats::approx(x, y, xout = 0, ties = "ordered")$y
    x <- c(x, 0)
    y <- c(y, zero_y)
    ordering <- order(x)
    x <- x[ordering]
    y <- y[ordering]
  }
  keep <- x >= 0
  .revealed_challenge_trapezoid_1d(x[keep], y[keep])
}

.revealed_challenge_group_columns_1d <- function(rows, group_columns) {
  group_columns <- unique(as.character(group_columns))
  if (!length(group_columns) || anyNA(group_columns) ||
      any(!nzchar(group_columns))) {
    stop("group_columns must contain at least one valid column name",
      call. = FALSE
    )
  }
  stop_if_missing_columns(rows, group_columns, "revealed-margin grouping input")
  group_columns
}

derive_revealed_challenge_margin_density_1d <- function(
  grid,
  group_columns = c("fold", "role", "count_state"),
  margin_column = "margin_inches",
  opportunity_density_column = "opportunity_density",
  propensity_column = "challenge_probability",
  opportunity_mass_tolerance = 0.01
) {
  x <- data.table::copy(data.table::as.data.table(grid))
  assert_revealed_challenge_propensity_outcome_free_1d(
    x, "revealed challenge density grid"
  )
  group_columns <- .revealed_challenge_group_columns_1d(x, group_columns)
  required <- c(
    group_columns, margin_column, opportunity_density_column,
    propensity_column
  )
  stop_if_missing_columns(x, required, "revealed challenge density grid")
  tolerance <- .revealed_challenge_scalar_number_1d(
    opportunity_mass_tolerance, "opportunity_mass_tolerance",
    lower = 0, upper = 0.25
  )
  margin <- suppressWarnings(as.numeric(x[[margin_column]]))
  density <- suppressWarnings(as.numeric(x[[opportunity_density_column]]))
  propensity <- suppressWarnings(as.numeric(x[[propensity_column]]))
  if (!nrow(x) || anyNA(margin) || any(!is.finite(margin)) ||
      anyNA(density) || any(!is.finite(density)) || any(density < 0) ||
      anyNA(propensity) || any(!is.finite(propensity)) ||
      any(propensity < 0 | propensity > 1)) {
    stop(
      "Density-grid margins/densities/propensities must be finite, with ",
      "density >= 0 and propensity in [0,1]",
      call. = FALSE
    )
  }
  x[, `:=`(
    margin_inches = margin,
    opportunity_density = density,
    challenge_probability = propensity
  )]
  data.table::setorderv(x, c(group_columns, "margin_inches"))
  duplicated_margin <- x[, anyDuplicated(margin_inches) > 0L,
    by = group_columns
  ][V1 == TRUE]
  too_short <- x[, .N, by = group_columns][N < 2L]
  nonincreasing <- x[, any(diff(margin_inches) <= 0),
    by = group_columns
  ][V1 == TRUE]
  if (nrow(duplicated_margin) || nrow(too_short) || nrow(nonincreasing)) {
    stop(
      "Every density group needs at least two unique, strictly increasing margins",
      call. = FALSE
    )
  }

  group_summary <- x[, {
    opportunity_mass <- .revealed_challenge_trapezoid_1d(
      margin_inches, opportunity_density
    )
    if (!is.finite(opportunity_mass) || opportunity_mass <= 0) {
      stop("Opportunity density has non-positive numerical mass",
        call. = FALSE
      )
    }
    normalized_opportunity <- opportunity_density / opportunity_mass
    selected_unnormalized <- normalized_opportunity * challenge_probability
    selected_action_mass <- .revealed_challenge_trapezoid_1d(
      margin_inches, selected_unnormalized
    )
    if (!is.finite(selected_action_mass) || selected_action_mass <= 0) {
      stop("Opportunity density and propensity imply no selected mass",
        call. = FALSE
      )
    }
    selected_density <- selected_unnormalized / selected_action_mass
    selected_success_rate <- .revealed_challenge_trapezoid_above_zero_1d(
      margin_inches, selected_density
    )
    selected_density_mass <- .revealed_challenge_trapezoid_1d(
      margin_inches, selected_density
    )
    list(
      opportunity_mass_numeric = opportunity_mass,
      opportunity_mass_error = opportunity_mass - 1,
      selected_action_mass_numeric = selected_action_mass,
      selected_success_rate_numeric = selected_success_rate,
      selected_failure_rate_numeric = 1 - selected_success_rate,
      selected_density_mass_numeric = selected_density_mass,
      selected_density_mass_error = selected_density_mass - 1
    )
  }, by = group_columns]
  if (any(abs(group_summary$opportunity_mass_error) > tolerance)) {
    stop(
      "At least one opportunity-density grid misses more than ", tolerance,
      " total probability mass",
      call. = FALSE
    )
  }

  normalizers <- group_summary[, c(
    group_columns, "opportunity_mass_numeric", "selected_action_mass_numeric"
  ), with = FALSE]
  x <- merge(x, normalizers, by = group_columns, all.x = TRUE, sort = FALSE)
  x[, `:=`(
    opportunity_density_normalized = opportunity_density /
      opportunity_mass_numeric,
    selected_density_unnormalized =
      opportunity_density / opportunity_mass_numeric * challenge_probability
  )]
  x[, selected_density := selected_density_unnormalized /
    selected_action_mass_numeric]
  data.table::setorderv(x, c(group_columns, "margin_inches"))
  result <- list(
    density = x[],
    summary = group_summary[],
    group_columns = group_columns,
    information_regime = paste(
      "derived selected-margin density from contextual opportunity density",
      "and held-out challenge propensity; no win model"
    ),
    excluded_information = revealed_challenge_margin_outcome_columns_1d()
  )
  class(result) <- "revealed_challenge_margin_density_1d"
  result
}

.revealed_challenge_probit_component_mass_1d <- function(
  mean, sd, intercept, slope
) {
  action_mass <- stats::pnorm(
    (intercept + slope * mean) / sqrt(1 + slope^2 * sd^2)
  )
  covariance <- matrix(c(
    sd^2, -slope * sd^2,
    -slope * sd^2, 1 + slope^2 * sd^2
  ), nrow = 2L, byrow = TRUE)
  # TVPACK handles the lower-orthant term deterministically.  Subtracting it
  # from P(action) gives P(M > 0, action) without randomized integration.
  correct_side_action <- as.numeric(mvtnorm::pmvnorm(
    lower = c(-Inf, -Inf), upper = c(0, intercept),
    mean = c(mean, -slope * mean), sigma = covariance,
    algorithm = mvtnorm::TVPACK()
  ))
  success_joint <- pmax(0, action_mass - correct_side_action)
  c(action_mass = action_mass, success_joint_mass = success_joint)
}

analytic_revealed_challenge_probit_mass_1d <- function(
  prior_fit,
  intercept,
  slope,
  policy_weight = NULL,
  context = NULL,
  role = NA_character_,
  fold = NA_integer_
) {
  .challenge_margin_validate_fit(prior_fit)
  intercept <- as.numeric(intercept)
  slope <- as.numeric(slope)
  if (!length(intercept) || length(slope) != length(intercept) ||
      anyNA(intercept) || any(!is.finite(intercept)) || anyNA(slope) ||
      any(!is.finite(slope)) || any(slope < 0)) {
    stop(
      "Probit intercepts and non-negative slopes must be finite and aligned",
      call. = FALSE
    )
  }
  if (is.null(policy_weight)) policy_weight <- rep(1, length(intercept))
  policy_weight <- as.numeric(policy_weight)
  if (length(policy_weight) != length(intercept) || anyNA(policy_weight) ||
      any(!is.finite(policy_weight)) || any(policy_weight < 0) ||
      sum(policy_weight) <= 0) {
    stop("policy_weight must contain aligned non-negative finite mass",
      call. = FALSE
    )
  }
  policy_weight <- policy_weight / sum(policy_weight)
  if (!is.null(context) && length(context) != 1L) {
    stop("Analytic probit mass accepts one contextual prior at a time",
      call. = FALSE
    )
  }
  resolved <- .challenge_margin_resolve_context_weights(
    prior_fit, context, 1L
  )
  component_weight <- as.numeric(resolved$weight[1L, ])
  action_mass <- success_joint_mass <- 0
  for (policy in seq_along(intercept)) {
    for (component in seq_len(prior_fit$components)) {
      mass <- .revealed_challenge_probit_component_mass_1d(
        prior_fit$mean[[component]], prior_fit$sd[[component]],
        intercept[[policy]], slope[[policy]]
      )
      weight <- policy_weight[[policy]] * component_weight[[component]]
      action_mass <- action_mass + weight * mass[["action_mass"]]
      success_joint_mass <- success_joint_mass +
        weight * mass[["success_joint_mass"]]
    }
  }
  if (!is.finite(action_mass) || action_mass <= 0 ||
      !is.finite(success_joint_mass) || success_joint_mass < 0 ||
      success_joint_mass > action_mass + 1e-10) {
    stop("Analytic probit selected masses are invalid", call. = FALSE)
  }
  prior_success_mass <- sum(
    component_weight * stats::pnorm(prior_fit$mean / prior_fit$sd)
  )
  data.table::data.table(
    fold = fold,
    role = as.character(role),
    count_state = resolved$context_value[[1L]],
    policy_strata = length(intercept),
    opportunity_success_mass_analytic = prior_success_mass,
    selected_action_mass_analytic = action_mass,
    selected_success_joint_mass_analytic = success_joint_mass,
    selected_success_rate_analytic = success_joint_mass / action_mass,
    selected_failure_rate_analytic =
      1 - success_joint_mass / action_mass,
    context_seen_in_training = resolved$context_seen_in_training[[1L]],
    context_fallback = resolved$context_fallback[[1L]],
    prior_fold_id = prior_fit$fold_id %||% NA_character_,
    global_draw_id = prior_fit$global_draw_id %||% NA_integer_
  )
}

validate_revealed_challenge_analytic_numeric_mass_1d <- function(
  numeric_summary,
  analytic_summary,
  by = c("fold", "role", "count_state"),
  action_tolerance = 0.002,
  success_tolerance = 0.002
) {
  numeric <- data.table::copy(data.table::as.data.table(numeric_summary))
  analytic <- data.table::copy(data.table::as.data.table(analytic_summary))
  by <- unique(as.character(by))
  stop_if_missing_columns(
    numeric,
    c(by, "selected_action_mass_numeric", "selected_success_rate_numeric"),
    "numeric selected-margin summary"
  )
  stop_if_missing_columns(
    analytic,
    c(by, "selected_action_mass_analytic", "selected_success_rate_analytic"),
    "analytic selected-margin summary"
  )
  action_tolerance <- .revealed_challenge_scalar_number_1d(
    action_tolerance, "action_tolerance", lower = 0, upper = 0.25
  )
  success_tolerance <- .revealed_challenge_scalar_number_1d(
    success_tolerance, "success_tolerance", lower = 0, upper = 0.25
  )
  if (anyDuplicated(numeric[, ..by]) || anyDuplicated(analytic[, ..by])) {
    stop("Analytic/numeric mass keys must be unique", call. = FALSE)
  }
  compared <- merge(
    numeric, analytic, by = by, all = TRUE, sort = FALSE
  )
  required_values <- c(
    "selected_action_mass_numeric", "selected_success_rate_numeric",
    "selected_action_mass_analytic", "selected_success_rate_analytic"
  )
  if (!nrow(compared) || anyNA(compared[, ..required_values])) {
    stop("Analytic and numeric selected-margin groups do not align",
      call. = FALSE
    )
  }
  compared[, `:=`(
    selected_action_mass_absolute_error = abs(
      selected_action_mass_numeric - selected_action_mass_analytic
    ),
    selected_success_rate_absolute_error = abs(
      selected_success_rate_numeric - selected_success_rate_analytic
    )
  )]
  compared[, `:=`(
    action_mass_within_tolerance =
      selected_action_mass_absolute_error <= action_tolerance,
    success_mass_within_tolerance =
      selected_success_rate_absolute_error <= success_tolerance
  )]
  if (any(!compared$action_mass_within_tolerance) ||
      any(!compared$success_mass_within_tolerance)) {
    stop(
      "Analytic and numeric selected-margin masses disagree beyond tolerance",
      call. = FALSE
    )
  }
  compared[]
}

revealed_challenge_geometry_success_1d <- function(role, margin_inches) {
  role <- as.character(role)
  margin <- suppressWarnings(as.numeric(margin_inches))
  if (length(role) != length(margin) || anyNA(role) ||
      any(!role %in% c("offense", "defense")) || anyNA(margin) ||
      any(!is.finite(margin))) {
    stop("Role and signed margin must be finite, aligned offense/defense rows",
      call. = FALSE
    )
  }
  # The rounded ABS boundary belongs to the strike region.  Therefore a called
  # strike at M=0 is correct for offense, while a called ball at M=0 is wrong
  # for defense.  The distinction has zero mass in a continuous model but is
  # retained for exact-geometry tests and official reconciliation.
  ifelse(role == "offense", margin > 0, margin >= 0)
}

.revealed_challenge_weighted_cdf_diagnostics_1d <- function(
  margin, observed_weight, predicted_weight
) {
  observed_total <- sum(observed_weight)
  predicted_total <- sum(predicted_weight)
  if (observed_total <= 0 || predicted_total <= 0) {
    return(c(
      selected_margin_cdf_ks = NA_real_,
      selected_margin_wasserstein_inches = NA_real_
    ))
  }
  x <- data.table::data.table(
    margin = margin,
    observed = observed_weight,
    predicted = predicted_weight
  )[, .(
    observed = sum(observed), predicted = sum(predicted)
  ), by = margin][order(margin)]
  observed_cdf <- cumsum(x$observed) / observed_total
  predicted_cdf <- cumsum(x$predicted) / predicted_total
  difference <- abs(observed_cdf - predicted_cdf)
  wasserstein <- if (nrow(x) > 1L) {
    sum(difference[-nrow(x)] * diff(x$margin))
  } else {
    0
  }
  c(
    selected_margin_cdf_ks = max(difference),
    selected_margin_wasserstein_inches = wasserstein
  )
}

.revealed_challenge_selection_summary_1d <- function(x) {
  observed_attempts <- sum(x$challenged)
  predicted_attempts <- sum(x$challenge_probability)
  observed_successes <- sum(x$challenged * x$geometry_success)
  predicted_successes <- sum(x$challenge_probability * x$geometry_success)
  distribution <- .revealed_challenge_weighted_cdf_diagnostics_1d(
    x$role_margin_inches, x$challenged, x$challenge_probability
  )
  data.table::data.table(
    opportunities = nrow(x),
    observed_challenges = observed_attempts,
    predicted_challenges = predicted_attempts,
    observed_challenge_rate = observed_attempts / nrow(x),
    predicted_challenge_rate = predicted_attempts / nrow(x),
    observed_geometry_successes = observed_successes,
    predicted_geometry_successes = predicted_successes,
    observed_selected_success_rate = if (observed_attempts > 0) {
      observed_successes / observed_attempts
    } else {
      NA_real_
    },
    predicted_selected_success_rate = if (predicted_attempts > 0) {
      predicted_successes / predicted_attempts
    } else {
      NA_real_
    },
    selected_success_calibration_error = if (observed_attempts > 0 &&
        predicted_attempts > 0) {
      observed_successes / observed_attempts -
        predicted_successes / predicted_attempts
    } else {
      NA_real_
    },
    observed_selected_margin_mean = if (observed_attempts > 0) {
      sum(x$challenged * x$role_margin_inches) / observed_attempts
    } else {
      NA_real_
    },
    predicted_selected_margin_mean = if (predicted_attempts > 0) {
      sum(x$challenge_probability * x$role_margin_inches) /
        predicted_attempts
    } else {
      NA_real_
    },
    selected_margin_cdf_ks = distribution[["selected_margin_cdf_ks"]],
    selected_margin_wasserstein_inches =
      distribution[["selected_margin_wasserstein_inches"]],
    propensity_weighted_truth_numerator = predicted_successes,
    propensity_weighted_truth_denominator = predicted_attempts,
    propensity_weighted_truth_identity_error = if (predicted_attempts > 0) {
      predicted_successes / predicted_attempts -
        sum(x$challenge_probability * x$geometry_success) /
          sum(x$challenge_probability)
    } else {
      NA_real_
    }
  )
}

.revealed_challenge_aggregation_specs_1d <- function() {
  list(
    overall = character(),
    role = "role",
    count = "count_state",
    tail = "margin_domain",
    role_count = c("role", "count_state"),
    role_tail = c("role", "margin_domain"),
    role_count_tail = c("role", "count_state", "margin_domain")
  )
}

.revealed_challenge_aggregate_1d <- function(x, summary_function) {
  specs <- .revealed_challenge_aggregation_specs_1d()
  pieces <- lapply(names(specs), function(name) {
    columns <- specs[[name]]
    value <- if (length(columns)) {
      x[, summary_function(.SD), by = columns]
    } else {
      summary_function(x)
    }
    value[, aggregation := name]
    for (column in c("role", "count_state", "margin_domain")) {
      if (!column %in% names(value)) value[, (column) := "all"]
    }
    data.table::setcolorder(
      value, c("aggregation", "role", "count_state", "margin_domain")
    )
    value
  })
  data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
}

normalize_revealed_challenge_oof_rows_1d <- function(
  rows,
  action_column = "challenged",
  probability_column = "challenge_probability",
  margin_column = "role_margin_inches"
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  assert_revealed_challenge_propensity_outcome_free_1d(
    x, "revealed challenge OOF rows"
  )
  required <- c(
    "game_pk", "pitch_order", "fold", "role", "count_state",
    action_column, probability_column, margin_column
  )
  stop_if_missing_columns(x, required, "revealed challenge OOF rows")
  x[, `:=`(
    game_pk = as.character(game_pk),
    pitch_order = as.integer(pitch_order),
    fold = as.integer(fold),
    role = as.character(role),
    count_state = as.character(count_state),
    challenged = as.integer(get(action_column)),
    challenge_probability = as.numeric(get(probability_column)),
    role_margin_inches = as.numeric(get(margin_column))
  )]
  if (!nrow(x) || anyNA(x[, .(
    game_pk, pitch_order, fold, role, count_state, challenged,
    challenge_probability, role_margin_inches
  )]) || any(!nzchar(x$game_pk)) || any(x$fold < 1L) ||
      any(!x$role %in% c("offense", "defense")) ||
      any(!x$challenged %in% 0:1) ||
      any(!is.finite(x$challenge_probability)) ||
      any(x$challenge_probability < 0 | x$challenge_probability > 1) ||
      any(!is.finite(x$role_margin_inches)) ||
      anyDuplicated(x[, .(role, game_pk, pitch_order)])) {
    stop("Revealed challenge OOF rows are invalid", call. = FALSE)
  }
  game_folds <- x[, data.table::uniqueN(fold), by = game_pk]
  if (any(game_folds$V1 != 1L)) {
    stop("Every held-out game must belong to exactly one OOF fold",
      call. = FALSE
    )
  }
  if ("inventory_before" %in% names(x)) {
    inventory <- suppressWarnings(as.integer(x$inventory_before))
    if (anyNA(inventory) || any(inventory < 1L)) {
      stop(
        "Selection likelihood rows must have positive observed inventory",
        call. = FALSE
      )
    }
  }
  x[, `:=`(
    row_id = paste(role, game_pk, pitch_order, sep = ":"),
    geometry_success = revealed_challenge_geometry_success_1d(
      role, role_margin_inches
    )
  )]
  x[]
}

.revealed_challenge_official_summary_1d <- function(x) {
  labeled <- x$challenged == 1L & !is.na(x$official_success)
  attempts <- sum(labeled)
  official_successes <- sum(x$official_success[labeled])
  geometry_successes <- sum(x$geometry_success[labeled])
  mismatches <- sum(
    x$official_success[labeled] != x$geometry_success[labeled]
  )
  data.table::data.table(
    official_labeled_challenges = attempts,
    official_successes = official_successes,
    official_selected_success_rate = if (attempts > 0) {
      official_successes / attempts
    } else {
      NA_real_
    },
    geometry_successes_on_official_rows = geometry_successes,
    official_geometry_mismatches = mismatches,
    official_geometry_mismatch_rate = if (attempts > 0) {
      mismatches / attempts
    } else {
      NA_real_
    }
  )
}

evaluate_revealed_challenge_margin_selection_1d <- function(
  oof_rows,
  official_labels = NULL,
  tail_cutoff_inches = 3,
  action_column = "challenged",
  probability_column = "challenge_probability",
  margin_column = "role_margin_inches"
) {
  cutoff <- .revealed_challenge_scalar_number_1d(
    tail_cutoff_inches, "tail_cutoff_inches", lower = 0,
    upper = Inf, lower_open = TRUE
  )
  scored <- normalize_revealed_challenge_oof_rows_1d(
    oof_rows,
    action_column = action_column,
    probability_column = probability_column,
    margin_column = margin_column
  )
  scored[, margin_domain := ifelse(
    abs(role_margin_inches) <= cutoff,
    paste0("local_abs_margin_le_", format(cutoff, trim = TRUE)),
    paste0("tail_abs_margin_gt_", format(cutoff, trim = TRUE))
  )]
  diagnostics <- .revealed_challenge_aggregate_1d(
    scored, .revealed_challenge_selection_summary_1d
  )
  if (any(abs(diagnostics$propensity_weighted_truth_identity_error) > 1e-12,
    na.rm = TRUE
  )) {
    stop("Propensity-weighted truth identity failed", call. = FALSE)
  }

  official_diagnostics <- data.table::data.table()
  if (!is.null(official_labels)) {
    labels <- data.table::copy(data.table::as.data.table(official_labels))
    allowed <- c("game_pk", "pitch_order", "role", "official_success")
    stop_if_missing_columns(
      labels, allowed, "official selected-margin labels"
    )
    unexpected <- setdiff(names(labels), allowed)
    if (length(unexpected)) {
      stop(
        "Official labels must use the evaluation-only allowlist; unexpected: ",
        paste(sort(unexpected), collapse = ", "),
        call. = FALSE
      )
    }
    labels[, `:=`(
      game_pk = as.character(game_pk),
      pitch_order = as.integer(pitch_order),
      role = as.character(role),
      official_success = as.logical(official_success)
    )]
    if (anyNA(labels[, .(game_pk, pitch_order, role)]) ||
        any(!labels$role %in% c("offense", "defense")) ||
        anyDuplicated(labels[, .(role, game_pk, pitch_order)])) {
      stop("Official selected-margin labels have invalid keys",
        call. = FALSE
      )
    }
    scored <- merge(
      scored, labels,
      by = c("role", "game_pk", "pitch_order"),
      all.x = TRUE, sort = FALSE
    )
    official_diagnostics <- .revealed_challenge_aggregate_1d(
      scored, .revealed_challenge_official_summary_1d
    )
  }
  result <- list(
    diagnostics = diagnostics[],
    official_diagnostics = official_diagnostics[],
    scored_rows = scored[],
    tail_cutoff_inches = cutoff,
    information_regime = paste(
      "OOF challenge propensities scored against deterministic signed-margin",
      "truth; official outcomes are evaluation-only"
    ),
    propensity_fit_outcome_columns = character(),
    official_columns_used_for_fit = character()
  )
  class(result) <- "revealed_challenge_margin_evaluation_1d"
  result
}
