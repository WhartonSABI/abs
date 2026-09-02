# Fast, outcome-free common discrimination-width estimates for the two sides
# of the challenge decision.  Each outer-fold estimate is trained only on the
# other games.  The probit margin coefficient is common within a role/fold,
# so sigma = 1 / beta_margin remains in physical inches.

joint_common_width_1d_roles <- function() c("offense", "defense")

joint_common_width_1d_default_context <- function() {
  challenge_discrimination_1d_default_context()
}

joint_common_width_1d_outcome_columns <- function() {
  unique(c(
    challenge_discrimination_1d_outcome_columns(),
    defense_challenge_discrimination_1d_outcome_columns()
  ))
}

assert_joint_common_width_1d_outcome_free <- function(rows, label) {
  leaked <- intersect(
    names(data.table::as.data.table(rows)),
    joint_common_width_1d_outcome_columns()
  )
  if (length(leaked)) {
    stop(
      label, " contains outcome/evaluation columns: ",
      paste(sort(leaked), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.joint_common_width_1d_validate_scalar <- function(
  value, name, lower = -Inf, integer = FALSE
) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (
    length(numeric_value) != 1L || !is.finite(numeric_value) ||
      numeric_value <= lower ||
      (isTRUE(integer) && numeric_value != as.integer(numeric_value))
  ) {
    kind <- if (isTRUE(integer)) "integer" else "number"
    stop(name, " must be one finite ", kind, " greater than ", lower)
  }
  if (isTRUE(integer)) as.integer(numeric_value) else numeric_value
}

normalize_joint_common_width_offense_1d <- function(
  rows,
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon()
) {
  assert_joint_common_width_1d_outcome_free(rows, "Offense width input")
  x <- normalize_challenge_discrimination_1d_rows(
    rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  x[, `:=`(
    role = "offense",
    decision_unit_id = as.character(batter_id),
    team_context_id = as.character(bat_team_id),
    role_margin_inches = as.numeric(margin_inches)
  )]
  x[]
}

normalize_joint_common_width_defense_1d <- function(
  rows,
  stake_log_odds_epsilon = defense_challenge_discrimination_1d_stake_epsilon()
) {
  assert_joint_common_width_1d_outcome_free(rows, "Defense width input")
  x <- normalize_defense_challenge_discrimination_1d_rows(
    rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  x[, `:=`(
    role = "defense",
    decision_unit_id = as.character(defensive_team_id),
    team_context_id = as.character(opponent_team_id),
    role_margin_inches = as.numeric(defense_margin_inches)
  )]
  x[]
}

normalize_joint_common_width_fold_assignment_1d <- function(
  fold_assignment, required_games
) {
  folds <- data.table::copy(data.table::as.data.table(fold_assignment))
  stop_if_missing_columns(
    folds, c("game_pk", "fold"), "joint common-width fold assignment"
  )
  folds <- folds[, .(game_pk, fold)]
  folds[, game_pk := as.character(game_pk)]
  numeric_fold <- suppressWarnings(as.numeric(folds$fold))
  integer_fold <- suppressWarnings(as.integer(numeric_fold))
  if (
    !nrow(folds) || anyNA(folds) || any(!nzchar(folds$game_pk)) ||
      any(!is.finite(numeric_fold)) || any(numeric_fold != integer_fold) ||
      any(integer_fold < 1L) || anyDuplicated(folds$game_pk)
  ) {
    stop(
      "Shared fold_assignment must assign each game once to a positive integer fold",
      call. = FALSE
    )
  }
  folds[, fold := integer_fold]
  fold_ids <- sort(unique(folds$fold))
  if (length(fold_ids) < 2L || !identical(fold_ids, seq_len(max(fold_ids)))) {
    stop("Shared fold_assignment must contain contiguous folds 1,...,K")
  }
  required_games <- sort(unique(as.character(required_games)))
  missing <- setdiff(required_games, folds$game_pk)
  if (length(missing)) {
    stop(
      "Shared fold_assignment omits eligible decision games: ",
      paste(utils::head(missing, 5L), collapse = ", "),
      call. = FALSE
    )
  }
  data.table::setorder(folds, game_pk)
  folds[]
}

assert_joint_common_width_game_separation_1d <- function(
  training_rows, heldout_rows, role = NULL
) {
  training <- data.table::as.data.table(training_rows)
  heldout <- data.table::as.data.table(heldout_rows)
  stop_if_missing_columns(training, "game_pk", "common-width training rows")
  stop_if_missing_columns(heldout, "game_pk", "common-width held-out rows")
  overlap <- intersect(
    unique(as.character(training$game_pk)),
    unique(as.character(heldout$game_pk))
  )
  if (length(overlap)) {
    prefix <- if (is.null(role)) "Common-width" else paste(role, "common-width")
    stop(
      prefix, " training and held-out games overlap: ",
      paste(utils::head(overlap, 5L), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.joint_common_width_1d_context_specification <- function(
  rows, categorical_context, numeric_context
) {
  columns <- validate_challenge_discrimination_1d_context(
    rows, categorical_context, numeric_context
  )
  design <- fit_continuous_design(
    rows, categorical = columns$categorical, numeric = columns$numeric
  )
  keep <- if (!ncol(design$matrix)) {
    logical()
  } else {
    apply(design$matrix, 2L, function(value) {
      all(is.finite(value)) && stats::sd(value) > 1e-10
    })
  }
  list(
    specification = design$specification,
    matrix = design$matrix[, keep, drop = FALSE],
    included_columns = colnames(design$matrix)[keep],
    dropped_columns = colnames(design$matrix)[!keep],
    context_columns = columns
  )
}

.joint_common_width_1d_model_data <- function(
  rows, context_matrix, group_levels = NULL
) {
  x <- data.table::as.data.table(rows)
  group_columns <- c(
    "decision_unit_id", "team_context_id", "umpire_id", "catcher_id"
  )
  stop_if_missing_columns(
    x,
    c("challenged", "role_margin_inches", group_columns),
    "joint common-width model rows"
  )
  out <- data.frame(
    challenged = as.integer(x$challenged),
    margin_inches = as.numeric(x$role_margin_inches),
    stringsAsFactors = FALSE
  )
  if (ncol(context_matrix)) {
    out <- cbind(out, as.data.frame(context_matrix, check.names = FALSE))
  }
  levels_out <- list()
  unseen <- list()
  for (column in group_columns) {
    value <- as.character(x[[column]])
    value[is.na(value) | !nzchar(value)] <- "__UNKNOWN__"
    levels <- if (is.null(group_levels)) {
      sort(unique(value))
    } else {
      group_levels[[column]]
    }
    if (!length(levels)) stop("Empty training levels for ", column)
    is_unseen <- !value %in% levels
    value[is_unseen] <- levels[[1L]]
    out[[column]] <- factor(value, levels = levels)
    levels_out[[column]] <- levels
    unseen[[column]] <- is_unseen
  }
  list(data = out, group_levels = levels_out, unseen = unseen)
}

.joint_common_width_1d_formula <- function(
  context_columns, group_levels
) {
  fixed <- c("margin_inches", context_columns)
  random_groups <- names(group_levels)[vapply(group_levels, length, integer(1)) > 1L]
  random <- sprintf("s(%s, bs = 're')", random_groups)
  rhs <- c(fixed, random)
  formula <- stats::as.formula(paste("challenged ~", paste(rhs, collapse = " + ")))
  list(
    formula = formula,
    formula_text = paste(deparse(formula, width.cutoff = 500L), collapse = " "),
    random_groups = random_groups,
    dropped_random_groups = setdiff(names(group_levels), random_groups)
  )
}

.joint_common_width_1d_predict <- function(
  fit, heldout_rows, context_specification, context_columns, group_levels,
  random_groups
) {
  heldout_context <- score_continuous_design(
    heldout_rows, context_specification
  )
  heldout_context <- heldout_context[, context_columns, drop = FALSE]
  model_data <- .joint_common_width_1d_model_data(
    heldout_rows, heldout_context, group_levels
  )
  terms <- stats::predict(fit, newdata = model_data$data, type = "terms")
  if (is.null(dim(terms))) terms <- matrix(terms, nrow = nrow(model_data$data))
  eta <- rep(unname(stats::coef(fit)[["(Intercept)"]]), nrow(model_data$data))
  if (ncol(terms)) {
    for (column in random_groups) {
      label <- paste0("s(", column, ")")
      if (label %in% colnames(terms)) {
        terms[model_data$unseen[[column]], label] <- 0
      }
    }
    eta <- eta + rowSums(terms)
  }
  probability <- stats::pnorm(eta)
  seen <- lapply(model_data$unseen, function(value) !value)
  list(
    probability = probability,
    linear_predictor = eta,
    seen = seen,
    unseen_counts = vapply(model_data$unseen, sum, integer(1))
  )
}

.joint_common_width_1d_threshold_summary <- function(
  fit, slope, confidence_level
) {
  intercept <- unname(stats::coef(fit)[["(Intercept)"]])
  threshold <- -intercept / slope
  covariance <- stats::vcov(fit, unconditional = TRUE)
  if (!all(c("(Intercept)", "margin_inches") %in% rownames(covariance))) {
    return(c(
      threshold_inches = threshold,
      threshold_std_error = NA_real_,
      threshold_lower = NA_real_,
      threshold_upper = NA_real_
    ))
  }
  gradient <- c(-1 / slope, intercept / slope^2)
  block <- covariance[
    c("(Intercept)", "margin_inches"),
    c("(Intercept)", "margin_inches"),
    drop = FALSE
  ]
  variance <- drop(t(gradient) %*% block %*% gradient)
  standard_error <- if (is.finite(variance) && variance >= 0) sqrt(variance) else NA_real_
  z <- stats::qnorm(1 - (1 - confidence_level) / 2)
  c(
    threshold_inches = threshold,
    threshold_std_error = standard_error,
    threshold_lower = threshold - z * standard_error,
    threshold_upper = threshold + z * standard_error
  )
}

fit_joint_common_width_role_fold_1d <- function(
  training_rows, heldout_rows, role,
  categorical_context = joint_common_width_1d_default_context()$categorical,
  numeric_context = joint_common_width_1d_default_context()$numeric,
  margin_limit_inches = 3, confidence_level = 0.95,
  minimum_training_rows = 100L, minimum_training_challenges = 5L,
  nthreads = 1L
) {
  role <- match.arg(role, joint_common_width_1d_roles())
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Fast common-width fitting requires the mgcv package")
  }
  limit <- validate_challenge_discrimination_1d_margin_limit(
    margin_limit_inches
  )
  if (!is.finite(limit)) stop("Fast common-width fitting requires a finite margin limit")
  confidence_level <- .joint_common_width_1d_validate_scalar(
    confidence_level, "confidence_level", 0
  )
  if (confidence_level >= 1) stop("confidence_level must be below one")
  minimum_training_rows <- .joint_common_width_1d_validate_scalar(
    minimum_training_rows, "minimum_training_rows", 1, integer = TRUE
  )
  minimum_training_challenges <- .joint_common_width_1d_validate_scalar(
    minimum_training_challenges, "minimum_training_challenges", 0,
    integer = TRUE
  )
  nthreads <- .joint_common_width_1d_validate_scalar(
    nthreads, "nthreads", 0, integer = TRUE
  )
  training <- data.table::copy(data.table::as.data.table(training_rows))
  heldout <- data.table::copy(data.table::as.data.table(heldout_rows))
  assert_joint_common_width_1d_outcome_free(training, paste(role, "training"))
  assert_joint_common_width_1d_outcome_free(heldout, paste(role, "heldout"))
  assert_joint_common_width_game_separation_1d(training, heldout, role)
  stop_if_missing_columns(
    training,
    c(
      "game_pk", "pitch_order", "challenged", "role_margin_inches",
      "decision_unit_id", "team_context_id", "umpire_id", "catcher_id"
    ),
    paste(role, "common-width training rows")
  )
  stop_if_missing_columns(
    heldout,
    c(
      "game_pk", "pitch_order", "challenged", "role_margin_inches",
      "decision_unit_id", "team_context_id", "umpire_id", "catcher_id"
    ),
    paste(role, "common-width held-out rows")
  )
  local_training <- training[abs(role_margin_inches) <= limit]
  local_heldout <- heldout[abs(role_margin_inches) <= limit]
  if (nrow(local_training) < minimum_training_rows) {
    stop(
      role, " fold has only ", nrow(local_training),
      " local training rows; need at least ", minimum_training_rows
    )
  }
  challenges <- sum(local_training$challenged)
  passes <- nrow(local_training) - challenges
  if (challenges < minimum_training_challenges || passes < 1L) {
    stop(
      role, " fold lacks enough local challenge/pass variation: ",
      challenges, " challenges and ", passes, " passes"
    )
  }
  if (stats::sd(local_training$role_margin_inches) < 1e-8) {
    stop(role, " fold has no usable local margin variation")
  }

  context <- .joint_common_width_1d_context_specification(
    local_training, categorical_context, numeric_context
  )
  model_data <- .joint_common_width_1d_model_data(
    local_training, context$matrix
  )
  formula <- .joint_common_width_1d_formula(
    context$included_columns, model_data$group_levels
  )
  warning_messages <- character()
  start_time <- proc.time()[["elapsed"]]
  fit <- withCallingHandlers(
    mgcv::bam(
      formula$formula,
      data = model_data$data,
      family = stats::binomial(link = "probit"),
      method = "fREML", discrete = TRUE,
      nthreads = nthreads, gc.level = 1L
    ),
    warning = function(condition) {
      warning_messages <<- unique(c(warning_messages, conditionMessage(condition)))
      invokeRestart("muffleWarning")
    }
  )
  elapsed <- proc.time()[["elapsed"]] - start_time
  model_summary <- summary(fit)
  coefficient_table <- model_summary$p.table
  if (!"margin_inches" %in% rownames(coefficient_table)) {
    stop(role, " fold fit did not retain the common margin coefficient")
  }
  slope <- unname(coefficient_table["margin_inches", "Estimate"])
  slope_se <- unname(coefficient_table["margin_inches", "Std. Error"])
  if (!is.finite(slope) || slope <= 0 || !is.finite(slope_se) || slope_se < 0) {
    stop(
      role, " fold did not identify a positive finite common margin slope",
      call. = FALSE
    )
  }
  z <- stats::qnorm(1 - (1 - confidence_level) / 2)
  slope_lower <- slope - z * slope_se
  slope_upper <- slope + z * slope_se
  sigma <- 1 / slope
  sigma_lower <- if (slope_upper > 0) 1 / slope_upper else NA_real_
  sigma_upper <- if (slope_lower > 0) 1 / slope_lower else Inf
  threshold <- .joint_common_width_1d_threshold_summary(
    fit, slope, confidence_level
  )

  prediction <- if (nrow(local_heldout)) {
    .joint_common_width_1d_predict(
      fit, local_heldout,
      context$specification, context$included_columns,
      model_data$group_levels, formula$random_groups
    )
  } else {
    list(
      probability = numeric(), linear_predictor = numeric(),
      seen = setNames(
        replicate(4L, logical(), simplify = FALSE),
        c("decision_unit_id", "team_context_id", "umpire_id", "catcher_id")
      ),
      unseen_counts = setNames(
        integer(4L),
        c("decision_unit_id", "team_context_id", "umpire_id", "catcher_id")
      )
    )
  }
  probability <- pmin(1 - 1e-12, pmax(1e-12, prediction$probability))
  log_loss <- if (length(probability)) {
    -mean(
      local_heldout$challenged * log(probability) +
        (1 - local_heldout$challenged) * log1p(-probability)
    )
  } else {
    NA_real_
  }
  brier <- if (length(probability)) {
    mean((local_heldout$challenged - probability)^2)
  } else {
    NA_real_
  }
  smooth_edf <- if (nrow(model_summary$s.table)) {
    sum(model_summary$s.table[, "edf"])
  } else {
    0
  }
  convergence <- if (!is.null(fit$outer.info$conv)) {
    as.character(fit$outer.info$conv)
  } else if (isTRUE(fit$converged)) {
    "converged"
  } else {
    "not_reported"
  }

  estimates <- data.table::data.table(
    role = role,
    sigma_inches = sigma,
    sigma_lower = sigma_lower,
    sigma_upper = sigma_upper,
    confidence_level = confidence_level,
    slope_per_inch = slope,
    slope_std_error = slope_se,
    slope_lower = slope_lower,
    slope_upper = slope_upper,
    slope_z = slope / slope_se,
    slope_p_value = unname(coefficient_table["margin_inches", "Pr(>|z|)"]),
    threshold_inches = unname(threshold[["threshold_inches"]]),
    threshold_std_error = unname(threshold[["threshold_std_error"]]),
    threshold_lower = unname(threshold[["threshold_lower"]]),
    threshold_upper = unname(threshold[["threshold_upper"]]),
    sigma_ci_status = if (is.finite(sigma_upper)) {
      "finite_positive_slope_interval"
    } else {
      "slope_interval_reaches_zero"
    }
  )
  diagnostics <- data.table::data.table(
    role = role,
    margin_limit_inches = limit,
    training_rows_all_margins = nrow(training),
    training_rows_local = nrow(local_training),
    training_tail_rows = nrow(training) - nrow(local_training),
    training_games = data.table::uniqueN(training$game_pk),
    training_challenges_local = challenges,
    training_challenge_rate_local = mean(local_training$challenged),
    heldout_rows_all_margins = nrow(heldout),
    heldout_rows_local = nrow(local_heldout),
    heldout_tail_rows = nrow(heldout) - nrow(local_heldout),
    heldout_games = data.table::uniqueN(heldout$game_pk),
    heldout_challenges_local = sum(local_heldout$challenged),
    heldout_challenge_rate_local = if (nrow(local_heldout)) {
      mean(local_heldout$challenged)
    } else {
      NA_real_
    },
    heldout_log_loss = log_loss,
    heldout_brier_score = brier,
    decision_units = length(model_data$group_levels$decision_unit_id),
    team_context_units = length(model_data$group_levels$team_context_id),
    umpires = length(model_data$group_levels$umpire_id),
    catchers = length(model_data$group_levels$catcher_id),
    unseen_heldout_decision_units = prediction$unseen_counts[["decision_unit_id"]],
    unseen_heldout_team_context_units = prediction$unseen_counts[["team_context_id"]],
    unseen_heldout_umpires = prediction$unseen_counts[["umpire_id"]],
    unseen_heldout_catchers = prediction$unseen_counts[["catcher_id"]],
    parametric_coefficients = nrow(coefficient_table),
    smooth_edf = smooth_edf,
    deviance_explained = model_summary$dev.expl,
    model_rank = fit$rank,
    coefficient_count = length(stats::coef(fit)),
    converged = isTRUE(fit$converged),
    convergence_message = convergence,
    boundary = isTRUE(fit$boundary),
    warning_count = length(warning_messages),
    warning_messages = paste(warning_messages, collapse = " | "),
    elapsed_seconds = elapsed,
    formula = formula$formula_text,
    included_context_columns = paste(context$included_columns, collapse = ","),
    dropped_context_columns = paste(context$dropped_columns, collapse = ","),
    included_random_effects = paste(formula$random_groups, collapse = ","),
    dropped_random_effects = paste(formula$dropped_random_groups, collapse = ","),
    oof_random_effect_fallback = "zero_population_effect_for_unseen_level"
  )
  oof <- data.table::data.table(
    game_pk = as.character(local_heldout$game_pk),
    pitch_order = local_heldout$pitch_order,
    role = role,
    challenged = as.integer(local_heldout$challenged),
    margin_inches = as.numeric(local_heldout$role_margin_inches),
    challenge_probability = probability,
    linear_predictor = prediction$linear_predictor,
    decision_unit_seen_in_training = prediction$seen$decision_unit_id,
    team_context_seen_in_training = prediction$seen$team_context_id,
    umpire_seen_in_training = prediction$seen$umpire_id,
    catcher_seen_in_training = prediction$seen$catcher_id
  )
  list(
    estimate = estimates,
    diagnostics = diagnostics,
    oof_predictions = oof,
    model = fit,
    context_specification = context$specification,
    group_levels = model_data$group_levels,
    formula = formula$formula
  )
}

.joint_common_width_1d_wide_estimates <- function(estimates) {
  columns <- c(
    "sigma_inches", "sigma_lower", "sigma_upper", "slope_per_inch",
    "slope_std_error", "threshold_inches"
  )
  parts <- lapply(joint_common_width_1d_roles(), function(role_name) {
    part <- data.table::copy(estimates[role == role_name, c("fold", columns), with = FALSE])
    data.table::setnames(
      part, columns, paste0(role_name, "_", columns)
    )
    part
  })
  Reduce(
    function(left, right) merge(left, right, by = "fold", all = TRUE),
    parts
  )[order(fold)]
}

joint_common_width_1d_fold_sigma <- function(result) {
  if (!inherits(result, "joint_common_width_1d_crossfit")) {
    stop("result must be a joint_common_width_1d_crossfit")
  }
  estimates <- data.table::as.data.table(result$estimates)
  folds <- sort(unique(estimates$fold))
  out <- lapply(folds, function(fold_id) {
    value <- estimates[fold == fold_id]
    value <- value[match(joint_common_width_1d_roles(), role)]
    if (anyNA(value$sigma_inches) || any(value$sigma_inches <= 0)) {
      stop("Fold ", fold_id, " lacks positive offense/defense widths")
    }
    stats::setNames(value$sigma_inches, value$role)
  })
  names(out) <- paste0("fold_", folds)
  out
}

crossfit_joint_common_width_1d <- function(
  offense_rows, defense_rows, fold_assignment,
  categorical_context = joint_common_width_1d_default_context()$categorical,
  numeric_context = joint_common_width_1d_default_context()$numeric,
  margin_limit_inches = 3, confidence_level = 0.95,
  minimum_training_rows = 100L, minimum_training_challenges = 5L,
  nthreads = 1L, keep_models = FALSE, progress = interactive(),
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon()
) {
  offense <- normalize_joint_common_width_offense_1d(
    offense_rows, stake_log_odds_epsilon
  )
  defense <- normalize_joint_common_width_defense_1d(
    defense_rows, stake_log_odds_epsilon
  )
  all_games <- union(offense$game_pk, defense$game_pk)
  folds <- normalize_joint_common_width_fold_assignment_1d(
    fold_assignment, all_games
  )
  offense[, fold := folds$fold[match(game_pk, folds$game_pk)]]
  defense[, fold := folds$fold[match(game_pk, folds$game_pk)]]
  if (anyNA(offense$fold) || anyNA(defense$fold)) {
    stop("Internal shared-fold alignment failed")
  }
  fold_ids <- sort(unique(folds$fold))
  estimates <- diagnostics <- predictions <- list()
  models <- if (isTRUE(keep_models)) vector("list", length(fold_ids)) else NULL
  result_index <- 0L
  for (fold_id in fold_ids) {
    if (isTRUE(progress)) {
      message("common-width outer fold ", fold_id, "/", length(fold_ids))
    }
    if (isTRUE(keep_models)) models[[fold_id]] <- list()
    for (role_name in joint_common_width_1d_roles()) {
      source <- if (role_name == "offense") offense else defense
      training <- source[fold != fold_id]
      heldout <- source[fold == fold_id]
      assert_joint_common_width_game_separation_1d(
        training, heldout, role_name
      )
      fitted <- fit_joint_common_width_role_fold_1d(
        training, heldout, role = role_name,
        categorical_context = categorical_context,
        numeric_context = numeric_context,
        margin_limit_inches = margin_limit_inches,
        confidence_level = confidence_level,
        minimum_training_rows = minimum_training_rows,
        minimum_training_challenges = minimum_training_challenges,
        nthreads = nthreads
      )
      result_index <- result_index + 1L
      fitted$estimate[, fold := fold_id]
      fitted$diagnostics[, fold := fold_id]
      fitted$oof_predictions[, fold := fold_id]
      estimates[[result_index]] <- fitted$estimate
      diagnostics[[result_index]] <- fitted$diagnostics
      predictions[[result_index]] <- fitted$oof_predictions
      if (isTRUE(keep_models)) models[[fold_id]][[role_name]] <- fitted$model
    }
  }
  estimate_table <- data.table::rbindlist(estimates, use.names = TRUE)
  diagnostic_table <- data.table::rbindlist(diagnostics, use.names = TRUE)
  prediction_table <- data.table::rbindlist(predictions, use.names = TRUE)
  data.table::setcolorder(estimate_table, c("fold", "role"))
  data.table::setcolorder(diagnostic_table, c("fold", "role"))
  data.table::setcolorder(
    prediction_table,
    c("fold", "role", "game_pk", "pitch_order")
  )
  data.table::setorder(estimate_table, fold, role)
  data.table::setorder(diagnostic_table, fold, role)
  data.table::setorder(prediction_table, fold, role, game_pk, pitch_order)

  result <- list(
    estimates = estimate_table[],
    fold_widths = .joint_common_width_1d_wide_estimates(estimate_table),
    diagnostics = diagnostic_table[],
    oof_predictions = prediction_table[],
    fold_assignment = folds[],
    margin_limit_inches = margin_limit_inches,
    confidence_level = confidence_level,
    context = list(
      categorical = categorical_context,
      numeric = numeric_context
    ),
    information_set = c(
      "challenge_or_pass", "exact_oriented_margin", "count", "pitch_family",
      "matchup", "decision_stakes", "decision_unit", "team_context",
      "umpire", "catcher"
    ),
    excluded_information = joint_common_width_1d_outcome_columns(),
    models = models,
    software = list(
      R = as.character(getRversion()),
      mgcv = as.character(utils::packageVersion("mgcv"))
    )
  )
  class(result) <- "joint_common_width_1d_crossfit"
  result$fold_perception_sigma <- joint_common_width_1d_fold_sigma(result)
  result
}

# Runner-facing name: one call fits both role-specific widths for every outer
# fold and returns `$fold_perception_sigma` in the format consumed by
# crossfit_joint_challenge_signal_mdp_1d().
fit_fold_joint_common_widths_1d <- function(
  offense_rows, defense_rows, fold_assignment,
  categorical_context = joint_common_width_1d_default_context()$categorical,
  numeric_context = joint_common_width_1d_default_context()$numeric,
  margin_limit_inches = 3, confidence_level = 0.95,
  minimum_training_rows = 100L, minimum_training_challenges = 5L,
  nthreads = 1L, keep_models = FALSE, progress = interactive(),
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon()
) {
  crossfit_joint_common_width_1d(
    offense_rows = offense_rows,
    defense_rows = defense_rows,
    fold_assignment = fold_assignment,
    categorical_context = categorical_context,
    numeric_context = numeric_context,
    margin_limit_inches = margin_limit_inches,
    confidence_level = confidence_level,
    minimum_training_rows = minimum_training_rows,
    minimum_training_challenges = minimum_training_challenges,
    nthreads = nthreads,
    keep_models = keep_models,
    progress = progress,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
}
