# Continuous one-dimensional prior for batter challenge decisions.
#
# The model is intentionally conditioned on a pitch having been taken and the
# initial call being a called strike.  It therefore represents the distribution
# of exact signed ABS margins in the information set where a batter can decide
# to challenge.  Challenge actions, official results, and ABS labels are not
# admitted to the input table: ball status is the deterministic event margin > 0.

challenge_margin_prior_1d_allowed_columns <- function() {
  c(
    "game_pk", "pitch_order", "at_bat_number", "pitch_number",
    "initial_call", "tracking_available", "edge_distance_inches",
    "balls_before", "strikes_before", "count_state", "pitch_family", "pitch_type",
    "matchup", "stand", "p_throws", "nearest_edge"
  )
}

.challenge_margin_require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      label, " is missing required columns: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(data)
}

.challenge_margin_scalar_id <- function(value, name) {
  if (length(value) != 1L || is.na(value)) {
    stop(name, " must be one non-missing value", call. = FALSE)
  }
  value
}

normalize_challenge_margin_prior_1d_input <- function(pitches) {
  source <- data.table::as.data.table(pitches)
  .challenge_margin_require_columns(
    source,
    c("game_pk", "initial_call", "tracking_available", "edge_distance_inches"),
    "challenge-margin prior input"
  )
  retained <- intersect(challenge_margin_prior_1d_allowed_columns(), names(source))
  x <- data.table::copy(source[, ..retained])
  x[, `:=`(
    game_pk = as.character(game_pk),
    initial_call = as.character(initial_call),
    tracking_available = as.logical(tracking_available),
    edge_distance_inches = as.numeric(edge_distance_inches)
  )]
  if (!"count_state" %in% names(x)) {
    if (all(c("balls_before", "strikes_before") %in% names(x))) {
      balls <- suppressWarnings(as.integer(x$balls_before))
      strikes <- suppressWarnings(as.integer(x$strikes_before))
      valid_count <- balls %in% 0:3 & strikes %in% 0:2
      x[, count_state := data.table::fifelse(
        valid_count, paste0(balls, "-", strikes), "unknown"
      )]
    } else {
      x[, count_state := "unknown"]
    }
  } else {
    x[, count_state := as.character(count_state)]
    x[is.na(count_state) | !nzchar(count_state), count_state := "unknown"]
  }
  x <- x[
    !is.na(game_pk) & nzchar(game_pk) &
      tracking_available %in% TRUE &
      initial_call == "called_strike" &
      is.finite(edge_distance_inches)
  ]
  if (!nrow(x)) {
    stop("No tracked taken called strikes remain for the 1D margin prior",
      call. = FALSE
    )
  }
  if (all(c("game_pk", "pitch_order") %in% names(x)) &&
      anyDuplicated(x[, .(game_pk, pitch_order)])) {
    stop("Challenge-margin prior input contains duplicate pitch keys",
      call. = FALSE
    )
  }
  data.table::setattr(
    x,
    "information_set",
    c(
      "taken_pitch", "initial_called_strike", "tracked_signed_abs_margin",
      "count_state"
    )
  )
  x[]
}

.challenge_margin_prior_fingerprint_1d <- function(rows) {
  x <- data.table::copy(data.table::as.data.table(rows))
  columns <- intersect(
    c(
      "game_pk", "pitch_order", "at_bat_number", "pitch_number",
      "edge_distance_inches", "count_state"
    ),
    names(x)
  )
  .challenge_margin_require_columns(
    x, c("game_pk", "edge_distance_inches", "count_state"),
    "challenge-margin fingerprint input"
  )
  canonical <- x[, ..columns]
  data.table::setorderv(canonical, columns, na.last = TRUE)
  digest::digest(
    list(
      schema = "challenge_margin_prior_1d_training_v1",
      rows = data.table::setDF(canonical)
    ),
    algo = "sha256", serialize = TRUE
  )
}

.challenge_margin_log_sum_exp_rows <- function(log_values) {
  log_values <- as.matrix(log_values)
  maximum <- apply(log_values, 1L, max)
  result <- maximum + log(rowSums(exp(log_values - maximum)))
  bad <- !is.finite(maximum)
  result[bad] <- maximum[bad]
  result
}

.challenge_margin_component_log_density <- function(margin, weight, mean, sd) {
  result <- vapply(seq_along(weight), function(component) {
    log(weight[[component]]) + stats::dnorm(
      margin, mean = mean[[component]], sd = sd[[component]], log = TRUE
    )
  }, numeric(length(margin)))
  if (is.null(dim(result))) {
    result <- matrix(result, nrow = length(margin), ncol = length(weight))
  }
  result
}

.challenge_margin_initialize_gmm <- function(margin, components, minimum_sd) {
  unique_margin <- sort(unique(margin))
  if (length(unique_margin) < components) {
    stop("Signed margins contain fewer unique values than mixture components",
      call. = FALSE
    )
  }
  indices <- pmax(
    1L,
    pmin(
      length(unique_margin),
      as.integer(round(seq(
        1, length(unique_margin), length.out = components + 2L
      )[-c(1L, components + 2L)]))
    )
  )
  mean <- unique_margin[indices]
  cluster <- max.col(
    -vapply(mean, function(center) (margin - center)^2, numeric(length(margin))),
    ties.method = "first"
  )
  global_sd <- stats::sd(margin)
  if (!is.finite(global_sd) || global_sd < minimum_sd) global_sd <- minimum_sd
  sd <- vapply(seq_len(components), function(component) {
    values <- margin[cluster == component]
    value <- if (length(values) > 1L) stats::sd(values) else global_sd
    if (!is.finite(value)) value <- global_sd
    max(minimum_sd, value)
  }, numeric(1L))
  count <- tabulate(cluster, nbins = components)
  weight <- pmax(count, 1) / sum(pmax(count, 1))
  ordering <- order(mean)
  list(weight = weight[ordering], mean = mean[ordering], sd = sd[ordering])
}

fit_challenge_margin_gmm_1d <- function(
  margin,
  components = 3L,
  tolerance = 1e-5,
  max_iterations = 500L,
  minimum_sd = NULL,
  weight_regularization = 1e-3
) {
  margin <- as.numeric(margin)
  margin <- margin[is.finite(margin)]
  components <- as.integer(components)
  max_iterations <- as.integer(max_iterations)
  if (!components %in% c(1L, 3L, 6L)) {
    stop("components must be one of 1, 3, or 6", call. = FALSE)
  }
  if (length(margin) < max(20L, 5L * components)) {
    stop("Too few signed margins to fit the requested mixture", call. = FALSE)
  }
  if (!is.finite(tolerance) || tolerance <= 0 || max_iterations < 1L) {
    stop("EM tolerance and maximum iterations must be positive", call. = FALSE)
  }
  if (!is.finite(weight_regularization) || weight_regularization < 0) {
    stop("weight_regularization must be finite and non-negative", call. = FALSE)
  }
  empirical_sd <- stats::sd(margin)
  if (is.null(minimum_sd)) {
    minimum_sd <- max(0.02, empirical_sd * 1e-3)
  }
  if (!is.finite(minimum_sd) || minimum_sd <= 0) {
    stop("minimum_sd must be one positive number", call. = FALSE)
  }

  if (components == 1L) {
    fitted_sd <- max(minimum_sd, stats::sd(margin) * sqrt((length(margin) - 1) /
      length(margin)))
    result <- list(
      components = 1L,
      weight = 1,
      mean = mean(margin),
      sd = fitted_sd,
      log_likelihood = sum(stats::dnorm(
        margin, mean(margin), fitted_sd, log = TRUE
      )),
      iterations = 1L,
      converged = TRUE,
      convergence_tolerance = tolerance,
      maximum_iterations = max_iterations,
      last_relative_log_likelihood_change = 0,
      minimum_sd = minimum_sd,
      initialization = "closed_form_single_normal",
      training_rows = length(margin)
    )
    class(result) <- "challenge_margin_gmm_1d"
    return(result)
  }

  parameter <- .challenge_margin_initialize_gmm(
    margin, components, minimum_sd
  )
  previous_log_likelihood <- -Inf
  converged <- FALSE
  iterations <- max_iterations
  last_relative_change <- Inf
  for (iteration in seq_len(max_iterations)) {
    log_component <- .challenge_margin_component_log_density(
      margin, parameter$weight, parameter$mean, parameter$sd
    )
    log_density <- .challenge_margin_log_sum_exp_rows(log_component)
    log_likelihood <- sum(log_density)
    responsibility <- exp(log_component - log_density)
    effective_count <- colSums(responsibility)
    weight <- (effective_count + weight_regularization) /
      (length(margin) + components * weight_regularization)
    component_mean <- colSums(responsibility * margin) / effective_count
    component_variance <- vapply(seq_len(components), function(component) {
      sum(
        responsibility[, component] *
          (margin - component_mean[[component]])^2
      ) / effective_count[[component]]
    }, numeric(1L))
    component_sd <- sqrt(pmax(component_variance, minimum_sd^2))
    ordering <- order(component_mean)
    parameter <- list(
      weight = weight[ordering] / sum(weight[ordering]),
      mean = component_mean[ordering],
      sd = component_sd[ordering]
    )
    if (is.finite(previous_log_likelihood)) {
      last_relative_change <- abs(
        log_likelihood - previous_log_likelihood
      ) / (1 + abs(previous_log_likelihood))
      if (last_relative_change <= tolerance) {
        converged <- TRUE
        iterations <- iteration
        break
      }
    }
    previous_log_likelihood <- log_likelihood
  }
  final_log_component <- .challenge_margin_component_log_density(
    margin, parameter$weight, parameter$mean, parameter$sd
  )
  result <- c(
    list(
      components = components,
      log_likelihood = sum(
        .challenge_margin_log_sum_exp_rows(final_log_component)
      ),
      iterations = iterations,
      converged = converged,
      convergence_tolerance = tolerance,
      maximum_iterations = max_iterations,
      last_relative_log_likelihood_change = last_relative_change,
      minimum_sd = minimum_sd,
      initialization = "deterministic_ordered_quantile_partition",
      training_rows = length(margin)
    ),
    parameter
  )
  class(result) <- "challenge_margin_gmm_1d"
  result
}

estimate_challenge_margin_context_weights_1d <- function(
  rows,
  mixture,
  context_column = "count_state",
  prior_strength = 100
) {
  x <- data.table::as.data.table(rows)
  if (!inherits(mixture, "challenge_margin_gmm_1d")) {
    stop("mixture must come from fit_challenge_margin_gmm_1d()",
      call. = FALSE
    )
  }
  .challenge_margin_require_columns(
    x, c("edge_distance_inches", context_column),
    "challenge-margin context-weight input"
  )
  if (length(context_column) != 1L || is.na(context_column) ||
      !identical(context_column, "count_state")) {
    stop("The v1 challenge-margin context must be count_state",
      call. = FALSE
    )
  }
  prior_strength <- as.numeric(prior_strength)
  if (length(prior_strength) != 1L || !is.finite(prior_strength) ||
      prior_strength <= 0) {
    stop("context prior strength must be one positive number",
      call. = FALSE
    )
  }
  context <- as.character(x[[context_column]])
  context[is.na(context) | !nzchar(context)] <- "unknown"
  levels <- sort(unique(context))
  log_component <- .challenge_margin_component_log_density(
    x$edge_distance_inches, mixture$weight, mixture$mean, mixture$sd
  )
  log_density <- .challenge_margin_log_sum_exp_rows(log_component)
  responsibility <- exp(log_component - log_density)
  context_weights <- matrix(
    NA_real_, nrow = length(levels), ncol = mixture$components,
    dimnames = list(levels, paste0("component_", seq_len(mixture$components)))
  )
  unshrunk_weights <- context_weights
  exposure <- integer(length(levels))
  names(exposure) <- levels
  for (level_index in seq_along(levels)) {
    selected <- context == levels[[level_index]]
    exposure[[level_index]] <- sum(selected)
    soft_count <- colSums(responsibility[selected, , drop = FALSE])
    unshrunk_weights[level_index, ] <- soft_count / sum(soft_count)
    context_weights[level_index, ] <- (
      soft_count + prior_strength * mixture$weight
    ) / (exposure[[level_index]] + prior_strength)
  }
  table <- data.table::rbindlist(lapply(seq_along(levels), function(index) {
    data.table::data.table(
      context_column = context_column,
      context_value = levels[[index]],
      context_exposure = exposure[[index]],
      component = seq_len(mixture$components),
      league_weight = mixture$weight,
      unshrunk_weight = unshrunk_weights[index, ],
      context_weight = context_weights[index, ],
      prior_strength = prior_strength
    )
  }))
  list(
    context_column = context_column,
    context_levels = levels,
    context_exposure = exposure,
    context_weights = context_weights,
    context_unshrunk_weights = unshrunk_weights,
    context_weight_table = table,
    context_prior_strength = prior_strength,
    context_weight_method =
      "soft_allocation_dirichlet_posterior_mean_toward_league"
  )
}

.challenge_margin_resolve_context_weights <- function(fit, context, size) {
  league <- matrix(
    rep(fit$weight, each = size), nrow = size, ncol = fit$components
  )
  if (is.null(context)) {
    return(list(
      weight = league,
      context_value = rep("league", size),
      context_seen_in_training = rep(TRUE, size),
      context_fallback = rep(FALSE, size)
    ))
  }
  context <- as.character(context)
  if (length(context) != 1L && length(context) != size) {
    stop("context must be scalar or align with the scored rows", call. = FALSE)
  }
  context <- rep_len(context, size)
  missing_context <- is.na(context) | !nzchar(context)
  context[missing_context] <- "unknown"
  has_context_fit <- !is.null(fit$context_weights) &&
    is.matrix(fit$context_weights) && ncol(fit$context_weights) == fit$components
  matched <- if (has_context_fit) {
    match(context, rownames(fit$context_weights))
  } else {
    rep(NA_integer_, size)
  }
  seen <- !is.na(matched)
  if (any(seen)) {
    league[seen, ] <- fit$context_weights[matched[seen], , drop = FALSE]
  }
  list(
    weight = league,
    context_value = context,
    context_seen_in_training = seen,
    context_fallback = !seen
  )
}

.challenge_margin_validate_fit <- function(fit) {
  if (!inherits(fit, "challenge_margin_prior_1d_fit")) {
    stop("fit must be a challenge_margin_prior_1d_fit", call. = FALSE)
  }
  required <- c("components", "weight", "mean", "sd", "fold_id", "global_draw_id")
  absent <- required[!vapply(required, function(name) !is.null(fit[[name]]), logical(1L))]
  if (length(absent)) {
    stop("Challenge-margin prior fit is missing: ", paste(absent, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(fit$weight) != fit$components || length(fit$mean) != fit$components ||
      length(fit$sd) != fit$components || any(fit$weight <= 0) ||
      abs(sum(fit$weight) - 1) > 1e-8 || any(fit$sd <= 0)) {
    stop("Challenge-margin prior contains invalid mixture parameters",
      call. = FALSE
    )
  }
  if (!is.null(fit$context_weights)) {
    if (!is.matrix(fit$context_weights) ||
        ncol(fit$context_weights) != fit$components ||
        is.null(rownames(fit$context_weights)) ||
        any(!is.finite(fit$context_weights)) ||
        any(fit$context_weights <= 0) ||
        any(abs(rowSums(fit$context_weights) - 1) > 1e-8)) {
      stop("Challenge-margin prior contains invalid contextual weights",
        call. = FALSE
      )
    }
  }
  invisible(fit)
}

fit_challenge_margin_prior_1d <- function(
  pitches,
  components = 3L,
  training_games = NULL,
  fold_id = "full",
  global_draw_id = 1L,
  context_column = "count_state",
  context_prior_strength = 100,
  ...
) {
  fold_id <- .challenge_margin_scalar_id(fold_id, "fold_id")
  global_draw_id <- .challenge_margin_scalar_id(global_draw_id, "global_draw_id")
  x <- normalize_challenge_margin_prior_1d_input(pitches)
  if (!is.null(training_games)) {
    training_games <- unique(as.character(training_games))
    x <- x[game_pk %in% training_games]
    if (!nrow(x)) stop("No requested training games remain", call. = FALSE)
  }
  mixture <- fit_challenge_margin_gmm_1d(
    x$edge_distance_inches, components = components, ...
  )
  context_model <- if (is.null(context_column)) {
    list(
      context_column = NULL,
      context_levels = character(),
      context_exposure = integer(),
      context_weights = NULL,
      context_unshrunk_weights = NULL,
      context_weight_table = data.table::data.table(),
      context_prior_strength = NA_real_,
      context_weight_method = "league_wide_only"
    )
  } else {
    estimate_challenge_margin_context_weights_1d(
      x,
      mixture,
      context_column = context_column,
      prior_strength = context_prior_strength
    )
  }
  mixture$training_rows <- NULL
  fit <- c(
    unclass(mixture),
    context_model,
    list(
      fold_id = fold_id,
      global_draw_id = global_draw_id,
      training_games = sort(unique(x$game_pk)),
      training_rows = nrow(x),
      training_fingerprint = .challenge_margin_prior_fingerprint_1d(x),
      training_fingerprint_algorithm = "sha256",
      conditioning = c("taken", "called_strike"),
      context_weight_status = if (is.null(context_column)) {
        "league_wide_v1"
      } else {
        "count_state_dirichlet_shrinkage_v1"
      },
      information_set = attr(x, "information_set"),
      outcome_action_columns_used = character()
    )
  )
  class(fit) <- "challenge_margin_prior_1d_fit"
  fit
}

challenge_margin_prior_density_1d <- function(
  fit, margin, log = FALSE, context = NULL
) {
  .challenge_margin_validate_fit(fit)
  margin <- as.numeric(margin)
  if (anyNA(margin) || any(!is.finite(margin))) {
    stop("margin must contain only finite values", call. = FALSE)
  }
  resolved <- .challenge_margin_resolve_context_weights(
    fit, context, length(margin)
  )
  log_component <- vapply(seq_len(fit$components), function(component) {
    log(resolved$weight[, component]) + stats::dnorm(
      margin,
      mean = fit$mean[[component]],
      sd = fit$sd[[component]],
      log = TRUE
    )
  }, numeric(length(margin)))
  if (is.null(dim(log_component))) {
    log_component <- matrix(
      log_component, nrow = length(margin), ncol = fit$components
    )
  }
  log_density <- .challenge_margin_log_sum_exp_rows(log_component)
  if (isTRUE(log)) log_density else exp(log_density)
}

score_challenge_margin_prior_1d <- function(
  fit, pitches, require_game_separation = FALSE
) {
  .challenge_margin_validate_fit(fit)
  x <- normalize_challenge_margin_prior_1d_input(pitches)
  overlap <- intersect(fit$training_games, unique(as.character(x$game_pk)))
  if (isTRUE(require_game_separation) && length(overlap)) {
    stop("Challenge-margin prior training and scoring games overlap",
      call. = FALSE
    )
  }
  scoring_fingerprint <- .challenge_margin_prior_fingerprint_1d(x)
  resolved <- .challenge_margin_resolve_context_weights(
    fit, x$count_state, nrow(x)
  )
  x[, `:=`(
    location_log_density = challenge_margin_prior_density_1d(
      fit, edge_distance_inches, log = TRUE, context = count_state
    ),
    prior_context = resolved$context_value,
    context_seen_in_training = resolved$context_seen_in_training,
    context_fallback = resolved$context_fallback,
    prior_components = fit$components,
    fold_id = fit$fold_id,
    global_draw_id = fit$global_draw_id,
    prior_training_fingerprint = if (!is.null(fit$training_fingerprint)) {
      fit$training_fingerprint
    } else {
      NA_character_
    },
    prior_scoring_fingerprint = scoring_fingerprint
  )]
  data.table::setattr(x, "game_separation", list(
    required = isTRUE(require_game_separation),
    overlapping_games = sort(overlap),
    training_fingerprint = fit$training_fingerprint,
    scoring_fingerprint = scoring_fingerprint
  ))
  x[]
}

challenge_margin_game_clustered_log_score_1d <- function(scored) {
  x <- data.table::as.data.table(scored)
  .challenge_margin_require_columns(
    x, c("game_pk", "location_log_density"), "margin-prior scores"
  )
  if (any(!is.finite(x$location_log_density))) {
    stop("All held-out location log densities must be finite", call. = FALSE)
  }
  n <- nrow(x)
  games <- data.table::uniqueN(x$game_pk)
  if (games < 2L) {
    stop("Clustered location scoring requires at least two held-out games",
      call. = FALSE
    )
  }
  estimate <- mean(x$location_log_density)
  cluster <- x[, .(
    residual_sum = sum(location_log_density - estimate)
  ), by = game_pk]
  standard_error <- sqrt(
    games / (games - 1) * sum(cluster$residual_sum^2) / n^2
  )
  data.table::data.table(
    heldout_rows = n,
    heldout_games = games,
    mean_log_score = estimate,
    game_clustered_se = standard_error
  )
}

select_challenge_margin_component_count_1d <- function(metrics) {
  x <- data.table::copy(data.table::as.data.table(metrics))
  .challenge_margin_require_columns(
    x,
    c("components", "mean_log_score", "game_clustered_se", "converged"),
    "challenge-margin candidate metrics"
  )
  if (anyDuplicated(x$components) || any(!x$components %in% c(1L, 3L, 6L)) ||
      any(!is.finite(x$mean_log_score)) || anyNA(x$game_clustered_se) ||
      any(!is.finite(x$game_clustered_se)) || any(x$game_clustered_se < 0) ||
      anyNA(x$converged)) {
    stop("Challenge-margin candidate metrics are invalid", call. = FALSE)
  }
  x[, eligible := as.logical(converged)]
  if (!any(x$eligible)) {
    stop(
      "No challenge-margin mixture candidate converged; component selection is unavailable",
      call. = FALSE
    )
  }
  best <- x[eligible == TRUE][order(-mean_log_score, components)][1L]
  threshold <- best$mean_log_score - best$game_clustered_se
  x[, `:=`(
    best_mean_log_score = best$mean_log_score,
    one_se_threshold = threshold,
    within_one_se = eligible & mean_log_score >= threshold,
    selected = FALSE
  )]
  selected_components <- min(x[within_one_se == TRUE, components])
  x[components == selected_components, selected := TRUE]
  data.table::setorder(x, components)
  x[]
}

select_challenge_margin_prior_1d <- function(
  pitches,
  fit_games,
  validation_games,
  components = c(1L, 3L, 6L),
  refit_selected = TRUE,
  fold_id = "full",
  global_draw_id = 1L,
  ...
) {
  fold_id <- .challenge_margin_scalar_id(fold_id, "fold_id")
  global_draw_id <- .challenge_margin_scalar_id(global_draw_id, "global_draw_id")
  components <- sort(unique(as.integer(components)))
  if (!length(components) || any(!components %in% c(1L, 3L, 6L))) {
    stop("components must be drawn from 1, 3, and 6", call. = FALSE)
  }
  fit_games <- sort(unique(as.character(fit_games)))
  validation_games <- sort(unique(as.character(validation_games)))
  if (!length(fit_games) || !length(validation_games) ||
      length(intersect(fit_games, validation_games))) {
    stop("Fit and validation games must be non-empty and disjoint",
      call. = FALSE
    )
  }
  x <- normalize_challenge_margin_prior_1d_input(pitches)
  absent_fit <- setdiff(fit_games, unique(x$game_pk))
  absent_validation <- setdiff(validation_games, unique(x$game_pk))
  if (length(absent_fit) || length(absent_validation)) {
    stop("Every requested fit and validation game must contain an eligible pitch",
      call. = FALSE
    )
  }
  candidate_fits <- lapply(components, function(component_count) {
    fit_challenge_margin_prior_1d(
      x,
      components = component_count,
      training_games = fit_games,
      fold_id = fold_id,
      global_draw_id = global_draw_id,
      ...
    )
  })
  names(candidate_fits) <- as.character(components)
  metric_rows <- lapply(seq_along(components), function(index) {
    scored <- score_challenge_margin_prior_1d(
      candidate_fits[[index]], x[game_pk %in% validation_games],
      require_game_separation = TRUE
    )
    metric <- challenge_margin_game_clustered_log_score_1d(scored)
    metric[, `:=`(
      components = components[[index]],
      converged = candidate_fits[[index]]$converged,
      iterations = candidate_fits[[index]]$iterations,
      convergence_tolerance =
        candidate_fits[[index]]$convergence_tolerance,
      last_relative_log_likelihood_change =
        candidate_fits[[index]]$last_relative_log_likelihood_change,
      fold_id = fold_id,
      global_draw_id = global_draw_id
    )]
    metric
  })
  candidate_metrics <- data.table::rbindlist(metric_rows, fill = TRUE)
  selection <- select_challenge_margin_component_count_1d(candidate_metrics)
  selected_components <- selection[selected == TRUE, components][[1L]]
  selected_fit <- candidate_fits[[as.character(selected_components)]]
  refit_games <- fit_games
  if (isTRUE(refit_selected)) {
    refit_games <- sort(unique(c(fit_games, validation_games)))
    selected_fit <- fit_challenge_margin_prior_1d(
      x,
      components = selected_components,
      training_games = refit_games,
      fold_id = fold_id,
      global_draw_id = global_draw_id,
      ...
    )
    if (!isTRUE(selected_fit$converged)) {
      stop(
        "The selected challenge-margin mixture failed to converge after refitting",
        call. = FALSE
      )
    }
  }
  selected_fit$component_selection <- selection
  selected_fit$component_fit_games <- fit_games
  selected_fit$component_validation_games <- validation_games
  selected_fit$refit_after_selection <- isTRUE(refit_selected)
  result <- list(
    fit = selected_fit,
    candidate_fits = candidate_fits,
    candidate_metrics = selection,
    selected_components = selected_components,
    component_fit_games = fit_games,
    component_validation_games = validation_games,
    refit_games = refit_games,
    fold_id = fold_id,
    global_draw_id = global_draw_id
  )
  class(result) <- "challenge_margin_prior_1d_selection"
  result
}

challenge_margin_prior_ball_rate_1d <- function(fit, context = NULL) {
  .challenge_margin_validate_fit(fit)
  size <- if (is.null(context)) 1L else max(1L, length(context))
  resolved <- .challenge_margin_resolve_context_weights(fit, context, size)
  as.numeric(
    resolved$weight %*% stats::pnorm(fit$mean / fit$sd)
  )
}

challenge_margin_subjective_ball_probability_1d <- function(
  fit, private_margin_signal, perception_sigma, context = NULL
) {
  .challenge_margin_validate_fit(fit)
  signal <- as.numeric(private_margin_signal)
  sigma <- as.numeric(perception_sigma)
  size <- max(length(signal), length(sigma))
  if (length(signal) != 1L && length(signal) != size ||
      length(sigma) != 1L && length(sigma) != size) {
    stop("signal and perception_sigma must be scalar or have a common length",
      call. = FALSE
    )
  }
  signal <- rep_len(signal, size)
  sigma <- rep_len(sigma, size)
  resolved <- .challenge_margin_resolve_context_weights(fit, context, size)
  if (anyNA(signal) || any(!is.finite(signal)) || anyNA(sigma) ||
      any(sigma < 0)) {
    stop("Signals must be finite and sigmas non-missing and non-negative",
      call. = FALSE
    )
  }
  component_prior_ball_rate <- stats::pnorm(fit$mean / fit$sd)
  prior_ball_rate <- as.numeric(
    resolved$weight %*% component_prior_ball_rate
  )
  result <- numeric(size)
  for (row in seq_len(size)) {
    scale_reference <- max(1, abs(fit$mean), fit$sd)
    if (is.infinite(sigma[[row]]) ||
        sigma[[row]] > sqrt(.Machine$double.xmax) / scale_reference) {
      result[[row]] <- prior_ball_rate[[row]]
      next
    }
    if (sigma[[row]] == 0) {
      result[[row]] <- as.numeric(signal[[row]] > 0)
      next
    }
    prior_variance <- fit$sd^2
    signal_variance <- sigma[[row]]^2
    total_variance <- prior_variance + signal_variance
    log_component_weight <- log(resolved$weight[row, ]) + stats::dnorm(
      signal[[row]], fit$mean, sqrt(total_variance), log = TRUE
    )
    maximum <- max(log_component_weight)
    posterior_weight <- exp(log_component_weight - maximum)
    posterior_weight <- posterior_weight / sum(posterior_weight)
    posterior_variance <- prior_variance * signal_variance / total_variance
    posterior_mean <- (
      signal_variance * fit$mean + prior_variance * signal[[row]]
    ) / total_variance
    component_ball_probability <- stats::pnorm(
      posterior_mean / sqrt(posterior_variance)
    )
    result[[row]] <- sum(posterior_weight * component_ball_probability)
  }
  pmin(1, pmax(0, result))
}

score_challenge_margin_subjective_q_1d <- function(
  fit, private_margin_signal, perception_sigma, row_id = NULL, context = NULL
) {
  signal <- as.numeric(private_margin_signal)
  sigma <- as.numeric(perception_sigma)
  if (length(sigma) != 1L && length(sigma) != length(signal)) {
    stop("perception_sigma must be scalar or align with the signals",
      call. = FALSE
    )
  }
  sigma <- rep_len(sigma, length(signal))
  if (is.null(row_id)) row_id <- seq_along(signal)
  if (length(row_id) != length(signal)) {
    stop("row_id must align with private_margin_signal", call. = FALSE)
  }
  resolved <- .challenge_margin_resolve_context_weights(
    fit, context, length(signal)
  )
  data.table::data.table(
    row_id = row_id,
    private_margin_signal = signal,
    perception_sigma = sigma,
    subjective_ball_probability =
      challenge_margin_subjective_ball_probability_1d(
        fit, signal, sigma, context = context
      ),
    prior_ball_rate = challenge_margin_prior_ball_rate_1d(
      fit, context = context
    ),
    prior_context = resolved$context_value,
    context_seen_in_training = resolved$context_seen_in_training,
    context_fallback = resolved$context_fallback,
    prior_components = fit$components,
    fold_id = fit$fold_id,
    global_draw_id = fit$global_draw_id
  )
}

.challenge_margin_normal_quadrature <- function(order, mean, sd) {
  order <- as.integer(order)
  if (length(order) != 1L || is.na(order) || order < 1L) {
    stop("quadrature order must be a positive integer", call. = FALSE)
  }
  if (sd == 0) {
    return(data.table::data.table(signal = mean, weight = 1))
  }
  if (order == 1L) {
    node <- 0
    weight <- 1
  } else {
    index <- seq_len(order - 1L)
    jacobi <- matrix(0, nrow = order, ncol = order)
    off_diagonal <- sqrt(index)
    jacobi[cbind(index, index + 1L)] <- off_diagonal
    jacobi[cbind(index + 1L, index)] <- off_diagonal
    decomposition <- eigen(jacobi, symmetric = TRUE)
    ordering <- order(decomposition$values)
    node <- decomposition$values[ordering]
    weight <- decomposition$vectors[1L, ordering]^2
  }
  data.table::data.table(
    signal = mean + sd * node,
    weight = weight / sum(weight)
  )
}

integrate_challenge_margin_choice_1d <- function(
  fit,
  true_margin,
  perception_sigma,
  action_function,
  quadrature_order = 11L,
  context = NULL
) {
  .challenge_margin_validate_fit(fit)
  if (!is.function(action_function)) {
    stop("action_function must be a function of q, signal, and row_index",
      call. = FALSE
    )
  }
  margin <- as.numeric(true_margin)
  sigma <- as.numeric(perception_sigma)
  size <- max(length(margin), length(sigma))
  if (length(margin) != 1L && length(margin) != size ||
      length(sigma) != 1L && length(sigma) != size) {
    stop("true_margin and perception_sigma must be scalar or aligned",
      call. = FALSE
    )
  }
  margin <- rep_len(margin, size)
  sigma <- rep_len(sigma, size)
  if (!is.null(context) && length(context) != 1L && length(context) != size) {
    stop("context must be scalar or align with the integrated rows",
      call. = FALSE
    )
  }
  row_context <- if (is.null(context)) {
    rep(NA_character_, size)
  } else {
    rep_len(as.character(context), size)
  }
  resolved_context <- .challenge_margin_resolve_context_weights(
    fit, if (is.null(context)) NULL else row_context, size
  )
  if (anyNA(margin) || any(!is.finite(margin)) || anyNA(sigma) ||
      any(!is.finite(sigma)) || any(sigma < 0)) {
    stop("Integration margins and sigmas must be finite and non-negative",
      call. = FALSE
    )
  }
  rows <- lapply(seq_len(size), function(row) {
    rule <- .challenge_margin_normal_quadrature(
      quadrature_order, margin[[row]], sigma[[row]]
    )
    q <- challenge_margin_subjective_ball_probability_1d(
      fit,
      rule$signal,
      sigma[[row]],
      context = if (is.null(context)) NULL else row_context[[row]]
    )
    action <- as.numeric(action_function(
      q = q, signal = rule$signal, row_index = row
    ))
    if (length(action) == 1L) action <- rep(action, nrow(rule))
    if (length(action) != nrow(rule) || anyNA(action) ||
        any(!is.finite(action)) || any(action < 0 | action > 1)) {
      stop("action_function must return one probability per signal node",
        call. = FALSE
      )
    }
    challenge_probability <- sum(rule$weight * action)
    q_signal_mean <- sum(rule$weight * q)
    q_chosen <- if (challenge_probability > 0) {
      sum(rule$weight * q * action) / challenge_probability
    } else {
      NA_real_
    }
    data.table::data.table(
      row_id = row,
      true_margin = margin[[row]],
      perception_sigma = sigma[[row]],
      predicted_challenge_probability = challenge_probability,
      q_signal_mean = q_signal_mean,
      q_chosen = q_chosen,
      prior_context = resolved_context$context_value[[row]],
      context_seen_in_training =
        resolved_context$context_seen_in_training[[row]],
      context_fallback = resolved_context$context_fallback[[row]],
      quadrature_order = as.integer(quadrature_order),
      fold_id = fit$fold_id,
      global_draw_id = fit$global_draw_id
    )
  })
  data.table::rbindlist(rows)
}

.challenge_margin_as_draw_matrix_1d <- function(value, rows, label) {
  if (is.matrix(value)) {
    result <- value
    storage.mode(result) <- "double"
    if (nrow(result) == 1L && rows > 1L) {
      result <- matrix(
        rep(result, each = rows), nrow = rows, ncol = ncol(result)
      )
    }
    if (nrow(result) != rows || !ncol(result)) {
      stop(label, " must have one row per pitch and at least one draw",
        call. = FALSE
      )
    }
    return(result)
  }
  value <- as.numeric(value)
  if (!length(value)) stop(label, " cannot be empty", call. = FALSE)
  if (length(value) == 1L) {
    return(matrix(rep(value, rows), nrow = rows, ncol = 1L))
  }
  if (rows == 1L) return(matrix(value, nrow = 1L))
  if (length(value) == rows) return(matrix(value, nrow = rows, ncol = 1L))
  stop(
    label,
    " must be scalar, row-aligned, or an explicit rows-by-draws matrix",
    call. = FALSE
  )
}

.challenge_margin_align_draw_matrices_1d <- function(threshold, sigma) {
  draws <- max(ncol(threshold), ncol(sigma))
  expand <- function(value, label) {
    if (ncol(value) == draws) return(value)
    if (ncol(value) == 1L) {
      return(matrix(
        rep(value[, 1L], draws), nrow = nrow(value), ncol = draws
      ))
    }
    stop(label, " draw count does not align with the other parameter",
      call. = FALSE
    )
  }
  list(
    threshold = expand(threshold, "threshold"),
    sigma = expand(sigma, "perception_sigma"),
    draws = draws
  )
}

.challenge_margin_row_vector_1d <- function(value, rows, label) {
  value <- as.numeric(value)
  if (length(value) != 1L && length(value) != rows) {
    stop(label, " must be scalar or align with the rows", call. = FALSE)
  }
  rep_len(value, rows)
}

.challenge_margin_log_sum_exp_1d <- function(value) {
  maximum <- max(value)
  if (is.infinite(maximum)) return(maximum)
  maximum + log(sum(exp(value - maximum)))
}

.challenge_margin_subjective_log_odds_scalar_1d <- function(
  fit, private_margin_signal, perception_sigma, component_weight
) {
  prior_variance <- fit$sd^2
  signal_variance <- perception_sigma^2
  total_variance <- prior_variance + signal_variance
  log_signal_component <- log(component_weight) + stats::dnorm(
    private_margin_signal,
    mean = fit$mean,
    sd = sqrt(total_variance),
    log = TRUE
  )
  posterior_variance <- prior_variance * signal_variance / total_variance
  posterior_mean <- (
    signal_variance * fit$mean +
      prior_variance * private_margin_signal
  ) / total_variance
  standardized_posterior_mean <- posterior_mean /
    sqrt(posterior_variance)
  log_ball_joint <- log_signal_component + stats::pnorm(
    standardized_posterior_mean, log.p = TRUE
  )
  log_strike_joint <- log_signal_component + stats::pnorm(
    standardized_posterior_mean, lower.tail = FALSE, log.p = TRUE
  )
  .challenge_margin_log_sum_exp_1d(log_ball_joint) -
    .challenge_margin_log_sum_exp_1d(log_strike_joint)
}

.challenge_margin_payoff_root_scalar_1d <- function(
  fit, gain, inventory_loss, perception_sigma, component_weight,
  root_tolerance, root_max_iterations, max_bracket_expansions
) {
  target_log_odds <- if (inventory_loss == 0) {
    -Inf
  } else if (gain == 0) {
    Inf
  } else {
    log(inventory_loss) - log(gain)
  }
  q_target <- stats::plogis(target_log_odds)
  empty_diagnostic <- function(threshold, status) {
    list(
      threshold = threshold,
      q_target = q_target,
      target_log_odds = target_log_odds,
      inversion_q = NA_real_,
      inversion_absolute_error = NA_real_,
      log_odds_residual = NA_real_,
      bracket_lower = NA_real_,
      bracket_upper = NA_real_,
      bracket_expansions = 0L,
      root_iterations = 0L,
      estimated_precision = 0,
      status = status
    )
  }
  if (inventory_loss == 0) {
    return(empty_diagnostic(-Inf, "analytic_zero_failure_cost_always"))
  }
  if (gain == 0) {
    return(empty_diagnostic(Inf, "analytic_zero_gain_never"))
  }
  numerical_zero <- perception_sigma == 0 ||
    perception_sigma <= sqrt(.Machine$double.xmin)
  if (numerical_zero) {
    return(empty_diagnostic(0, "analytic_zero_sigma_generalized_inverse"))
  }
  scale_reference <- max(1, abs(fit$mean), fit$sd)
  diffuse <- is.infinite(perception_sigma) ||
    perception_sigma > sqrt(.Machine$double.xmax) / scale_reference
  if (diffuse) {
    prior_rate <- sum(
      component_weight * stats::pnorm(fit$mean / fit$sd)
    )
    if (prior_rate > q_target) {
      return(empty_diagnostic(
        -Inf, "analytic_diffuse_sigma_prior_above_threshold"
      ))
    }
    return(empty_diagnostic(
      Inf, "analytic_diffuse_sigma_prior_not_above_threshold"
    ))
  }

  objective <- function(signal) {
    value <- .challenge_margin_subjective_log_odds_scalar_1d(
      fit, signal, perception_sigma, component_weight
    ) - target_log_odds
    if (is.nan(value)) {
      stop("Payoff-threshold log-odds evaluation was indeterminate",
        call. = FALSE
      )
    }
    if (is.infinite(value)) sign(value) * .Machine$double.xmax else value
  }
  center <- sum(component_weight * fit$mean)
  signal_variance <- sum(component_weight * (
    fit$sd^2 + perception_sigma^2 + (fit$mean - center)^2
  ))
  width <- max(8, 4 * sqrt(signal_variance))
  lower <- center - width
  upper <- center + width
  lower_value <- objective(lower)
  upper_value <- objective(upper)
  expansions <- 0L
  while (lower_value > 0 && expansions < max_bracket_expansions) {
    width <- width * 2
    lower <- center - width
    lower_value <- objective(lower)
    expansions <- expansions + 1L
  }
  while (upper_value < 0 && expansions < max_bracket_expansions) {
    width <- width * 2
    upper <- center + width
    upper_value <- objective(upper)
    expansions <- expansions + 1L
  }
  if (lower_value > 0 || upper_value < 0 ||
      !is.finite(lower) || !is.finite(upper)) {
    stop(
      "Could not bracket the payoff-implied private-signal threshold after ",
      max_bracket_expansions, " expansions",
      call. = FALSE
    )
  }
  root <- stats::uniroot(
    objective,
    interval = c(lower, upper),
    f.lower = lower_value,
    f.upper = upper_value,
    tol = root_tolerance,
    maxiter = root_max_iterations
  )
  root_log_odds <- .challenge_margin_subjective_log_odds_scalar_1d(
    fit, root$root, perception_sigma, component_weight
  )
  inversion_q <- stats::plogis(root_log_odds)
  list(
    threshold = root$root,
    q_target = q_target,
    target_log_odds = target_log_odds,
    inversion_q = inversion_q,
    inversion_absolute_error = abs(inversion_q - q_target),
    log_odds_residual = root_log_odds - target_log_odds,
    bracket_lower = lower,
    bracket_upper = upper,
    bracket_expansions = expansions,
    root_iterations = root$iter,
    estimated_precision = root$estim.prec,
    status = "interior_log_odds_root"
  )
}

solve_challenge_margin_payoff_threshold_1d <- function(
  fit,
  gain,
  inventory_loss,
  perception_sigma_draws,
  context,
  row_id = NULL,
  draw_id = NULL,
  root_tolerance = 1e-10,
  root_max_iterations = 200L,
  max_bracket_expansions = 40L
) {
  .challenge_margin_validate_fit(fit)
  if (missing(context) || is.null(context)) {
    stop("context must be supplied explicitly for payoff inversion",
      call. = FALSE
    )
  }
  row_candidates <- c(
    length(gain), length(inventory_loss), length(context),
    if (is.null(row_id)) 1L else length(row_id),
    if (is.matrix(perception_sigma_draws)) {
      nrow(perception_sigma_draws)
    } else {
      1L
    }
  )
  rows <- max(row_candidates)
  input_lengths <- c(length(gain), length(inventory_loss))
  if (!rows || any(input_lengths != 1L & input_lengths != rows)) {
    stop("gain and inventory_loss must be non-empty and row-aligned",
      call. = FALSE
    )
  }
  gain <- .challenge_margin_row_vector_1d(gain, rows, "gain")
  inventory_loss <- .challenge_margin_row_vector_1d(
    inventory_loss, rows, "inventory_loss"
  )
  if (anyNA(gain) || any(!is.finite(gain)) || any(gain < 0) ||
      anyNA(inventory_loss) || any(!is.finite(inventory_loss)) ||
      any(inventory_loss < 0) || any(gain + inventory_loss <= 0)) {
    stop(
      "gain and inventory_loss must be finite, non-negative, and not both zero",
      call. = FALSE
    )
  }
  context <- as.character(context)
  if (length(context) != 1L && length(context) != rows) {
    stop("context must be scalar or align with the payoff rows", call. = FALSE)
  }
  context <- rep_len(context, rows)
  if (is.null(row_id)) row_id <- seq_len(rows)
  if (length(row_id) != rows || anyNA(row_id) || anyDuplicated(row_id)) {
    stop("row_id must uniquely align with the payoff rows", call. = FALSE)
  }
  sigma <- .challenge_margin_as_draw_matrix_1d(
    perception_sigma_draws, rows, "perception_sigma_draws"
  )
  draws <- ncol(sigma)
  if (anyNA(sigma) || any(sigma < 0)) {
    stop("perception-sigma draws must be non-missing and non-negative",
      call. = FALSE
    )
  }
  if (is.null(draw_id)) {
    draw_id <- if (draws == 1L && !is.null(fit$global_draw_id)) {
      fit$global_draw_id
    } else {
      seq_len(draws)
    }
  }
  if (length(draw_id) != draws || anyNA(draw_id) || anyDuplicated(draw_id)) {
    stop("draw_id must uniquely align with perception-sigma draws",
      call. = FALSE
    )
  }
  root_tolerance <- as.numeric(root_tolerance)
  root_max_iterations <- as.integer(root_max_iterations)
  max_bracket_expansions <- as.integer(max_bracket_expansions)
  if (length(root_tolerance) != 1L || !is.finite(root_tolerance) ||
      root_tolerance <= 0 || length(root_max_iterations) != 1L ||
      is.na(root_max_iterations) || root_max_iterations < 1L ||
      length(max_bracket_expansions) != 1L ||
      is.na(max_bracket_expansions) || max_bracket_expansions < 1L) {
    stop("Payoff root controls must be positive scalar values", call. = FALSE)
  }

  resolved_context <- .challenge_margin_resolve_context_weights(
    fit, context, rows
  )
  threshold <- matrix(NA_real_, nrow = rows, ncol = draws)
  results <- vector("list", rows * draws)
  result_index <- 0L
  for (row in seq_len(rows)) {
    for (draw in seq_len(draws)) {
      result_index <- result_index + 1L
      solved <- .challenge_margin_payoff_root_scalar_1d(
        fit = fit,
        gain = gain[[row]],
        inventory_loss = inventory_loss[[row]],
        perception_sigma = sigma[row, draw],
        component_weight = resolved_context$weight[row, ],
        root_tolerance = root_tolerance,
        root_max_iterations = root_max_iterations,
        max_bracket_expansions = max_bracket_expansions
      )
      threshold[row, draw] <- solved$threshold
      results[[result_index]] <- data.table::data.table(
        row_index = row,
        row_id = row_id[[row]],
        draw_index = draw,
        global_draw_id = draw_id[[draw]],
        gain = gain[[row]],
        inventory_loss = inventory_loss[[row]],
        q_target = solved$q_target,
        target_log_odds = solved$target_log_odds,
        perception_sigma = sigma[row, draw],
        threshold_inches = solved$threshold,
        inversion_q = solved$inversion_q,
        inversion_absolute_error = solved$inversion_absolute_error,
        log_odds_residual = solved$log_odds_residual,
        bracket_lower = solved$bracket_lower,
        bracket_upper = solved$bracket_upper,
        bracket_expansions = solved$bracket_expansions,
        root_iterations = solved$root_iterations,
        estimated_root_precision = solved$estimated_precision,
        root_status = solved$status,
        prior_context = resolved_context$context_value[[row]],
        context_seen_in_training =
          resolved_context$context_seen_in_training[[row]],
        context_fallback = resolved_context$context_fallback[[row]],
        fold_id = fit$fold_id,
        prior_global_draw_id = fit$global_draw_id,
        prior_training_fingerprint = if (!is.null(fit$training_fingerprint)) {
          fit$training_fingerprint
        } else {
          NA_character_
        }
      )
    }
  }
  draw_table <- data.table::rbindlist(results)
  summary <- data.table::rbindlist(lapply(seq_len(rows), function(row) {
    block <- draw_table[row_index == row]
    threshold_summary <- .challenge_margin_draw_summary_1d(
      block$threshold_inches
    )
    data.table::data.table(
      row_index = row,
      row_id = row_id[[row]],
      gain = gain[[row]],
      inventory_loss = inventory_loss[[row]],
      q_target = block$q_target[[1L]],
      prior_context = block$prior_context[[1L]],
      context_seen_in_training = block$context_seen_in_training[[1L]],
      context_fallback = block$context_fallback[[1L]],
      threshold_inches_mean = threshold_summary[[1L]],
      threshold_inches_lower_95 = threshold_summary[[2L]],
      threshold_inches_median = threshold_summary[[3L]],
      threshold_inches_upper_95 = threshold_summary[[4L]],
      always_challenge_draws = sum(block$threshold_inches == -Inf),
      never_challenge_draws = sum(block$threshold_inches == Inf),
      finite_threshold_draws = sum(is.finite(block$threshold_inches)),
      maximum_inversion_absolute_error = if (any(is.finite(
        block$inversion_absolute_error
      ))) {
        max(block$inversion_absolute_error, na.rm = TRUE)
      } else {
        NA_real_
      },
      maximum_absolute_log_odds_residual = if (any(is.finite(
        block$log_odds_residual
      ))) {
        max(abs(block$log_odds_residual), na.rm = TRUE)
      } else {
        NA_real_
      },
      maximum_bracket_expansions = max(block$bracket_expansions),
      posterior_draws = nrow(block),
      fold_id = fit$fold_id,
      prior_training_fingerprint = block$prior_training_fingerprint[[1L]]
    )
  }))
  result <- list(
    summary = summary[],
    draws = draw_table[],
    threshold_draws = threshold,
    perception_sigma_draws = sigma,
    context = context,
    row_id = row_id,
    global_draw_id = draw_id,
    model = "deterministic_expected_payoff_private_signal_threshold",
    payoff_rule = "q * gain - (1 - q) * inventory_loss > 0",
    root_equation = "logit(q(r, context)) = log(inventory_loss / gain)",
    root_controls = list(
      tolerance = root_tolerance,
      maximum_iterations = root_max_iterations,
      maximum_bracket_expansions = max_bracket_expansions
    ),
    prior_training_fingerprint = fit$training_fingerprint
  )
  class(result) <- "challenge_margin_payoff_threshold_1d_solution"
  result
}

.challenge_margin_truncated_q_1d <- function(
  fit, true_margin, threshold, perception_sigma, context,
  subdivisions, relative_tolerance, absolute_tolerance
) {
  prior_rate <- challenge_margin_prior_ball_rate_1d(fit, context = context)
  if (is.infinite(threshold) && threshold > 0) {
    return(list(
      challenge_probability = 0,
      log_challenge_probability = -Inf,
      q_chosen = NA_real_,
      absolute_error = 0,
      subdivisions = 0L,
      status = "analytic_never_challenge_threshold"
    ))
  }
  always_challenge <- is.infinite(threshold) && threshold < 0
  if (is.infinite(perception_sigma)) {
    return(list(
      challenge_probability = if (always_challenge) 1 else 0.5,
      log_challenge_probability = if (always_challenge) 0 else log(0.5),
      q_chosen = prior_rate,
      absolute_error = 0,
      subdivisions = 0L,
      status = if (always_challenge) {
        "analytic_always_challenge_infinite_sigma"
      } else {
        "analytic_infinite_sigma_limit"
      }
    ))
  }
  if (perception_sigma == 0) {
    chosen <- always_challenge || true_margin > threshold
    return(list(
      challenge_probability = as.numeric(chosen),
      log_challenge_probability = if (chosen) 0 else -Inf,
      q_chosen = if (chosen) {
        challenge_margin_subjective_ball_probability_1d(
          fit, true_margin, 0, context = context
        )
      } else {
        NA_real_
      },
      absolute_error = 0,
      subdivisions = 0L,
      status = if (always_challenge) {
        "analytic_always_challenge_zero_sigma"
      } else {
        "analytic_zero_sigma_limit"
      }
    ))
  }

  scale_reference <- max(
    1, abs(true_margin),
    if (always_challenge) 0 else abs(threshold),
    abs(fit$mean), fit$sd
  )
  if (perception_sigma >
      sqrt(.Machine$double.xmax) / scale_reference) {
    return(list(
      challenge_probability = if (always_challenge) 1 else 0.5,
      log_challenge_probability = if (always_challenge) 0 else log(0.5),
      q_chosen = prior_rate,
      absolute_error = 0,
      subdivisions = 0L,
      status = if (always_challenge) {
        "analytic_always_challenge_diffuse_sigma"
      } else {
        "analytic_diffuse_sigma_limit"
      }
    ))
  }

  cutoff <- if (always_challenge) {
    -Inf
  } else {
    (threshold - true_margin) / perception_sigma
  }
  log_challenge_probability <- stats::pnorm(
    cutoff, lower.tail = FALSE, log.p = TRUE
  )
  challenge_probability <- exp(log_challenge_probability)
  integrand <- function(unit_tail_probability) {
    unit_tail_probability <- pmin(
      1 - .Machine$double.eps,
      pmax(.Machine$double.xmin, unit_tail_probability)
    )
    standardized_signal <- stats::qnorm(
      log_challenge_probability + log(unit_tail_probability),
      lower.tail = FALSE, log.p = TRUE
    )
    private_signal <- true_margin +
      perception_sigma * standardized_signal
    challenge_margin_subjective_ball_probability_1d(
      fit,
      private_margin_signal = private_signal,
      perception_sigma = perception_sigma,
      context = context
    )
  }
  integral <- stats::integrate(
    integrand,
    lower = 0,
    upper = 1,
    subdivisions = subdivisions,
    rel.tol = relative_tolerance,
    abs.tol = absolute_tolerance,
    stop.on.error = FALSE
  )
  if (!identical(integral$message, "OK") || !is.finite(integral$value) ||
      integral$value < -absolute_tolerance ||
      integral$value > 1 + absolute_tolerance) {
    stop(
      "Adaptive truncated-normal q_chosen integration failed: ",
      integral$message,
      call. = FALSE
    )
  }
  list(
    challenge_probability = challenge_probability,
    log_challenge_probability = log_challenge_probability,
    q_chosen = pmin(1, pmax(0, integral$value)),
    absolute_error = integral$abs.error,
    subdivisions = integral$subdivisions,
    status = if (always_challenge) {
      "adaptive_untruncated_normal_always_challenge"
    } else if (challenge_probability == 0 &&
      is.finite(log_challenge_probability)) {
      "adaptive_truncated_normal_probability_underflow"
    } else {
      "adaptive_truncated_normal"
    }
  )
}

.challenge_margin_draw_summary_1d <- function(value) {
  value <- as.numeric(value)
  value <- value[is.finite(value)]
  if (!length(value)) return(rep(NA_real_, 4L))
  c(
    mean(value),
    stats::quantile(
      value, c(0.025, 0.5, 0.975), names = FALSE, type = 8
    )
  )
}

compose_challenge_margin_discrimination_1d <- function(
  fit,
  true_margin,
  threshold_draws,
  perception_sigma_draws,
  context,
  row_id = NULL,
  draw_id = NULL,
  subdivisions = 200L,
  relative_tolerance = 1e-8,
  absolute_tolerance = 1e-10
) {
  .challenge_margin_validate_fit(fit)
  margin <- as.numeric(true_margin)
  if (!length(margin) || anyNA(margin) || any(!is.finite(margin))) {
    stop("true_margin must contain finite values", call. = FALSE)
  }
  rows <- length(margin)
  if (missing(context) || is.null(context)) {
    stop("context must be supplied explicitly for reduced-form composition",
      call. = FALSE
    )
  }
  context <- as.character(context)
  if (length(context) != 1L && length(context) != rows) {
    stop("context must be scalar or align with true_margin", call. = FALSE)
  }
  context <- rep_len(context, rows)
  if (is.null(row_id)) row_id <- seq_len(rows)
  if (length(row_id) != rows || anyNA(row_id) || anyDuplicated(row_id)) {
    stop("row_id must uniquely align with true_margin", call. = FALSE)
  }

  threshold <- .challenge_margin_as_draw_matrix_1d(
    threshold_draws, rows, "threshold_draws"
  )
  sigma <- .challenge_margin_as_draw_matrix_1d(
    perception_sigma_draws, rows, "perception_sigma_draws"
  )
  aligned <- .challenge_margin_align_draw_matrices_1d(threshold, sigma)
  threshold <- aligned$threshold
  sigma <- aligned$sigma
  draws <- aligned$draws
  if (anyNA(threshold) || any(is.nan(threshold))) {
    stop("threshold draws must be non-missing real values", call. = FALSE)
  }
  if (anyNA(sigma) || any(sigma < 0)) {
    stop("perception-sigma draws must be non-missing and non-negative",
      call. = FALSE
    )
  }
  subdivisions <- as.integer(subdivisions)
  if (length(subdivisions) != 1L || is.na(subdivisions) || subdivisions < 20L) {
    stop("subdivisions must be one integer of at least 20", call. = FALSE)
  }
  tolerances <- c(relative_tolerance, absolute_tolerance)
  if (anyNA(tolerances) || any(!is.finite(tolerances)) ||
      any(tolerances <= 0)) {
    stop("integration tolerances must be finite and positive", call. = FALSE)
  }
  if (is.null(draw_id)) {
    draw_id <- if (draws == 1L && !is.null(fit$global_draw_id)) {
      fit$global_draw_id
    } else {
      seq_len(draws)
    }
  }
  if (length(draw_id) != draws || anyNA(draw_id) || anyDuplicated(draw_id)) {
    stop("draw_id must uniquely align with the parameter draws", call. = FALSE)
  }

  resolved_context <- .challenge_margin_resolve_context_weights(
    fit, context, rows
  )
  prior_rate <- challenge_margin_prior_ball_rate_1d(fit, context = context)
  results <- vector("list", rows * draws)
  index <- 0L
  for (row in seq_len(rows)) {
    for (draw in seq_len(draws)) {
      index <- index + 1L
      integrated <- .challenge_margin_truncated_q_1d(
        fit = fit,
        true_margin = margin[[row]],
        threshold = threshold[row, draw],
        perception_sigma = sigma[row, draw],
        context = context[[row]],
        subdivisions = subdivisions,
        relative_tolerance = relative_tolerance,
        absolute_tolerance = absolute_tolerance
      )
      results[[index]] <- data.table::data.table(
        row_index = row,
        row_id = row_id[[row]],
        draw_index = draw,
        global_draw_id = draw_id[[draw]],
        true_margin = margin[[row]],
        threshold_inches = threshold[row, draw],
        perception_sigma = sigma[row, draw],
        predicted_challenge_probability =
          integrated$challenge_probability,
        log_predicted_challenge_probability =
          integrated$log_challenge_probability,
        q_chosen = integrated$q_chosen,
        prior_ball_rate = prior_rate[[row]],
        prior_context = resolved_context$context_value[[row]],
        context_seen_in_training =
          resolved_context$context_seen_in_training[[row]],
        context_fallback = resolved_context$context_fallback[[row]],
        numerical_absolute_error = integrated$absolute_error,
        numerical_subdivisions = integrated$subdivisions,
        numerical_status = integrated$status,
        fold_id = fit$fold_id,
        prior_global_draw_id = fit$global_draw_id,
        prior_training_fingerprint = if (!is.null(fit$training_fingerprint)) {
          fit$training_fingerprint
        } else {
          NA_character_
        }
      )
    }
  }
  draw_table <- data.table::rbindlist(results)
  summary <- data.table::rbindlist(lapply(seq_len(rows), function(row) {
    block <- draw_table[row_index == row]
    probability <- .challenge_margin_draw_summary_1d(
      block$predicted_challenge_probability
    )
    chosen <- .challenge_margin_draw_summary_1d(block$q_chosen)
    data.table::data.table(
      row_index = row,
      row_id = row_id[[row]],
      true_margin = margin[[row]],
      prior_context = block$prior_context[[1L]],
      context_seen_in_training = block$context_seen_in_training[[1L]],
      context_fallback = block$context_fallback[[1L]],
      prior_ball_rate = block$prior_ball_rate[[1L]],
      predicted_challenge_probability_mean = probability[[1L]],
      predicted_challenge_probability_lower_95 = probability[[2L]],
      predicted_challenge_probability_median = probability[[3L]],
      predicted_challenge_probability_upper_95 = probability[[4L]],
      q_chosen_mean = chosen[[1L]],
      q_chosen_lower_95 = chosen[[2L]],
      q_chosen_median = chosen[[3L]],
      q_chosen_upper_95 = chosen[[4L]],
      numerical_max_absolute_error = max(
        block$numerical_absolute_error, na.rm = TRUE
      ),
      numerical_failures = sum(!block$numerical_status %in% c(
        "adaptive_truncated_normal",
        "adaptive_truncated_normal_probability_underflow",
        "analytic_zero_sigma_limit",
        "analytic_never_challenge_threshold",
        "analytic_always_challenge_zero_sigma",
        "analytic_always_challenge_infinite_sigma",
        "analytic_always_challenge_diffuse_sigma",
        "adaptive_untruncated_normal_always_challenge",
        "analytic_infinite_sigma_limit",
        "analytic_diffuse_sigma_limit"
      )),
      posterior_draws = nrow(block),
      fold_id = fit$fold_id,
      prior_training_fingerprint = block$prior_training_fingerprint[[1L]]
    )
  }))
  result <- list(
    summary = summary[],
    draws = draw_table[],
    global_draw_id = draw_id,
    model = "reduced_form_latent_normal_signal_hard_threshold",
    challenge_probability = "Phi((true_margin - threshold) / sigma)",
    q_chosen_integration =
      "adaptive_upper_truncated_normal_probability_transform",
    integration_tolerances = list(
      relative = relative_tolerance,
      absolute = absolute_tolerance,
      subdivisions = subdivisions
    ),
    prior_training_fingerprint = fit$training_fingerprint
  )
  class(result) <- "challenge_margin_discrimination_1d_composition"
  result
}
