# Cross-fitted continuous signed-margin priors for revealed challenge opportunities.
# Action, inventory, and official-result fields never enter this subsystem.

revealed_challenge_prior_1d_roles <- function() c("offense", "defense")

revealed_challenge_prior_1d_forbidden_columns <- function() unique(c(
  "challenge_occurred", "challenged", "challenger_role", "challenger_team_id",
  "bat_team_challenges_before", "fld_team_challenges_before",
  "adverse_challenges_before", "inventory_before", "abs_call", "final_call",
  "challenge_outcome", "is_overturned", "overturned", "challenge_success",
  "official_success", "official_outcome", "actual_wpa_gain", "actual_re_gain",
  "delta_home_win_exp", "delta_run_exp", "outcome"
))

revealed_challenge_prior_1d_source_allowlist <- function() c(
  "game_pk", "pitch_order", "at_bat_number", "pitch_number", "initial_call",
  "tracking_available", "abs_eligible", "edge_distance_inches",
  "balls_before", "strikes_before", "pitch_type"
)

assert_revealed_challenge_prior_1d_clean <- function(rows, label = "prior rows") {
  leaked <- intersect(names(data.table::as.data.table(rows)),
                      revealed_challenge_prior_1d_forbidden_columns())
  if (length(leaked)) stop(
    label, " contains forbidden action/outcome columns: ",
    paste(sort(leaked), collapse = ", "), call. = FALSE
  )
  invisible(TRUE)
}

.revealed_prior_pitch_family_1d <- function(pitch_type) {
  value <- as.character(pitch_type)
  data.table::fcase(
    value %in% c("FF", "FA", "SI", "FC"), "fastball",
    value %in% c("SL", "ST", "SV", "CU", "KC", "CS"), "breaking",
    value %in% c("CH", "FS", "FO", "SC"), "offspeed",
    default = "other"
  )
}

build_revealed_challenge_prior_role_1d <- function(pitch_ledger, role) {
  role <- match.arg(role, revealed_challenge_prior_1d_roles())
  source <- data.table::as.data.table(pitch_ledger)
  required <- c("game_pk", "pitch_order", "initial_call", "tracking_available",
                "edge_distance_inches", "balls_before", "strikes_before")
  stop_if_missing_columns(source, required, paste(role, "revealed-prior source"))
  retained <- intersect(revealed_challenge_prior_1d_source_allowlist(), names(source))
  x <- data.table::copy(source[, ..retained])
  assert_revealed_challenge_prior_1d_clean(x)
  adverse_call <- if (role == "offense") "called_strike" else "ball"
  x <- x[
    !is.na(game_pk) & tracking_available %in% TRUE &
      initial_call == adverse_call & is.finite(edge_distance_inches)
  ]
  balls <- suppressWarnings(as.integer(x$balls_before))
  strikes <- suppressWarnings(as.integer(x$strikes_before))
  valid_count <- balls %in% 0:3 & strikes %in% 0:2
  x[, `:=`(
    game_pk = as.character(game_pk),
    role = role,
    physical_edge_distance_inches = as.numeric(edge_distance_inches),
    edge_distance_inches = if (role == "offense") {
      as.numeric(edge_distance_inches)
    } else {
      -as.numeric(edge_distance_inches)
    },
    count_state_original = data.table::fifelse(
      valid_count, paste0(balls, "-", strikes), "unknown"
    ),
    pitch_family_coarse = .revealed_prior_pitch_family_1d(pitch_type)
  )]
  x[, context_count_family := paste(
    count_state_original, pitch_family_coarse, sep = "__"
  )]
  data.table::setorder(x, game_pk, pitch_order)
  if (anyDuplicated(x[, .(game_pk, pitch_order)])) stop(
    "Revealed-prior rows contain duplicate pitch keys", call. = FALSE
  )
  assert_revealed_challenge_prior_1d_clean(x)
  x[]
}

build_revealed_challenge_prior_1d <- function(pitch_ledger) {
  data.table::rbindlist(lapply(
    revealed_challenge_prior_1d_roles(),
    function(role) build_revealed_challenge_prior_role_1d(pitch_ledger, role)
  ), use.names = TRUE, fill = TRUE)
}

.revealed_prior_adapter_1d <- function(rows, context_column) {
  x <- data.table::copy(data.table::as.data.table(rows))
  assert_revealed_challenge_prior_1d_clean(x)
  stop_if_missing_columns(
    x, c("game_pk", "pitch_order", "edge_distance_inches", context_column),
    "revealed-prior fitting rows"
  )
  out <- x[, .(
    game_pk = as.character(game_pk), pitch_order,
    initial_call = "called_strike", tracking_available = TRUE,
    edge_distance_inches = as.numeric(edge_distance_inches),
    count_state = as.character(get(context_column))
  )]
  out[is.na(count_state) | !nzchar(count_state), count_state := "unknown"]
  out[]
}

.revealed_prior_fingerprint_1d <- function(rows, context_column) {
  x <- .revealed_prior_adapter_1d(rows, context_column)
  data.table::setorder(x, game_pk, pitch_order)
  digest::digest(
    list(schema = "revealed_challenge_role_prior_1d_v1",
         context = context_column, rows = data.table::setDF(x)),
    algo = "sha256", serialize = TRUE
  )
}

.revealed_prior_refit_weights_1d <- function(fit, rows, context_column,
                                             context_prior_strength) {
  adapted <- .revealed_prior_adapter_1d(rows, context_column)
  mixture <- fit
  class(mixture) <- "challenge_margin_gmm_1d"
  context <- estimate_challenge_margin_context_weights_1d(
    adapted, mixture, context_column = "count_state",
    prior_strength = context_prior_strength
  )
  keep <- names(context)
  fit[keep] <- context
  fit$context_source_column <- context_column
  fit$context_weight_status <- paste0(context_column, "_dirichlet_shrinkage_v1")
  fit
}

.revealed_prior_score_1d <- function(fit, rows, context_column,
                                     require_game_separation = TRUE) {
  adapted <- .revealed_prior_adapter_1d(rows, context_column)
  score_challenge_margin_prior_1d(
    fit, adapted, require_game_separation = require_game_separation
  )[, .(game_pk, pitch_order, edge_distance_inches, prior_context,
        context_seen_in_training, context_fallback, location_log_density,
        prior_components, fold_id, prior_training_fingerprint,
        prior_scoring_fingerprint)]
}

fit_revealed_challenge_prior_outer_fold_1d <- function(
  training_rows, heldout_rows, role, outer_fold,
  components = c(1L, 3L, 6L), inner_folds = 3L, seed = 20260826L,
  context_prior_strength = 100, tolerance = 1e-4, max_iterations = 500L
) {
  role <- match.arg(role, revealed_challenge_prior_1d_roles())
  role_value <- role
  train <- data.table::copy(data.table::as.data.table(training_rows))[role == role_value]
  test <- data.table::copy(data.table::as.data.table(heldout_rows))[role == role_value]
  assert_revealed_challenge_prior_1d_clean(train, "outer training rows")
  assert_revealed_challenge_prior_1d_clean(test, "outer held-out rows")
  overlap <- intersect(unique(train$game_pk), unique(test$game_pk))
  if (length(overlap)) stop("Outer training and held-out games overlap", call. = FALSE)
  inner_folds <- min(as.integer(inner_folds), data.table::uniqueN(train$game_pk))
  if (inner_folds < 2L) stop("At least two inner game folds are required", call. = FALSE)
  inner <- continuous_game_folds(
    train$game_pk, folds = inner_folds,
    seed = as.integer(seed) + as.integer(outer_fold) * 101L + match(role, revealed_challenge_prior_1d_roles())
  )
  train[inner, fold_inner := i.fold, on = "game_pk"]
  candidate_scores <- list()
  candidate_diagnostics <- list()
  index <- 0L
  for (fold_inner in seq_len(inner_folds)) {
    fold_value <- fold_inner
    fit_rows <- train[fold_inner != fold_value]
    validation_rows <- train[fold_inner == fold_value]
    for (component_count in sort(unique(as.integer(components)))) {
      index <- index + 1L
      fit <- fit_challenge_margin_prior_1d(
        .revealed_prior_adapter_1d(fit_rows, "context_count_family"),
        components = component_count, fold_id = paste0(outer_fold, ".", fold_inner),
        context_prior_strength = context_prior_strength,
        tolerance = tolerance, max_iterations = max_iterations
      )
      score <- .revealed_prior_score_1d(
        fit, validation_rows, "context_count_family", TRUE
      )
      score[, `:=`(components = component_count, inner_fold = fold_inner)]
      candidate_scores[[index]] <- score
      candidate_diagnostics[[index]] <- data.table::data.table(
        components = component_count, inner_fold = fold_inner,
        converged = fit$converged, iterations = fit$iterations
      )
    }
  }
  all_scores <- data.table::rbindlist(candidate_scores)
  diagnostics <- data.table::rbindlist(candidate_diagnostics)
  metrics <- all_scores[, {
    metric <- challenge_margin_game_clustered_log_score_1d(.SD)
    metric
  }, by = components]
  convergence <- diagnostics[, .(
    converged = all(converged), iterations = max(iterations)
  ), by = components]
  metrics <- convergence[metrics, on = "components"]
  selection <- select_challenge_margin_component_count_1d(metrics)
  selection[, `:=`(role = role, outer_fold = as.integer(outer_fold))]
  selected_k <- selection[selected == TRUE, components][[1L]]
  primary_fit <- fit_challenge_margin_prior_1d(
    .revealed_prior_adapter_1d(train, "context_count_family"),
    components = selected_k, fold_id = outer_fold,
    context_prior_strength = context_prior_strength,
    tolerance = tolerance, max_iterations = max_iterations
  )
  if (!isTRUE(primary_fit$converged)) stop("Selected outer-training refit failed", call. = FALSE)
  primary_fit$role <- role
  primary_fit$context_source_column <- "context_count_family"
  count_fit <- .revealed_prior_refit_weights_1d(
    primary_fit, train, "count_state_original", context_prior_strength
  )
  primary_score <- .revealed_prior_score_1d(
    primary_fit, test, "context_count_family", TRUE
  )
  count_score <- .revealed_prior_score_1d(
    count_fit, test, "count_state_original", TRUE
  )[, .(game_pk, pitch_order, count_only_log_density = location_log_density,
        count_context_fallback = context_fallback)]
  oof <- count_score[primary_score, on = .(game_pk, pitch_order)]
  oof[, `:=`(role = role, outer_fold = as.integer(outer_fold),
             selected_components = selected_k)]
  list(
    primary_fit = primary_fit, count_only_fit = count_fit,
    candidate_metrics = selection,
    candidate_oof_scores = all_scores,
    oof_scores = oof,
    diagnostic = data.table::data.table(
      role = role, outer_fold = as.integer(outer_fold),
      training_games = data.table::uniqueN(train$game_pk),
      heldout_games = data.table::uniqueN(test$game_pk),
      overlap_games = length(overlap), selected_components = selected_k,
      training_fingerprint = .revealed_prior_fingerprint_1d(train, "context_count_family"),
      scoring_fingerprint = .revealed_prior_fingerprint_1d(test, "context_count_family"),
      action_outcome_columns_used = ""
    )
  )
}

.revealed_prior_parameter_tables_1d <- function(fits) {
  parameters <- data.table::rbindlist(lapply(fits, function(item) {
    fit <- item$primary_fit
    data.table::data.table(
      role = fit$role, outer_fold = as.integer(fit$fold_id),
      component = seq_len(fit$components), weight = fit$weight,
      mean = fit$mean, sd = fit$sd
    )
  }))
  contexts <- data.table::rbindlist(lapply(fits, function(item) {
    primary <- data.table::copy(item$primary_fit$context_weight_table)
    primary[, `:=`(role = item$primary_fit$role,
                    outer_fold = as.integer(item$primary_fit$fold_id),
                    context_variant = "count_x_pitch_family")]
    count <- data.table::copy(item$count_only_fit$context_weight_table)
    count[, `:=`(role = item$primary_fit$role,
                  outer_fold = as.integer(item$primary_fit$fold_id),
                  context_variant = "count_only")]
    data.table::rbindlist(list(primary, count), fill = TRUE)
  }), fill = TRUE)
  list(parameters = parameters, contexts = contexts)
}

crossfit_revealed_challenge_prior_1d <- function(
  rows, fold_assignment = NULL, folds = 5L, inner_folds = 3L,
  seed = 20260826L, components = c(1L, 3L, 6L),
  context_prior_strength = 100, tolerance = 1e-4,
  max_iterations = 500L, progress = interactive()
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  assert_revealed_challenge_prior_1d_clean(x)
  stop_if_missing_columns(x, c("game_pk", "role"), "revealed-prior rows")
  if (is.null(fold_assignment)) {
    fold_assignment <- continuous_game_folds(x$game_pk, folds, seed)
  }
  fold_assignment <- normalize_joint_common_width_fold_assignment_1d(
    fold_assignment, unique(x$game_pk)
  )
  x[fold_assignment, fold := i.fold, on = "game_pk"]
  fold_ids <- sort(unique(x$fold))
  fits <- list(); index <- 0L
  for (outer_fold in fold_ids) for (role in revealed_challenge_prior_1d_roles()) {
    index <- index + 1L
    if (isTRUE(progress)) message("revealed-prior fold ", outer_fold, " role ", role)
    fits[[index]] <- fit_revealed_challenge_prior_outer_fold_1d(
      x[fold != outer_fold], x[fold == outer_fold], role, outer_fold,
      components, inner_folds, seed, context_prior_strength,
      tolerance, max_iterations
    )
  }
  tables <- .revealed_prior_parameter_tables_1d(fits)
  primary_fits <- lapply(fold_ids, function(fold_value) {
    items <- fits[vapply(
      fits,
      function(item) identical(
        as.integer(item$diagnostic$outer_fold[[1L]]), as.integer(fold_value)
      ),
      logical(1L)
    )]
    out <- lapply(items, `[[`, "primary_fit")
    names(out) <- vapply(out, `[[`, character(1L), "role")
    out[revealed_challenge_prior_1d_roles()]
  })
  names(primary_fits) <- as.character(fold_ids)
  count_only_fits <- lapply(fold_ids, function(fold_value) {
    items <- fits[vapply(
      fits,
      function(item) identical(
        as.integer(item$diagnostic$outer_fold[[1L]]), as.integer(fold_value)
      ),
      logical(1L)
    )]
    out <- lapply(items, `[[`, "count_only_fit")
    names(out) <- vapply(items, function(item) item$primary_fit$role, character(1L))
    out[revealed_challenge_prior_1d_roles()]
  })
  names(count_only_fits) <- as.character(fold_ids)
  result <- list(
    fold_fits = fits,
    primary_fits = primary_fits,
    count_only_fits = count_only_fits,
    candidate_metrics = data.table::rbindlist(lapply(fits, `[[`, "candidate_metrics"), fill = TRUE),
    candidate_oof_scores = data.table::rbindlist(lapply(fits, `[[`, "candidate_oof_scores"), fill = TRUE),
    oof_log_scores = data.table::rbindlist(lapply(fits, `[[`, "oof_scores"), fill = TRUE),
    parameter_table = tables$parameters, context_table = tables$contexts,
    diagnostics = data.table::rbindlist(lapply(fits, `[[`, "diagnostic"), fill = TRUE),
    fold_assignment = fold_assignment,
    specification = list(
      roles = revealed_challenge_prior_1d_roles(),
      components = sort(unique(as.integer(components))),
      primary_context = "count_state_original x pitch_family_coarse",
      sensitivity_context = "count_state_original",
      context_prior_strength = context_prior_strength,
      action_outcome_columns_used = character()
    )
  )
  class(result) <- "revealed_challenge_prior_1d_crossfit"
  result
}
