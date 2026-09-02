# Lightweight development-only EDA for the latent challenge-error link.
#
# Challenge/pass observations do not reveal latent errors, so a QQ plot or a
# direct normality test is unavailable. This script instead compares observable
# implications: otherwise identical game-held-out binary-response models use a
# Gaussian CDF (probit), heavier-tailed symmetric CDFs, asymmetric CDFs, or a
# flexible probit-scale margin curve. It is a quick diagnostic and deliberately
# does not replace the richer production nuisance model.

project_root <- rprojroot::find_root(
  rprojroot::has_file("_targets.R"), path = getwd()
)
source(file.path(project_root, "config", "project.R"))
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

required_packages <- c("data.table", "targets", "ggplot2", "gridExtra")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1L), quietly = TRUE
)]
if (length(missing_packages)) {
  stop(
    "The quick challenge-link EDA requires: ",
    paste(missing_packages, collapse = ", "), call. = FALSE
  )
}

output_directory <- file.path(
  project_root, "data", "processed", "perception",
  "challenge_error_link_eda_quick_1d"
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

seed <- 20260826L
folds <- 5L
local_margin_limit_inches <- 3
margin_bin_width_inches <- 0.25
probability_epsilon <- 1e-10

.quick_clip_probability_1d <- function(value) {
  pmin(1 - probability_epsilon, pmax(probability_epsilon, value))
}

.quick_robit_link_1d <- function(df) {
  force(df)
  out <- list(
    linkfun = function(mu) stats::qt(.quick_clip_probability_1d(mu), df = df),
    linkinv = function(eta) stats::pt(eta, df = df),
    mu.eta = function(eta) pmax(
      stats::dt(eta, df = df), .Machine$double.eps
    ),
    valideta = function(eta) TRUE,
    name = paste0("robit_t", df)
  )
  class(out) <- "link-glm"
  out
}

.quick_loglog_link_1d <- function() {
  out <- list(
    linkfun = function(mu) {
      mu <- .quick_clip_probability_1d(mu)
      -log(-log(mu))
    },
    linkinv = function(eta) exp(-exp(-eta)),
    mu.eta = function(eta) pmax(
      exp(-eta - exp(-eta)), .Machine$double.eps
    ),
    valideta = function(eta) TRUE,
    name = "loglog"
  )
  class(out) <- "link-glm"
  out
}

.quick_clustered_mean_1d <- function(value, game_pk) {
  value <- as.numeric(value)
  game_pk <- as.character(game_pk)
  estimate <- mean(value)
  by_game <- data.table::data.table(
    game_pk = game_pk,
    centered = value - estimate
  )[, .(centered_sum = sum(centered)), by = game_pk]
  games <- nrow(by_game)
  standard_error <- if (games >= 2L) {
    sqrt(
      games / (games - 1L) *
        sum(by_game$centered_sum^2) / length(value)^2
    )
  } else {
    NA_real_
  }
  list(estimate = estimate, standard_error = standard_error, games = games)
}

.quick_standardize_role_data_1d <- function(rows) {
  x <- data.table::copy(data.table::as.data.table(rows))
  speed_median <- stats::median(x$release_speed_mph, na.rm = TRUE)
  if (!is.finite(speed_median)) speed_median <- 90
  x[!is.finite(release_speed_mph), release_speed_mph := speed_median]
  numeric_columns <- c(
    "stake_G", "inning", "role_score_margin", "release_speed_mph"
  )
  for (column in numeric_columns) {
    value <- as.numeric(x[[column]])
    center <- mean(value[is.finite(value)])
    if (!is.finite(center)) center <- 0
    value[!is.finite(value)] <- center
    scale <- stats::sd(value)
    if (!is.finite(scale) || scale <= 0) scale <- 1
    x[, (paste0(column, "_z")) := (value - center) / scale]
  }
  x
}

base_context_terms <- paste(
  c(
    "count_state", "pitch_family_coarse", "matchup", "outs_state",
    "inventory_state", "team_id", "release_speed_mph_z",
    "I(release_speed_mph_z^2)", "stake_G_z", "I(stake_G_z^2)",
    "inning_z", "I(inning_z^2)", "role_score_margin_z",
    "I(role_score_margin_z^2)"
  ),
  collapse = " + "
)

design_formulas <- list(
  common = stats::as.formula(paste(
    "~ role_margin_inches +", base_context_terms
  )),
  flexible_margin = stats::as.formula(paste(
    "~ splines::ns(role_margin_inches, df = 5) +", base_context_terms
  )),
  count_slopes = stats::as.formula(paste(
    "~ role_margin_inches * count_state +",
    paste(setdiff(
      strsplit(base_context_terms, " + ", fixed = TRUE)[[1L]],
      "count_state"
    ), collapse = " + ")
  )),
  family_slopes = stats::as.formula(paste(
    "~ role_margin_inches * pitch_family_coarse +",
    paste(setdiff(
      strsplit(base_context_terms, " + ", fixed = TRUE)[[1L]],
      "pitch_family_coarse"
    ), collapse = " + ")
  )),
  count_family_slopes = stats::as.formula(paste(
    "~ role_margin_inches * count_state +",
    "role_margin_inches * pitch_family_coarse +",
    paste(setdiff(
      strsplit(base_context_terms, " + ", fixed = TRUE)[[1L]],
      c("count_state", "pitch_family_coarse")
    ), collapse = " + ")
  ))
)

model_specifications <- list(
  probit = list(
    design = "common", family = stats::binomial(link = "probit")
  ),
  logit = list(
    design = "common", family = stats::binomial(link = "logit")
  ),
  robit_t10 = list(
    design = "common",
    family = stats::binomial(link = .quick_robit_link_1d(10))
  ),
  robit_t5 = list(
    design = "common",
    family = stats::binomial(link = .quick_robit_link_1d(5))
  ),
  robit_t3 = list(
    design = "common",
    family = stats::binomial(link = .quick_robit_link_1d(3))
  ),
  cauchit = list(
    design = "common", family = stats::binomial(link = "cauchit")
  ),
  cloglog = list(
    design = "common", family = stats::binomial(link = "cloglog")
  ),
  loglog = list(
    design = "common",
    family = stats::binomial(link = .quick_loglog_link_1d())
  ),
  flexible_probit_df5 = list(
    design = "flexible_margin", family = stats::binomial(link = "probit")
  ),
  probit_count_slopes = list(
    design = "count_slopes", family = stats::binomial(link = "probit")
  ),
  probit_family_slopes = list(
    design = "family_slopes", family = stats::binomial(link = "probit")
  ),
  probit_count_family_slopes = list(
    design = "count_family_slopes",
    family = stats::binomial(link = "probit")
  )
)

model_labels <- c(
  probit = "Probit (Gaussian)",
  logit = "Logit (logistic)",
  robit_t10 = "Robit t(10)",
  robit_t5 = "Robit t(5)",
  robit_t3 = "Robit t(3)",
  cauchit = "Cauchit (Cauchy)",
  cloglog = "Complementary log-log",
  loglog = "Log-log",
  flexible_probit_df5 = "Flexible probit margin (5 df)",
  probit_count_slopes = "Probit: count-specific slopes",
  probit_family_slopes = "Probit: family-specific slopes",
  probit_count_family_slopes = "Probit: count + family slopes"
)

message("Loading the immutable pitch ledger and RE table")
pitch_ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
re_model <- targets::tar_read(re_model)
snapshot <- validate_fixed_clock_confirmation_snapshot_1d(pitch_ledger)
development <- data.table::copy(snapshot$split$development)
development_game_ids <- sort(unique(as.character(development$game_pk)))
confirmation_game_ids_used <- character()
if (length(development_game_ids) != 1490L) {
  stop("The quick EDA did not recover exactly 1,490 development games")
}
if (length(confirmation_game_ids_used) != 0L) {
  stop("The quick EDA must use zero confirmation games")
}

message("Constructing development-only adverse-call challenge opportunities")
selection <- build_revealed_challenge_selection_1d(
  development, re_model = re_model, require_positive_inventory = TRUE
)
selection <- selection[
  abs(role_margin_inches) <= local_margin_limit_inches
]
if (!nrow(selection)) stop("The local selection sample is empty")

fold_assignment <- continuous_game_folds(
  development_game_ids, folds = folds, seed = seed
)
selection[, game_pk := as.character(game_pk)]
selection[, fold := fold_assignment$fold[
  match(game_pk, fold_assignment$game_pk)
]]
if (anyNA(selection$fold)) stop("The development fold assignment is incomplete")

sample_summary <- selection[, .(
  rows = .N,
  games = data.table::uniqueN(game_pk),
  challenges = sum(challenged),
  challenge_rate = mean(challenged),
  minimum_margin_inches = min(role_margin_inches),
  maximum_margin_inches = max(role_margin_inches)
), by = role]
sample_summary[, `:=`(
  development_games_used = length(development_game_ids),
  confirmation_games_used = length(confirmation_game_ids_used)
)]

prediction_parts <- list()
fit_parts <- list()
prediction_index <- 0L
fit_index <- 0L

for (role_value in revealed_challenge_selection_1d_roles()) {
  role_rows <- .quick_standardize_role_data_1d(
    selection[role == role_value]
  )
  role_rows[, observation_id := .I]
  design_matrices <- lapply(
    design_formulas,
    stats::model.matrix,
    data = role_rows
  )

  for (fold_id in seq_len(folds)) {
    training <- role_rows$fold != fold_id
    heldout <- !training
    for (model_name in names(model_specifications)) {
      specification <- model_specifications[[model_name]]
      design <- design_matrices[[specification$design]]
      family <- specification$family
      fit_warnings <- character()
      started <- proc.time()[["elapsed"]]
      fit <- withCallingHandlers(
        stats::glm.fit(
          x = design[training, , drop = FALSE],
          y = role_rows$challenged[training],
          family = family,
          control = stats::glm.control(maxit = 100L)
        ),
        warning = function(condition) {
          fit_warnings <<- unique(c(
            fit_warnings, conditionMessage(condition)
          ))
          invokeRestart("muffleWarning")
        }
      )
      elapsed_seconds <- proc.time()[["elapsed"]] - started
      coefficients <- fit$coefficients
      coefficients[!is.finite(coefficients)] <- 0
      probability <- family$linkinv(drop(
        design[heldout, , drop = FALSE] %*% coefficients
      ))
      probability <- .quick_clip_probability_1d(probability)

      prediction_index <- prediction_index + 1L
      prediction_parts[[prediction_index]] <- role_rows[heldout, .(
        observation_id, game_pk, pitch_order, role, fold, challenged,
        role_margin_inches, count_state, pitch_family_coarse
      )][, `:=`(
        model = model_name,
        probability = probability
      )]

      margin_coefficient <- if (
        "role_margin_inches" %in% names(fit$coefficients)
      ) {
        unname(fit$coefficients[["role_margin_inches"]])
      } else {
        NA_real_
      }
      fit_index <- fit_index + 1L
      fit_parts[[fit_index]] <- data.table::data.table(
        role = role_value,
        fold = fold_id,
        model = model_name,
        design = specification$design,
        training_rows = sum(training),
        heldout_rows = sum(heldout),
        design_columns = ncol(design),
        converged = isTRUE(fit$converged),
        iterations = fit$iter,
        margin_coefficient = margin_coefficient,
        effective_probit_width_inches = if (
          identical(model_name, "probit") &&
            is.finite(margin_coefficient) && margin_coefficient > 0
        ) 1 / margin_coefficient else NA_real_,
        elapsed_seconds = elapsed_seconds,
        warning_count = length(fit_warnings),
        warnings = paste(fit_warnings, collapse = " | ")
      )
    }
  }
  rm(design_matrices)
  invisible(gc(FALSE))
}

oof_predictions <- data.table::rbindlist(
  prediction_parts, use.names = TRUE
)
fit_diagnostics <- data.table::rbindlist(fit_parts, use.names = TRUE)
oof_predictions[, `:=`(
  log_loss = -(
    challenged * log(probability) +
      (1 - challenged) * log1p(-probability)
  ),
  brier = (challenged - probability)^2
)]

model_metrics <- oof_predictions[, {
  log_loss_result <- .quick_clustered_mean_1d(log_loss, game_pk)
  brier_result <- .quick_clustered_mean_1d(brier, game_pk)
  calibration_result <- .quick_clustered_mean_1d(
    probability - challenged, game_pk
  )
  .(
    rows = .N,
    games = log_loss_result$games,
    challenges = sum(challenged),
    observed_rate = mean(challenged),
    predicted_rate = mean(probability),
    log_loss = log_loss_result$estimate,
    log_loss_game_clustered_se = log_loss_result$standard_error,
    brier = brier_result$estimate,
    brier_game_clustered_se = brier_result$standard_error,
    calibration_error = calibration_result$estimate,
    calibration_game_clustered_se = calibration_result$standard_error
  )
}, by = .(role, model)]
fit_status <- fit_diagnostics[, .(
  all_folds_converged = all(converged),
  warning_count = sum(warning_count),
  elapsed_seconds = sum(elapsed_seconds)
), by = .(role, model)]
model_metrics <- merge(
  model_metrics, fit_status, by = c("role", "model"), sort = FALSE
)

reference <- oof_predictions[model == "probit", .(
  role, fold, observation_id, game_pk,
  probit_log_loss = log_loss,
  probit_brier = brier
)]
paired <- merge(
  oof_predictions[model != "probit"], reference,
  by = c("role", "fold", "observation_id", "game_pk"),
  sort = FALSE
)
paired_comparisons <- paired[, {
  log_loss_difference <- .quick_clustered_mean_1d(
    log_loss - probit_log_loss, game_pk
  )
  brier_difference <- .quick_clustered_mean_1d(
    brier - probit_brier, game_pk
  )
  .(
    rows = .N,
    games = log_loss_difference$games,
    delta_log_loss_vs_probit = log_loss_difference$estimate,
    delta_log_loss_game_clustered_se =
      log_loss_difference$standard_error,
    delta_log_loss_lower_95 = log_loss_difference$estimate -
      1.96 * log_loss_difference$standard_error,
    delta_log_loss_upper_95 = log_loss_difference$estimate +
      1.96 * log_loss_difference$standard_error,
    delta_log_loss_in_clustered_se = log_loss_difference$estimate /
      log_loss_difference$standard_error,
    delta_brier_vs_probit = brier_difference$estimate,
    delta_brier_game_clustered_se = brier_difference$standard_error,
    improves_log_loss_by_one_clustered_se =
      log_loss_difference$estimate +
        log_loss_difference$standard_error < 0
  )
}, by = .(role, model)]

margin_bin_count <- as.integer(round(
  2 * local_margin_limit_inches / margin_bin_width_inches
))
oof_predictions[, margin_bin_index := pmin(
  margin_bin_count - 1L,
  pmax(0L, as.integer(floor(
    (role_margin_inches + local_margin_limit_inches) /
      margin_bin_width_inches
  )))
)]
margin_calibration <- oof_predictions[, .(
  rows = .N,
  challenges = sum(challenged),
  margin_bin_lower_inches = -local_margin_limit_inches +
    margin_bin_index[[1L]] * margin_bin_width_inches,
  margin_bin_upper_inches = -local_margin_limit_inches +
    (margin_bin_index[[1L]] + 1L) * margin_bin_width_inches,
  margin_bin_center_inches = -local_margin_limit_inches +
    (margin_bin_index[[1L]] + 0.5) * margin_bin_width_inches,
  mean_margin_inches = mean(role_margin_inches),
  observed_rate = mean(challenged),
  predicted_rate = mean(probability),
  observed_binomial_se = sqrt(
    mean(challenged) * (1 - mean(challenged)) / .N
  )
), by = .(role, model, margin_bin_index)]
margin_calibration[, residual_observed_minus_predicted :=
  observed_rate - predicted_rate]

run_metadata <- data.table::data.table(
  schema = "challenge_error_link_eda_quick_1d_v1",
  script = "analysis/eda_challenge_error_link_quick_1d.R",
  cutoff = format(fixed_clock_confirmation_cutoff_1d(), "%Y-%m-%d"),
  development_games_used = length(development_game_ids),
  confirmation_games_used = length(confirmation_game_ids_used),
  development_rows_used = nrow(selection),
  development_challenges_used = sum(selection$challenged),
  folds = folds,
  seed = seed,
  local_margin_limit_inches = local_margin_limit_inches,
  margin_bin_width_inches = margin_bin_width_inches,
  development_game_sha256 = fixed_clock_hash_object_1d(development_game_ids),
  selection_sha256 = fixed_clock_hash_object_1d(selection[, .(
    game_pk, pitch_order, role, challenged, role_margin_inches, fold
  )]),
  production_model_changed = FALSE
)

data.table::setorder(model_metrics, role, log_loss)
data.table::setorder(
  paired_comparisons, role, delta_log_loss_vs_probit
)
data.table::setorder(margin_calibration, role, model, margin_bin_index)
data.table::setorder(fit_diagnostics, role, model, fold)

data.table::fwrite(
  run_metadata, file.path(output_directory, "run_metadata.csv")
)
data.table::fwrite(
  sample_summary, file.path(output_directory, "sample_summary.csv")
)
data.table::fwrite(
  model_metrics, file.path(output_directory, "oof_model_metrics.csv")
)
data.table::fwrite(
  paired_comparisons,
  file.path(output_directory, "paired_game_clustered_comparisons.csv")
)
data.table::fwrite(
  margin_calibration,
  file.path(output_directory, "calibration_by_margin.csv")
)
data.table::fwrite(
  fit_diagnostics, file.path(output_directory, "fit_diagnostics.csv")
)
saveRDS(
  list(
    schema = run_metadata$schema[[1L]],
    run_metadata = run_metadata,
    development_game_ids = development_game_ids,
    confirmation_game_ids_used = confirmation_game_ids_used,
    fold_assignment = fold_assignment,
    sample_summary = sample_summary,
    model_metrics = model_metrics,
    paired_comparisons = paired_comparisons,
    margin_calibration = margin_calibration,
    fit_diagnostics = fit_diagnostics,
    oof_predictions = oof_predictions,
    session_info = utils::sessionInfo()
  ),
  file.path(output_directory, "challenge_error_link_eda_quick_1d.rds"),
  version = 3
)

plot_models <- c(
  "probit", "logit", "robit_t3", "cauchit", "cloglog", "loglog",
  "flexible_probit_df5"
)
observed_plot_data <- unique(margin_calibration[, .(
  role, margin_bin_index, margin_bin_center_inches,
  observed_rate, observed_binomial_se
)])
curve_plot_data <- margin_calibration[model %in% plot_models]
curve_plot_data[, model_label := factor(
  model_labels[model], levels = unname(model_labels[plot_models])
)]
curve_plot <- ggplot2::ggplot(
  curve_plot_data,
  ggplot2::aes(
    x = margin_bin_center_inches,
    y = predicted_rate,
    color = model_label,
    group = model_label
  )
) +
  ggplot2::geom_line(linewidth = 0.65, alpha = 0.9) +
  ggplot2::geom_point(
    data = observed_plot_data,
    ggplot2::aes(x = margin_bin_center_inches, y = observed_rate),
    inherit.aes = FALSE, color = "black", size = 1.4
  ) +
  ggplot2::geom_errorbar(
    data = observed_plot_data,
    ggplot2::aes(
      x = margin_bin_center_inches,
      ymin = pmax(0, observed_rate - 1.96 * observed_binomial_se),
      ymax = pmin(1, observed_rate + 1.96 * observed_binomial_se)
    ),
    inherit.aes = FALSE, color = "grey45", width = 0,
    linewidth = 0.25
  ) +
  ggplot2::facet_wrap(~role, nrow = 1L) +
  ggplot2::scale_color_brewer(palette = "Dark2", name = NULL) +
  ggplot2::labs(
    title = "Development-only challenge transition",
    subtitle = paste(
      "Black points: observed 0.25-inch-bin rates; lines: game-held-out",
      "predictions (confirmation games used: 0)"
    ),
    x = "Role-oriented true margin (inches; positive means call is wrong)",
    y = "Challenge probability"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(legend.position = "bottom")

comparison_plot_data <- data.table::copy(paired_comparisons)
comparison_plot_data[, model_label := factor(
  model_labels[model], levels = rev(unname(model_labels[model != "probit"]))
)]
comparison_plot <- ggplot2::ggplot(
  comparison_plot_data,
  ggplot2::aes(
    x = model_label,
    y = delta_log_loss_vs_probit,
    color = role
  )
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey45") +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = delta_log_loss_lower_95,
      ymax = delta_log_loss_upper_95
    ),
    width = 0.18, linewidth = 0.45,
    position = ggplot2::position_dodge(width = 0.45)
  ) +
  ggplot2::geom_point(
    size = 1.8, position = ggplot2::position_dodge(width = 0.45)
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_color_brewer(palette = "Set1", name = "Role") +
  ggplot2::labs(
    title = "Paired OOF log loss relative to probit",
    subtitle = "Positive favors probit; 95% intervals cluster complete games",
    x = NULL,
    y = "Alternative log loss - probit log loss"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(legend.position = "bottom")

grDevices::png(
  file.path(output_directory, "challenge_error_link_eda_quick_1d.png"),
  width = 1800, height = 1700, res = 160
)
gridExtra::grid.arrange(
  curve_plot, comparison_plot, ncol = 1L, heights = c(1.15, 1)
)
grDevices::dev.off()

.quick_markdown_table_1d <- function(rows, columns, digits = 6L) {
  x <- data.table::copy(data.table::as.data.table(rows))[, ..columns]
  for (column in names(x)) {
    if (is.numeric(x[[column]])) {
      x[, (column) := formatC(
        get(column), digits = digits, format = "f"
      )]
    }
  }
  header <- paste0("| ", paste(names(x), collapse = " | "), " |")
  rule <- paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |")
  body <- apply(x, 1L, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  })
  c(header, rule, body)
}

headline_checks <- paired_comparisons[
  model %in% c(
    "logit", "robit_t10", "robit_t5", "robit_t3", "cauchit",
    "cloglog", "loglog", "flexible_probit_df5",
    "probit_count_slopes", "probit_family_slopes",
    "probit_count_family_slopes"
  )
]
any_one_se_improvement <- any(
  headline_checks$improves_log_loss_by_one_clustered_se
)
summary_lines <- c(
  "# Quick development-only challenge-error link EDA",
  "",
  paste0(
    "The analysis uses **", format(nrow(selection), big.mark = ","),
    "** eligible, inventory-available adverse-call opportunities within ±",
    local_margin_limit_inches, " inches from **",
    length(development_game_ids), " development games**. " ,
    "`confirmation_games_used=0`."
  ),
  "",
  "## Result",
  "",
  if (any_one_se_improvement) {
    paste(
      "At least one alternative improved paired OOF log loss by more than",
      "one game-clustered SE; inspect the comparison table before treating",
      "the Gaussian link as adequate."
    )
  } else {
    paste(
      "No heavier-tailed, asymmetric, flexible-margin, count-slope, or",
      "pitch-family-slope alternative improved paired OOF log loss over",
      "the common-slope probit by one game-clustered SE. The observable",
      "choice curve is therefore compatible with the Gaussian composite-error",
      "link, while not proving latent normality."
    )
  },
  "",
  "### OOF model metrics",
  "",
  .quick_markdown_table_1d(
    model_metrics[, .(
      role,
      model,
      log_loss,
      brier,
      observed_rate,
      predicted_rate
    )],
    c(
      "role", "model", "log_loss", "brier", "observed_rate",
      "predicted_rate"
    )
  ),
  "",
  "### Paired differences from probit",
  "",
  "Positive delta log loss favors probit; negative favors the alternative.",
  "",
  .quick_markdown_table_1d(
    paired_comparisons[, .(
      role,
      model,
      delta_log_loss_vs_probit,
      delta_log_loss_game_clustered_se,
      delta_log_loss_in_clustered_se,
      improves_log_loss_by_one_clustered_se
    )],
    c(
      "role", "model", "delta_log_loss_vs_probit",
      "delta_log_loss_game_clustered_se",
      "delta_log_loss_in_clustered_se",
      "improves_log_loss_by_one_clustered_se"
    )
  ),
  "",
  "## What this can and cannot establish",
  "",
  paste(
    "Challenge/pass data reveal only a binary response curve. They cannot",
    "directly expose a latent residual for a QQ plot or Shapiro-Wilk test."
  ),
  paste(
    "The link represents the **composite** of sensory error, action noise,",
    "and residual threshold heterogeneity after observed controls; it does",
    "not identify the sensory distribution alone or split sensory from action",
    "variance."
  ),
  paste(
    "Normality is consequently a structural reduced-form assumption. Better",
    "held-out probit performance is evidence of adequacy, not verification."
  ),
  "",
  "## Simplifications relative to production",
  "",
  paste(
    "- This quick EDA uses ordinary fixed-effect GLMs with team indicators,",
    "not the production `mgcv::bam` smooths and batter or pitcher-catcher-dyad",
    "random effects."
  ),
  paste(
    "- Speed, modeled stake, inning, and score margin enter as standardized",
    "linear and quadratic terms; the production selection model uses richer",
    "smooth terms and a speed-aware benchmark."
  ),
  paste(
    "- All factor bases and standardizations use development covariates only.",
    "No confirmation rows, outcomes, or geometry are used."
  ),
  paste(
    "- The diagnostic is restricted to ±3 inches because that is the local",
    "window identifying the production effective width. It does not alter the",
    "running production analysis or frozen policy."
  ),
  "",
  "## Files",
  "",
  "- `oof_model_metrics.csv`: model-level OOF scores and calibration.",
  "- `paired_game_clustered_comparisons.csv`: paired alternatives versus probit.",
  "- `calibration_by_margin.csv`: observed and OOF-predicted rates by 0.25-inch bin.",
  "- `fit_diagnostics.csv`: fold-level convergence, warnings, and timing.",
  "- `challenge_error_link_eda_quick_1d.rds`: complete compact analysis object, including OOF predictions.",
  "- `challenge_error_link_eda_quick_1d.png`: visual calibration and score comparison."
)
writeLines(summary_lines, file.path(output_directory, "summary.md"))

message("Wrote quick challenge-error link EDA to ", output_directory)
