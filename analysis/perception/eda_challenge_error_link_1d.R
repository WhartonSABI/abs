# Development-only EDA for the latent challenge-error link.
#
# Binary challenge/pass data do not expose latent errors for a QQ or normality
# test.  This script instead checks the observable implication of a Gaussian
# composite error: conditional challenge propensities should follow a probit
# transition in the role-oriented signed margin.  Identical game-held-out
# models are compared under probit, logit, Student-t(3) robit, complementary
# log-log, and a flexible-margin probit benchmark.

project_root <- rprojroot::find_root(
  rprojroot::has_file("_targets.R"), path = getwd()
)
source(file.path(project_root, "config", "project.R"))
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

if (!requireNamespace("mgcv", quietly = TRUE) ||
    !requireNamespace("ggplot2", quietly = TRUE) ||
    !requireNamespace("gridExtra", quietly = TRUE)) {
  stop("This EDA requires mgcv, ggplot2, and gridExtra", call. = FALSE)
}

output_directory <- file.path(
  project_root, "data", "processed", "perception",
  "challenge_error_link_eda_1d"
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

seed <- 20260826L
folds <- 5L
local_limit_inches <- 3
margin_bin_width_inches <- 0.15

robit_link <- function(df = 3) {
  force(df)
  structure(list(
    linkfun = function(mu) stats::qt(mu, df = df),
    linkinv = function(eta) stats::pt(eta, df = df),
    mu.eta = function(eta) pmax(
      stats::dt(eta, df = df), .Machine$double.eps
    ),
    valideta = function(eta) TRUE,
    name = paste0("robit_t", df)
  ), class = "link-glm")
}

model_specifications <- list(
  probit = list(family = stats::binomial(link = "probit"), flexible = FALSE),
  logit = list(family = stats::binomial(link = "logit"), flexible = FALSE),
  robit_t3 = list(
    family = stats::binomial(link = robit_link(3)), flexible = FALSE
  ),
  cloglog = list(
    family = stats::binomial(link = "cloglog"), flexible = FALSE
  ),
  flexible_probit = list(
    family = stats::binomial(link = "probit"), flexible = TRUE
  )
)

predict_selection_link <- function(fit, rows, specification, random_columns) {
  scored <- .revealed_selection_score_data_1d(rows, specification)
  terms <- stats::predict(fit, newdata = scored$data, type = "terms")
  if (is.null(dim(terms))) terms <- matrix(terms, nrow = nrow(scored$data))
  eta <- rep(unname(stats::coef(fit)[["(Intercept)"]]), nrow(scored$data))
  if (ncol(terms)) {
    for (column in random_columns) {
      label <- paste0("s(", column, ")")
      if (label %in% colnames(terms)) {
        terms[scored$unseen[[column]], label] <- 0
      }
    }
    eta <- eta + rowSums(terms)
  }
  probability <- fit$family$linkinv(eta)
  list(
    probability = pmin(1 - 1e-12, pmax(1e-12, probability)),
    eta = eta
  )
}

clustered_mean <- function(value, game) {
  value <- as.numeric(value)
  game <- as.character(game)
  estimate <- mean(value)
  clustered <- data.table::data.table(
    game_pk = game, residual = value - estimate
  )[, .(residual_sum = sum(residual)), by = game_pk]
  games <- nrow(clustered)
  se <- if (games >= 2L) {
    sqrt(
      games / (games - 1L) *
        sum(clustered$residual_sum^2) / length(value)^2
    )
  } else {
    NA_real_
  }
  list(estimate = estimate, se = se, games = games)
}

message("Loading the immutable development split")
pitch_ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
re_model <- targets::tar_read(re_model)
snapshot <- validate_fixed_clock_confirmation_snapshot_1d(pitch_ledger)
development <- snapshot$split$development
selection <- build_revealed_challenge_selection_1d(
  development, re_model = re_model, require_positive_inventory = TRUE
)
selection <- selection[abs(role_margin_inches) <= local_limit_inches]
if (!nrow(selection)) stop("The local selection sample is empty", call. = FALSE)

assignment <- continuous_game_folds(
  sort(unique(as.character(selection$game_pk))), folds = folds, seed = seed
)
selection[, game_pk := as.character(game_pk)]
selection[assignment, fold := i.fold, on = "game_pk"]
if (anyNA(selection$fold)) stop("Fold assignment is incomplete", call. = FALSE)

sample_summary <- selection[, .(
  rows = .N,
  games = data.table::uniqueN(game_pk),
  challenges = sum(challenged),
  challenge_rate = mean(challenged),
  minimum_margin_inches = min(role_margin_inches),
  maximum_margin_inches = max(role_margin_inches)
), by = role]
data.table::fwrite(
  sample_summary, file.path(output_directory, "sample_summary.csv")
)

prediction_parts <- list()
fit_parts <- list()
part_index <- 0L
fit_index <- 0L

for (fold_id in seq_len(folds)) {
  for (role_value in revealed_challenge_selection_1d_roles()) {
    train <- selection[fold != fold_id & role == role_value]
    heldout <- selection[fold == fold_id & role == role_value]
    base_specification <- .revealed_selection_training_spec_1d(
      train, full_support = FALSE
    )
    base_formula <- .revealed_selection_formula_1d(base_specification)

    for (model_name in names(model_specifications)) {
      model <- model_specifications[[model_name]]
      formula_bundle <- base_formula
      if (isTRUE(model$flexible)) {
        formula_bundle$formula <- stats::update(
          formula_bundle$formula,
          . ~ . - role_margin_inches +
            s(role_margin_inches, k = 10, bs = "cr")
        )
        formula_bundle$formula_text <- paste(
          base_formula$formula_text,
          "with linear role margin replaced by s(role_margin_inches, k=10)"
        )
      }
      message(
        "link EDA fold ", fold_id, "/", folds, " ", role_value,
        " ", model_name
      )
      fit_warnings <- character()
      started <- proc.time()[["elapsed"]]
      fit <- withCallingHandlers(
        mgcv::bam(
          formula_bundle$formula,
          data = base_specification$data,
          family = model$family,
          method = "fREML",
          discrete = TRUE,
          nthreads = 1L,
          gc.level = 1L
        ),
        warning = function(condition) {
          fit_warnings <<- unique(c(fit_warnings, conditionMessage(condition)))
          invokeRestart("muffleWarning")
        }
      )
      elapsed <- proc.time()[["elapsed"]] - started
      prediction <- predict_selection_link(
        fit, heldout, base_specification, formula_bundle$random_columns
      )
      part_index <- part_index + 1L
      prediction_parts[[part_index]] <- heldout[, .(
        game_pk, pitch_order, role, fold, challenged,
        role_margin_inches, count_state, pitch_family_coarse
      )][, `:=`(
        model = model_name,
        probability = prediction$probability,
        linear_predictor = prediction$eta
      )]
      beta_margin <- if (!isTRUE(model$flexible)) {
        unname(stats::coef(fit)[["role_margin_inches"]])
      } else {
        NA_real_
      }
      fit_index <- fit_index + 1L
      fit_parts[[fit_index]] <- data.table::data.table(
        fold = fold_id,
        role = role_value,
        model = model_name,
        training_rows = nrow(train),
        heldout_rows = nrow(heldout),
        converged = isTRUE(fit$converged),
        margin_coefficient = beta_margin,
        probit_width_inches = if (
          identical(model_name, "probit") &&
            is.finite(beta_margin) && beta_margin > 0
        ) 1 / beta_margin else NA_real_,
        elapsed_seconds = elapsed,
        warning_count = length(fit_warnings),
        warnings = paste(fit_warnings, collapse = " | "),
        formula = paste(deparse(formula_bundle$formula), collapse = " ")
      )
      rm(fit)
      invisible(gc(FALSE))
    }
  }
}

oof <- data.table::rbindlist(prediction_parts, use.names = TRUE)
fit_diagnostics <- data.table::rbindlist(fit_parts, use.names = TRUE)
oof[, `:=`(
  log_loss = -(
    challenged * log(probability) +
      (1 - challenged) * log1p(-probability)
  ),
  brier = (challenged - probability)^2
)]

model_metrics <- oof[, {
  log_score <- clustered_mean(log_loss, game_pk)
  brier_score <- clustered_mean(brier, game_pk)
  calibration <- clustered_mean(probability - challenged, game_pk)
  .(
    rows = .N,
    games = log_score$games,
    challenges = sum(challenged),
    observed_rate = mean(challenged),
    predicted_rate = mean(probability),
    log_loss = log_score$estimate,
    log_loss_game_clustered_se = log_score$se,
    brier = brier_score$estimate,
    brier_game_clustered_se = brier_score$se,
    calibration_error = calibration$estimate,
    calibration_game_clustered_se = calibration$se
  )
}, by = .(role, model)]

keys <- c("game_pk", "pitch_order", "role", "fold")
reference <- oof[model == "probit", c(
  keys, "log_loss", "brier", "probability"
), with = FALSE]
data.table::setnames(
  reference,
  c("log_loss", "brier", "probability"),
  c("probit_log_loss", "probit_brier", "probit_probability")
)
paired <- merge(oof[model != "probit"], reference, by = keys, sort = FALSE)
paired_comparisons <- paired[, {
  log_difference <- clustered_mean(log_loss - probit_log_loss, game_pk)
  brier_difference <- clustered_mean(brier - probit_brier, game_pk)
  .(
    rows = .N,
    games = log_difference$games,
    delta_log_loss_vs_probit = log_difference$estimate,
    delta_log_loss_game_clustered_se = log_difference$se,
    delta_log_loss_lower_95 = log_difference$estimate -
      1.96 * log_difference$se,
    delta_log_loss_upper_95 = log_difference$estimate +
      1.96 * log_difference$se,
    improvement_in_clustered_se = -log_difference$estimate /
      log_difference$se,
    delta_brier_vs_probit = brier_difference$estimate,
    delta_brier_game_clustered_se = brier_difference$se
  )
}, by = .(role, model)]

bin_count <- as.integer(round(2 * local_limit_inches / margin_bin_width_inches))
oof[, margin_bin_index := pmin(
  bin_count - 1L,
  pmax(0L, as.integer(floor(
    (role_margin_inches + local_limit_inches) / margin_bin_width_inches
  )))
)]
margin_calibration <- oof[, .(
  rows = .N,
  challenges = sum(challenged),
  margin_bin_center_inches = -local_limit_inches +
    (margin_bin_index[[1L]] + 0.5) * margin_bin_width_inches,
  mean_margin_inches = mean(role_margin_inches),
  observed_rate = mean(challenged),
  predicted_rate = mean(probability),
  observed_binomial_se = sqrt(
    mean(challenged) * (1 - mean(challenged)) / .N
  )
), by = .(role, model, margin_bin_index)]
margin_calibration[, residual := observed_rate - predicted_rate]

prediction_calibration <- oof[, {
  breaks <- unique(stats::quantile(
    probability, probs = seq(0, 1, length.out = 11L),
    na.rm = TRUE, names = FALSE, type = 8
  ))
  if (length(breaks) < 3L) {
    calibration_bin <- rep(1L, .N)
  } else {
    calibration_bin <- findInterval(
      probability, breaks[-c(1L, length(breaks))]
    ) + 1L
  }
  data.table::data.table(
    probability = probability,
    challenged = challenged,
    calibration_bin = calibration_bin
  )[, .(
    rows = .N,
    predicted_rate = mean(probability),
    observed_rate = mean(challenged),
    observed_binomial_se = sqrt(
      mean(challenged) * (1 - mean(challenged)) / .N
    )
  ), by = calibration_bin]
}, by = .(role, model)]

data.table::fwrite(
  model_metrics, file.path(output_directory, "oof_link_metrics.csv")
)
data.table::fwrite(
  paired_comparisons,
  file.path(output_directory, "paired_link_comparisons.csv")
)
data.table::fwrite(
  margin_calibration,
  file.path(output_directory, "margin_bin_calibration.csv")
)
data.table::fwrite(
  prediction_calibration,
  file.path(output_directory, "prediction_calibration.csv")
)
data.table::fwrite(
  fit_diagnostics, file.path(output_directory, "fit_diagnostics.csv")
)
saveRDS(
  list(
    schema = "challenge_error_link_eda_1d_v1",
    development_games = sort(unique(as.character(development$game_pk))),
    confirmation_games_used = character(),
    fold_assignment = assignment,
    sample_summary = sample_summary,
    fit_diagnostics = fit_diagnostics,
    model_metrics = model_metrics,
    paired_comparisons = paired_comparisons,
    margin_calibration = margin_calibration,
    prediction_calibration = prediction_calibration,
    oof_predictions = oof,
    assumptions = c(
      "latent errors are not observed and were not normality-tested directly",
      "all link comparisons use identical development-game folds and covariates",
      "the current width model is evaluated on role-oriented margins within 3 inches"
    )
  ),
  file.path(output_directory, "challenge_error_link_eda.rds")
)

model_labels <- c(
  probit = "Probit (Gaussian)",
  logit = "Logit",
  robit_t3 = "Robit t(3)",
  cloglog = "Complementary log-log",
  flexible_probit = "Flexible-margin probit"
)
plot_margin <- ggplot2::ggplot(
  margin_calibration,
  ggplot2::aes(x = margin_bin_center_inches)
) +
  ggplot2::geom_point(
    data = unique(margin_calibration[, .(
      role, margin_bin_index, margin_bin_center_inches,
      observed_rate, observed_binomial_se
    )]),
    ggplot2::aes(y = observed_rate), color = "black", size = 1.4
  ) +
  ggplot2::geom_errorbar(
    data = unique(margin_calibration[, .(
      role, margin_bin_index, margin_bin_center_inches,
      observed_rate, observed_binomial_se
    )]),
    ggplot2::aes(
      ymin = pmax(0, observed_rate - 1.96 * observed_binomial_se),
      ymax = pmin(1, observed_rate + 1.96 * observed_binomial_se)
    ),
    color = "grey45", width = 0, linewidth = 0.25
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = predicted_rate, color = model_labels[model], group = model
    ),
    linewidth = 0.7
  ) +
  ggplot2::facet_wrap(~role, nrow = 1L) +
  ggplot2::scale_color_brewer(palette = "Dark2", name = NULL) +
  ggplot2::labs(
    title = "Development-only challenge transition",
    subtitle = "Points are observed 0.15-inch-bin rates; lines average held-out predictions",
    x = "Role-oriented true margin (inches; positive means the call is wrong)",
    y = "Challenge probability"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(legend.position = "bottom")

probit_residual <- margin_calibration[model == "probit"]
plot_residual <- ggplot2::ggplot(
  probit_residual,
  ggplot2::aes(x = margin_bin_center_inches, y = residual)
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey50") +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = residual - 1.96 * observed_binomial_se,
      ymax = residual + 1.96 * observed_binomial_se
    ),
    fill = "#56B4E9", alpha = 0.18
  ) +
  ggplot2::geom_line(color = "#0072B2", linewidth = 0.65) +
  ggplot2::facet_wrap(~role, nrow = 1L) +
  ggplot2::labs(
    title = "Probit calibration residual by true margin",
    subtitle = "Observed minus held-out predicted rate; ribbon is a descriptive binomial 95% band",
    x = "Role-oriented true margin (inches)",
    y = "Observed - predicted"
  ) +
  ggplot2::theme_minimal(base_size = 10)

comparison_plot_data <- data.table::copy(paired_comparisons)
comparison_plot_data[, model_label := factor(
  model_labels[model], levels = rev(unname(model_labels[names(model_labels) != "probit"]))
)]
plot_comparison <- ggplot2::ggplot(
  comparison_plot_data,
  ggplot2::aes(
    x = delta_log_loss_vs_probit, y = model_label, color = role
  )
) +
  ggplot2::geom_vline(xintercept = 0, color = "grey50") +
  ggplot2::geom_errorbarh(
    ggplot2::aes(
      xmin = delta_log_loss_lower_95,
      xmax = delta_log_loss_upper_95
    ),
    height = 0.2, linewidth = 0.5
  ) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_brewer(palette = "Set1", name = "Role") +
  ggplot2::labs(
    title = "Paired game-held-out log loss relative to probit",
    subtitle = "Negative favors the alternative; intervals use whole-game clustering",
    x = "Alternative log loss - probit log loss",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(legend.position = "bottom")

grDevices::png(
  file.path(output_directory, "challenge_error_link_eda.png"),
  width = 1800, height = 2100, res = 160
)
gridExtra::grid.arrange(
  plot_margin, plot_residual, plot_comparison,
  ncol = 1L, heights = c(1.2, 1, 0.9)
)
grDevices::dev.off()

best <- model_metrics[order(role, log_loss), .SD[1L], by = role]
comparison_lines <- paired_comparisons[order(role, delta_log_loss_vs_probit)]
summary_lines <- c(
  "# Development-only challenge-error link EDA",
  "",
  paste0(
    "This analysis uses ", format(nrow(selection), big.mark = ","),
    " local adverse-call opportunities from ",
    data.table::uniqueN(selection$game_pk),
    " development games. Confirmation games used: 0."
  ),
  "",
  "Latent errors are unobserved, so this is an observable link/calibration check, not a direct normality test.",
  "",
  "## Best held-out link by role",
  "",
  vapply(seq_len(nrow(best)), function(i) sprintf(
    "- %s: %s (log loss %.8f)",
    best$role[[i]], model_labels[[best$model[[i]]]], best$log_loss[[i]]
  ), character(1L)),
  "",
  "## Paired differences from probit",
  "",
  vapply(seq_len(nrow(comparison_lines)), function(i) sprintf(
    "- %s, %s: delta %.8g (clustered SE %.8g; %.2f SE improvement)",
    comparison_lines$role[[i]],
    model_labels[[comparison_lines$model[[i]]]],
    comparison_lines$delta_log_loss_vs_probit[[i]],
    comparison_lines$delta_log_loss_game_clustered_se[[i]],
    comparison_lines$improvement_in_clustered_se[[i]]
  ), character(1L)),
  "",
  "Interpretation rule: a materially negative paired difference indicates that the Gaussian-CDF propensity is not the best observable description. Failure to improve on probit supports adequacy, not proof of latent normality."
)
writeLines(summary_lines, file.path(output_directory, "summary.md"))

message("Wrote challenge-error link EDA to ", output_directory)
