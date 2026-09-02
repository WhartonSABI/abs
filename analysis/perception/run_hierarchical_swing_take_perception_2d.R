# Location-aware 2D hierarchical Gaussian batter perception.
#
# Fast pilot:
#   Sys.setenv(SWING_TAKE_2D_STAGE = "fast")
#   source("analysis/perception/run_hierarchical_swing_take_perception_2d.R")
#
# Only after the fast pilot passes:
#   Sys.setenv(SWING_TAKE_2D_STAGE = "overnight")
#   source("analysis/perception/run_hierarchical_swing_take_perception_2d.R")

project_root <- if (requireNamespace("rprojroot", quietly = TRUE)) {
  rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd())
} else {
  normalizePath(getwd())
}
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

arguments <- commandArgs(trailingOnly = TRUE)
stage_environment <- tolower(Sys.getenv("SWING_TAKE_2D_STAGE", unset = ""))
stage <- if (nzchar(stage_environment)) {
  stage_environment
} else if (length(arguments)) {
  tolower(arguments[[1L]])
} else {
  "fast"
}
if (!stage %in% c("fast", "overnight")) {
  stop("SWING_TAKE_2D_STAGE must be fast or overnight")
}

source_dir <- file.path(
  project_root, "data", "processed", "swing_take_perception"
)
output_dir <- file.path(
  project_root, "data", "processed", "swing_take_perception_2d"
)
fit_dir <- file.path(output_dir, "fits")
dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)

write_small_csv <- function(x, path) {
  if (requireNamespace("readr", quietly = TRUE)) {
    write_csv_atomic(x, path)
  } else {
    data.table::fwrite(x, path)
  }
  path
}

write_large_table <- function(x, parquet_path) {
  if (requireNamespace("arrow", quietly = TRUE)) {
    write_parquet_atomic(x, parquet_path)
    return(parquet_path)
  }
  rds_path <- sub("[.]parquet$", ".rds", parquet_path)
  saveRDS(x, rds_path)
  rds_path
}

read_large_table <- function(parquet_path) {
  rds_path <- sub("[.]parquet$", ".rds", parquet_path)
  if (file.exists(parquet_path) && requireNamespace("arrow", quietly = TRUE)) {
    return(data.table::as.data.table(arrow::read_parquet(parquet_path)))
  }
  if (file.exists(rds_path)) return(data.table::as.data.table(readRDS(rds_path)))
  stop("Missing table: ", parquet_path, " (or its RDS fallback)")
}

env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = ifelse(default, "true", "false")))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be true or false")
  }
  value %in% c("true", "1", "yes")
}

choices <- read_large_table(file.path(source_dir, "swing_take_choices.parquet"))
split <- data.table::fread(
  file.path(source_dir, "game_split.csv"),
  colClasses = c(game_pk = "character")
)
choices[, game_key__ := as.character(game_pk)]
choices[split, on = c(game_key__ = "game_pk"), split := i.split]
if (anyNA(choices$split)) stop("Some 2D swing/take games lack a split")
train_full <- choices[split == "train"]
validation <- choices[split == "validation"]
if (length(intersect(
  unique(as.character(train_full$game_pk)),
  unique(as.character(validation$game_pk))
))) {
  stop("2D swing/take training and validation games overlap")
}

# The flexible model is a benchmark only. It proves x/z is useful, but its
# smooth surface does not itself estimate perception sigma.
flexible_fit_file <- file.path(fit_dir, "flexible_2d_benchmark.rds")
if (file.exists(flexible_fit_file)) {
  flexible_fit <- readRDS(flexible_fit_file)
} else {
  message("Fitting the quick held-out 2D location benchmark")
  flexible_fit <- fit_swing_take_2d_feasibility(train_full)
  saveRDS(flexible_fit, flexible_fit_file)
}
base_predictions <- read_large_table(file.path(
  source_dir, "swing_take_heldout_predictions.parquet"
))
key <- c("game_pk", "at_bat_number", "pitch_number")
if (anyDuplicated(base_predictions[, ..key]) ||
    nrow(base_predictions) != nrow(validation)) {
  stop("The saved held-out predictions do not match the 2D validation split")
}
scored_benchmark <- score_swing_take_2d_feasibility(
  flexible_fit, base_predictions
)
benchmark_probability <- scored_benchmark$prediction_2d_flexible
benchmark_loss <- swing_take_log_loss(
  scored_benchmark$swing, benchmark_probability
)
write_small_csv(
  data.table::data.table(
    validation_pitches = nrow(scored_benchmark),
    validation_games = data.table::uniqueN(scored_benchmark$game_pk),
    flexible_2d_log_loss = benchmark_loss
  ),
  file.path(output_dir, "flexible_2d_benchmark.csv")
)

controls <- if (stage == "fast") {
  list(
    chains = 2L, iter = 2000L, warmup = 1000L,
    training = subsample_swing_take_pilot(
      train_full, fraction = 0.2, min_per_batter = 100L, seed = 42L
    ),
    score_draws = 250L
  )
} else {
  list(
    chains = 4L, iter = 4000L, warmup = 2000L,
    training = train_full,
    score_draws = 500L
  )
}

prefix <- paste0("hierarchical_2d_", stage)
fit_file <- file.path(fit_dir, paste0(prefix, "_fit.rds"))
force_refit <- env_flag("SWING_TAKE_2D_FORCE_REFIT", FALSE)
message(
  "Fitting the ", stage, " 2D Gaussian perception model on ",
  format(nrow(controls$training), big.mark = ","), " pitches from ",
  data.table::uniqueN(controls$training$game_pk), " games"
)
message(
  "The 2D surface is orthogonal to ABS-boundary distance, so it cannot ",
  "replace the slope used to estimate sigma."
)
fit <- fit_hierarchical_swing_take_perception_2d(
  controls$training,
  chains = controls$chains,
  cores = controls$chains,
  iter = controls$iter,
  warmup = controls$warmup,
  seed = 42L,
  adapt_delta = 0.97,
  max_treedepth = 12L,
  file = fit_file,
  refresh = 100L,
  force_refit = force_refit
)

diagnostics <- swing_take_fit_diagnostics(fit)
summaries <- swing_take_2d_fit_summaries(fit)
write_small_csv(
  diagnostics, file.path(output_dir, paste0(prefix, "_diagnostics.csv"))
)
write_small_csv(
  summaries$population,
  file.path(output_dir, paste0(prefix, "_population_sigma.csv"))
)
write_large_table(
  summaries$players,
  file.path(output_dir, paste0(prefix, "_player_sigma.parquet"))
)
write_small_csv(
  summaries$strategy_limits,
  file.path(output_dir, paste0(prefix, "_strategy_limits.csv"))
)
write_small_csv(
  summaries$identifiability,
  file.path(output_dir, paste0(prefix, "_identifiability.csv"))
)

message(
  "Scoring ", format(nrow(scored_benchmark), big.mark = ","),
  " pitches from held-out games"
)
scored <- score_hierarchical_swing_take_2d(
  fit, scored_benchmark, ndraws = controls$score_draws, seed = 42L
)
if (anyDuplicated(scored[, ..key])) stop("2D hierarchical scores duplicate pitches")
write_large_table(
  scored,
  file.path(output_dir, paste0(prefix, "_heldout_predictions.parquet"))
)

validation_result <- validate_hierarchical_swing_take_2d(scored)
write_small_csv(
  validation_result$metrics,
  file.path(output_dir, paste0(prefix, "_predictive_gate.csv"))
)
write_small_csv(
  validation_result$by_game,
  file.path(output_dir, paste0(prefix, "_heldout_game_metrics.csv"))
)
write_small_csv(
  validation_result$distance_curve,
  file.path(output_dir, paste0(prefix, "_heldout_distance_curve.csv"))
)
write_small_csv(
  validation_result$cells,
  file.path(output_dir, paste0(prefix, "_heldout_2d_cells.csv"))
)
ggplot2::ggsave(
  file.path(output_dir, paste0(prefix, "_distance_curve.png")),
  plot_hierarchical_swing_take(list(curve = validation_result$distance_curve)),
  width = 10, height = 10, dpi = 180
)
ggplot2::ggsave(
  file.path(output_dir, paste0(prefix, "_observed_vs_predicted_heatmap.png")),
  plot_swing_take_2d_validation(validation_result),
  width = 11, height = 10, dpi = 180
)
ggplot2::ggsave(
  file.path(output_dir, paste0(prefix, "_error_heatmap.png")),
  plot_swing_take_2d_error(validation_result),
  width = 11, height = 6, dpi = 180
)

identifiability_pass <- all(summaries$identifiability$pass)
overall_gate <- data.table::data.table(
  predictive_gate_pass = validation_result$metrics$predictive_pass,
  diagnostics_pass = diagnostics$pass,
  maximum_absolute_sigma_strategy_or_surface_correlation = max(
    abs(summaries$identifiability$sigma_posterior_correlation)
  ),
  identifiability_tolerance = 0.8,
  identifiability_pass = identifiability_pass,
  pass = validation_result$metrics$predictive_pass &&
    diagnostics$pass && identifiability_pass
)
write_small_csv(
  overall_gate, file.path(output_dir, paste0(prefix, "_overall_gate.csv"))
)

print(diagnostics)
print(summaries$population)
print(validation_result$metrics)
print(overall_gate)
if (isTRUE(overall_gate$pass[[1L]])) {
  message(
    "The ", stage, " 2D perception model passed. Review the heatmaps ",
    "before moving to challenge validation."
  )
} else {
  message(
    "The ", stage, " 2D perception model failed at least one gate. ",
    "Do not call sigma eyesight or run the human-eyes MDP."
  )
}
message("Artifacts written under: ", output_dir)
