# Hierarchical batter perception from swing/take choices only.
#
# Run from the project root in an R console:
#   Sys.setenv(SWING_TAKE_STAGE = "fast")
#   source("analysis/perception/run_hierarchical_swing_take_perception.R")
#
# The strategy-adjusted revision is:
#   Sys.setenv(SWING_TAKE_STAGE = "fast_strategy")
#   source("analysis/perception/run_hierarchical_swing_take_perception.R")
#
# If that stage passes, its longer run is:
#   Sys.setenv(SWING_TAKE_STAGE = "overnight_strategy")
#   source("analysis/perception/run_hierarchical_swing_take_perception.R")

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
stage_environment <- tolower(Sys.getenv("SWING_TAKE_STAGE", unset = ""))
stage <- if (nzchar(stage_environment)) {
  stage_environment
} else if (length(arguments)) {
  tolower(arguments[[1L]])
} else {
  "fast"
}
valid_stages <- c("fast", "overnight", "fast_strategy", "overnight_strategy")
if (!stage %in% valid_stages) {
  stop("SWING_TAKE_STAGE must be one of: ", paste(valid_stages, collapse = ", "))
}
model_variant <- if (grepl("strategy$", stage)) "strategy_limits" else "simple"
run_size <- if (grepl("^fast", stage)) "fast" else "overnight"

output_dir <- file.path(
  project_root, "data", "processed", "swing_take_perception"
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

choices <- read_large_table(file.path(output_dir, "swing_take_choices.parquet"))
split <- data.table::fread(
  file.path(output_dir, "game_split.csv"),
  colClasses = c(game_pk = "character")
)
choices[, game_key__ := as.character(game_pk)]
choices[split, on = c(game_key__ = "game_pk"), split := i.split]
if (anyNA(choices$split)) stop("Some swing/take games lack a split")
train_full <- choices[split == "train"]
validation <- choices[split == "validation"]
if (length(intersect(
  unique(as.character(train_full$game_pk)),
  unique(as.character(validation$game_pk))
))) {
  stop("Swing/take training and validation games overlap")
}

controls <- if (run_size == "fast") {
  list(
    chains = 2L, iter = 2000L, warmup = 1000L,
    training = subsample_swing_take_pilot(
      train_full, fraction = 0.2, min_per_batter = 100L, seed = 42L
    ),
    score_draws = 500L
  )
} else {
  list(
    chains = 4L, iter = 4000L, warmup = 2000L,
    training = train_full,
    score_draws = 1000L
  )
}

prefix <- paste0("hierarchical_", stage)
fit_file <- file.path(fit_dir, paste0(prefix, "_fit.rds"))
force_refit <- env_flag("SWING_TAKE_FORCE_REFIT", FALSE)
message(
  "Fitting the ", stage, " hierarchical swing/take model on ",
  format(nrow(controls$training), big.mark = ","), " pitches from ",
  data.table::uniqueN(controls$training$game_pk), " games"
)
message(
  "Challenge decisions and overturn outcomes are not present in this model."
)

fit <- fit_hierarchical_swing_take_perception(
  controls$training,
  chains = controls$chains,
  cores = controls$chains,
  iter = controls$iter,
  warmup = controls$warmup,
  seed = 42L,
  distance_bin_width = 0.25,
  adapt_delta = 0.97,
  max_treedepth = 12L,
  model_variant = model_variant,
  file = fit_file,
  refresh = 100L,
  force_refit = force_refit
)

diagnostics <- swing_take_fit_diagnostics(fit)
summaries <- swing_take_fit_summaries(fit)
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
  summaries$identifiability,
  file.path(output_dir, paste0(prefix, "_identifiability.csv"))
)
if (nrow(summaries$strategy_limits)) {
  write_small_csv(
    summaries$strategy_limits,
    file.path(output_dir, paste0(prefix, "_strategy_limits.csv"))
  )
}

message(
  "Scoring ", format(nrow(validation), big.mark = ","),
  " pitches from games the model did not train on"
)
feasibility_predictions <- read_large_table(file.path(
  output_dir, "swing_take_heldout_predictions.parquet"
))
key <- c("game_pk", "at_bat_number", "pitch_number")
if (anyDuplicated(feasibility_predictions[, ..key])) {
  stop("Existing held-out swing/take scores contain duplicate pitches")
}
expected_keys <- unique(validation[, ..key])
matched_keys <- merge(
  expected_keys, feasibility_predictions[, ..key], by = key, all = FALSE
)
if (nrow(matched_keys) != nrow(expected_keys) ||
    nrow(feasibility_predictions) != nrow(expected_keys)) {
  stop("Existing feasibility scores do not match the held-out game split")
}
scored <- score_hierarchical_swing_take(
  fit, feasibility_predictions, ndraws = controls$score_draws, seed = 42L
)
if (anyDuplicated(scored[, ..key])) stop("Hierarchical scores duplicate pitches")
write_large_table(
  scored,
  file.path(output_dir, paste0(prefix, "_heldout_predictions.parquet"))
)

validation_result <- validate_hierarchical_swing_take(scored)
write_small_csv(
  validation_result$metrics,
  file.path(output_dir, paste0(prefix, "_predictive_gate.csv"))
)
write_small_csv(
  validation_result$by_game,
  file.path(output_dir, paste0(prefix, "_heldout_game_metrics.csv"))
)
write_small_csv(
  validation_result$curve,
  file.path(output_dir, paste0(prefix, "_heldout_location_curve.csv"))
)
ggplot2::ggsave(
  file.path(output_dir, paste0(prefix, "_heldout_location_curve.png")),
  plot_hierarchical_swing_take(validation_result),
  width = 10, height = 10, dpi = 180
)

identifiability_pass <- all(summaries$identifiability$pass)
overall_gate <- data.table::data.table(
  predictive_gate_pass = validation_result$metrics$predictive_pass,
  diagnostics_pass = diagnostics$pass,
  maximum_absolute_sigma_strategy_correlation = max(
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
    "The ", stage, " swing/take perception model passed. Its sigma is a ",
    "credible behavioral transition width; challenge validation is still next."
  )
} else {
  message(
    "The ", stage, " swing/take perception model did not pass every gate. ",
    "Do not call sigma eyesight until the failed check is understood."
  )
}
message("Artifacts written under: ", output_dir)
