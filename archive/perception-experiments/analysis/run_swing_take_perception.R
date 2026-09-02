# Feasibility screen for an independent batter perception signal.
# This stage intentionally stops before estimating eyesight sigma.

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

output_dir <- file.path(
  project_root, "data", "processed", "swing_take_perception"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write_large_table <- function(x, parquet_path) {
  if (requireNamespace("arrow", quietly = TRUE)) {
    write_parquet_atomic(x, parquet_path)
    return(parquet_path)
  }
  rds_path <- sub("[.]parquet$", ".rds", parquet_path)
  saveRDS(x, rds_path)
  rds_path
}

write_small_csv <- function(x, path) {
  if (requireNamespace("readr", quietly = TRUE)) {
    write_csv_atomic(x, path)
  } else {
    data.table::fwrite(x, path)
  }
  path
}

paths <- list.files(
  file.path(project_root, "data", "raw", "statcast"),
  pattern = "[.]csv$",
  full.names = TRUE
)
if (!length(paths)) stop("No current-season Statcast files were found")

message("Reading all tracked swing/take pitches")
raw <- read_swing_take_statcast(paths)
raw[, classified_decision := classify_swing_take(description)]
coverage <- raw[, .(
  pitches = .N,
  classified_swing_take = sum(!is.na(classified_decision)),
  classified_rate = mean(!is.na(classified_decision)),
  tracking_available = sum(
    is.finite(plate_x) & is.finite(plate_z) &
      is.finite(sz_top) & is.finite(sz_bot)
  ),
  tracking_rate = mean(
    is.finite(plate_x) & is.finite(plate_z) &
      is.finite(sz_top) & is.finite(sz_bot)
  ),
  games = uniqueN(game_pk),
  batters = uniqueN(batter)
)]
write_small_csv(
  coverage,
  file.path(output_dir, "swing_take_source_coverage.csv")
)

choices <- prepare_swing_take_choices(raw, boundary_inches = 6)
if (nrow(choices) < 10000L) stop("Too few tracked swing/take choices")
write_large_table(
  choices,
  file.path(output_dir, "swing_take_choices.parquet")
)

split_file <- file.path(output_dir, "game_split.csv")
if (file.exists(split_file)) {
  split <- data.table::fread(split_file, colClasses = c(game_pk = "character"))
} else {
  split <- deterministic_swing_take_split(choices, 0.8, 42L)
  write_small_csv(split, split_file)
}
if (length(intersect(
  split[split == "train", game_pk],
  split[split == "validation", game_pk]
))) {
  stop("Swing/take train and validation games overlap")
}
choices[split, on = c(game_key__ = "game_pk"), split := i.split]
if (anyNA(choices$split)) stop("Some swing/take games were not assigned a split")
train <- choices[split == "train"]
validation <- choices[split == "validation"]

message(
  "Fitting the quick population model on ",
  format(nrow(train), big.mark = ","), " pitches"
)
fits <- fit_swing_take_feasibility(train)
saveRDS(fits, file.path(output_dir, "swing_take_feasibility_fits.rds"))

scored <- score_swing_take_feasibility(fits, validation)
write_large_table(
  scored,
  file.path(output_dir, "swing_take_heldout_predictions.parquet")
)
result <- validate_swing_take_signal(scored)
write_small_csv(result$metrics, file.path(output_dir, "feasibility_gate.csv"))
write_small_csv(result$by_game, file.path(output_dir, "heldout_game_metrics.csv"))
write_small_csv(result$curve, file.path(output_dir, "heldout_location_curve.csv"))
write_small_csv(result$monotonicity, file.path(output_dir, "location_monotonicity.csv"))
write_small_csv(result$calibration, file.path(output_dir, "heldout_calibration.csv"))

smooth_table <- data.table::as.data.table(
  summary(fits$edge)$s.table,
  keep.rownames = "smooth"
)
write_small_csv(smooth_table, file.path(output_dir, "location_smooth_tests.csv"))

ggplot2::ggsave(
  file.path(output_dir, "heldout_swing_rate_by_distance.png"),
  plot_swing_take_signal(result),
  width = 10, height = 10, dpi = 180
)
ggplot2::ggsave(
  file.path(output_dir, "heldout_swing_take_calibration.png"),
  plot_swing_take_calibration(result),
  width = 8, height = 7, dpi = 180
)

print(coverage)
print(result$metrics)
print(result$monotonicity)
if (isTRUE(result$metrics$pass[[1L]])) {
  message(
    "Swing/take location signal passed the feasibility screen. ",
    "A hierarchical batter pilot is justified; sigma is not estimated yet."
  )
} else {
  message(
    "Swing/take location signal did not pass. Do not interpret swing behavior ",
    "as a batter perception measurement without revising the design."
  )
}
message("Artifacts written under: ", output_dir)
