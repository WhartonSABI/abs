#!/usr/bin/env Rscript

# Export the compact, versioned inputs needed to reproduce the final analysis.

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(digest)
  library(targets)
})

find_project_root <- function(path = getwd()) {
  current <- normalizePath(path, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "_targets.R"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find the project root containing _targets.R", call. = FALSE)
    }
    current <- parent
  }
}

project_root <- find_project_root()
setwd(project_root)

required_targets <- c(
  "pitch_ledger", "history_statcast", "re_model",
  "challenge_events", "savant"
)
available_targets <- targets::tar_manifest(fields = name)$name
missing_targets <- setdiff(required_targets, available_targets)
if (length(missing_targets)) {
  stop(
    "The targets graph is missing: ", paste(missing_targets, collapse = ", "),
    call. = FALSE
  )
}

missing_objects <- required_targets[
  !vapply(required_targets, targets::tar_exist_objects, logical(1))
]
if (length(missing_objects)) {
  stop(
    "Build the following targets before exporting: ",
    paste(missing_objects, collapse = ", "), call. = FALSE
  )
}

output_directory <- file.path(project_root, "data", "analysis")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

pitch_ledger <- as.data.table(targets::tar_read(pitch_ledger))
history_statcast <- as.data.table(targets::tar_read(history_statcast))
re_model <- targets::tar_read(re_model)
challenge_events <- as.data.table(targets::tar_read(challenge_events))
savant <- as.data.table(targets::tar_read(savant))

history_columns <- c(
  "game_pk", "game_date", "inning", "inning_topbot", "outs_when_up",
  "balls", "strikes", "on_1b", "on_2b", "on_3b", "events",
  "description", "bat_score", "fld_score", "post_bat_score",
  "post_fld_score", "home_score", "away_score", "post_home_score",
  "post_away_score", "at_bat_number", "pitch_number"
)
missing_history_columns <- setdiff(history_columns, names(history_statcast))
if (length(missing_history_columns)) {
  stop(
    "Historical RE input is missing columns: ",
    paste(missing_history_columns, collapse = ", "), call. = FALSE
  )
}

files <- c(
  challenge_events = file.path(output_directory, "challenge_events.parquet"),
  history_re_inputs = file.path(output_directory, "history_re_inputs.parquet"),
  pitch_ledger = file.path(output_directory, "pitch_ledger.parquet"),
  re288_model = file.path(output_directory, "re288_model.rds"),
  savant_challenges = file.path(output_directory, "savant_challenges.parquet")
)

arrow::write_parquet(
  challenge_events, files[["challenge_events"]],
  compression = "zstd", compression_level = 19
)
arrow::write_parquet(
  history_statcast[, ..history_columns], files[["history_re_inputs"]],
  compression = "zstd", compression_level = 19
)
arrow::write_parquet(
  pitch_ledger, files[["pitch_ledger"]],
  compression = "zstd", compression_level = 19
)
saveRDS(re_model, files[["re288_model"]], compress = "xz")
arrow::write_parquet(
  savant, files[["savant_challenges"]],
  compression = "zstd", compression_level = 19
)

checksums <- vapply(
  files, digest::digest, character(1), algo = "sha256", file = TRUE,
  serialize = FALSE
)
checksum_lines <- paste(checksums, basename(files))
writeLines(checksum_lines, file.path(output_directory, "SHA256SUMS"))

sizes_mb <- round(file.info(files)$size / 1024^2, 2)
message(
  "Wrote public analysis inputs (", round(sum(sizes_mb), 2), " MB):\n",
  paste(sprintf("  %-30s %7.2f MB", basename(files), sizes_mb), collapse = "\n")
)
