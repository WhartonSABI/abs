read_abs_exclusions <- function(path = "config/exclusions.csv") {
  if (!file.exists(path)) return(data.table::data.table())
  x <- data.table::fread(path, na.strings = c("", "NA"))
  if (!nrow(x)) return(x)
  stop_if_missing_columns(x, c("game_pk", "at_bat_number", "pitch_number", "reason"),
    "ABS exclusions")
  x
}

apply_abs_eligibility <- function(pitches, exclusions = read_abs_exclusions()) {
  x <- data.table::copy(data.table::as.data.table(pitches))
  if ("statcast_identity_matched" %in% names(x)) {
    x[, `:=`(
      abs_eligible = statcast_identity_matched & tracking_available,
      exclusion_reason = data.table::fifelse(
        !statcast_identity_matched, "statcast_identity_mismatch",
        data.table::fifelse(!tracking_available, "missing_tracking", NA_character_)
      )
    )]
  } else {
    x[, `:=`(abs_eligible = TRUE, exclusion_reason = NA_character_)]
  }
  if (nrow(exclusions)) {
    flags <- exclusions[, .(game_pk, at_bat_number, pitch_number, exclusion_reason = reason)]
    x <- merge(x, flags, by = c("game_pk", "at_bat_number", "pitch_number"),
      all.x = TRUE, suffixes = c("", "_manual"), sort = FALSE)
    x[!is.na(exclusion_reason_manual), `:=`(
      abs_eligible = FALSE,
      exclusion_reason = exclusion_reason_manual
    )]
    x[, exclusion_reason_manual := NULL]
  }
  x[, correctable_opportunity := correctable_opportunity & abs_eligible]
  x[]
}
