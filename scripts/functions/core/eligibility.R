read_abs_exclusions <- function(path = "data/reference/exclusions.csv") {
  if (!file.exists(path)) return(data.table::data.table())
  x <- data.table::fread(path, na.strings = c("", "NA"))
  if (!nrow(x)) return(x)
  stop_if_missing_columns(x, c("game_pk", "at_bat_number", "pitch_number", "reason"),
    "ABS exclusions")
  x
}

read_abs_game_exclusions <- function(
  path = "data/reference/game_exclusions.csv"
) {
  if (!file.exists(path)) return(data.table::data.table())
  x <- data.table::fread(path, na.strings = c("", "NA"))
  if (!nrow(x)) return(x)
  stop_if_missing_columns(x, c("game_pk", "reason", "source"),
    "ABS game exclusions")
  if (anyDuplicated(x$game_pk)) {
    stop("ABS game exclusions must contain one row per game_pk")
  }
  x
}

apply_abs_eligibility <- function(
  pitches,
  exclusions = read_abs_exclusions(),
  game_exclusions = read_abs_game_exclusions()
) {
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
  if ("pitcher_primary_position_code" %in% names(x)) {
    position_code <- trimws(as.character(x$pitcher_primary_position_code))
    position_player_pitching <- !is.na(position_code) & nzchar(position_code) &
      !position_code %in% c("1", "Y")
    x[position_player_pitching, `:=`(
      abs_eligible = FALSE,
      exclusion_reason = "position_player_pitching"
    )]
  }
  if ("same_pitch_non_abs_review" %in% names(x)) {
    x[same_pitch_non_abs_review %in% TRUE, `:=`(
      abs_eligible = FALSE,
      exclusion_reason = "same_pitch_non_abs_review"
    )]
  }
  if (nrow(game_exclusions)) {
    game_flags <- game_exclusions[, .(
      game_pk,
      exclusion_reason = reason
    )]
    x <- merge(x, game_flags, by = "game_pk", all.x = TRUE,
      suffixes = c("", "_game"), sort = FALSE)
    x[!is.na(exclusion_reason_game), `:=`(
      abs_eligible = FALSE,
      exclusion_reason = exclusion_reason_game
    )]
    x[, exclusion_reason_game := NULL]
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
