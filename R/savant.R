read_savant_team <- function(path) {
  x <- jsonlite::fromJSON(path, simplifyVector = TRUE)$data
  if (is.null(x) || !nrow(x)) return(data.table::data.table())
  data.table::as.data.table(x)
}

read_savant_teams <- function(paths, cutoff_date = NULL) {
  x <- safe_rbindlist(lapply(paths, read_savant_team))
  if (!nrow(x)) return(x)
  if (!is.null(cutoff_date)) x <- x[as.Date(game_date) <= as.Date(cutoff_date)]
  if (!"play_id" %in% names(x)) stop("Savant ABS detail is missing play_id")
  data.table::setorder(x, play_id, against)
  unique(x, by = "play_id")
}

prepare_savant_challenges <- function(x) {
  x <- data.table::copy(data.table::as.data.table(x))
  required <- c(
    "game_pk", "play_id", "is_challengeABS_overturned", "original_isStrike_ump",
    "challenging_player_id", "bat_team_id", "fld_team_id", "edge_dist_calc"
  )
  stop_if_missing_columns(x, required, "Savant ABS detail")
  x[, savant_outcome := data.table::fifelse(
    as.integer(is_challengeABS_overturned) == 1L, "overturned", "upheld"
  )]
  x[, savant_initial_call := data.table::fifelse(
    as.integer(original_isStrike_ump) == 1L, "called_strike", "ball"
  )]
  x[, savant_challenger_team_id := data.table::fifelse(
    savant_initial_call == "called_strike", as.integer(bat_team_id), as.integer(fld_team_id)
  )]
  x[]
}

