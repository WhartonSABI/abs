read_statcast_file <- function(path) {
  x <- data.table::fread(path, na.strings = c("", "NA", "null"), showProgress = FALSE)
  data.table::setnames(x, trimws(names(x)))
  x
}

read_statcast_files <- function(paths, game_ids = NULL) {
  pieces <- lapply(paths, function(path) {
    x <- read_statcast_file(path)
    if (!is.null(game_ids)) x <- x[game_pk %in% game_ids]
    x
  })
  safe_rbindlist(pieces)
}

read_statcast_history_files <- function(paths, game_ids = NULL) {
  columns <- c(
    "game_pk", "game_date", "inning", "inning_topbot", "at_bat_number",
    "pitch_number", "balls", "strikes", "outs_when_up", "on_1b", "on_2b",
    "on_3b", "bat_score", "fld_score", "post_bat_score", "post_fld_score",
    "batter", "pitcher", "home_team", "away_team", "game_year"
  )
  pieces <- lapply(paths, function(path) {
    available <- names(data.table::fread(path, nrows = 0L, showProgress = FALSE))
    x <- data.table::fread(path, select = intersect(columns, available),
      na.strings = c("", "NA", "null"), showProgress = FALSE)
    if ("game_date" %in% names(x)) x[, game_date := as.Date(game_date)]
    if (!is.null(game_ids)) x <- x[game_pk %in% game_ids]
    x
  })
  safe_rbindlist(pieces)
}

read_statcast_history_mirror <- function(path, config, game_ids = NULL) {
  columns <- c(
    "game_pk", "game_date", "game_type", "inning", "inning_topbot",
    "at_bat_number", "pitch_number", "balls", "strikes", "outs_when_up",
    "on_1b", "on_2b", "on_3b", "bat_score", "fld_score",
    "post_bat_score", "post_fld_score", "batter", "pitcher", "home_team",
    "away_team", "game_year"
  )
  dataset <- arrow::open_dataset(path)
  available <- intersect(columns, names(dataset))
  start_time <- as.POSIXct(config$history_start, tz = "UTC")
  end_time <- as.POSIXct(config$history_end, tz = "UTC")
  query <- dataset |>
    dplyr::select(dplyr::all_of(available)) |>
    dplyr::filter(
      game_date >= start_time,
      game_date <= end_time
    )
  if ("game_type" %in% available) query <- dplyr::filter(query, game_type == "R")
  x <- data.table::as.data.table(dplyr::collect(query))
  if ("game_date" %in% names(x)) x[, game_date := as.Date(game_date)]
  if (!is.null(game_ids)) x <- x[game_pk %in% game_ids]
  x[]
}

prepare_statcast_for_ledger <- function(statcast) {
  x <- data.table::as.data.table(statcast)
  required <- c(
    "game_pk", "at_bat_number", "pitch_number", "balls", "strikes",
    "outs_when_up", "inning", "inning_topbot", "plate_x", "plate_z",
    "sz_top", "sz_bot", "on_1b", "on_2b", "on_3b", "bat_score",
    "fld_score", "home_team", "away_team", "delta_run_exp",
    "delta_home_win_exp", "home_win_exp"
  )
  stop_if_missing_columns(x, required, "Statcast input")
  data.table::setorder(x, game_pk, at_bat_number, pitch_number)
  automatic <- c("automatic_ball", "automatic_strike")
  x[, statcast_pitch_number := pitch_number]
  x[, pitch_number := cumsum(!description %in% automatic),
    by = .(game_pk, at_bat_number)]
  x <- x[!description %in% automatic]
  keep <- intersect(c(
    required, "statcast_pitch_number", "game_date", "game_type", "batter", "pitcher", "fielder_2",
    "description", "type", "events", "post_bat_score", "post_fld_score",
    "launch_speed", "release_speed", "pitch_type", "p_throws", "stand"
  ), names(x))
  x <- x[, ..keep]
  data.table::setnames(x, c(
    "balls", "strikes", "outs_when_up", "plate_x", "plate_z", "sz_top", "sz_bot",
    "inning"
  ), c(
    "balls_before", "strikes_before", "outs_before", "sc_plate_x", "sc_plate_z",
    "sc_sz_top", "sc_sz_bot", "sc_inning"
  ))
  optional_renames <- c(
    game_date = "sc_game_date", batter = "sc_batter_id", pitcher = "sc_pitcher_id",
    description = "sc_description", type = "sc_call_code"
  )
  present <- intersect(names(optional_renames), names(x))
  if (length(present)) data.table::setnames(x, present, unname(optional_renames[present]))
  x[]
}

build_pitch_ledger <- function(statcast, live_data, config) {
  sc <- prepare_statcast_for_ledger(statcast)
  feed <- data.table::copy(live_data$called_pitches)
  reviews <- data.table::copy(live_data$challenges)
  key <- c("game_pk", "at_bat_number", "pitch_number")

  if (anyDuplicated(feed[, ..key])) stop("Duplicate called-pitch keys in live feeds")
  if (anyDuplicated(reviews[, ..key])) stop("Duplicate challenge keys in live feeds")

  review_cols <- c(
    key, "challenger_team_id", "challenger_player_id", "challenger_name",
    "challenger_role", "is_overturned", "challenge_outcome", "review_type",
    "linkage_source"
  )
  feed <- merge(feed, reviews[, ..review_cols], by = key, all.x = TRUE, sort = FALSE)
  feed[, challenge_occurred := !is.na(review_type)]
  feed[, initial_call := data.table::fifelse(
    challenge_occurred & challenge_outcome == "overturned",
    opposite_call(final_call), final_call
  )]

  x <- merge(feed, sc, by = key, all.x = TRUE, sort = FALSE)
  umpire_fields <- c("game_pk", "umpire_id", "umpire_name")
  if (all(umpire_fields %in% names(live_data$metadata))) {
    umpires <- unique(data.table::as.data.table(live_data$metadata)[, ..umpire_fields])
    if (anyDuplicated(umpires$game_pk)) {
      stop("Live-feed metadata contains conflicting home-plate umpires")
    }
    x <- merge(x, umpires, by = "game_pk", all.x = TRUE, sort = FALSE)
  } else {
    x[, `:=`(umpire_id = NA_integer_, umpire_name = NA_character_)]
  }
  # Savant's ABS service uses the Statcast plate location and the official
  # strike-zone limits rounded to three decimal places. Never use a Statcast
  # row whose batter/pitcher identity disagrees with the live feed.
  x[, statcast_matched := !is.na(balls_before)]
  x[, statcast_identity_matched := statcast_matched &
    batter_id == sc_batter_id & pitcher_id == sc_pitcher_id]
  x[, plate_x := ifelse(statcast_identity_matched, sc_plate_x, NA_real_)]
  x[, plate_z := ifelse(statcast_identity_matched, sc_plate_z, NA_real_)]
  x[, sz_top := round(data.table::fcoalesce(feed_sz_top, sc_sz_top), 3L)]
  x[, sz_bot := round(data.table::fcoalesce(feed_sz_bot, sc_sz_bot), 3L)]
  x[, tracking_available := !is.na(plate_x) & !is.na(plate_z) &
    !is.na(sz_top) & !is.na(sz_bot)]
  x <- add_abs_geometry(x)
  x <- classify_opportunity(x, config$geometry_sensitivity_inches)
  x <- apply_abs_eligibility(x)
  data.table::setorder(x, game_pk, at_bat_number, pitch_number)
  x[, pitch_order := seq_len(.N), by = game_pk]
  x <- reconstruct_inventory(x)
  x[, adverse_challenges_before := data.table::fifelse(
    adverse_team_id == bat_team_id, bat_team_challenges_before,
    fld_team_challenges_before
  )]
  x[, opportunity_status := data.table::fcase(
    !correctable_opportunity, "not_correctable",
    challenge_occurred, "challenged",
    adverse_challenges_before > 0L, "missed_available",
    adverse_challenges_before == 0L, "out_of_challenges",
    default = "unknown"
  )]
  x[, challenge_key := paste(game_pk, at_bat_number, pitch_number, sep = "-")]
  x[]
}
