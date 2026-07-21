read_schedule <- function(path) {
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  rows <- lapply(x$dates %||% list(), function(day) {
    lapply(day$games %||% list(), function(game) {
      data.frame(
        game_pk = scalar_int(game$gamePk),
        game_date = as.Date(scalar_chr(game$officialDate, day$date)),
        game_type = scalar_chr(game$gameType),
        status_code = scalar_chr(game$status$statusCode),
        abstract_state = scalar_chr(game$status$abstractGameState),
        detailed_state = scalar_chr(game$status$detailedState),
        away_team_id = scalar_int(game$teams$away$team$id),
        away_team = scalar_chr(game$teams$away$team$name),
        home_team_id = scalar_int(game$teams$home$team$id),
        home_team = scalar_chr(game$teams$home$team$name),
        venue_id = scalar_int(game$venue$id),
        stringsAsFactors = FALSE
      )
    })
  })
  out <- safe_rbindlist(unlist(rows, recursive = FALSE))
  out[game_type == "R"][]
}

completed_games <- function(schedule) {
  schedule[
    (abstract_state == "Final" |
      detailed_state %in% c("Final", "Completed Early", "Game Over")) &
      !detailed_state %in% c("Cancelled", "Postponed"),
  ][]
}

completed_game_ids <- function(schedule) sort(unique(completed_games(schedule)$game_pk))

completed_game_dates <- function(schedule) sort(unique(completed_games(schedule)$game_date))

schedule_team_ids <- function(schedule) {
  sort(unique(c(schedule$away_team_id, schedule$home_team_id)))
}

monthly_game_periods <- function(schedule) {
  dates <- completed_game_dates(schedule)
  x <- data.table::data.table(date = dates, month = format(dates, "%Y-%m"))
  periods <- x[, .(start = min(date), end = max(date)), by = month]
  split(periods, seq_len(nrow(periods)))
}
