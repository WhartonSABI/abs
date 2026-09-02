project_environment <- function() {
  variables <- c(
    "ABS_START", "ABS_CUTOFF", "ABS_REFRESH",
    "ABS_BOOTSTRAP_REPS"
  )
  stats::setNames(Sys.getenv(variables, unset = NA_character_), variables)
}

project_config <- function(env = project_environment()) {
  env_value <- function(name, default) {
    value <- unname(env[[name]])
    if (is.null(value) || is.na(value) || !nzchar(value)) default else value
  }
  list(
    season = 2026L,
    start_date = as.Date(env_value("ABS_START", "2026-03-25")),
    cutoff_date = as.Date(env_value("ABS_CUTOFF", "2026-08-25")),
    refresh = identical(tolower(env_value("ABS_REFRESH", "false")), "true"),
    history_start = as.Date("2023-03-30"),
    history_end = as.Date("2025-09-28"),
    game_type = "R",
    level = "mlb",
    bootstrap_reps = as.integer(env_value("ABS_BOOTSTRAP_REPS", "500")),
    geometry_sensitivity_inches = 0.25,
    coverage_league_min = 0.995,
    coverage_team_min = 0.99,
    raw_dir = "data/raw",
    derived_dir = "data/processed",
    fixture_dir = "data/fixtures",
    user_agent = "mlb-abs-run-value/0.1 research"
  )
}
