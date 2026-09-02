library(targets)

source("config/project.R")
source("scripts/load_functions.R")
tar_source(abs_function_files())

tar_option_set(
  packages = c(
    "arrow", "curl", "data.table", "digest", "dplyr", "ggplot2", "httr2",
    "jsonlite", "lubridate", "mgcv", "purrr", "readr", "tidyr"
  ),
  format = "rds",
  error = "stop"
)

list(
  tar_target(config_env, project_environment(), cue = tar_cue(mode = "always")),
  tar_target(config, project_config(config_env)),
  tar_target(directories, ensure_directories(config)),

  tar_target(
    schedule_file,
    download_schedule(config),
    format = "file"
  ),
  tar_target(schedule, read_schedule(schedule_file)),
  tar_target(game_ids, completed_game_ids(schedule)),
  tar_target(statcast_dates, completed_game_dates(schedule)),
  tar_target(team_ids, schedule_team_ids(schedule)),

  tar_target(statcast_date, statcast_dates, iteration = "vector"),
  tar_target(
    statcast_file,
    download_statcast_day(statcast_date, config),
    pattern = map(statcast_date),
    format = "file"
  ),
  tar_target(live_game_id, game_ids, iteration = "vector"),
  tar_target(
    live_feed_file,
    download_live_feed(live_game_id, config),
    pattern = map(live_game_id),
    format = "file"
  ),
  tar_target(savant_team_id, team_ids, iteration = "vector"),
  tar_target(
    savant_dashboard_file,
    download_savant_dashboard(config),
    format = "file"
  ),
  tar_target(
    savant_team_file,
    download_savant_team(savant_team_id, config),
    pattern = map(savant_team_id),
    format = "file"
  ),

  tar_target(statcast_manifest,
    write_source_manifest(statcast_file, "statcast_2026", config), format = "file"),
  tar_target(live_manifest,
    write_source_manifest(live_feed_file, "live_feeds_2026", config), format = "file"),
  tar_target(savant_manifest,
    write_source_manifest(c(savant_dashboard_file, savant_team_file),
      "savant_abs_2026", config), format = "file"),

  tar_target(statcast, read_statcast_files(statcast_file, game_ids)),
  tar_target(live_data, parse_live_feeds(live_feed_file, cores = 8L)),
  tar_target(savant, read_savant_teams(savant_team_file, config$cutoff_date)),
  tar_target(pitch_ledger_unvalued, build_pitch_ledger(statcast, live_data, config)),

  tar_target(history_years, 2023:2025),
  tar_target(history_year, history_years, iteration = "vector"),
  tar_target(
    history_schedule_file,
    download_schedule(
      config,
      as.Date(sprintf("%s-03-01", history_year)),
      as.Date(sprintf("%s-10-31", history_year))
    ),
    pattern = map(history_year),
    format = "file"
  ),
  tar_target(history_schedule,
    safe_rbindlist(lapply(history_schedule_file, read_schedule))),
  tar_target(history_game_ids, completed_game_ids(history_schedule)),
  tar_target(history_2025_dates,
    completed_game_dates(history_schedule[format(game_date, "%Y") == "2025"])),
  tar_target(
    history_statcast_mirror_file,
    download_statcast_history_mirror(config),
    format = "file"
  ),
  tar_target(
    history_statcast_2025_file,
    download_statcast_history_days(history_2025_dates, config, cores = 8L),
    format = "file"
  ),
  tar_target(history_manifest,
    write_source_manifest(
      c(history_statcast_mirror_file, history_statcast_2025_file),
      "statcast_history_2023_2025", config
    ), format = "file"),
  tar_target(history_statcast, safe_rbindlist(list(
    read_statcast_history_mirror(
      history_statcast_mirror_file, config, history_game_ids
    ),
    read_statcast_history_files(history_statcast_2025_file, history_game_ids)
  ))),
  tar_target(history_coverage,
    validate_history_coverage(history_statcast, history_game_ids)),
  tar_target(re_model, {
    history_coverage
    estimate_re288(history_statcast)
  }),
  tar_target(wpa_model, {
    history_coverage
    fit_home_win_gam(history_statcast)
  }),

  tar_target(pitch_ledger,
    add_pitch_values(pitch_ledger_unvalued, re_model, wpa_model)),
  tar_target(remaining_opportunities, build_remaining_opportunities(pitch_ledger)),
  tar_target(continuous_location_features,
    prepare_continuous_location_features(statcast)),
  tar_target(continuous_swing_features,
    prepare_continuous_swing_features(statcast)),
  tar_target(continuous_call_features,
    prepare_continuous_call_features(pitch_ledger)),
  tar_target(continuous_decision_features,
    build_batter_decision_features(
      pitch_ledger, remaining_opportunities, re_model
    )),
  tar_target(continuous_challenge_labels,
    build_batter_challenge_labels(pitch_ledger)),
  tar_target(challenge_events,
    build_challenge_events(pitch_ledger, remaining_opportunities, savant)),
  tar_target(model_validation,
    model_validation_metrics(re_model, wpa_model, challenge_events)),
  tar_target(propensity_rows, crossfit_challenge_propensity(remaining_opportunities)),
  tar_target(adjusted_teams, opportunity_adjusted_teams(propensity_rows)),
  tar_target(team_summaries,
    summarize_teams(pitch_ledger, challenge_events, remaining_opportunities, adjusted_teams)),
  tar_target(player_summaries, summarize_players(challenge_events)),
  tar_target(team_game,
    team_game_metrics(pitch_ledger, challenge_events, remaining_opportunities)),
  tar_target(team_intervals,
    bootstrap_team_intervals(team_game, config$bootstrap_reps)),
  tar_target(adjusted_intervals,
    bootstrap_adjusted_intervals(propensity_rows, config$bootstrap_reps)),
  tar_target(validation, run_acceptance_checks(pitch_ledger, live_data, savant, config)),

  tar_target(pitch_ledger_file,
    write_parquet_atomic(pitch_ledger, file.path(config$derived_dir, "pitch_ledger.parquet")),
    format = "file"),
  tar_target(challenge_events_file,
    write_parquet_atomic(challenge_events, file.path(config$derived_dir, "challenge_events.parquet")),
    format = "file"),
  tar_target(remaining_opportunities_file,
    write_parquet_atomic(remaining_opportunities,
      file.path(config$derived_dir, "remaining_opportunities.parquet")),
    format = "file"),
  tar_target(continuous_location_features_file,
    write_parquet_atomic(continuous_location_features,
      file.path(config$derived_dir, "continuous_location_features.parquet")),
    format = "file"),
  tar_target(continuous_swing_features_file,
    write_parquet_atomic(continuous_swing_features,
      file.path(config$derived_dir, "continuous_swing_features.parquet")),
    format = "file"),
  tar_target(continuous_call_features_file,
    write_parquet_atomic(continuous_call_features,
      file.path(config$derived_dir, "continuous_call_features.parquet")),
    format = "file"),
  tar_target(continuous_decision_features_file,
    write_parquet_atomic(continuous_decision_features,
      file.path(config$derived_dir, "continuous_decision_features.parquet")),
    format = "file"),
  tar_target(continuous_challenge_labels_file,
    write_parquet_atomic(continuous_challenge_labels,
      file.path(config$derived_dir, "continuous_challenge_labels.parquet")),
    format = "file"),
  tar_target(team_summaries_file,
    write_parquet_atomic(team_summaries, file.path(config$derived_dir, "team_summaries.parquet")),
    format = "file"),
  tar_target(player_summaries_file,
    write_parquet_atomic(player_summaries,
      file.path(config$derived_dir, "player_summaries.parquet")),
    format = "file"),
  tar_target(team_intervals_file,
    write_parquet_atomic(team_intervals,
      file.path(config$derived_dir, "team_intervals.parquet")),
    format = "file"),
  tar_target(adjusted_intervals_file,
    write_parquet_atomic(adjusted_intervals,
      file.path(config$derived_dir, "adjusted_intervals.parquet")),
    format = "file"),
  tar_target(history_coverage_file,
    write_csv_atomic(history_coverage,
      file.path(config$derived_dir, "history_coverage.csv")),
    format = "file"),
  tar_target(validation_checks_file,
    write_csv_atomic(validation$checks,
      file.path(config$derived_dir, "validation_checks.csv")),
    format = "file"),
  tar_target(model_validation_file,
    write_csv_atomic(model_validation,
      file.path(config$derived_dir, "model_validation.csv")),
    format = "file"),
  tar_target(coverage_file,
    write_csv_atomic(validation$coverage,
      file.path(config$derived_dir, "coverage.csv")),
    format = "file")
)
