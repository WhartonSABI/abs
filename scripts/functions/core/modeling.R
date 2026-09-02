base_state_code <- function(on_1b, on_2b, on_3b) {
  paste0(as.integer(!is.na(on_1b) & on_1b != 0),
    as.integer(!is.na(on_2b) & on_2b != 0),
    as.integer(!is.na(on_3b) & on_3b != 0))
}

prepare_re_observations <- function(statcast) {
  x <- data.table::copy(data.table::as.data.table(statcast))
  required <- c(
    "game_pk", "inning", "inning_topbot", "at_bat_number", "pitch_number",
    "balls", "strikes", "outs_when_up", "on_1b", "on_2b", "on_3b",
    "bat_score", "post_bat_score", "game_date"
  )
  stop_if_missing_columns(x, required, "RE training Statcast")
  x[, game_date := as.Date(game_date)]
  x[, base_state := base_state_code(on_1b, on_2b, on_3b)]
  x[, half_final_bat_score := max(c(bat_score, post_bat_score), na.rm = TRUE),
    by = .(game_pk, inning, inning_topbot)]
  x[, runs_to_end := half_final_bat_score - bat_score]
  x[
    data.table::between(balls, 0L, 3L) & data.table::between(strikes, 0L, 2L) &
      data.table::between(outs_when_up, 0L, 2L) & is.finite(runs_to_end),
    .(
      game_pk, game_date, balls = as.integer(balls), strikes = as.integer(strikes),
      outs = as.integer(outs_when_up), base_state, runs_to_end
    )
  ]
}

fit_re288_table <- function(observations, shrinkage = 20) {
  x <- data.table::as.data.table(observations)
  re24 <- x[, .(re24 = mean(runs_to_end), n_re24 = .N), by = .(outs, base_state)]
  re288 <- x[, .(sum_runs = sum(runs_to_end), n = .N),
    by = .(outs, base_state, balls, strikes)]
  grid <- data.table::CJ(
    outs = 0:2, base_state = sprintf("%03d", 0:7), balls = 0:3, strikes = 0:2
  )
  # sprintf produces decimal strings; replace with three-bit occupancy codes.
  grid[, base_state := c("000", "001", "010", "011", "100", "101", "110", "111")[
    match(base_state, sprintf("%03d", 0:7))
  ]]
  table <- merge(grid, re24, by = c("outs", "base_state"), all.x = TRUE)
  table <- merge(table, re288, by = c("outs", "base_state", "balls", "strikes"), all.x = TRUE)
  overall <- mean(x$runs_to_end)
  table[is.na(re24), re24 := overall]
  table[is.na(n), `:=`(n = 0L, sum_runs = 0)]
  table[, re := (sum_runs + shrinkage * re24) / (n + shrinkage)]
  data.table::setkey(table, outs, base_state, balls, strikes)
  table[]
}

evaluate_re288 <- function(table, observations) {
  x <- merge(
    data.table::as.data.table(observations),
    table[, .(outs, base_state, balls, strikes, prediction = re)],
    by = c("outs", "base_state", "balls", "strikes"), all.x = TRUE
  )
  data.frame(
    n = nrow(x),
    rmse = sqrt(mean((x$runs_to_end - x$prediction)^2, na.rm = TRUE)),
    mae = mean(abs(x$runs_to_end - x$prediction), na.rm = TRUE),
    mean_prediction = mean(x$prediction, na.rm = TRUE),
    mean_outcome = mean(x$runs_to_end, na.rm = TRUE)
  )
}

estimate_re288 <- function(statcast_history) {
  observations <- prepare_re_observations(statcast_history)
  training <- observations[game_date < as.Date("2025-01-01")]
  holdout <- observations[game_date >= as.Date("2025-01-01")]
  validation_table <- fit_re288_table(training)
  validation <- evaluate_re288(validation_table, holdout)
  final_table <- fit_re288_table(observations)
  list(table = final_table, validation = validation)
}

validate_history_coverage <- function(statcast_history, scheduled_game_ids,
  minimum = 0.995) {
  observed <- unique(statcast_history$game_pk)
  expected <- unique(scheduled_game_ids)
  coverage <- mean(expected %in% observed)
  if (!is.finite(coverage) || coverage < minimum) {
    stop(sprintf(
      "Historical Statcast game coverage %.3f%% is below %.3f%%",
      100 * coverage, 100 * minimum
    ))
  }
  data.frame(
    scheduled_games = length(expected),
    observed_games = sum(expected %in% observed),
    coverage = coverage
  )
}

prepare_wpa_observations <- function(statcast) {
  x <- data.table::copy(data.table::as.data.table(statcast))
  required <- c(
    "game_pk", "game_date", "inning", "inning_topbot", "outs_when_up",
    "balls", "strikes", "on_1b", "on_2b", "on_3b", "bat_score",
    "fld_score", "post_bat_score", "post_fld_score"
  )
  stop_if_missing_columns(x, required, "WPA training Statcast")
  x[, game_date := as.Date(game_date)]
  x[, base_state := factor(base_state_code(on_1b, on_2b, on_3b),
    levels = c("000", "001", "010", "011", "100", "101", "110", "111"))]
  x[, is_bottom := as.integer(tolower(inning_topbot) == "bot")]
  x[, home_score := ifelse(is_bottom == 1L, bat_score, fld_score)]
  x[, away_score := ifelse(is_bottom == 1L, fld_score, bat_score)]
  x[, score_diff := home_score - away_score]
  finals <- x[, .(
    final_home = max(ifelse(is_bottom == 1L, post_bat_score, post_fld_score), na.rm = TRUE),
    final_away = max(ifelse(is_bottom == 1L, post_fld_score, post_bat_score), na.rm = TRUE)
  ), by = game_pk]
  finals[, home_win := as.integer(final_home > final_away)]
  x <- merge(x, finals[, .(game_pk, home_win)], by = "game_pk")
  x[, count_state := factor(paste0(balls, "-", strikes),
    levels = as.vector(outer(0:3, 0:2, paste, sep = "-")))]
  x[, .(
    game_pk, game_date, home_win, inning = pmin(as.integer(inning), 12L),
    is_bottom, score_diff = pmax(-10, pmin(10, score_diff)),
    outs = as.integer(outs_when_up), base_state, count_state
  )]
}

fit_home_win_gam <- function(statcast_history) {
  x <- prepare_wpa_observations(statcast_history)
  train <- x[game_date < as.Date("2025-01-01")]
  holdout <- x[game_date >= as.Date("2025-01-01")]
  formula <- home_win ~ s(inning, k = 10) + s(score_diff, k = 15) +
    is_bottom + outs + base_state + count_state + ti(inning, score_diff, k = c(8, 10))
  validation_model <- suppressWarnings(mgcv::bam(
    formula, data = train, family = stats::binomial(), method = "fREML", discrete = TRUE
  ))
  holdout_prediction <- stats::predict(validation_model, newdata = holdout, type = "response")
  validation <- data.frame(
    n = nrow(holdout),
    brier = mean((holdout$home_win - holdout_prediction)^2, na.rm = TRUE),
    log_loss = -mean(
      holdout$home_win * log(pmax(holdout_prediction, 1e-8)) +
        (1 - holdout$home_win) * log(pmax(1 - holdout_prediction, 1e-8)),
      na.rm = TRUE
    )
  )
  final_model <- suppressWarnings(mgcv::bam(
    formula, data = x, family = stats::binomial(), method = "fREML", discrete = TRUE
  ))
  list(model = final_model, validation = validation)
}

model_validation_metrics <- function(re_model, wpa_model, challenge_events) {
  re_holdout <- data.table::as.data.table(re_model$validation)
  re_holdout[, validation := "re288_2025_holdout"]
  re_holdout[, `:=`(correlation = NA_real_, mean_difference = NA_real_)]

  wpa_holdout <- data.table::as.data.table(wpa_model$validation)
  wpa_holdout[, validation := "home_wp_2025_holdout"]
  wpa_holdout[, `:=`(
    rmse = NA_real_, mae = NA_real_, mean_prediction = NA_real_,
    mean_outcome = NA_real_, correlation = NA_real_, mean_difference = NA_real_
  )]

  challenged <- data.table::as.data.table(challenge_events)[
    publication_eligible == TRUE & challenge_outcome == "overturned" &
      is.finite(actual_re_gain) & is.finite(sz_challenge_overturned_runs)
  ]
  savant_comparison <- challenged[, .(
    validation = "re288_vs_savant_overturns",
    n = .N,
    rmse = sqrt(mean((actual_re_gain - sz_challenge_overturned_runs)^2)),
    mae = mean(abs(actual_re_gain - sz_challenge_overturned_runs)),
    mean_prediction = mean(actual_re_gain),
    mean_outcome = mean(sz_challenge_overturned_runs),
    correlation = stats::cor(actual_re_gain, sz_challenge_overturned_runs),
    mean_difference = mean(actual_re_gain - sz_challenge_overturned_runs),
    brier = NA_real_,
    log_loss = NA_real_
  )]
  safe_rbindlist(list(re_holdout, wpa_holdout, savant_comparison))[]
}
