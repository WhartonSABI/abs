crossfit_challenge_propensity <- function(timeline, folds = 5L) {
  x <- data.table::copy(data.table::as.data.table(timeline))[
    adverse_for_team == TRUE & inventory_before > 0L
  ]
  x[, fold := (match(game_pk, sort(unique(game_pk))) - 1L) %% folds + 1L]
  x[, count_state := factor(paste0(balls_before, "-", strikes_before),
    levels = as.vector(outer(0:3, 0:2, paste, sep = "-")))]
  x[, role := factor(role, levels = c("offense", "defense"))]
  x[, predicted_challenge := NA_real_]
  if (data.table::uniqueN(x$game_pk) < max(4L, folds) || nrow(x) < 200L) {
    x[, predicted_challenge := mean(challenge_by_team, na.rm = TRUE)]
    return(x[])
  }
  formula <- challenge_by_team ~ s(inning, k = 9) + s(score_diff, k = 12) +
    outs_before + role + count_state + s(as.numeric(abs(score_diff)), k = 8)
  for (fold_id in seq_len(folds)) {
    train <- x[fold != fold_id]
    test <- x[fold == fold_id]
    model <- mgcv::bam(
      formula, data = train, family = stats::binomial(), method = "fREML", discrete = TRUE
    )
    x[fold == fold_id, predicted_challenge := as.numeric(
      stats::predict(model, newdata = test, type = "response")
    )]
  }
  x[, predicted_challenge := pmax(1e-5, pmin(1 - 1e-5, predicted_challenge))]
  x[]
}

opportunity_adjusted_teams <- function(propensity_rows) {
  x <- data.table::as.data.table(propensity_rows)
  x[, .(
    adverse_called_pitches = .N,
    observed_attempts = sum(challenge_by_team),
    expected_attempts = sum(predicted_challenge),
    challenge_residual = sum(challenge_by_team - predicted_challenge),
    observed_rate = mean(challenge_by_team),
    expected_rate = mean(predicted_challenge)
  ), by = team_id]
}
