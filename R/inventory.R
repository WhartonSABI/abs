reconstruct_inventory_game <- function(game_pitches) {
  x <- data.table::copy(data.table::as.data.table(game_pitches))
  data.table::setorder(x, at_bat_number, pitch_number)
  away_id <- if (x$is_top[[1L]]) x$bat_team_id[[1L]] else x$fld_team_id[[1L]]
  home_id <- if (x$is_top[[1L]]) x$fld_team_id[[1L]] else x$bat_team_id[[1L]]
  inventory <- setNames(c(2L, 2L), c(as.character(away_id), as.character(home_id)))
  before_away <- after_away <- before_home <- after_home <- integer(nrow(x))
  previous_inning <- NA_integer_

  for (i in seq_len(nrow(x))) {
    inning_now <- x$inning[[i]]
    if (!is.na(previous_inning) && inning_now != previous_inning && inning_now > 9L) {
      inventory[inventory == 0L] <- 1L
    }
    before_away[[i]] <- inventory[[as.character(away_id)]]
    before_home[[i]] <- inventory[[as.character(home_id)]]

    if (isTRUE(x$challenge_occurred[[i]])) {
      outcome <- x$challenge_outcome[[i]]
      team_key <- as.character(x$challenger_team_id[[i]])
      if (!team_key %in% names(inventory)) {
        stop("Challenge team not in game at game ", x$game_pk[[i]])
      }
      if (is.na(outcome) || outcome == "unresolved") {
        stop("Cannot reconstruct inventory with unresolved challenge at ",
          paste(x$game_pk[[i]], x$at_bat_number[[i]], x$pitch_number[[i]], sep = "-"))
      }
      if (inventory[[team_key]] <= 0L) {
        stop("Challenge occurred with no reconstructed inventory at game ", x$game_pk[[i]])
      }
      if (outcome == "upheld") {
        inventory[[team_key]] <- inventory[[team_key]] - 1L
      }
    }
    after_away[[i]] <- inventory[[as.character(away_id)]]
    after_home[[i]] <- inventory[[as.character(home_id)]]
    previous_inning <- inning_now
  }

  x[, `:=`(
    away_team_id = away_id,
    home_team_id = home_id,
    away_challenges_before = before_away,
    away_challenges_after = after_away,
    home_challenges_before = before_home,
    home_challenges_after = after_home
  )]
  x[, bat_team_challenges_before := data.table::fifelse(
    bat_team_id == away_id, away_challenges_before, home_challenges_before
  )]
  x[, fld_team_challenges_before := data.table::fifelse(
    fld_team_id == away_id, away_challenges_before, home_challenges_before
  )]
  x[, challenge_remaining_before := data.table::fifelse(
    challenger_team_id == away_id, away_challenges_before,
    data.table::fifelse(challenger_team_id == home_id, home_challenges_before, NA_integer_)
  )]
  x[, challenge_remaining_after := data.table::fifelse(
    challenger_team_id == away_id, away_challenges_after,
    data.table::fifelse(challenger_team_id == home_id, home_challenges_after, NA_integer_)
  )]
  x[]
}

reconstruct_inventory <- function(pitches) {
  x <- data.table::as.data.table(pitches)
  pieces <- lapply(split(x, x$game_pk), reconstruct_inventory_game)
  out <- safe_rbindlist(pieces)
  data.table::setorder(out, game_pk, at_bat_number, pitch_number)
  out[]
}

team_timeline <- function(pitch_ledger) {
  x <- data.table::as.data.table(pitch_ledger)
  away <- x[, .(
    game_pk, game_date, pitch_order, inning, half, outs_before, balls_before,
    strikes_before, team_id = away_team_id,
    role = ifelse(bat_team_id == away_team_id, "offense", "defense"),
    score_diff = ifelse(bat_team_id == away_team_id, bat_score - fld_score, fld_score - bat_score),
    inventory_before = away_challenges_before,
    challenge_by_team = challenge_occurred & challenger_team_id == away_team_id,
    challenge_outcome,
    adverse_for_team = adverse_team_id == away_team_id,
    correctable_for_team = correctable_opportunity & adverse_team_id == away_team_id,
    potential_re = ifelse(correctable_opportunity & adverse_team_id == away_team_id,
      potential_challenger_re, 0),
    potential_wpa = ifelse(correctable_opportunity & adverse_team_id == away_team_id,
      potential_challenger_wpa, 0)
  )]
  home <- x[, .(
    game_pk, game_date, pitch_order, inning, half, outs_before, balls_before,
    strikes_before, team_id = home_team_id,
    role = ifelse(bat_team_id == home_team_id, "offense", "defense"),
    score_diff = ifelse(bat_team_id == home_team_id, bat_score - fld_score, fld_score - bat_score),
    inventory_before = home_challenges_before,
    challenge_by_team = challenge_occurred & challenger_team_id == home_team_id,
    challenge_outcome,
    adverse_for_team = adverse_team_id == home_team_id,
    correctable_for_team = correctable_opportunity & adverse_team_id == home_team_id,
    potential_re = ifelse(correctable_opportunity & adverse_team_id == home_team_id,
      potential_challenger_re, 0),
    potential_wpa = ifelse(correctable_opportunity & adverse_team_id == home_team_id,
      potential_challenger_wpa, 0)
  )]
  out <- data.table::rbindlist(list(away, home))
  out[, score_bucket := score_bucket(score_diff)]
  data.table::setorder(out, game_pk, team_id, pitch_order)
  out[]
}
