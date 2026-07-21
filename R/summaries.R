build_challenge_events <- function(pitch_ledger, remaining, savant) {
  ledger <- data.table::as.data.table(pitch_ledger)[challenge_occurred == TRUE]
  inventory_cols <- c(
    "game_pk", "team_id", "pitch_order", "marginal_count_1_to_0",
    "marginal_count_2_to_1", "marginal_re_1_to_0", "marginal_re_2_to_1",
    "marginal_wpa_1_to_0", "marginal_wpa_2_to_1"
  )
  inv <- data.table::as.data.table(remaining)[, ..inventory_cols]
  data.table::setnames(inv, "team_id", "challenger_team_id")
  x <- merge(ledger, inv, by = c("game_pk", "challenger_team_id", "pitch_order"),
    all.x = TRUE, sort = FALSE)
  x[, `:=`(
    failed_inventory_cost_count = data.table::fcase(
      challenge_outcome != "upheld", 0,
      challenge_remaining_before == 2L, marginal_count_2_to_1,
      challenge_remaining_before == 1L, marginal_count_1_to_0,
      default = NA_real_
    ),
    failed_inventory_cost_re = data.table::fcase(
      challenge_outcome != "upheld", 0,
      challenge_remaining_before == 2L, marginal_re_2_to_1,
      challenge_remaining_before == 1L, marginal_re_1_to_0,
      default = NA_real_
    ),
    failed_inventory_cost_wpa = data.table::fcase(
      challenge_outcome != "upheld", 0,
      challenge_remaining_before == 2L, marginal_wpa_2_to_1,
      challenge_remaining_before == 1L, marginal_wpa_1_to_0,
      default = NA_real_
    )
  )]
  attach_savant_values(x, prepare_savant_challenges(savant))
}

summarize_teams <- function(pitch_ledger, challenge_events, remaining, adjusted = NULL) {
  all_events <- data.table::as.data.table(challenge_events)
  events <- all_events[publication_eligible == TRUE]
  challenge <- events[, .(
    attempts = .N,
    overturns = sum(challenge_outcome == "overturned"),
    upheld = sum(challenge_outcome == "upheld"),
    success_rate = mean(challenge_outcome == "overturned"),
    gross_re = sum(actual_re_gain, na.rm = TRUE),
    gross_wpa = sum(actual_wpa_gain, na.rm = TRUE),
    failed_inventory_cost_re = sum(failed_inventory_cost_re, na.rm = TRUE),
    failed_inventory_cost_wpa = sum(failed_inventory_cost_wpa, na.rm = TRUE)
  ), by = .(team_id = challenger_team_id)]
  quarantine <- all_events[publication_eligible == FALSE, .(
    quarantined_feed_only = .N
  ), by = .(team_id = challenger_team_id)]
  challenge <- merge(challenge, quarantine, by = "team_id", all = TRUE)
  challenge[is.na(quarantined_feed_only), quarantined_feed_only := 0L]
  challenge[, `:=`(
    net_re = gross_re - failed_inventory_cost_re,
    net_wpa = gross_wpa - failed_inventory_cost_wpa
  )]

  ledger <- data.table::as.data.table(pitch_ledger)
  opportunity <- ledger[correctable_opportunity == TRUE, .(
    missed_available = sum(opportunity_status == "missed_available"),
    missed_available_re = sum(ifelse(opportunity_status == "missed_available",
      potential_challenger_re, 0), na.rm = TRUE),
    missed_available_wpa = sum(ifelse(opportunity_status == "missed_available",
      potential_challenger_wpa, 0), na.rm = TRUE),
    out_of_challenges = sum(opportunity_status == "out_of_challenges"),
    out_of_challenges_re = sum(ifelse(opportunity_status == "out_of_challenges",
      potential_challenger_re, 0), na.rm = TRUE),
    out_of_challenges_wpa = sum(ifelse(opportunity_status == "out_of_challenges",
      potential_challenger_wpa, 0), na.rm = TRUE)
  ), by = .(team_id = adverse_team_id)]

  timeline <- data.table::as.data.table(remaining)
  starts <- timeline[, .SD[which.min(pitch_order)], by = .(game_pk, team_id)]
  inventory <- starts[, .(
    games = .N,
    expected_correctable_remaining = mean(expected_correctable_remaining, na.rm = TRUE),
    expected_re_remaining = mean(expected_re_remaining, na.rm = TRUE),
    expected_wpa_remaining = mean(expected_wpa_remaining, na.rm = TRUE),
    marginal_re_second_unit = mean(marginal_re_2_to_1, na.rm = TRUE),
    marginal_re_first_unit = mean(marginal_re_1_to_0, na.rm = TRUE),
    marginal_wpa_second_unit = mean(marginal_wpa_2_to_1, na.rm = TRUE),
    marginal_wpa_first_unit = mean(marginal_wpa_1_to_0, na.rm = TRUE)
  ), by = team_id]
  out <- Reduce(function(a, b) merge(a, b, by = "team_id", all = TRUE),
    Filter(Negate(is.null), list(challenge, opportunity, inventory, adjusted)))
  data.table::setorder(out, -net_re)
  out[]
}

summarize_players <- function(challenge_events) {
  x <- data.table::as.data.table(challenge_events)[publication_eligible == TRUE]
  x[, .(
    player_name = challenger_name[[1L]],
    role = challenger_role[[1L]],
    team_id = challenger_team_id[[1L]],
    attempts = .N,
    overturns = sum(challenge_outcome == "overturned"),
    upheld = sum(challenge_outcome == "upheld"),
    success_rate = mean(challenge_outcome == "overturned"),
    gross_re = sum(actual_re_gain, na.rm = TRUE),
    gross_wpa = sum(actual_wpa_gain, na.rm = TRUE),
    inventory_cost_re = sum(failed_inventory_cost_re, na.rm = TRUE)
  ), by = .(player_id = challenger_player_id)]
}

team_game_metrics <- function(pitch_ledger, challenge_events, remaining) {
  ledger <- data.table::as.data.table(pitch_ledger)
  events <- data.table::as.data.table(challenge_events)[publication_eligible == TRUE]
  challenged <- events[, .(
    attempts = .N,
    overturns = sum(challenge_outcome == "overturned"),
    upheld = sum(challenge_outcome == "upheld"),
    gross_re = sum(actual_re_gain, na.rm = TRUE),
    gross_wpa = sum(actual_wpa_gain, na.rm = TRUE),
    inventory_cost_re = sum(failed_inventory_cost_re, na.rm = TRUE),
    inventory_cost_wpa = sum(failed_inventory_cost_wpa, na.rm = TRUE)
  ), by = .(game_pk, team_id = challenger_team_id)]
  opportunities <- ledger[correctable_opportunity == TRUE, .(
    missed_re = sum(ifelse(opportunity_status == "missed_available",
      potential_challenger_re, 0), na.rm = TRUE),
    out_re = sum(ifelse(opportunity_status == "out_of_challenges",
      potential_challenger_re, 0), na.rm = TRUE),
    missed_wpa = sum(ifelse(opportunity_status == "missed_available",
      potential_challenger_wpa, 0), na.rm = TRUE),
    out_wpa = sum(ifelse(opportunity_status == "out_of_challenges",
      potential_challenger_wpa, 0), na.rm = TRUE)
  ), by = .(game_pk, team_id = adverse_team_id)]
  starts <- data.table::as.data.table(remaining)[
    , .SD[which.min(pitch_order)], by = .(game_pk, team_id)
  ]
  inventory <- starts[, .(
    game_pk, team_id,
    mean_expected_correctable_start = expected_correctable_remaining,
    mean_expected_re_start = expected_re_remaining,
    mean_expected_wpa_start = expected_wpa_remaining,
    mean_marginal_count_first_unit = marginal_count_1_to_0,
    mean_marginal_count_second_unit = marginal_count_2_to_1,
    mean_marginal_re_first_unit = marginal_re_1_to_0,
    mean_marginal_re_second_unit = marginal_re_2_to_1,
    mean_marginal_wpa_first_unit = marginal_wpa_1_to_0,
    mean_marginal_wpa_second_unit = marginal_wpa_2_to_1
  )]
  out <- Reduce(function(a, b) merge(a, b,
    by = c("game_pk", "team_id"), all = TRUE),
    list(challenged, opportunities, inventory))
  total_cols <- setdiff(
    union(names(challenged), names(opportunities)), c("game_pk", "team_id")
  )
  for (column in total_cols) out[is.na(get(column)), (column) := 0]
  out[, `:=`(
    net_re = gross_re - inventory_cost_re,
    net_wpa = gross_wpa - inventory_cost_wpa
  )]
  out[]
}

bootstrap_team_intervals <- function(team_game, reps = 500L, seed = 20260719L) {
  x <- data.table::as.data.table(team_game)
  metric_cols <- setdiff(names(x), c("game_pk", "team_id"))
  games <- unique(x$game_pk)
  set.seed(seed)
  estimates <- vector("list", reps)
  for (b in seq_len(reps)) {
    sampled <- games[sample.int(length(games), length(games), replace = TRUE)]
    weights <- data.table::data.table(game_pk = sampled)[, .(weight = .N), by = game_pk]
    draw <- merge(x, weights, by = "game_pk")
    estimates[[b]] <- draw[, lapply(names(.SD), function(column) {
      value <- .SD[[column]]
      if (startsWith(column, "mean_")) {
        stats::weighted.mean(value, weight, na.rm = TRUE)
      } else {
        sum(value * weight, na.rm = TRUE)
      }
    }), by = team_id, .SDcols = metric_cols]
    data.table::setnames(estimates[[b]], c("team_id", metric_cols))
    estimates[[b]][, replicate := b]
  }
  draws <- safe_rbindlist(estimates)
  draws[, (metric_cols) := lapply(.SD, as.numeric), .SDcols = metric_cols]
  long <- data.table::melt(draws, id.vars = c("team_id", "replicate"),
    variable.name = "metric", value.name = "estimate", variable.factor = FALSE)
  long[, .(
    lower = stats::quantile(estimate, 0.025, na.rm = TRUE),
    upper = stats::quantile(estimate, 0.975, na.rm = TRUE)
  ), by = .(team_id, metric)]
}

bootstrap_adjusted_intervals <- function(
  propensity_rows, reps = 500L, seed = 20260720L
) {
  x <- data.table::as.data.table(propensity_rows)[, .(
    observed_attempts = sum(challenge_by_team),
    expected_attempts = sum(predicted_challenge),
    challenge_residual = sum(challenge_by_team - predicted_challenge)
  ), by = .(game_pk, team_id)]
  bootstrap_team_intervals(x, reps = reps, seed = seed)
}
