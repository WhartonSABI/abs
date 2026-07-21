add_realized_remaining <- function(timeline) {
  x <- data.table::copy(data.table::as.data.table(timeline))
  data.table::setorder(x, game_pk, team_id, -pitch_order)
  x[, `:=`(
    future_correctable_game = cumsum(data.table::shift(as.integer(correctable_for_team),
      fill = 0L)),
    future_re_game = cumsum(data.table::shift(data.table::fcoalesce(potential_re, 0), fill = 0)),
    future_wpa_game = cumsum(data.table::shift(data.table::fcoalesce(potential_wpa, 0), fill = 0)),
    future_correctable_offense_game = cumsum(data.table::shift(
      as.integer(correctable_for_team & role == "offense"), fill = 0L)),
    future_correctable_defense_game = cumsum(data.table::shift(
      as.integer(correctable_for_team & role == "defense"), fill = 0L))
  ), by = .(game_pk, team_id)]
  # Regulation is one segment; each extra inning starts a new replenishment segment.
  x[, replenishment_segment := ifelse(inning <= 9L, 0L, inning)]
  x[, `:=`(
    future_correctable_segment = cumsum(data.table::shift(
      as.integer(correctable_for_team), fill = 0L)),
    future_re_segment = cumsum(data.table::shift(data.table::fcoalesce(potential_re, 0), fill = 0)),
    future_wpa_segment = cumsum(data.table::shift(data.table::fcoalesce(potential_wpa, 0), fill = 0)),
    future_correctable_offense_segment = cumsum(data.table::shift(
      as.integer(correctable_for_team & role == "offense"), fill = 0L)),
    future_correctable_defense_segment = cumsum(data.table::shift(
      as.integer(correctable_for_team & role == "defense"), fill = 0L))
  ), by = .(game_pk, team_id, replenishment_segment)]
  x[, `:=`(
    realized_correctable_remaining = ifelse(
      inventory_before == 0L, future_correctable_segment, future_correctable_game),
    realized_re_remaining = ifelse(inventory_before == 0L, future_re_segment, future_re_game),
    realized_wpa_remaining = ifelse(inventory_before == 0L, future_wpa_segment, future_wpa_game),
    realized_correctable_offense_remaining = ifelse(
      inventory_before == 0L, future_correctable_offense_segment,
      future_correctable_offense_game),
    realized_correctable_defense_remaining = ifelse(
      inventory_before == 0L, future_correctable_defense_segment,
      future_correctable_defense_game)
  )]
  data.table::setorder(x, game_pk, team_id, pitch_order)
  x[]
}

remaining_bucket_columns <- function() {
  c("inning", "half", "outs_before", "role", "score_bucket")
}

add_loog_expected_remaining <- function(timeline) {
  x <- data.table::copy(data.table::as.data.table(timeline))
  group <- remaining_bucket_columns()
  values <- c(
    "realized_correctable_remaining", "realized_re_remaining", "realized_wpa_remaining",
    "realized_correctable_offense_remaining", "realized_correctable_defense_remaining"
  )
  overall <- x[, c(
    list(bucket_n = .N),
    lapply(.SD, sum, na.rm = TRUE)
  ), by = group, .SDcols = values]
  game <- x[, c(
    list(game_n = .N),
    lapply(.SD, sum, na.rm = TRUE)
  ), by = c(group, "game_pk"), .SDcols = values]
  sum_names <- paste0(values, "_sum")
  data.table::setnames(overall, values, sum_names)
  data.table::setnames(game, values, paste0(values, "_game_sum"))
  x <- merge(x, overall, by = group, all.x = TRUE, sort = FALSE)
  x <- merge(x, game, by = c(group, "game_pk"), all.x = TRUE, sort = FALSE)
  denominator <- pmax(x$bucket_n - x$game_n, 1L)
  x[, expected_correctable_remaining :=
    (realized_correctable_remaining_sum - realized_correctable_remaining_game_sum) / denominator]
  x[, expected_re_remaining :=
    (realized_re_remaining_sum - realized_re_remaining_game_sum) / denominator]
  x[, expected_wpa_remaining :=
    (realized_wpa_remaining_sum - realized_wpa_remaining_game_sum) / denominator]
  x[, expected_correctable_offense_remaining :=
    (realized_correctable_offense_remaining_sum -
      realized_correctable_offense_remaining_game_sum) / denominator]
  x[, expected_correctable_defense_remaining :=
    (realized_correctable_defense_remaining_sum -
      realized_correctable_defense_remaining_game_sum) / denominator]
  drop <- c("bucket_n", "game_n", sum_names, paste0(values, "_game_sum"))
  x[, (intersect(drop, names(x))) := NULL]
  data.table::setorder(x, game_pk, team_id, pitch_order)
  x[]
}

estimate_behavior_probability <- function(timeline, prior_n = 30) {
  x <- data.table::copy(data.table::as.data.table(timeline))
  x[, adverse_pitch := adverse_for_team]
  x[, behavior_bucket := paste(
    pmin(inning, 10L), half, outs_before, role, score_bucket,
    paste0(balls_before, "-", strikes_before), sep = "|"
  )]
  training_row <- x$adverse_pitch & x$inventory_before > 0L
  league_p <- x[training_row == TRUE, mean(challenge_by_team, na.rm = TRUE)]
  total <- x[training_row == TRUE, .(
    attempts = sum(challenge_by_team, na.rm = TRUE), opportunities = .N
  ), by = behavior_bucket]
  by_game <- x[training_row == TRUE, .(
    game_attempts = sum(challenge_by_team, na.rm = TRUE), game_opportunities = .N
  ), by = .(behavior_bucket, game_pk)]
  x <- merge(x, total, by = "behavior_bucket", all.x = TRUE, sort = FALSE)
  x <- merge(x, by_game, by = c("behavior_bucket", "game_pk"), all.x = TRUE, sort = FALSE)
  x[is.na(attempts), attempts := 0L]
  x[is.na(game_attempts), game_attempts := 0L]
  x[is.na(opportunities), opportunities := 0L]
  x[is.na(game_opportunities), game_opportunities := 0L]
  x[, challenge_probability_loog := (
    attempts - game_attempts + prior_n * league_p
  ) / (
    opportunities - game_opportunities + prior_n
  )]
  x[, c("attempts", "opportunities", "game_attempts", "game_opportunities") := NULL]
  x[]
}

inventory_recursion_game_team <- function(x) {
  x <- data.table::copy(x)
  data.table::setorder(x, -pitch_order)
  metrics <- c("count", "re", "wpa")
  continuation <- matrix(0, nrow = 3L, ncol = length(metrics),
    dimnames = list(as.character(0:2), metrics))
  output <- array(0, dim = c(nrow(x), 3L, length(metrics)),
    dimnames = list(NULL, as.character(0:2), metrics))
  previous_inning <- NA_integer_
  for (i in seq_len(nrow(x))) {
    inning_now <- x$inning[[i]]
    if (!is.na(previous_inning) && previous_inning > 9L && inning_now != previous_inning) {
      continuation["0", ] <- continuation["1", ]
    }
    p <- data.table::fcoalesce(x$challenge_probability_loog[[i]], 0)
    adverse <- isTRUE(x$adverse_for_team[[i]])
    correct <- isTRUE(x$correctable_for_team[[i]])
    gains <- c(
      count = as.numeric(correct),
      re = ifelse(is.na(x$potential_re[[i]]), 0, x$potential_re[[i]]),
      wpa = ifelse(is.na(x$potential_wpa[[i]]), 0, x$potential_wpa[[i]])
    )
    next_value <- continuation
    if (adverse) {
      for (k in 1:2) {
        if (correct) {
          continuation[as.character(k), ] <- next_value[as.character(k), ] + p * gains
        } else {
          continuation[as.character(k), ] <-
            (1 - p) * next_value[as.character(k), ] + p * next_value[as.character(k - 1L), ]
        }
      }
    }
    continuation["0", ] <- next_value["0", ]
    output[i, , ] <- continuation
    previous_inning <- inning_now
  }
  # Return to chronological order.
  for (k in 0:2) for (m in metrics) {
    x[[paste0("reachable_", m, "_k", k)]] <- output[, k + 1L, m]
  }
  data.table::setorder(x, pitch_order)
  for (m in metrics) {
    x[[paste0("marginal_", m, "_1_to_0")]] <-
      x[[paste0("reachable_", m, "_k1")]] - x[[paste0("reachable_", m, "_k0")]]
    x[[paste0("marginal_", m, "_2_to_1")]] <-
      x[[paste0("reachable_", m, "_k2")]] - x[[paste0("reachable_", m, "_k1")]]
  }
  x[]
}

add_inventory_recursion <- function(timeline) {
  x <- estimate_behavior_probability(timeline)
  out <- safe_rbindlist(lapply(split(x, interaction(x$game_pk, x$team_id, drop = TRUE)),
    inventory_recursion_game_team))
  data.table::setorder(out, game_pk, team_id, pitch_order)
  out[]
}

build_remaining_opportunities <- function(pitch_ledger) {
  pitch_ledger <- data.table::copy(data.table::as.data.table(pitch_ledger))
  for (name in c("potential_challenger_re", "potential_challenger_wpa")) {
    if (!name %in% names(pitch_ledger)) pitch_ledger[, (name) := NA_real_]
  }
  timeline <- team_timeline(pitch_ledger)
  timeline <- add_realized_remaining(timeline)
  timeline <- add_loog_expected_remaining(timeline)
  add_inventory_recursion(timeline)
}
