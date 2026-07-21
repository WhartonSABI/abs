validate_challenge_resolution <- function(challenges) {
  x <- data.table::as.data.table(challenges)
  allowed <- c("overturned", "upheld")
  bad <- x[is.na(challenge_outcome) | !challenge_outcome %in% allowed]
  if (nrow(bad)) stop(nrow(bad), " challenges are unresolved or invalid")
  if (anyDuplicated(x$play_id)) stop("Duplicate challenge play_id values")
  data.frame(
    check = "challenge_resolution",
    passed = TRUE,
    observed = nrow(x),
    expected = nrow(x),
    detail = "Every challenge is explicitly overturned or upheld"
  )
}

reconcile_feed_totals <- function(challenges, totals) {
  parsed <- data.table::as.data.table(challenges)[, .(
    parsed_successful = sum(challenge_outcome == "overturned"),
    parsed_failed = sum(challenge_outcome == "upheld")
  ), by = .(game_pk, team_id = challenger_team_id)]
  expected <- data.table::as.data.table(totals)
  x <- merge(expected, parsed, by = c("game_pk", "team_id"), all.x = TRUE)
  x[, `:=`(
    parsed_successful = data.table::fcoalesce(parsed_successful, 0L),
    parsed_failed = data.table::fcoalesce(parsed_failed, 0L)
  )]
  mismatch <- x[used_successful != parsed_successful | used_failed != parsed_failed]
  if (nrow(mismatch)) {
    stop("Parsed challenge totals disagree with final feed totals in ",
      data.table::uniqueN(mismatch$game_pk), " games")
  }
  data.frame(
    check = "feed_total_reconciliation",
    passed = TRUE,
    observed = sum(x$parsed_successful + x$parsed_failed),
    expected = sum(x$used_successful + x$used_failed),
    detail = "Per-team successful and failed totals match every game feed"
  )
}

reconcile_final_inventory <- function(pitch_ledger, totals) {
  x <- data.table::as.data.table(pitch_ledger)
  final <- x[order(pitch_order), .SD[.N], by = game_pk]
  final <- merge(final, x[, .(final_inning = max(inning)), by = game_pk], by = "game_pk")
  # The feed's final `remaining` field reflects the two-challenge regulation
  # bucket and does not expose an unused extra-inning grant.
  final <- final[final_inning <= 9L]
  away <- final[, .(
    game_pk, team_id = away_team_id,
    reconstructed_remaining = away_challenges_after
  )]
  home <- final[, .(
    game_pk, team_id = home_team_id,
    reconstructed_remaining = home_challenges_after
  )]
  reconstructed <- data.table::rbindlist(list(away, home))
  expected <- data.table::as.data.table(totals)[game_pk %in% final$game_pk]
  check <- merge(expected, reconstructed, by = c("game_pk", "team_id"), all.x = TRUE)
  mismatch <- check[is.na(reconstructed_remaining) |
    remaining_final != reconstructed_remaining]
  if (nrow(mismatch)) {
    stop("Reconstructed final challenge inventory disagrees with ", nrow(mismatch),
      " game-team feed totals")
  }
  data.frame(
    check = "final_inventory_reconciliation",
    passed = TRUE,
    observed = nrow(check),
    expected = nrow(check),
    detail = "Sequential final inventory matches each regulation-game feed; extras are validated event-by-event"
  )
}

read_feed_only_challenge_quarantine <- function(
  path = "config/feed_only_challenges.csv"
) {
  if (!file.exists(path)) return(data.table::data.table())
  data.table::fread(path, na.strings = c("", "NA"))
}

validate_savant_matches <- function(
  pitch_ledger, savant, tolerance = 1e-6,
  quarantine = read_feed_only_challenge_quarantine()
) {
  local <- data.table::as.data.table(pitch_ledger)[challenge_occurred == TRUE]
  official <- prepare_savant_challenges(savant)
  fields <- c(
    "play_id", "game_pk", "savant_outcome", "savant_initial_call",
    "savant_challenger_team_id", "challenging_player_id", "edge_dist_calc"
  )
  x <- merge(local, official[, ..fields], by = "play_id", all.x = TRUE,
    suffixes = c("_local", "_savant"))
  x[, source_match := !is.na(savant_outcome) &
    game_pk_local == game_pk_savant &
    challenge_outcome == savant_outcome &
    initial_call == savant_initial_call &
    challenger_team_id == savant_challenger_team_id &
    challenger_player_id == challenging_player_id]
  missing_local <- official[!play_id %in% local$play_id]
  if (nrow(missing_local)) {
    stop(nrow(missing_local), " Savant challenge rows are absent from the live-feed parser")
  }
  x[, quarantined_feed_only := is.na(savant_outcome) &
    play_id %in% quarantine$play_id]
  mismatch <- x[(is.na(source_match) | source_match == FALSE) &
    quarantined_feed_only == FALSE]
  if (nrow(mismatch)) {
    stop(nrow(mismatch), " challenge rows have unexplained Savant mismatches")
  }
  data.frame(
    check = c("savant_challenge_match", "feed_only_challenge_quarantine"),
    passed = TRUE,
    observed = c(sum(x$source_match, na.rm = TRUE), sum(x$quarantined_feed_only)),
    expected = c(nrow(official), nrow(quarantine)),
    detail = c(
      "Every Savant row matches live feed on play_id, game, challenger, original call, and outcome",
      "Known live-feed-only challenges are explicitly quarantined and excluded from published success rate"
    )
  )
}

validate_geometry_outcomes <- function(pitch_ledger, savant = NULL) {
  x <- data.table::as.data.table(pitch_ledger)[
    challenge_occurred == TRUE & tracking_available == TRUE
  ]
  if (!is.null(savant)) x <- x[play_id %in% savant$play_id]
  x[, official_abs_call := ifelse(
    challenge_outcome == "overturned", opposite_call(initial_call), initial_call
  )]
  agreement <- x$abs_call == x$official_abs_call
  if (anyNA(agreement) || !all(agreement)) {
    stop(sum(is.na(agreement) | !agreement), " tracked challenged pitches fail geometry agreement")
  }
  data.frame(
    check = "geometry_agreement",
    passed = TRUE,
    observed = sum(agreement),
    expected = nrow(x),
    detail = "ABS geometry agrees with 100% of tracked official rulings"
  )
}

coverage_metrics <- function(pitch_ledger) {
  x <- data.table::as.data.table(pitch_ledger)
  x[, covered := statcast_identity_matched & tracking_available]
  league <- x[, .(
    scope = "league", team_id = NA_integer_, candidates = .N,
    covered = sum(covered), coverage = mean(covered)
  )]
  team <- x[, .(
    scope = "team", candidates = .N, covered = sum(covered), coverage = mean(covered)
  ), by = .(team_id = adverse_team_id)]
  data.table::rbindlist(list(league, team), fill = TRUE)
}

assert_candidate_coverage <- function(pitch_ledger, config) {
  coverage <- coverage_metrics(pitch_ledger)
  league_bad <- coverage[scope == "league" & coverage < config$coverage_league_min]
  team_bad <- coverage[scope == "team" & coverage < config$coverage_team_min]
  if (nrow(league_bad) || nrow(team_bad)) {
    stop("Candidate coverage is below the publication threshold; missed/out rankings are blocked")
  }
  coverage
}

validate_manual_audit <- function(
  pitch_ledger, path = "config/manual_audit.csv"
) {
  if (!file.exists(path)) stop("The required manual audit file is missing")
  audit <- data.table::fread(path, na.strings = c("", "NA"))
  if (nrow(audit) != 50L || anyDuplicated(audit$audit_id) ||
      any(audit$status != "pass")) {
    stop("Manual audit must contain 50 unique passing reviews")
  }
  if (audit[audit_category == "upheld_challenge", .N] < 10L ||
      audit[audit_category == "zero_inventory_correctable", .N] < 10L) {
    stop("Manual audit lacks the required upheld/zero-inventory strata")
  }
  key <- c("game_pk", "at_bat_number", "pitch_number", "play_id")
  x <- merge(
    audit,
    data.table::as.data.table(pitch_ledger),
    by = key, all.x = TRUE, suffixes = c("_audit", "")
  )
  if (nrow(x) != 50L || anyNA(x$initial_call)) {
    stop("Manual audit keys do not resolve uniquely to the pitch ledger")
  }
  logical_pass <- x[, data.table::fcase(
    audit_category == "upheld_challenge",
      challenge_outcome == "upheld" & initial_call == final_call &
        final_call == abs_call & challenge_remaining_after ==
        challenge_remaining_before - 1L,
    audit_category == "zero_inventory_correctable",
      opportunity_status == "out_of_challenges" &
        adverse_challenges_before == 0L & initial_call != abs_call,
    audit_category == "overturned_challenge",
      challenge_outcome == "overturned" & initial_call != final_call &
        final_call == abs_call & challenge_remaining_after ==
        challenge_remaining_before,
    audit_category == "missed_available",
      opportunity_status == "missed_available" &
        adverse_challenges_before > 0L & initial_call != abs_call,
    audit_category == "pa_terminal_linkage",
      challenge_occurred == TRUE & linkage_source == "plate_appearance_terminal",
    audit_category == "extra_inning_challenge",
      challenge_occurred == TRUE & inning > 9L &
        challenge_outcome %in% c("overturned", "upheld"),
    default = FALSE
  )]
  if (anyNA(logical_pass) || !all(logical_pass)) {
    stop(sum(is.na(logical_pass) | !logical_pass),
      " manually audited rows no longer satisfy their reviewed invariants")
  }
  data.frame(
    check = "manual_stratified_audit",
    passed = TRUE,
    observed = nrow(x),
    expected = 50L,
    detail = "Fifty reviewed events pass, including ten upheld and ten zero-inventory correctable calls"
  )
}

run_acceptance_checks <- function(pitch_ledger, live_data, savant, config) {
  checks <- data.table::rbindlist(list(
    validate_challenge_resolution(live_data$challenges),
    reconcile_feed_totals(live_data$challenges, live_data$totals),
    reconcile_final_inventory(pitch_ledger, live_data$totals),
    validate_savant_matches(pitch_ledger, savant),
    validate_geometry_outcomes(pitch_ledger, savant),
    validate_manual_audit(pitch_ledger)
  ), fill = TRUE)
  coverage <- assert_candidate_coverage(pitch_ledger, config)
  list(checks = checks, coverage = coverage)
}
