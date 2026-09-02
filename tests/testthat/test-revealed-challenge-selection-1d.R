revealed_selection_test_ledger_1d <- function(games = 12L, rows_per_game = 30L) {
  set.seed(871)
  x <- data.table::rbindlist(lapply(seq_len(games), function(game) {
    n <- rows_per_game
    role <- rep(c("offense", "defense"), length.out = n)
    margin <- stats::rnorm(n, 0, 3.5)
    speed <- stats::runif(n, 68, 99)
    pitch_type <- sample(c("FF", "SL", "CH", "EP"), n, replace = TRUE)
    eta <- -2.2 + 0.65 * margin +
      0.8 * (speed < 75) + 0.9 * (pitch_type == "EP") +
      0.5 * (abs(margin) > 3)
    challenged <- stats::rbinom(n, 1L, stats::pnorm(eta))
    bat <- 100L + game %% 5L
    fld <- 200L + game %% 4L
    out <- data.table::data.table(
      game_pk = paste0("sel_", game), pitch_order = seq_len(n),
      at_bat_number = seq_len(n), pitch_number = 1L,
      initial_call = ifelse(role == "offense", "called_strike", "ball"),
      challenge_occurred = challenged == 1L,
      challenger_role = ifelse(
        challenged == 0L, NA_character_,
        ifelse(role == "offense", "batter", "catcher")
      ),
      challenger_team_id = ifelse(
        challenged == 0L, NA_integer_, ifelse(role == "offense", bat, fld)
      ),
      batter_id = 1000L + seq_len(n) %% 8L,
      pitcher_id = 2000L + seq_len(n) %% 6L,
      fielder_2 = 3000L + seq_len(n) %% 4L,
      bat_team_id = bat, fld_team_id = fld,
      bat_team_challenges_before = 2L,
      fld_team_challenges_before = 2L,
      tracking_available = TRUE, abs_eligible = TRUE,
      edge_distance_inches = ifelse(role == "offense", margin, -margin),
      balls_before = seq_len(n) %% 4L,
      strikes_before = seq_len(n) %% 3L,
      inning = 1L + seq_len(n) %% 9L, outs_before = seq_len(n) %% 3L,
      release_speed = speed, pitch_type = pitch_type
    )
    out[, challenge_outcome := ifelse(
      challenge_occurred, sample(c("upheld", "overturned"), .N, TRUE), NA
    )]
    out[, `:=`(
      is_overturned = challenge_occurred & challenge_outcome == "overturned",
      final_call = initial_call,
      abs_call = ifelse(edge_distance_inches > 0, "ball", "called_strike"),
      delta_home_win_exp = stats::rnorm(.N),
      actual_wpa_gain = stats::rnorm(.N)
    )]
    out[]
  }))
  x[]
}

test_that("revealed-selection builders orient roles and strip outcomes", {
  ledger <- revealed_selection_test_ledger_1d(4L, 24L)
  offense <- build_revealed_challenge_selection_offense_1d(ledger)
  defense <- build_revealed_challenge_selection_defense_1d(ledger)

  expect_true(all(offense$role == "offense"))
  expect_true(all(defense$role == "defense"))
  expect_equal(
    offense$role_margin_inches, offense$physical_edge_distance_inches
  )
  expect_equal(
    defense$role_margin_inches, -defense$physical_edge_distance_inches
  )
  expect_true(all(offense$decision_unit_id == offense$batter_id))
  expect_true(all(
    defense$decision_unit_id == defense$pitcher_catcher_dyad_id
  ))
  expect_true(all(offense$inventory_before > 0))
  expect_true(all(defense$inventory_before > 0))
  expect_length(intersect(
    names(offense), revealed_challenge_selection_1d_outcome_columns()
  ), 0L)
  expect_setequal(
    unique(c(offense$speed_stratum, defense$speed_stratum)),
    c("regular_speed", "slow_non_eephus", "eephus")
  )
  expect_setequal(
    unique(c(offense$margin_stratum, defense$margin_stratum)),
    c("local_abs_margin_le_3", "tail_abs_margin_gt_3")
  )
})

test_that("official outcomes cannot change opportunity-selection inputs", {
  ledger <- revealed_selection_test_ledger_1d(4L, 24L)
  altered <- data.table::copy(ledger)
  altered[, `:=`(
    challenge_outcome = ifelse(
      challenge_outcome == "upheld", "overturned", "upheld"
    ),
    is_overturned = !is_overturned,
    final_call = ifelse(final_call == "ball", "called_strike", "ball"),
    abs_call = ifelse(abs_call == "ball", "called_strike", "ball"),
    delta_home_win_exp = delta_home_win_exp + 100,
    actual_wpa_gain = actual_wpa_gain - 100
  )]
  one <- build_revealed_challenge_selection_1d(ledger)
  two <- build_revealed_challenge_selection_1d(altered)
  expect_equal(one, two, tolerance = 0)
  expect_error(
    assert_revealed_challenge_selection_1d_outcome_free(
      data.table::data.table(challenged = 1L, challenge_outcome = "upheld"),
      "bad"
    ),
    "forbidden"
  )
})

test_that("zero-inventory rows and wrong-role challenges are rejected", {
  ledger <- revealed_selection_test_ledger_1d(4L, 24L)
  zero_key <- ledger[initial_call == "called_strike"][1L, .(game_pk, pitch_order)]
  ledger[zero_key, on = .(game_pk, pitch_order), bat_team_challenges_before := 0L]
  offense <- build_revealed_challenge_selection_offense_1d(ledger)
  expect_false(any(
    offense$game_pk == zero_key$game_pk &
      offense$pitch_order == zero_key$pitch_order
  ))

  bad <- data.table::copy(ledger)
  index <- which(bad$initial_call == "called_strike")[[2L]]
  bad[index, `:=`(
    challenge_occurred = TRUE,
    challenger_role = "catcher",
    challenger_team_id = fld_team_id
  )]
  expect_error(
    build_revealed_challenge_selection_offense_1d(bad),
    "wrong team or role"
  )
})

test_that("clustered comparison and promotion gate have explicit direction", {
  x <- data.table::data.table(
    game_pk = rep(paste0("g", 1:8), each = 4L),
    role = rep(c("offense", "defense"), each = 16L),
    margin_stratum = rep(c(
      "local_abs_margin_le_3", "tail_abs_margin_gt_3"
    ), 16L),
    speed_stratum = "regular_speed",
    full_support_log_loss = 0.20,
    local_baseline_log_loss = 0.30
  )
  comparison <- compare_revealed_challenge_selection_1d(x)
  expect_true(all(
    comparison[scope == "full_support", improvement_full_over_local] > 0
  ))
  gate <- gate_revealed_challenge_selection_1d(comparison)
  expect_true(all(gate$promote_speed_aware_full_support))
})

test_that("cross-fitted benchmark returns full-support OOF predictions", {
  skip_if_not_installed("mgcv")
  ledger <- revealed_selection_test_ledger_1d(12L, 60L)
  rows <- build_revealed_challenge_selection_1d(ledger)
  folds <- continuous_game_folds(
    rows$game_pk, folds = 3L, seed = 913L
  )
  result <- crossfit_revealed_challenge_selection_1d(
    rows, fold_assignment = folds, folds = 3L,
    nthreads = 1L, keep_models = TRUE, progress = FALSE
  )
  expect_equal(nrow(result$oof_predictions), nrow(rows))
  expect_equal(
    data.table::uniqueN(result$oof_predictions[, .(game_pk, pitch_order)]),
    nrow(rows)
  )
  expect_true(all(data.table::between(
    result$oof_predictions$full_support_probability, 0, 1
  )))
  expect_true(all(data.table::between(
    result$oof_predictions$local_baseline_probability, 0, 1
  )))
  expect_true(any(result$oof_predictions$baseline_extrapolated))
  expect_equal(nrow(result$diagnostics), 3L * 2L)
  expect_equal(nrow(result$width_estimates), 3L * 2L)
  expect_true(all(result$width_estimates$sigma_inches > 0))
  expect_true(all(grepl(
    "ti\\(role_margin_inches, release_speed_model",
    result$diagnostics$full_formula
  )))
  expect_true(all(grepl("stake_G", result$diagnostics$full_formula)))
  expect_setequal(result$gate$role, c("offense", "defense"))
  expect_true(all(c(
    "challenge_probability", "selected_model", "unit_fallback",
    "team_fallback"
  ) %in% names(result$oof_predictions)))
  clock_score <- score_revealed_challenge_selection_policy_clock_1d(
    rows, result
  )
  expect_equal(nrow(clock_score), nrow(rows))
  expect_true(all(data.table::between(clock_score$probability_k1, 0, 1)))
  expect_true(all(data.table::between(clock_score$probability_k2, 0, 1)))
  expect_length(intersect(
    names(result$oof_predictions),
    revealed_challenge_selection_1d_outcome_columns()
  ), 0L)
})

test_that("role pooling is scoped and tail strata do not enter the GAM", {
  rows <- build_revealed_challenge_selection_1d(
    revealed_selection_test_ledger_1d(4L, 20L)
  )
  offense <- .revealed_selection_training_spec_1d(
    rows[role == "offense"], full_support = TRUE
  )
  defense <- .revealed_selection_training_spec_1d(
    rows[role == "defense"], full_support = TRUE
  )
  expect_setequal(names(offense$random_levels), c("batter_id", "team_id"))
  expect_setequal(
    names(defense$random_levels),
    c("pitcher_catcher_dyad_id", "team_id")
  )
  expect_length(
    intersect(
      c("margin_stratum", "speed_stratum"),
      names(offense$factor_levels)
    ),
    0L
  )
})

test_that("the local common-probit fit recovers a known effective width", {
  skip_if_not_installed("mgcv")
  set.seed(1813)
  true_width <- 1.8
  ledger <- revealed_selection_test_ledger_1d(40L, 80L)
  rows <- build_revealed_challenge_selection_1d(ledger)
  rows[, challenged := stats::rbinom(
    .N, 1L,
    stats::pnorm((role_margin_inches - 0.35) / true_width)
  )]
  games <- sort(unique(rows$game_pk))
  fitted <- fit_revealed_challenge_selection_role_fold_1d(
    rows[game_pk %in% games[1:30]],
    rows[game_pk %in% games[31:40]],
    role = "offense",
    nthreads = 1L,
    keep_models = FALSE
  )
  expect_equal(
    fitted$diagnostics$effective_width_inches,
    true_width,
    tolerance = 0.35
  )
})
