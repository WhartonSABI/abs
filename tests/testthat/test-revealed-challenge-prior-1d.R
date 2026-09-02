revealed_prior_test_ledger_1d <- function(games = 12L, rows_per_game = 60L) {
  set.seed(992)
  data.table::rbindlist(lapply(seq_len(games), function(game) {
    n <- rows_per_game
    call <- rep(c("called_strike", "ball"), length.out = n)
    margin <- c(stats::rnorm(n / 2, -0.5, 2), stats::rnorm(n / 2, 0.8, 3))
    data.table::data.table(
      game_pk = paste0("prior_", game), pitch_order = seq_len(n),
      initial_call = call, tracking_available = TRUE, abs_eligible = TRUE,
      edge_distance_inches = margin,
      balls_before = seq_len(n) %% 4L, strikes_before = seq_len(n) %% 3L,
      pitch_type = rep(c("FF", "SL", "CH", "EP"), length.out = n),
      challenge_occurred = seq_len(n) %% 17L == 0L,
      challenger_role = "batter", bat_team_challenges_before = 2L,
      fld_team_challenges_before = 2L,
      challenge_outcome = "upheld", abs_call = "ball",
      delta_home_win_exp = stats::rnorm(n)
    )
  }))
}

test_that("role-prior builders ignore inventory/action and orient margin", {
  ledger <- revealed_prior_test_ledger_1d(4L, 40L)
  one <- build_revealed_challenge_prior_1d(ledger)
  changed <- data.table::copy(ledger)
  changed[, `:=`(
    challenge_occurred = !challenge_occurred,
    bat_team_challenges_before = 0L, fld_team_challenges_before = 0L,
    challenge_outcome = "overturned", abs_call = "called_strike",
    delta_home_win_exp = delta_home_win_exp + 100
  )]
  two <- build_revealed_challenge_prior_1d(changed)
  expect_equal(one, two, tolerance = 0)
  expect_equal(one[role == "offense", edge_distance_inches],
               one[role == "offense", physical_edge_distance_inches])
  expect_equal(one[role == "defense", edge_distance_inches],
               -one[role == "defense", physical_edge_distance_inches])
  expect_length(intersect(names(one), revealed_challenge_prior_1d_forbidden_columns()), 0L)
  expect_true(all(grepl("__", one$context_count_family, fixed = TRUE)))
})

test_that("strict clean assertion rejects action and outcome columns", {
  expect_error(assert_revealed_challenge_prior_1d_clean(
    data.table::data.table(game_pk = "g", challenged = 0L)
  ), "forbidden")
  expect_error(assert_revealed_challenge_prior_1d_clean(
    data.table::data.table(game_pk = "g", challenge_outcome = "upheld")
  ), "forbidden")
})

test_that("outer-fold prior selection is separated and shares shapes", {
  ledger <- revealed_prior_test_ledger_1d(12L, 60L)
  rows <- build_revealed_challenge_prior_1d(ledger)
  folds <- continuous_game_folds(rows$game_pk, 3L, 22L)
  result <- crossfit_revealed_challenge_prior_1d(
    rows, folds, folds = 3L, inner_folds = 2L,
    components = c(1L, 3L), tolerance = 1e-3,
    max_iterations = 200L, progress = FALSE
  )
  expect_equal(nrow(result$oof_log_scores), nrow(rows))
  expect_true(all(is.finite(result$oof_log_scores$location_log_density)))
  expect_true(all(is.finite(result$oof_log_scores$count_only_log_density)))
  expect_true(all(result$diagnostics$overlap_games == 0L))
  expect_equal(nrow(result$diagnostics), 6L)
  expect_true(all(result$candidate_metrics[selected == TRUE, components] %in% c(1L, 3L)))
  expect_true(all(result$candidate_metrics[selected == TRUE, within_one_se]))
  expect_length(result$fold_fits, 6L)
  for (item in result$fold_fits) {
    expect_equal(item$primary_fit$mean, item$count_only_fit$mean)
    expect_equal(item$primary_fit$sd, item$count_only_fit$sd)
    expect_equal(item$primary_fit$weight, item$count_only_fit$weight)
    expect_length(intersect(item$primary_fit$outcome_action_columns_used,
                            revealed_challenge_prior_1d_forbidden_columns()), 0L)
  }
  expect_setequal(unique(result$context_table$context_variant),
                  c("count_x_pitch_family", "count_only"))
  expect_true(all(nzchar(result$diagnostics$training_fingerprint)))
  expect_true(all(nzchar(result$diagnostics$scoring_fingerprint)))
})

test_that("changing actions and outcomes leaves crossfit fingerprints invariant", {
  ledger <- revealed_prior_test_ledger_1d(8L, 40L)
  changed <- data.table::copy(ledger)
  changed[, `:=`(challenge_occurred = !challenge_occurred,
                 challenge_outcome = "overturned", abs_call = "called_strike")]
  one <- build_revealed_challenge_prior_1d(ledger)
  two <- build_revealed_challenge_prior_1d(changed)
  expect_equal(
    .revealed_prior_fingerprint_1d(one, "context_count_family"),
    .revealed_prior_fingerprint_1d(two, "context_count_family")
  )
})
