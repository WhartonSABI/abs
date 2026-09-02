synthetic_fixed_clock_re_table_1d <- function() {
  out <- data.table::CJ(
    outs = 0:2,
    base_state = c("000", "001", "010", "011", "100", "101", "110", "111"),
    balls = 0:3,
    strikes = 0:2,
    sorted = TRUE
  )
  out[, re := 0.5 - 0.12 * outs + 0.04 * balls - 0.05 * strikes +
    0.02 * nchar(gsub("0", "", base_state))
  ]
  out[]
}

synthetic_fixed_clock_gain_rows_1d <- function() {
  data.table::data.table(
    game_pk = c("1", "1"),
    initial_call = c("called_strike", "ball"),
    adverse_team_id = c(10L, 20L),
    bat_team_id = c(10L, 10L),
    balls_before = c(1L, 3L),
    strikes_before = c(1L, 1L),
    outs_before = c(1L, 1L),
    inning = c(5L, 5L),
    half = c("top", "top"),
    on_1b = c(NA_integer_, NA_integer_),
    on_2b = c(NA_integer_, NA_integer_),
    on_3b = c(NA_integer_, NA_integer_),
    edge_distance_inches = c(-100, 100),
    challenge_outcome = c("upheld", "overturned")
  )
}

test_that("fixed-clock G uses branch state but no pitch geometry", {
  table <- synthetic_fixed_clock_re_table_1d()
  rows <- synthetic_fixed_clock_gain_rows_1d()
  scored <- fixed_clock_pitch_gains_1d(rows, table, "G_decision")
  permuted <- data.table::copy(rows)
  permuted[, `:=`(
    edge_distance_inches = rev(edge_distance_inches),
    challenge_outcome = rev(challenge_outcome)
  )]
  rescored <- fixed_clock_pitch_gains_1d(permuted, table, "G_decision")
  expect_equal(scored$G_decision, rescored$G_decision)
  expect_true(all(is.finite(scored$G_decision)))
  expect_identical(
    fixed_clock_re288_hash_1d(table),
    fixed_clock_re288_hash_1d(data.table::copy(table))
  )
})

test_that("weighted RE288 refit represents a whole-game bootstrap", {
  observations <- data.table::data.table(
    game_pk = rep(c("a", "b"), each = 2L),
    outs = 0L,
    base_state = "000",
    balls = 0L,
    strikes = 0L,
    runs_to_end = c(0, 0, 2, 2)
  )
  a_only <- fixed_clock_re288_weighted_table_1d(
    observations,
    data.table::data.table(game_pk = c("a", "b"), bootstrap_weight = c(2, 0)),
    shrinkage = 0
  )
  b_only <- fixed_clock_re288_weighted_table_1d(
    observations,
    data.table::data.table(game_pk = c("a", "b"), bootstrap_weight = c(0, 2)),
    shrinkage = 0
  )
  expect_equal(a_only[outs == 0 & base_state == "000" & balls == 0 & strikes == 0, re], 0)
  expect_equal(b_only[outs == 0 & base_state == "000" & balls == 0 & strikes == 0, re], 2)
  expect_equal(nrow(a_only), 288L)
})

test_that("context-adjusted observations define all required nuisance fields", {
  rows <- data.table::data.table(
    game_pk = c("1", "1"),
    game_date = as.Date(c("2023-04-01", "2023-04-01")),
    inning = 1L,
    inning_topbot = "Top",
    balls = c(0L, 1L),
    strikes = c(0L, 0L),
    outs_when_up = 0L,
    on_1b = NA_integer_,
    on_2b = NA_integer_,
    on_3b = NA_integer_,
    bat_score = c(0L, 0L),
    post_bat_score = c(0L, 1L),
    batter = 101L,
    pitcher = 201L,
    home_team = "PHI"
  )
  out <- prepare_fixed_clock_context_re_observations_1d(rows)
  expect_equal(nrow(out), 2L)
  expect_false(anyNA(out$state_288))
  expect_equal(out$season_centered, c(-1, -1))
  expect_true(is.factor(out$batter_id))
  expect_true(all(out$runs_to_end == 1L))
})
