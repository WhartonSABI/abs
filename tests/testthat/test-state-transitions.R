test_that("full-count call flips create walk versus strikeout states", {
  ball <- call_branch_state("ball", 3, 2, 1, NA, NA, NA)
  strike <- call_branch_state("called_strike", 3, 2, 1, NA, NA, NA)
  expect_equal(c(ball$on_1b, ball$outs, ball$balls, ball$strikes), c(1L, 1L, 0L, 0L))
  expect_equal(c(strike$on_1b, strike$outs), c(0L, 2L))
})

test_that("forced walk scores a run with bases loaded", {
  state <- call_branch_state("ball", 3, 1, 0, 1, 1, 1)
  expect_equal(state$runs, 1L)
  expect_equal(c(state$on_1b, state$on_2b, state$on_3b), c(1L, 1L, 1L))
})

test_that("concurrent steals are preserved in both call branches", {
  movement <- list(remove_1b = 1L, add_2b = 1L, outs = 0L, runs = 0L)
  ball <- call_branch_state("ball", 1, 1, 0, 1, NA, NA, movement)
  strike <- call_branch_state("called_strike", 1, 1, 0, 1, NA, NA, movement)
  expect_equal(c(ball$on_1b, ball$on_2b), c(0L, 1L))
  expect_equal(c(strike$on_1b, strike$on_2b), c(0L, 1L))
})

test_that("manual position-player and outage exclusions are respected", {
  pitches <- data.table::data.table(
    game_pk = c(1L, 1L), at_bat_number = c(1L, 2L), pitch_number = 1L,
    correctable_opportunity = TRUE
  )
  exclusions <- data.table::data.table(
    game_pk = 1L, at_bat_number = 2L, pitch_number = 1L,
    reason = "position_player_pitching"
  )
  out <- apply_abs_eligibility(pitches, exclusions)
  expect_equal(out$abs_eligible, c(TRUE, FALSE))
  expect_equal(out$correctable_opportunity, c(TRUE, FALSE))
})

test_that("vectorized call branches reproduce scalar transitions", {
  rows <- data.table::data.table(
    balls_before = c(3L, 1L, 0L, 2L),
    strikes_before = c(1L, 2L, 0L, 1L),
    outs_before = c(0L, 1L, 2L, 1L),
    on_1b = c(1L, NA, 1L, 1L),
    on_2b = c(1L, NA, NA, NA),
    on_3b = c(1L, NA, NA, NA),
    concurrent_remove_1b = c(0L, 0L, 1L, 1L),
    concurrent_remove_2b = 0L,
    concurrent_remove_3b = 0L,
    concurrent_add_1b = 0L,
    concurrent_add_2b = c(0L, 0L, 1L, 1L),
    concurrent_add_3b = 0L,
    concurrent_runs = 0L,
    concurrent_outs = 0L,
    inning = c(1L, 4L, 8L, 10L),
    half = c("top", "bottom", "top", "bottom")
  )
  calls <- c("ball", "called_strike", "ball", "called_strike")
  vectorized <- vectorized_call_branch(rows, calls)
  scalar <- data.table::rbindlist(lapply(seq_len(nrow(rows)), function(i) {
    row <- as.list(rows[i])
    movement <- list(
      remove_1b = row$concurrent_remove_1b,
      remove_2b = row$concurrent_remove_2b,
      remove_3b = row$concurrent_remove_3b,
      add_1b = row$concurrent_add_1b,
      add_2b = row$concurrent_add_2b,
      add_3b = row$concurrent_add_3b,
      runs = row$concurrent_runs,
      outs = row$concurrent_outs
    )
    call_branch_state(
      calls[[i]], row$balls_before, row$strikes_before, row$outs_before,
      row$on_1b, row$on_2b, row$on_3b, movement, row$inning, row$half
    )
  }))
  expect_equal(vectorized, scalar, ignore_attr = TRUE)
})

test_that("cancelled schedule entries are not completed games", {
  schedule <- data.table::data.table(
    abstract_state = c("Final", "Final"),
    detailed_state = c("Final", "Cancelled"),
    game_pk = c(1L, 2L)
  )
  expect_equal(completed_game_ids(schedule), 1L)
})
