synthetic_inventory_game <- function() {
  data.table::data.table(
    game_pk = 1L, at_bat_number = 1:5, pitch_number = 1L,
    inning = c(1L, 2L, 9L, 10L, 11L), is_top = TRUE,
    bat_team_id = 10L, fld_team_id = 20L,
    challenge_occurred = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    challenge_outcome = c("upheld", "upheld", NA, NA, NA),
    challenger_team_id = c(10L, 10L, NA, NA, NA)
  )
}

test_that("upheld challenges decrement and extra innings replenish zero", {
  x <- reconstruct_inventory_game(synthetic_inventory_game())
  expect_equal(x$away_challenges_before, c(2L, 1L, 0L, 1L, 1L))
  expect_equal(x$away_challenges_after, c(1L, 0L, 0L, 1L, 1L))
})

test_that("overturned challenge retains inventory", {
  x <- synthetic_inventory_game()[1]
  x[, challenge_outcome := "overturned"]
  out <- reconstruct_inventory_game(x)
  expect_equal(out$away_challenges_before, 2L)
  expect_equal(out$away_challenges_after, 2L)
})

test_that("zero inventory replenishes on entry to each extra inning", {
  x <- synthetic_inventory_game()
  x$challenge_occurred <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  x$challenge_outcome <- c("upheld", "upheld", NA, "upheld", NA)
  x$challenger_team_id <- c(10L, 10L, NA, 10L, NA)
  out <- reconstruct_inventory_game(x)
  expect_equal(out$away_challenges_before, c(2L, 1L, 0L, 1L, 1L))
  expect_equal(out$away_challenges_after, c(1L, 0L, 0L, 0L, 1L))
})

test_that("unresolved challenge blocks inventory reconstruction", {
  x <- synthetic_inventory_game()[1]
  x[, challenge_outcome := "unresolved"]
  expect_error(reconstruct_inventory_game(x), "unresolved")
})
