test_that("fixed-clock context standardizes count, matchup, and pitch family", {
  rows <- data.table::data.table(
    balls = 1L, strikes = 2L, pitch_type = "FF", stand = "R", p_throws = "L"
  )
  out <- continuous_context_fields(rows)

  expect_equal(out$count_state, "1-2")
  expect_equal(out$matchup, "R-L")
  expect_equal(out$pitch_family, "fastball")
})

test_that("fixed-clock game folds are deterministic and complete", {
  one <- continuous_game_folds(1:25, folds = 5L, seed = 1L)
  two <- continuous_game_folds(1:25, folds = 5L, seed = 1L)

  expect_identical(one, two)
  expect_true(validate_continuous_game_folds(one, 5L))
  expect_equal(nrow(one), 25L)
  expect_error(
    validate_continuous_game_folds(one[fold != 5L], 5L),
    "exactly one complete fixed-clock fold"
  )
})

test_that("fixed-clock inventory loss follows the Poisson opportunity value", {
  lambda <- c(0, 1, 2)
  expected_re <- c(0, 0.2, 0.6)
  average_gain <- c(0, 0.2, 0.3)

  expect_equal(
    continuous_expected_inventory_loss(lambda, expected_re, 1L),
    (1 - exp(-lambda)) * average_gain
  )
  expect_equal(
    continuous_expected_inventory_loss(lambda, expected_re, 2L),
    (1 - exp(-lambda) * (1 + lambda)) * average_gain
  )
})
