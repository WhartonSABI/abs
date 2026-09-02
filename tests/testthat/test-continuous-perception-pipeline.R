test_that("continuous context and coordinates use physical inches", {
  rows <- data.table::data.table(
    plate_x = 0.5, plate_z = 3, sz_bot = 2, sz_top = 4,
    balls = 1L, strikes = 2L, pitch_type = "FF", stand = "R", p_throws = "L"
  )
  out <- continuous_context_fields(continuous_physical_coordinates(rows))
  expect_equal(out$x_in, 6)
  expect_equal(out$z_rel_in, 0)
  expect_equal(out$zone_half_height_in, 12)
  expect_equal(out$count_state, "1-2")
  expect_equal(out$pitch_family, "fastball")
})

test_that("game folds contain each game exactly once", {
  folds <- continuous_game_folds(1:25, folds = 5, seed = 1)
  expect_true(validate_continuous_game_folds(folds, 5))
  expect_equal(nrow(folds), 25L)
})

test_that("fit allowlist rejects challenge and outcome leakage", {
  expect_true(assert_perception_fit_is_label_free(c("x_in", "pitch_family")))
  expect_error(
    assert_perception_fit_is_label_free(c("x_in", "challenge_outcome")),
    "leaked forbidden labels"
  )
})

test_that("hypothetical decision gain does not consult ABS or outcomes", {
  pitch <- data.table::data.table(
    balls_before = 1L, strikes_before = 1L, outs_before = 0L,
    on_1b = 0L, on_2b = 0L, on_3b = 0L,
    inning = 1L, half = "top",
    concurrent_outs = 0L, concurrent_runs = 0L,
    concurrent_remove_1b = 0L, concurrent_remove_2b = 0L,
    concurrent_remove_3b = 0L, concurrent_add_1b = 0L,
    concurrent_add_2b = 0L, concurrent_add_3b = 0L,
    abs_call = "ball", challenge_outcome = "overturned"
  )
  re_model <- list(table = data.table::data.table(
    outs = c(0L, 0L), base_state = c("000", "000"),
    balls = c(2L, 1L), strikes = c(1L, 2L), re = c(0.5, 0.3)
  ))
  reference <- continuous_hypothetical_batter_gain(pitch, re_model)
  changed <- data.table::copy(pitch)
  changed[, `:=`(abs_call = "called_strike", challenge_outcome = "upheld")]
  expect_equal(reference, 0.2)
  expect_identical(
    continuous_hypothetical_batter_gain(changed, re_model), reference
  )
})

test_that("inventory loss uses a challenge-action-free Poisson opportunity value", {
  lambda <- c(0, 1, 2)
  expected_re <- c(0, 0.2, 0.6)
  first <- continuous_expected_inventory_loss(lambda, expected_re, 1L)
  second <- continuous_expected_inventory_loss(lambda, expected_re, 2L)
  average_gain <- c(0, 0.2, 0.3)

  expect_equal(first, (1 - exp(-lambda)) * average_gain)
  expect_equal(second, (1 - exp(-lambda) * (1 + lambda)) * average_gain)
  expect_true(all(second <= first + 1e-12))
  expect_error(
    continuous_expected_inventory_loss(1, 0.2, 0L),
    "invalid"
  )
})
