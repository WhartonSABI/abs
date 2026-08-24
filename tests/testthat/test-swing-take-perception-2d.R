synthetic_swing_take_2d <- function(pitches_per_strike = 120L) {
  data.table::rbindlist(lapply(0:2, function(strikes) {
    index <- seq_len(pitches_per_strike)
    x_inches <- seq(-15, 15, length.out = pitches_per_strike)
    z_inches <- 8 * sin(index / 9)
    data.table::data.table(
      game_pk = 1L + index %% 20L,
      at_bat_number = index,
      pitch_number = 1L,
      batter_id = as.character(1L + index %% 12L),
      swing = as.integer(x_inches^2 + z_inches^2 < 120),
      edge_distance_inches = (abs(x_inches) - 9.95),
      plate_x = x_inches / 12,
      plate_z = 2.5 + z_inches / 12,
      sz_bot = 1.5,
      sz_top = 3.5,
      balls = index %% 4L,
      strikes = strikes,
      pitch_family = c("fastball", "breaking", "offspeed")[1L + index %% 3L],
      matchup = c("L-L", "L-R", "R-L", "R-R")[1L + index %% 4L],
      count_state = paste0(index %% 4L, "-", strikes)
    )
  }))
}

test_that("2D coordinates are measured in physical inches", {
  choices <- synthetic_swing_take_2d(20L)
  result <- add_swing_take_2d_coordinates(choices)

  expect_equal(result$x_location_inches, 12 * result$plate_x)
  expect_equal(
    result$z_location_inches,
    12 * (result$plate_z - (result$sz_bot + result$sz_top) / 2)
  )
  expect_true(all(result$zone_height_inches == 24))
})

test_that("2D strategy basis cannot reproduce the global distance slope", {
  choices <- synthetic_swing_take_2d(160L)
  fitted <- fit_swing_take_2d_basis(choices)

  for (strike in 0:2) {
    rows <- which(fitted$coordinates$strikes == strike)
    expect_lt(max(abs(colMeans(fitted$basis[rows, , drop = FALSE]))), 1e-8)
    correlations <- apply(
      fitted$basis[rows, , drop = FALSE], 2L,
      stats::cor, y = fitted$coordinates$edge_distance_inches[rows]
    )
    expect_lt(max(abs(correlations)), 1e-8)
  }
})

test_that("training and scoring reproduce the same 2D basis", {
  choices <- synthetic_swing_take_2d(80L)
  fitted <- fit_swing_take_2d_basis(choices)
  scored <- score_swing_take_2d_basis(choices, fitted$specification)

  expect_equal(scored$basis, fitted$basis, tolerance = 1e-10)
  expect_equal(
    colnames(scored$basis), fitted$specification$column_names
  )
})

test_that("2D Stan data never use challenge results", {
  choices <- synthetic_swing_take_2d(80L)
  choices[, `:=`(
    challenged = rep(c(0L, 1L), length.out = .N),
    actual_wrong = rep(c(1L, 0L), length.out = .N)
  )]
  bundle <- prepare_swing_take_2d_stan_data(choices)

  expect_equal(bundle$data$N, nrow(choices))
  expect_equal(bundle$data$S, 16L)
  expect_false(any(c("challenged", "actual_wrong") %in% names(bundle$data)))
  expect_true(all(bundle$data$strike_group %in% 1:3))
})

test_that("2D Gaussian perception Stan program parses", {
  skip_if_not_installed("rstan")
  parsed <- rstan::stanc(
    file = default_swing_take_perception_2d_stan_file()
  )
  expect_true(parsed$status)
})
