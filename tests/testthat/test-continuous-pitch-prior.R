synthetic_continuous_pitch_prior <- function(rows = 240L) {
  index <- seq_len(rows)
  component <- 1L + index %% 3L
  x_center <- c(-8, 0, 8)[component]
  z_center <- c(-6, 7, -2)[component]
  data.table::data.table(
    game_pk = 1L + index %% 24L,
    at_bat_number = index,
    pitch_number = 1L,
    pitcher_id = paste0("p", 1L + index %% 16L),
    batter_id = paste0("b", 1L + index %% 20L),
    plate_x = (x_center + 2 * sin(index)) / 12,
    plate_z = 2.5 + (z_center + 2 * cos(index)) / 12,
    sz_top = 3.5,
    sz_bot = 1.5,
    balls = index %% 4L,
    strikes = index %% 3L,
    pitch_family = c("fastball", "breaking", "offspeed")[component],
    matchup = c("L-R", "R-L")[1L + index %% 2L]
  )
}

test_that("continuous pitch prior has no batter or challenge pathway", {
  pitches <- synthetic_continuous_pitch_prior()
  pitches[, `:=`(
    challenged = rep(0:1, length.out = .N),
    challenge_success = rep(1:0, length.out = .N),
    abs_call = rep(c("ball", "called_strike"), length.out = .N)
  )]
  first <- prepare_continuous_pitch_prior(pitches, components = 3L, seed = 9L)
  pitches[, `:=`(
    batter_id = rev(batter_id),
    challenged = 1L - challenged,
    challenge_success = 1L - challenge_success,
    abs_call = rev(abs_call)
  )]
  second <- prepare_continuous_pitch_prior(pitches, components = 3L, seed = 9L)

  expect_equal(first$data, second$data)
  expect_equal(first$data$K, 3L)
  expect_equal(dim(first$data$anchor_mean), c(3L, 2L))
  expect_true(all(first$data$anchor_scale >= 1))
  expect_false(any(c(
    "batter_id", "challenged", "challenge_success", "abs_call"
  ) %in% names(first$rows)))
})

test_that("pitch-family sensitivity removes only that context", {
  pitches <- synthetic_continuous_pitch_prior()
  full <- prepare_continuous_pitch_prior(
    pitches, components = 1L, include_pitch_family = TRUE
  )
  sensitivity <- prepare_continuous_pitch_prior(
    pitches, components = 1L, include_pitch_family = FALSE
  )

  full_columns <- colnames(full$data$X)
  sensitivity_columns <- colnames(sensitivity$data$X)
  expect_true(any(grepl("^pitch_family__", full_columns)))
  expect_false(any(grepl("^pitch_family__", sensitivity_columns)))
  expect_true(all(c("count_state", "matchup") %in%
    names(sensitivity$context_specification$categorical)))
})

test_that("continuous pitch-prior Stan program parses", {
  skip_if_not_installed("rstan")
  parsed <- rstan::stanc(file = default_continuous_pitch_prior_stan_file())
  expect_true(parsed$status)
})

test_that("continuous pitch-prior scoring returns draw-level densities", {
  pitches <- synthetic_continuous_pitch_prior()
  bundle <- prepare_continuous_pitch_prior(pitches, components = 1L)
  draw_count <- 3L
  draws <- cbind(
    `component_mean[1,1]` = rep(0, draw_count),
    `component_mean[1,2]` = rep(0, draw_count),
    `component_scale[1,1]` = rep(8, draw_count),
    `component_scale[1,2]` = rep(10, draw_count),
    `component_rho[1]` = rep(0, draw_count)
  )
  fit <- list(
    stan_fit = draws,
    components = 1L,
    pitcher_table = bundle$pitcher_table,
    context_specification = bundle$context_specification,
    initialization = bundle$initialization,
    include_pitch_family = TRUE
  )
  class(fit) <- "continuous_pitch_prior_fit"
  scored <- score_continuous_pitch_prior(
    fit, pitches[1:7], ndraws = 3L, return_draws = TRUE
  )

  expect_equal(scored$draw_id, 1:3)
  expect_equal(dim(scored$location_density), c(7L, 3L))
  expect_true(all(is.finite(scored$summary$mean_log_density)))
  expect_true(all(scored$location_density > 0))
})
