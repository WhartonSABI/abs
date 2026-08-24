test_that("standard swing and take descriptions are classified deterministically", {
  descriptions <- c(
    "swinging_strike", "foul", "hit_into_play", "called_strike", "ball",
    "blocked_ball", "foul_bunt", "automatic_strike", "hit_by_pitch"
  )
  result <- classify_swing_take(descriptions)

  expect_equal(result[1:3], rep(1L, 3))
  expect_equal(result[4:6], rep(0L, 3))
  expect_true(all(is.na(result[7:9])))
})

test_that("swing take preparation uses ball-edge geometry and unique pitches", {
  pitches <- data.table::data.table(
    game_pk = 1L,
    at_bat_number = 1L,
    pitch_number = 1:4,
    batter = 10L,
    pitcher = 20L,
    description = c("ball", "called_strike", "foul", "automatic_ball"),
    pitch_type = "FF",
    balls = 0:3,
    strikes = c(0L, 1L, 2L, 0L),
    plate_x = c(0, 0.7, 0.8, 0),
    plate_z = c(2.5, 2.5, 2.5, 2.5),
    sz_top = 3.5,
    sz_bot = 1.5,
    stand = "R",
    p_throws = "R",
    game_type = "R"
  )
  choices <- prepare_swing_take_choices(pitches, boundary_inches = 12)

  expect_equal(nrow(choices), 3L)
  expect_equal(choices$swing, c(0L, 0L, 1L))
  expect_true(all(is.finite(choices$edge_distance_inches)))
  expect_false(anyDuplicated(
    choices[, .(game_pk, at_bat_number, pitch_number)]
  ))
})

test_that("swing take game split is deterministic and separated", {
  choices <- data.table::data.table(game_pk = rep(1:20, each = 5))
  one <- deterministic_swing_take_split(choices, 0.8, 42L)
  two <- deterministic_swing_take_split(choices, 0.8, 42L)

  expect_equal(one, two)
  expect_length(intersect(
    one[split == "train", game_pk],
    one[split == "validation", game_pk]
  ), 0L)
  expect_equal(one[, sum(split == "train")], 16L)
})

test_that("pilot subsampling is reproducible and preserves sparse batters", {
  choices <- data.table::data.table(
    batter_id = c(rep("many", 1000), rep("few", 40)),
    game_pk = 1L,
    at_bat_number = seq_len(1040),
    pitch_number = 1L
  )
  one <- subsample_swing_take_pilot(choices, 0.2, 100L, 42L)
  two <- subsample_swing_take_pilot(choices, 0.2, 100L, 42L)

  expect_equal(one, two)
  expect_equal(one[batter_id == "many", .N], 200L)
  expect_equal(one[batter_id == "few", .N], 40L)
})

test_that("Stan preparation uses only swing choices and distance", {
  choices <- data.table::data.table(
    game_pk = rep(1:2, each = 6),
    batter_id = rep(c("a", "b"), each = 6),
    swing = rep(c(0L, 1L), 6),
    actual_wrong = rep(c(1L, 0L), 6),
    challenged = rep(c(0L, 1L, 0L), 4),
    edge_distance_inches = seq(-2.75, 2.75, by = 0.5),
    balls = rep(0:3, 3),
    strikes = rep(0:2, each = 4),
    pitch_family = rep(c("fastball", "breaking"), 6),
    matchup = "R-R"
  )
  bundle <- prepare_swing_take_stan_data(choices, distance_bin_width = 0)

  expect_equal(bundle$data$N, nrow(choices))
  expect_equal(bundle$data$P, 2L)
  expect_false(any(c("actual_wrong", "challenged") %in% names(bundle$data)))
  expect_true(all(bundle$data$trials == 1L))
})

test_that("hierarchical swing take Stan program parses", {
  skip_if_not_installed("rstan")
  simple <- rstan::stanc(
    file = default_swing_take_perception_stan_file("simple")
  )
  strategy <- rstan::stanc(
    file = default_swing_take_perception_stan_file("strategy_limits")
  )
  expect_true(simple$status)
  expect_true(strategy$status)
})

test_that("strategy variant adds count-specific nuisance limits", {
  choices <- data.table::data.table(
    game_pk = 1L,
    batter_id = rep(c("a", "b"), each = 6),
    swing = rep(c(0L, 1L), 6),
    edge_distance_inches = seq(-2.75, 2.75, by = 0.5),
    balls = rep(0:3, 3),
    strikes = rep(0:2, each = 4),
    pitch_family = rep(c("fastball", "breaking"), 6),
    matchup = "R-R"
  )
  bundle <- prepare_swing_take_stan_data(
    choices, distance_bin_width = 0, model_variant = "strategy_limits"
  )

  expect_true(all(bundle$data$strike_group %in% 1:3))
  expect_length(bundle$data$lower_prior_mean, 3L)
  expect_length(bundle$data$range_prior_mean, 3L)
})
