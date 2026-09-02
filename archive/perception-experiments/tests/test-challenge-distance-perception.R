test_that("challenge-distance data use only actual challenges and omit outcomes", {
  choices <- data.table::data.table(
    game_pk = 1:6,
    role = rep(c("batter", "catcher", "pitcher"), 2),
    player_id = c(1, 2, 3, 1, 2, 3),
    challenged = c(1L, 1L, 1L, 0L, 1L, 1L),
    adverse_margin = c(-1, 0, 1, 2, 0.5, -0.5),
    actual_wrong = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE),
    challenge_outcome = "must_not_enter_stan"
  )
  bundle <- prepare_challenge_distance_data(choices)

  expect_equal(bundle$data$N, 5L)
  expect_false(any(c("actual_wrong", "challenge_outcome") %in% names(bundle$data)))
  expect_equal(bundle$data$R, 3L)
  expect_true(all(bundle$data$player %in% seq_len(bundle$data$P)))
})

test_that("challenge-distance validation rejects game leakage", {
  fake_fit <- structure(list(
    training_games = "1", roles = challenge_distance_role_levels()
  ), class = "challenge_distance_perception_fit")
  heldout <- data.table::data.table(
    game_pk = 1L, role = "batter", challenged = 1L, adverse_margin = 0
  )
  expect_error(
    validate_challenge_distance_fit(fake_fit, heldout),
    "overlap"
  )
})

test_that("challenge-distance Stan program parses", {
  skip_if_not_installed("rstan")
  parsed <- rstan::stanc(
    file = default_challenge_distance_stan_file(), allow_undefined = FALSE
  )
  expect_true(parsed$status)
})

test_that("zero perception spread exactly reproduces tracking probabilities", {
  opportunities <- data.table::data.table(
    game_pk = 1L,
    pitch_order = 1:4,
    role = c("offense", "offense", "defense", "defense"),
    initial_call = c("called_strike", "called_strike", "ball", "ball"),
    p_hat = c(0.01, 0.30, 0.70, 0.99)
  )
  sigma <- data.table::data.table(
    observer_role = c("batter", "catcher"), sigma_inches = 0
  )
  scale <- data.table::data.table(
    initial_call = c("ball", "called_strike"), spatial_scale = c(2, 3)
  )
  scored <- score_challenge_distance_perception(
    opportunities, sigma, scale
  )

  expect_equal(scored$p_human, opportunities$p_hat, tolerance = 1e-12)
  expect_equal(
    scored$observer_role,
    c("batter", "batter", "catcher", "catcher")
  )
})

test_that("positive perception spread moves probabilities toward one half", {
  opportunities <- data.table::data.table(
    game_pk = 1L,
    pitch_order = 1:2,
    role = c("offense", "defense"),
    initial_call = c("called_strike", "ball"),
    p_hat = c(0.99, 0.01)
  )
  sigma <- data.table::data.table(
    observer_role = c("batter", "catcher"), sigma_inches = c(1.69, 1.46)
  )
  scale <- data.table::data.table(
    initial_call = c("ball", "called_strike"), spatial_scale = c(2, 3)
  )
  scored <- score_challenge_distance_perception(
    opportunities, sigma, scale
  )

  expect_true(all(abs(scored$p_human - 0.5) < abs(scored$p_tracking - 0.5)))
  expect_true(all(scored$p_human >= 0 & scored$p_human <= 1))
})

test_that("human MDP decomposition uses the two intended paired gaps", {
  replays <- data.table::data.table(
    policy = c("tracking_mdp", "human_eyes_mdp", "observed", "never"),
    game_pk = 1L,
    team_id = 10L,
    role = "offense",
    policy_challenge = c(TRUE, TRUE, TRUE, FALSE),
    policy_success = c(TRUE, TRUE, TRUE, FALSE),
    captured_re = c(3, 2, 1, 0),
    inventory_before_policy = 2L
  )
  result <- summarize_human_mdp_comparison(
    replays, bootstrap_reps = 20L, seed = 42L
  )

  expect_equal(
    result$decomposition[component == "perception_cost", total_re], 1
  )
  expect_equal(
    result$decomposition[component == "strategy_cost", total_re], 1
  )
})

test_that("human MDP fitting rejects overlapping train and test games early", {
  training <- data.table::data.table(game_pk = 1L)
  expect_error(
    fit_and_replay_human_mdp(
      training, training,
      sigma_table = data.table::data.table(),
      spatial_scale = data.table::data.table(),
      require_game_separation = TRUE
    ),
    "overlap"
  )
})
