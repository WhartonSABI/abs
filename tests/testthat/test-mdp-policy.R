synthetic_mdp_opportunities <- function(games = 20L) {
  data.table::rbindlist(lapply(seq_len(games), function(game) {
    data.table::data.table(
      game_pk = game,
      team_id = ifelse(game %% 2L, 10L, 20L),
      pitch_order = 1:3,
      inning = 1L,
      stage = c(0L, 0L, 1L),
      role = c("offense", "offense", "defense"),
      state_key = mdp_state_key(
        c(0L, 0L, 1L), c("offense", "offense", "defense")
      ),
      raw_out_index = c(0L, 0L, 1L),
      p_hat = c(0.2, 0.6, 0.9),
      stake_G = c(0.1, 0.2, 0.5),
      actual_wrong = c(FALSE, game %% 2L == 0L, TRUE),
      challenge_occurred = c(FALSE, FALSE, TRUE),
      challenger_team_id = c(NA_integer_, NA_integer_, ifelse(game %% 2L, 10L, 20L))
    )
  }))
}

test_that("MDP stages preserve regulation and collapse repeating extra innings", {
  raw <- mdp_out_index(
    c(1L, 9L, 10L, 11L),
    c("top", "bottom", "top", "bottom"),
    c(0L, 2L, 0L, 2L)
  )
  expect_equal(raw, c(0L, 53L, 54L, 65L))
  expect_equal(collapse_mdp_stage(raw), c(0L, 53L, 54L, 59L))
})

test_that("terminal opportunity has zero inventory cost", {
  x <- data.table::data.table(
    game_pk = 1:10,
    team_id = 10L,
    pitch_order = 1L,
    inning = 9L,
    stage = 53L,
    role = "offense",
    state_key = mdp_state_key(53L, "offense"),
    raw_out_index = 53L,
    p_hat = 0.5,
    stake_G = 0.2,
    actual_wrong = TRUE,
    challenge_occurred = FALSE,
    challenger_team_id = NA_integer_
  )
  fit <- fit_challenge_mdp(x)
  decision <- mdp_action(fit, 53L, "offense", 1L, 0.5, 0.2)
  expect_equal(decision$marginal_re, 0)
  expect_equal(decision$recommended_action, "challenge")
})

test_that("negative empirical RE stakes are valid and are never challenged", {
  x <- data.table::data.table(
    game_pk = 1:10,
    team_id = 10L,
    pitch_order = 1L,
    inning = 9L,
    stage = 53L,
    role = "offense",
    state_key = mdp_state_key(53L, "offense"),
    raw_out_index = 53L,
    p_hat = 0.9,
    stake_G = -0.1,
    actual_wrong = TRUE,
    challenge_occurred = FALSE,
    challenger_team_id = NA_integer_
  )
  fit <- fit_challenge_mdp(x)
  decision <- mdp_action(fit, 53L, "offense", 1L, 0.9, -0.1)

  expect_identical(decision$recommended_action, "wait")
  expect_lt(decision$challenge_advantage_re, 0)
  expect_true(is.na(decision$p_threshold))
})

test_that("last challenge is saved for a stronger future opportunity", {
  x <- data.table::rbindlist(lapply(1:20, function(game) {
    data.table::data.table(
      game_pk = game, team_id = 10L, pitch_order = 1:2, inning = 1L,
      stage = 0:1, role = "offense",
      state_key = mdp_state_key(0:1, "offense"), raw_out_index = 0:1,
      p_hat = c(0.2, 0.9), stake_G = c(0.1, 0.5),
      actual_wrong = c(FALSE, TRUE), challenge_occurred = FALSE,
      challenger_team_id = NA_integer_
    )
  }))
  fit <- fit_challenge_mdp(x)
  replay <- replay_challenge_policy(x[game_pk == 1L], fit, "mdp", initial_inventory = 1L)
  expect_false(replay$policy_challenge[[1L]])
  expect_true(replay$policy_challenge[[2L]])
  expect_equal(sum(replay$captured_re), 0.5)
  expect_lt(fit$residual, 1e-10)
})

test_that("success retains inventory and failure consumes it", {
  x <- synthetic_mdp_opportunities(1L)[1:2]
  x[, actual_wrong := c(TRUE, FALSE)]
  x[, p_hat := 1]
  x[, stake_G := 1]
  fit <- fit_challenge_mdp(synthetic_mdp_opportunities())
  replay <- replay_challenge_policy(x, fit, "positive_ev")
  expect_equal(replay$inventory_before_policy, c(2L, 2L))
  expect_equal(replay$inventory_after_policy, c(2L, 1L))
})

test_that("extra-inning grant makes zero and one equal across the boundary", {
  x <- data.table::rbindlist(lapply(1:20, function(game) {
    data.table::data.table(
      game_pk = game, team_id = 10L, pitch_order = 1:2,
      inning = c(9L, 10L), stage = c(53L, 54L), role = "offense",
      state_key = mdp_state_key(c(53L, 54L), "offense"),
      raw_out_index = c(53L, 54L), p_hat = c(0, 0.9),
      stake_G = c(0.1, 0.5), actual_wrong = c(FALSE, TRUE),
      challenge_occurred = FALSE, challenger_team_id = NA_integer_
    )
  }))
  fit <- fit_challenge_mdp(x)
  row <- fit$state_values[stage == 53L & role == "offense"]
  expect_equal(row$continuation_re_k0, row$continuation_re_k1, tolerance = 1e-10)
  expect_equal(row$marginal_re_1_to_0, 0, tolerance = 1e-10)
})

test_that("cross-fitting keeps held-out games outside training", {
  x <- synthetic_mdp_opportunities()
  result <- crossfit_challenge_mdp(x, folds = 5L, bootstrap_reps = 10L)
  expect_equal(nrow(result$pitch_decisions), nrow(x))
  for (fold_id in 1:5) {
    held_out <- result$fold_assignment[fold == fold_id, game_pk]
    expect_length(intersect(result$fold_fits[[fold_id]]$training_games, held_out), 0L)
  }
  expect_true(all(result$pitch_decisions$inventory_after_policy >= 0L))
})
