test_that("human action is integrated after the nonlinear decision link", {
  q <- matrix(c(0.1, 0.9), nrow = 1L)
  w <- matrix(c(0.5, 0.5), nrow = 1L)
  integrated <- integrate_human_action(
    q, w, gain = 1, inventory_loss = 1,
    linear_predictor = -1, decision_slope = 4
  )
  plug_in <- plogis(-1 + 4 * challenge_expected_utility(0.5, 1, 1))
  expect_false(isTRUE(all.equal(integrated$challenge_probability, plug_in)))
  expect_gt(integrated$choice_conditioned_q, integrated$marginal_signal_q)
})

test_that("take conditioning reweights private signals", {
  weights <- matrix(c(0.5, 0.5), nrow = 1L)
  conditioned <- condition_signal_weights_on_take(
    weights, matrix(c(0.9, 0.1), nrow = 1L)
  )
  expect_equal(as.numeric(conditioned), c(0.9, 0.1))
  expect_equal(rowSums(conditioned), 1)
})

test_that("decision preparation quarantines outcome labels", {
  rows <- data.table::data.table(
    game_pk = 1L, pitch_order = 1L, batter_id = "10", bat_team_id = "20",
    initial_call = "called_strike", challenged = 1L, stake_G = 0.2,
    inventory_loss = 0.1, abs_call = "ball", is_overturned = TRUE
  )
  first <- prepare_continuous_decision_rows(rows)
  rows[, `:=`(abs_call = "called_strike", is_overturned = FALSE)]
  second <- prepare_continuous_decision_rows(rows)
  expect_identical(first, second)
  expect_false(any(continuous_decision_outcome_columns() %in% names(first)))
})

test_that("call trust fails closed without predictive improvement", {
  scores <- data.table::data.table(
    omega = c(0, 0.5, 1), log_loss = c(0.20, 0.199, 0.201),
    game_se = rep(0.01, 3)
  )
  gate <- stage_continuous_call_trust(
    scores, correlation = 0.2, interval = c(0.4, 0.8)
  )
  expect_false(gate$promote_estimated_call_update)
  expect_equal(gate$primary_omega, 0)
})

test_that("global draw maps are complete and unique", {
  draw_map <- make_global_draw_map(100, 90, 80, ndraws = 50, seed = 1)
  expect_true(validate_global_draw_map(draw_map))
  expect_equal(data.table::uniqueN(draw_map$global_draw_id), 50L)
})

test_that("held-out decision scoring integrates signals for every posterior draw", {
  rows <- data.table::data.table(
    game_pk = 1:2, pitch_order = 1:2, batter_id = c("b1", "new"),
    bat_team_id = c("t1", "new-team"), initial_call = "called_strike",
    challenged = c(0L, 1L), stake_G = c(0.2, 0.3),
    inventory_loss = c(0.1, 0.1), sampling_offset = 0
  )
  draws <- cbind(
    mu_player = c(-2, -2), tau_player = c(0.2, 0.2),
    `alpha_player[1]` = c(-1.5, -1.5), tau_team = c(0.1, 0.1),
    `alpha_team[1]` = c(0.1, 0.1), decision_slope = c(2, 2)
  )
  fit <- list(
    fit = draws, players = "b1", teams = "t1", data = list(K = 0L),
    context_specification = NULL
  )
  class(fit) <- "continuous_human_decision_fit"
  q <- matrix(c(0.2, 0.8, 0.4, 0.9), nrow = 2, byrow = TRUE)
  w <- matrix(0.5, 2, 2)
  scored <- score_continuous_human_decision(
    fit, rows, q, w, ndraws = 2L, return_draws = TRUE
  )
  expect_equal(dim(scored$p_challenge), c(2L, 2L))
  expect_true(all(scored$p_challenge > 0 & scored$p_challenge < 1))
  expect_gt(scored$summary$q_chosen_mean[[1]], scored$summary$q_signal_mean[[1]])
  expect_false(scored$summary$batter_decision_seen_in_training[[2]])
})
