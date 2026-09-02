manual_signal_mdp_prior_1d <- function(
  weight = 1, mean = 0, sd = 2, games = character()
) {
  structure(
    list(
      components = length(weight),
      weight = weight,
      mean = mean,
      sd = sd,
      context_column = "count_state",
      context_weights = rbind(`0-0` = weight, `3-2` = rev(weight)),
      fold_id = "synthetic",
      global_draw_id = 1L,
      training_games = as.character(games),
      training_fingerprint = "synthetic-prior"
    ),
    class = "challenge_margin_prior_1d_fit"
  )
}

synthetic_signal_mdp_rows_1d <- function(games = 20L) {
  data.table::rbindlist(lapply(seq_len(games), function(game) {
    data.table::data.table(
      game_pk = paste0("train_", game),
      team_id = "A",
      pitch_order = 1:2,
      inning = 1L,
      stage = 0:1,
      role = "offense",
      state_key = mdp_state_key(0:1, "offense"),
      raw_out_index = 0:1,
      edge_distance_inches = c(-1, 1),
      count_state = c("0-0", "0-0"),
      stake_G = c(0.1, 0.5),
      actual_wrong = c(FALSE, TRUE),
      observed_batter_challenge = FALSE,
      balls_before = 0L,
      strikes_before = 0L,
      initial_call = "called_strike",
      tracking_available = TRUE
    )
  }))
}

test_that("the continuous lookup closes prior mass and inverts payoff odds", {
  fit <- manual_signal_mdp_prior_1d(
    weight = c(0.65, 0.35), mean = c(-1.5, 1), sd = c(1.2, 0.8)
  )
  lookup <- build_challenge_signal_lookup_1d(
    fit, perception_sigma = 2, context = c("0-0", "3-2"),
    grid_step = 0.005
  )
  expect_lt(lookup$maximum_prior_mass_closure_error, 1e-5)

  terms <- challenge_signal_payoff_terms_1d(
    lookup,
    gain = c(0.1, 0.2, 0, 0.2),
    inventory_loss = c(0.2, 0, 0.2, 0.1),
    context = c("0-0", "0-0", "0-0", "unseen")
  )
  finite <- is.finite(terms$threshold_inches)
  q_at_threshold <- challenge_margin_subjective_ball_probability_1d(
    fit,
    private_margin_signal = terms$threshold_inches[finite],
    perception_sigma = 2,
    context = terms$prior_context[finite]
  )
  expect_equal(q_at_threshold, terms$q_target[finite], tolerance = 2e-5)
  expect_identical(terms$threshold_inches[[2L]], -Inf)
  expect_identical(terms$threshold_inches[[3L]], Inf)
  expect_true(terms$context_fallback[[4L]])
  expect_true(all(terms$prior_expected_advantage_re >= 0))

  extreme <- challenge_signal_payoff_terms_1d(
    lookup,
    gain = c(1e-40, 1),
    inventory_loss = c(1, 1e-40),
    context = "0-0"
  )
  expect_true(all(extreme$exact_root_fallback))
  expect_true(all(extreme$prior_challenge_probability >= 0 &
    extreme$prior_challenge_probability <= 1))
  expect_true(all(
    extreme$prior_success_and_challenge_probability <=
      extreme$prior_challenge_probability
  ))
})

test_that("zero sensory width is the exact deterministic-geometry endpoint", {
  fit <- manual_signal_mdp_prior_1d(
    weight = c(0.6, 0.4), mean = c(-1, 1), sd = c(1.1, 0.9)
  )
  lookup <- build_challenge_signal_lookup_1d(
    fit, perception_sigma = 0, context = "0-0"
  )
  prior_success <- challenge_margin_prior_ball_rate_1d(fit, "0-0")
  terms <- challenge_signal_payoff_terms_1d(
    lookup,
    gain = c(0.2, 0.2, 0),
    inventory_loss = c(0.1, 0, 0.1),
    context = "0-0"
  )

  expect_equal(lookup$maximum_prior_mass_closure_error, 0)
  expect_equal(terms$threshold_inches, c(0, -Inf, Inf))
  expect_equal(
    terms$prior_challenge_probability,
    c(prior_success, 1, 0),
    tolerance = 1e-14
  )
  expect_equal(
    terms$prior_success_and_challenge_probability,
    c(prior_success, prior_success, 0),
    tolerance = 1e-14
  )
  expect_equal(terms$prior_failure_and_challenge_probability[[1L]], 0)
})

test_that("lookup success and failure masses match a bivariate-normal reference", {
  fit <- manual_signal_mdp_prior_1d(
    weight = c(0.6, 0.4), mean = c(-1.2, 1.1), sd = c(1.3, 0.9)
  )
  sigma <- 1.7
  lookup <- build_challenge_signal_lookup_1d(
    fit, sigma, context = "0-0", grid_step = 0.0025
  )
  terms <- challenge_signal_payoff_terms_1d(
    lookup, gain = 0.3, inventory_loss = 0.12, context = "0-0"
  )
  threshold <- terms$threshold_inches[[1L]]
  success <- 0
  action <- 0
  for (component in seq_along(fit$weight)) {
    tau <- fit$sd[[component]]
    covariance <- matrix(c(
      tau^2, tau^2,
      tau^2, tau^2 + sigma^2
    ), nrow = 2L)
    success <- success + fit$weight[[component]] * as.numeric(
      mvtnorm::pmvnorm(
        lower = c(0, threshold), upper = c(Inf, Inf),
        mean = rep(fit$mean[[component]], 2L), sigma = covariance,
        algorithm = mvtnorm::TVPACK()
      )
    )
    action <- action + fit$weight[[component]] * stats::pnorm(
      threshold, fit$mean[[component]], sqrt(tau^2 + sigma^2),
      lower.tail = FALSE
    )
  }
  expect_equal(
    terms$prior_challenge_probability[[1L]], action, tolerance = 1e-10
  )
  expect_equal(
    terms$prior_success_and_challenge_probability[[1L]],
    success,
    tolerance = 2e-5
  )
  expected_advantage <- 0.3 * success - 0.12 * (action - success)
  expect_equal(
    terms$prior_expected_advantage_re[[1L]],
    expected_advantage,
    tolerance = 2e-5
  )
})

test_that("Bellman inventory loss saves the last challenge for the future", {
  rows <- synthetic_signal_mdp_rows_1d(games = 13L)
  prior <- manual_signal_mdp_prior_1d(games = unique(rows$game_pk))
  fit <- fit_challenge_signal_mdp_1d(
    rows, prior, perception_sigma = 1.5, prior_n = 0,
    lookup_grid_step = 0.005
  )
  early_one <- challenge_signal_mdp_action_1d(
    fit, stage = 0, inventory = 1, gain = 0.1, context = "0-0",
    true_margin = -1
  )
  early_two <- challenge_signal_mdp_action_1d(
    fit, stage = 0, inventory = 2, gain = 0.1, context = "0-0",
    true_margin = -1
  )
  terminal <- challenge_signal_mdp_action_1d(
    fit, stage = 1, inventory = 1, gain = 0.5, context = "0-0",
    true_margin = 1
  )

  expect_gt(early_one$marginal_inventory_re, 0)
  expect_true(is.finite(early_one$signal_threshold_inches))
  expect_identical(early_two$signal_threshold_inches[[1L]], -Inf)
  expect_identical(terminal$signal_threshold_inches[[1L]], -Inf)
  expect_lt(
    early_one$challenge_probability_given_true_margin,
    early_two$challenge_probability_given_true_margin
  )
  expect_lt(fit$residual, 1e-8)
  expect_equal(fit$states$n, rep(13L, 2L))
  expect_equal(fit$states$parent_n, rep(13L, 2L))
  expect_true(all(fit$state_values$marginal_re_1_to_0 >= 0))
  expect_true(all(fit$state_values$marginal_re_2_to_1 >= 0))
})

test_that("the decision integrates after the private-signal threshold", {
  rows <- synthetic_signal_mdp_rows_1d()
  prior <- manual_signal_mdp_prior_1d(games = unique(rows$game_pk))
  sigma <- 1.5
  fit <- fit_challenge_signal_mdp_1d(
    rows, prior, sigma, prior_n = 0, lookup_grid_step = 0.005
  )
  action <- challenge_signal_mdp_action_1d(
    fit, stage = 0, inventory = 1, gain = 0.1, context = "0-0",
    true_margin = c(-2, 0, 2)
  )
  expected <- stats::pnorm(
    (c(-2, 0, 2) - action$signal_threshold_inches) / sigma
  )
  expect_equal(
    action$challenge_probability_given_true_margin,
    expected,
    tolerance = 1e-14
  )
  expect_true(all(diff(action$challenge_probability_given_true_margin) > 0))
})

test_that("Bellman state shrinkage varies with actual state exposure", {
  rows <- synthetic_signal_mdp_rows_1d(games = 13L)
  rows <- rows[!(stage == 1L & game_pk %in% paste0("train_", 1:5))]
  prior <- manual_signal_mdp_prior_1d(games = unique(rows$game_pk))
  fit <- fit_challenge_signal_mdp_1d(
    rows, prior, 1.5, prior_n = 30, lookup_grid_step = 0.005
  )
  expect_equal(fit$states[stage == 0L, n], 13L)
  expect_equal(fit$states[stage == 1L, n], 8L)
  expect_equal(fit$states[stage == 0L, shrink_weight], 13 / 43)
  expect_equal(fit$states[stage == 1L, shrink_weight], 8 / 38)
})

test_that("Bellman fitting is quarantined from challenge actions and outcomes", {
  rows <- synthetic_signal_mdp_rows_1d()
  prior <- manual_signal_mdp_prior_1d(games = unique(rows$game_pk))
  altered <- data.table::copy(rows)
  altered[, `:=`(
    actual_wrong = !actual_wrong,
    observed_batter_challenge = !observed_batter_challenge,
    edge_distance_inches = edge_distance_inches + 100,
    challenge_outcome = "deliberately altered",
    is_overturned = TRUE
  )]
  one <- fit_challenge_signal_mdp_1d(
    rows, prior, 1.5, prior_n = 0, lookup_grid_step = 0.005
  )
  two <- fit_challenge_signal_mdp_1d(
    altered, prior, 1.5, prior_n = 0, lookup_grid_step = 0.005
  )
  expect_equal(one$arrival, two$arrival, tolerance = 0)
  expect_equal(one$continuation, two$continuation, tolerance = 0)

  leaky_prior <- prior
  leaky_prior$training_games <- c(prior$training_games, "heldout_game")
  expect_error(
    fit_challenge_signal_mdp_1d(
      rows, leaky_prior, 1.5, prior_n = 0, lookup_grid_step = 0.005
    ),
    "exactly the same games"
  )
})

test_that("heldout replay propagates inventory probability without Monte Carlo", {
  training <- synthetic_signal_mdp_rows_1d()
  prior <- manual_signal_mdp_prior_1d(games = unique(training$game_pk))
  fit <- fit_challenge_signal_mdp_1d(
    training, prior, 1.5, prior_n = 0, lookup_grid_step = 0.005
  )
  heldout <- data.table::copy(training[game_pk == "train_1"])
  heldout[, game_pk := "heldout"]
  heldout[, `:=`(
    edge_distance_inches = c(-1, 1),
    actual_wrong = c(FALSE, TRUE)
  )]
  replay <- replay_challenge_signal_mdp_1d(
    heldout, fit, initial_inventory = 1L
  )
  expect_equal(
    rowSums(as.matrix(replay[, .(
      inventory_probability_0,
      inventory_probability_1,
      inventory_probability_2
    )])),
    rep(1, nrow(replay)),
    tolerance = 1e-14
  )
  expect_gt(replay$inventory_probability_0[[2L]], 0)
  expect_true(all(replay$policy_challenge_probability >= 0 &
    replay$policy_challenge_probability <= 1))
  expect_equal(
    replay$expected_captured_re[[2L]],
    replay$expected_policy_success[[2L]] * replay$stake_G[[2L]],
    tolerance = 1e-14
  )
  expect_error(
    replay_challenge_signal_mdp_1d(training, fit),
    "overlaps its training games"
  )
})

test_that("zero inventory and nonpositive gains force waiting", {
  rows <- synthetic_signal_mdp_rows_1d()
  prior <- manual_signal_mdp_prior_1d(games = unique(rows$game_pk))
  fit <- fit_challenge_signal_mdp_1d(
    rows, prior, 1.5, prior_n = 0, lookup_grid_step = 0.005
  )
  unavailable <- challenge_signal_mdp_action_1d(
    fit, stage = 0, inventory = 0, gain = 0.2, context = "0-0",
    true_margin = 2
  )
  negative <- challenge_signal_mdp_action_1d(
    fit, stage = 0, inventory = 1, gain = -0.1, context = "0-0",
    true_margin = 2
  )
  expect_equal(unavailable$challenge_probability_given_true_margin, 0)
  expect_true(is.na(unavailable$signal_threshold_inches))
  expect_equal(negative$challenge_probability_given_true_margin, 0)
  expect_identical(negative$signal_threshold_inches[[1L]], Inf)
})

test_that("a replay beginning in extras applies the zero-to-one grant", {
  training <- synthetic_signal_mdp_rows_1d(games = 12L)
  training[, `:=`(
    inning = 10L,
    stage = 54L + stage,
    raw_out_index = 54L + raw_out_index,
    state_key = mdp_state_key(54L + stage, "offense")
  )]
  prior <- manual_signal_mdp_prior_1d(games = unique(training$game_pk))
  fit <- fit_challenge_signal_mdp_1d(
    training, prior, 1.5, prior_n = 0, lookup_grid_step = 0.005
  )
  heldout <- data.table::copy(training[game_pk == "train_1"][1L])
  heldout[, game_pk := "heldout_extra"]
  replay <- replay_challenge_signal_mdp_1d(
    heldout, fit, initial_inventory = 0L
  )
  expect_equal(replay$inventory_probability_0, 0)
  expect_equal(replay$inventory_probability_1, 1)
  expect_equal(replay$inventory_probability_2, 0)
})

test_that("cross-fitting freezes prior and Bellman state before heldout replay", {
  rows <- synthetic_signal_mdp_rows_1d(games = 40L)
  result <- crossfit_challenge_signal_mdp_1d(
    rows,
    perception_sigma = 1.5,
    folds = 5L,
    seed = 99L,
    prior_components = 1L,
    prior_n = 0,
    tol = 1e-8,
    lookup_grid_step = 0.01,
    bootstrap_reps = 5L,
    progress = FALSE
  )
  expect_equal(nrow(result$replay), nrow(rows))
  expect_equal(sort(unique(result$replay$fold)), 1:5)
  for (fold_id in 1:5) {
    heldout <- result$fold_assignment[fold == fold_id, game_pk]
    expect_length(
      intersect(result$fold_fits[[fold_id]]$training_games, heldout), 0L
    )
    expect_true(all(
      result$fold_fits[[fold_id]]$training_games %in%
        result$prior_fits[[fold_id]]$training_games
    ))
  }
  expect_true(all(result$fold_diagnostics$bellman_residual < 1e-8))
  expect_true(all(result$replay$policy_challenge_probability >= 0 &
    result$replay$policy_challenge_probability <= 1))
})
