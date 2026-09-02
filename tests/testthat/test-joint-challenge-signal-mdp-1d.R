joint_test_prior_1d <- function(
  games, mean = 0, sd = 1.5, context = "0-0"
) {
  structure(
    list(
      components = 1L,
      weight = 1,
      mean = mean,
      sd = sd,
      context_column = "count_state",
      context_weights = matrix(
        1, nrow = 1L, dimnames = list(context, "component_1")
      ),
      fold_id = "joint_synthetic",
      global_draw_id = 1L,
      training_games = as.character(games),
      training_fingerprint = "joint-synthetic-prior"
    ),
    class = "challenge_margin_prior_1d_fit"
  )
}

joint_test_rows_1d <- function(games = 16L) {
  data.table::rbindlist(lapply(seq_len(games), function(game) {
    role <- rep(c("offense", "defense"), 3L)
    margin <- c(-0.25, 0.5, 1.25, -0.75, 0.25, 1.5)
    data.table::data.table(
      game_pk = paste0("joint_train_", game),
      team_id = "TEAM",
      pitch_order = seq_along(role),
      inning = 1L,
      stage = 0:5,
      role = role,
      state_key = mdp_state_key(0:5, role),
      raw_out_index = 0:5,
      count_state = "0-0",
      stake_G = c(0.2, 0.25, 0.3, 0.35, 0.4, 0.45),
      role_margin_inches = margin,
      actual_wrong = margin > 0,
      decision_mode = "structural",
      observed_team_challenge = FALSE,
      observed_success = FALSE,
      exogenous_action_probability = 0,
      exogenous_success_and_action_probability = 0,
      exogenous_probability_source = "none"
    )
  }))
}

joint_test_priors_1d <- function(rows) {
  games <- unique(rows$game_pk)
  list(
    offense = joint_test_prior_1d(games, mean = -0.6, sd = 1.1),
    defense = joint_test_prior_1d(games, mean = 0.7, sd = 1.8)
  )
}

joint_test_re_table <- function() {
  bases <- c("000", "100", "010", "001", "110", "101", "011", "111")
  x <- data.table::CJ(
    outs = 0:2, base_state = bases, balls = 0:3, strikes = 0:2
  )
  x[, re := 0.2 * balls - 0.1 * strikes - 0.3 * outs]
  x[]
}

test_that("joint preparation retains the full adverse-call clock and orients roles", {
  ledger <- data.table::data.table(
    game_pk = "g1",
    pitch_order = 1:5,
    inning = 1L,
    half = "top",
    outs_before = 0L,
    balls_before = 0L,
    strikes_before = 0L,
    initial_call = c("called_strike", "ball", "ball", "ball", "called_strike"),
    adverse_team_id = c(10L, 20L, 20L, 20L, 10L),
    adverse_role = c("offense", "defense", "defense", "defense", "offense"),
    bat_team_id = 10L,
    tracking_available = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    edge_distance_inches = c(1, -1, 1, 0, NA_real_),
    challenge_occurred = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    challenger_role = c("batter", "catcher", "pitcher", NA, NA),
    challenger_team_id = c(10L, 20L, 20L, NA_integer_, NA_integer_),
    challenge_outcome = c("overturned", "overturned", "overturned", NA, NA),
    abs_eligible = c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  prepared <- prepare_joint_challenge_signal_mdp_opportunities_1d(
    ledger, joint_test_re_table()
  )
  prepared_by_pitch <- prepared[order(pitch_order)]

  expect_equal(nrow(prepared), nrow(ledger))
  expect_equal(prepared_by_pitch$role_margin_inches[1:4], c(1, 1, -1, 0))
  expect_equal(prepared_by_pitch$actual_wrong[1:4], c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(
    unique(prepared[role == "defense", decision_agent]),
    "catcher_or_pitcher"
  )
  expect_equal(prepared_by_pitch$decision_mode, c(
    "structural", "structural", "structural", "structural", "passive"
  ))
  expect_true(all(prepared$stake_G[1:4] > 0))
  expect_equal(sum(prepared$observed_team_challenge), 3L)
  expect_equal(sum(prepared$official_geometry_mismatch), 1L)
})

test_that("joint Bellman dispatches role-specific priors and signal widths", {
  rows <- joint_test_rows_1d()
  fit <- fit_joint_challenge_signal_mdp_1d(
    rows,
    prior_fits = joint_test_priors_1d(rows),
    perception_sigma = c(offense = 0.7, defense = 2.2),
    prior_n = 0,
    lookup_grid_step = 0.01
  )
  expect_s3_class(fit, "joint_challenge_signal_mdp_1d")
  expect_setequal(names(fit$lookups), c("offense", "defense"))
  expect_equal(fit$perception_sigma, c(defense = 2.2, offense = 0.7))
  expect_true(all(c("offense", "defense") %in% fit$states$role))
  expect_lt(fit$residual, 1e-8)

  action <- joint_challenge_signal_mdp_action_1d(
    fit,
    stage = c(0L, 1L),
    role = c("offense", "defense"),
    inventory = 1L,
    gain = 0.3,
    context = "0-0",
    true_margin = c(0.4, 0.4)
  )
  expected <- stats::pnorm(
    (c(0.4, 0.4) - action$signal_threshold_inches) /
      c(0.7, 2.2)
  )
  expect_equal(
    action$challenge_probability_given_true_margin,
    expected,
    tolerance = 1e-14
  )
  expect_false(isTRUE(all.equal(
    action$prior_challenge_probability[[1L]],
    action$prior_challenge_probability[[2L]]
  )))
})

test_that("joint Bellman accepts a zero-width perception profile endpoint", {
  rows <- joint_test_rows_1d()
  fit <- fit_joint_challenge_signal_mdp_1d(
    rows,
    prior_fits = joint_test_priors_1d(rows),
    perception_sigma = c(offense = 0, defense = 0),
    prior_n = 0,
    lookup_grid_step = 0.01
  )
  action <- joint_challenge_signal_mdp_action_1d(
    fit,
    stage = c(0L, 0L),
    role = c("offense", "offense"),
    inventory = 1L,
    gain = 0.2,
    context = "0-0",
    true_margin = c(-0.01, 0.01)
  )

  expect_equal(action$signal_threshold_inches, c(0, 0))
  expect_equal(action$challenge_probability_given_true_margin, c(0, 1))
  expect_equal(action$prior_failure_and_challenge_probability, c(0, 0))
})

test_that("one forward inventory mass is shared by offense and defense", {
  training <- joint_test_rows_1d()
  fit <- fit_joint_challenge_signal_mdp_1d(
    training,
    joint_test_priors_1d(training),
    c(offense = 0.8, defense = 1.7),
    prior_n = 0,
    lookup_grid_step = 0.01
  )
  heldout <- data.table::copy(training[game_pk == "joint_train_1"])
  heldout[, game_pk := "joint_heldout"]
  heldout[1L, `:=`(
    role_margin_inches = -0.05,
    actual_wrong = FALSE
  )]
  replay <- replay_joint_challenge_signal_mdp_1d(
    heldout, fit, initial_inventory = 1L
  )

  expect_gt(replay$expected_policy_failure[[1L]], 0)
  expect_gt(replay$inventory_probability_0[[2L]], 0)
  expect_equal(replay$role[[1L]], "offense")
  expect_equal(replay$role[[2L]], "defense")
  expect_equal(
    rowSums(as.matrix(replay[, .(
      inventory_probability_0,
      inventory_probability_1,
      inventory_probability_2
    )])),
    rep(1, nrow(replay)),
    tolerance = 1e-12
  )
  expect_equal(
    replay$expected_policy_success + replay$expected_policy_failure,
    replay$policy_challenge_probability,
    tolerance = 1e-12
  )
  expect_true(all(c(
    "context_fallback_k1", "context_fallback_k2",
    "exact_root_fallback_k1", "exact_root_fallback_k2"
  ) %in% names(replay)))
  expect_false(any(replay$context_fallback_k1))
  expect_false(any(replay$context_fallback_k2))
})

test_that("passive unavailable rows force waiting without breaking the clock", {
  training <- joint_test_rows_1d()
  fit <- fit_joint_challenge_signal_mdp_1d(
    training,
    joint_test_priors_1d(training),
    c(offense = 0.8, defense = 1.7),
    prior_n = 0,
    lookup_grid_step = 0.01
  )
  heldout <- data.table::copy(training[game_pk == "joint_train_1"])
  heldout[, game_pk := "joint_passive"]
  heldout[2L, `:=`(
    decision_mode = "passive",
    role_margin_inches = NA_real_,
    actual_wrong = NA,
    stake_G = 0
  )]
  replay <- replay_joint_challenge_signal_mdp_1d(
    heldout, fit, initial_inventory = 2L
  )
  expect_equal(replay$policy_challenge_probability[[2L]], 0)
  expect_equal(replay$expected_policy_success[[2L]], 0)
  expect_equal(replay$expected_policy_failure[[2L]], 0)
  expect_equal(
    unlist(replay[2L, .(
      inventory_probability_0,
      inventory_probability_1,
      inventory_probability_2
    )], use.names = FALSE),
    c(
      0,
      replay$expected_policy_failure[[1L]],
      1 - replay$expected_policy_failure[[1L]]
    ),
    tolerance = 1e-12
  )
})

test_that("joint fitting quarantines heldout truth and observed choices", {
  rows <- joint_test_rows_1d()
  priors <- joint_test_priors_1d(rows)
  altered <- data.table::copy(rows)
  altered[, `:=`(
    role_margin_inches = role_margin_inches + 100,
    actual_wrong = !actual_wrong,
    observed_team_challenge = !observed_team_challenge,
    observed_success = !observed_success
  )]
  one <- fit_joint_challenge_signal_mdp_1d(
    rows, priors, c(offense = 0.8, defense = 1.7),
    prior_n = 0, lookup_grid_step = 0.01
  )
  two <- fit_joint_challenge_signal_mdp_1d(
    altered, priors, c(offense = 0.8, defense = 1.7),
    prior_n = 0, lookup_grid_step = 0.01
  )
  expect_equal(one$arrival, two$arrival, tolerance = 0)
  expect_equal(one$continuation, two$continuation, tolerance = 0)
})

test_that("joint summaries reconcile offense, defense, and total run value", {
  training <- joint_test_rows_1d()
  fit <- fit_joint_challenge_signal_mdp_1d(
    training,
    joint_test_priors_1d(training),
    c(offense = 0.8, defense = 1.7),
    prior_n = 0,
    lookup_grid_step = 0.01
  )
  heldout <- data.table::copy(training[game_pk == "joint_train_1"])
  heldout[, game_pk := "joint_summary"]
  heldout[c(2L, 3L), `:=`(
    observed_team_challenge = TRUE,
    observed_success = actual_wrong
  )]
  replay <- replay_joint_challenge_signal_mdp_1d(heldout, fit)
  summary <- summarize_joint_challenge_signal_mdp_1d(
    replay, bootstrap_reps = 20L, seed = 7L
  )

  expect_setequal(summary$season$role, c("offense", "defense", "total"))
  expect_equal(
    summary$season[role == "total", model_re],
    summary$season[role != "total", sum(model_re)],
    tolerance = 1e-12
  )
  expect_equal(
    summary$season[role == "total", observed_re],
    summary$season[role != "total", sum(observed_re)],
    tolerance = 1e-12
  )
  expect_setequal(
    unique(summary$comparators$policy),
    c("model", "observed", "no_challenge", "exact_location_oracle")
  )
  expect_equal(
    summary$comparators[policy == "no_challenge", captured_re],
    rep(0, 3L)
  )
  expect_equal(nrow(summary$bootstrap), 20L * 3L)
  expect_setequal(
    summary$bootstrap_interval$role,
    c("offense", "defense", "total")
  )
  expect_true(all(summary$season$zero_inventory_rate >= 0 &
    summary$season$zero_inventory_rate <= 1))
})

test_that("joint cross-fitting freezes both role priors before shared replay", {
  rows <- joint_test_rows_1d(12L)
  result <- crossfit_joint_challenge_signal_mdp_1d(
    rows,
    perception_sigma = c(offense = 2, defense = 2),
    folds = 3L,
    seed = 19L,
    prior_components = 1L,
    prior_n = 0,
    tol = 1e-6,
    lookup_grid_step = 0.02,
    bootstrap_reps = 5L,
    progress = FALSE
  )
  expect_equal(nrow(result$replay), nrow(rows))
  expect_equal(nrow(result$bootstrap), 5L * 3L)
  expect_true(all(result$fold_diagnostics$bellman_residual <= 1e-6))
  for (fold in 1:3) {
    fold_id <- fold
    heldout_games <- result$fold_assignment[fold == fold_id, game_pk]
    expect_length(intersect(
      result$fold_fits[[fold]]$training_games,
      heldout_games
    ), 0L)
    expect_setequal(
      names(result$prior_fits[[fold]]), c("offense", "defense")
    )
  }
})
