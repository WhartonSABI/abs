empirical_prior_test_ledger_1d <- function(games = 6L, rows_per_game = 48L) {
  set.seed(2108)
  data.table::rbindlist(lapply(seq_len(games), function(game) {
    n <- rows_per_game
    call <- rep(c("called_strike", "ball"), length.out = n)
    balls <- rep(rep(0:3, each = 2L), length.out = n)
    strikes <- rep(rep(0:2, each = 8L), length.out = n)
    role_shift <- ifelse(call == "called_strike", -0.4, 0.7)
    count_shift <- (balls - 1.5) * 0.35
    data.table::data.table(
      game_pk = paste0("emp_", game),
      pitch_order = seq_len(n),
      initial_call = call,
      tracking_available = TRUE,
      abs_eligible = TRUE,
      edge_distance_inches = round(
        stats::rnorm(n, role_shift + count_shift, 1.1), 3
      ),
      balls_before = balls,
      strikes_before = strikes,
      pitch_type = rep(c("FF", "SL", "CH", "FC"), length.out = n),
      challenge_occurred = seq_len(n) %% 7L == 0L,
      challenge_outcome = rep(c("upheld", "overturned"), length.out = n),
      abs_call = rep(c("called_strike", "ball"), length.out = n)
    )
  }))
}

test_that("empirical bins preserve the wrong-call boundary at zero", {
  pitches <- data.table::data.table(
    game_pk = "g", pitch_order = 1:5,
    initial_call = "called_strike", tracking_available = TRUE,
    edge_distance_inches = c(-0.006, -0.004, 0, 0.004, 0.006),
    balls_before = 0L, strikes_before = 0L
  )
  fit <- fit_empirical_challenge_margin_prior_1d(
    pitches, context_prior_strength = 10, bin_width_inches = 0.01
  )

  expect_equal(
    .empirical_margin_bin_index_1d(
      c(-0.004, 0, 0.004), 0.01
    ),
    c(-1L, -1L, 0L)
  )
  expect_true(all(fit$mean[fit$bin_index < 0] < 0))
  expect_true(all(fit$mean[fit$bin_index >= 0] > 0))
  expect_equal(challenge_margin_prior_ball_rate_1d(fit), 2 / 5)
})

test_that("count histograms use the stated role-wide shrinkage identity", {
  pitches <- data.table::data.table(
    game_pk = rep(c("g1", "g2"), each = 8L),
    pitch_order = rep(1:8, 2L),
    initial_call = "called_strike", tracking_available = TRUE,
    edge_distance_inches = rep(
      c(-1.01, -1.01, -0.51, -0.51, 0.49, 0.49, 1.01, 1.01), 2L
    ),
    balls_before = rep(c(rep(0L, 4L), rep(3L, 4L)), 2L),
    strikes_before = 0L
  )
  alpha <- 25
  fit <- fit_empirical_challenge_margin_prior_1d(
    pitches, context_prior_strength = alpha
  )
  table <- fit$context_weight_table
  expected <- (
    table$context_count + alpha * table$role_weight
  ) / (table$context_exposure + alpha)

  expect_equal(table$context_weight, expected, tolerance = 1e-14)
  expect_equal(
    unname(rowSums(fit$context_weights)), rep(1, 2), tolerance = 1e-14
  )
  expect_equal(
    challenge_margin_prior_ball_rate_1d(fit, "unseen"),
    challenge_margin_prior_ball_rate_1d(fit),
    tolerance = 0
  )
  resolved <- .empirical_margin_resolve_weights_1d(fit, "0-0", 1L)
  expect_false(resolved$context_fallback)
})

test_that("posterior q is the exact discrete Bayes calculation", {
  pitches <- data.table::data.table(
    game_pk = "g", pitch_order = 1:6,
    initial_call = "called_strike", tracking_available = TRUE,
    edge_distance_inches = c(-2.004, -1.004, -1.004, 0.996, 0.996, 2.006),
    balls_before = 0L, strikes_before = 0L
  )
  fit <- fit_empirical_challenge_margin_prior_1d(pitches, context_prior_strength = 5)
  signal <- c(-0.7, 0, 0.9)
  sigma <- 1.2
  weight <- fit$context_weights["0-0", ]
  brute <- vapply(signal, function(r) {
    likelihood <- weight * stats::dnorm(r, fit$mean, sigma)
    sum(likelihood[fit$mean > 0]) / sum(likelihood)
  }, numeric(1L))

  expect_equal(
    challenge_margin_subjective_ball_probability_1d(
      fit, signal, sigma, "0-0"
    ),
    brute,
    tolerance = 1e-13
  )
  expect_equal(
    challenge_margin_subjective_ball_probability_1d(
      fit, c(-1, 0, 1), 0, "0-0"
    ),
    c(0, 0, 1)
  )
})

test_that("FFT signal lookup matches brute-force bin sums and is monotone", {
  pitches <- data.table::data.table(
    game_pk = rep(c("g1", "g2"), each = 10L),
    pitch_order = rep(1:10, 2L),
    initial_call = "called_strike", tracking_available = TRUE,
    edge_distance_inches = rep(seq(-2.245, 2.255, length.out = 10L), 2L),
    balls_before = rep(c(rep(0L, 5L), rep(3L, 5L)), 2L),
    strikes_before = 0L
  )
  fit <- fit_empirical_challenge_margin_prior_1d(
    pitches, context_prior_strength = 10
  )
  sigma <- 0.8
  lookup <- build_challenge_signal_lookup_1d(
    fit, sigma, c("0-0", "3-0"), grid_step = 0.02,
    minimum_half_width = 5
  )
  table <- lookup$tables[["0-0"]]
  selected <- match(c(-1, 0, 1), table$signal)
  expect_false(anyNA(selected))
  brute_q <- vapply(table$signal[selected], function(r) {
    joint <- table$component_weight * stats::dnorm(r, fit$mean, sigma)
    sum(joint[fit$mean > 0]) / sum(joint)
  }, numeric(1L))
  brute_action <- vapply(table$signal[selected], function(r) {
    sum(table$component_weight * stats::pnorm(
      r, fit$mean, sigma, lower.tail = FALSE
    ))
  }, numeric(1L))
  brute_success <- vapply(table$signal[selected], function(r) {
    keep <- fit$mean > 0
    sum(table$component_weight[keep] * stats::pnorm(
      r, fit$mean[keep], sigma, lower.tail = FALSE
    ))
  }, numeric(1L))

  expect_identical(lookup$convolution_method, "regular_grid_zero_padded_fft")
  expect_lte(max(diff(table$q) * -1), 1e-12)
  expect_equal(table$q[selected], brute_q, tolerance = 2e-9)
  expect_equal(table$action_tail[selected], brute_action, tolerance = 2e-5)
  expect_equal(table$success_tail[selected], brute_success, tolerance = 2e-5)
  expect_lt(lookup$maximum_prior_mass_closure_error, 2e-5)
  expect_lt(lookup$maximum_action_mass_closure_error, 2e-5)
})

test_that("empirical payoff lookup supports sigma zero and fixed-clock masses", {
  pitches <- data.table::data.table(
    game_pk = "g", pitch_order = 1:4,
    initial_call = "called_strike", tracking_available = TRUE,
    edge_distance_inches = c(-1.2, -0.4, 0.4, 1.2),
    balls_before = 0L, strikes_before = 0L
  )
  fit <- fit_empirical_challenge_margin_prior_1d(pitches)
  lookup <- build_challenge_signal_lookup_1d(
    fit, 0, "0-0", grid_step = 0.02, minimum_half_width = 5
  )
  terms <- challenge_signal_payoff_terms_1d(
    lookup, gain = 0.2, inventory_loss = 0.1, context = "0-0"
  )
  masses <- .fixed_clock_direct_lookup_threshold_masses_1d(
    lookup, terms$threshold_inches, "0-0"
  )

  expect_equal(terms$threshold_inches, 0)
  expect_equal(terms$q_target, 1 / 3)
  expect_equal(masses$action_probability, 0.5)
  expect_equal(masses$success_and_action_probability, 0.5)
  expect_equal(masses$failure_and_action_probability, 0)
})

test_that("empirical prior input is structural ABS eligible and action free", {
  ledger <- empirical_prior_test_ledger_1d(3L, 24L)
  ledger[game_pk == "emp_1" & pitch_order == 1L, `:=`(
    abs_eligible = FALSE,
    edge_distance_inches = 999
  )]
  one <- build_revealed_empirical_margin_prior_rows_1d(ledger)
  changed <- data.table::copy(ledger)
  changed[, `:=`(
    challenge_occurred = !challenge_occurred,
    challenge_outcome = "changed",
    abs_call = "changed"
  )]
  two <- build_revealed_empirical_margin_prior_rows_1d(changed)

  expect_false(any(one$edge_distance_inches == 999))
  expect_true(all(one$abs_eligible))
  expect_equal(one, two, tolerance = 0)
  expect_length(
    intersect(names(one), revealed_challenge_prior_1d_forbidden_columns()), 0L
  )
  no_flag <- data.table::copy(ledger)[, abs_eligible := NULL]
  expect_error(
    build_revealed_empirical_margin_prior_rows_1d(no_flag),
    "abs_eligible"
  )
})

test_that("game cross-fitting has no overlap and raw counts do not fallback", {
  ledger <- empirical_prior_test_ledger_1d(6L, 48L)
  rows <- build_revealed_empirical_margin_prior_rows_1d(ledger)
  folds <- continuous_game_folds(rows$game_pk, folds = 3L, seed = 44L)
  result <- crossfit_revealed_empirical_margin_prior_1d(
    rows, fold_assignment = folds, folds = 3L, alpha = 50,
    progress = FALSE
  )

  expect_s3_class(result, "revealed_empirical_margin_prior_1d_crossfit")
  expect_true(all(result$diagnostics$overlap_games == 0L))
  expect_equal(nrow(result$diagnostics), 6L)
  expect_equal(nrow(result$oof_scores), nrow(rows))
  expect_true(all(is.finite(result$oof_scores$crps_inches)))
  for (fold in result$primary_fits) for (fit in fold) {
    resolved <- .empirical_margin_resolve_weights_1d(fit, "0-0", 1L)
    expect_false(resolved$context_fallback)
  }

  opportunities <- data.table::data.table(
    count_state = c("0-0__fastball", "3-2__breaking"),
    raw_count_state = c("0-0", "3-2")
  )
  routed <- route_revealed_empirical_count_context_1d(opportunities)
  expect_equal(routed$count_state, c("0-0", "3-2"))
  expect_equal(routed$prior_context, c("0-0", "3-2"))
  expect_equal(
    routed$mixture_context_count_family,
    c("0-0__fastball", "3-2__breaking")
  )
})

test_that("named role-specific alpha values retain their role mapping", {
  ledger <- empirical_prior_test_ledger_1d(4L, 36L)
  rows <- build_revealed_empirical_margin_prior_rows_1d(ledger)
  folds <- continuous_game_folds(rows$game_pk, folds = 2L, seed = 144L)
  alpha <- c(defense = 175, offense = 25)
  result <- crossfit_revealed_empirical_margin_prior_1d(
    rows, fold_assignment = folds, folds = 2L, alpha = alpha,
    progress = FALSE
  )
  expect_equal(result$specification$alpha[["offense"]], 25)
  expect_equal(result$specification$alpha[["defense"]], 175)
  for (fold in result$primary_fits) {
    expect_equal(fold$offense$context_prior_strength, 25)
    expect_equal(fold$defense$context_prior_strength, 175)
  }
})

test_that("frozen empirical direct and public policies restore and evaluate", {
  rows <- build_revealed_empirical_margin_prior_rows_1d(
    empirical_prior_test_ledger_1d(4L, 48L)
  )
  priors <- fit_revealed_empirical_margin_priors_1d(
    rows, alpha = c(offense = 25, defense = 175),
    bin_width_inches = 0.01
  )
  make_clock <- function(prefix) data.table::rbindlist(lapply(1:2, function(i) {
    data.table::data.table(
      game_pk = paste0(prefix, i), team_id = paste0("t", i),
      pitch_order = 1:4, inning = c(1L, 3L, 6L, 9L),
      stage = c(0L, 18L, 36L, 54L),
      role = rep(c("offense", "defense"), 2L),
      count_state = c("0-0", "1-1", "2-2", "3-2"),
      stake_G = c(0.2, 0.3, 0.25, 0.35),
      decision_mode = "structural"
    )
  }))
  training <- make_clock("policy_train_")
  confirmation <- make_clock("policy_confirm_")
  truth <- confirmation[, .(
    game_pk, team_id, pitch_order, role,
    role_margin_inches = rep(c(-0.4, 0.6, 0.8, -0.2), length.out = .N)
  )]
  scenario <- data.table::data.table(
    scenario_id = "middle", offense_kappa = 0.5, defense_kappa = 0.5
  )

  direct <- fit_fixed_clock_direct_policy_1d(
    training, priors, scenarios = scenario,
    effective_width = c(offense = 1, defense = 1),
    stage_df = 2L, lookup_grid_step = 0.1,
    optimizer_control = list(maxit = 1L, reltol = 1e-5),
    progress = FALSE
  )
  direct_frozen <- freeze_fixed_clock_policy_1d(
    direct, union(direct$training_games, unique(rows$game_pk)),
    cutoff = as.Date("2026-07-20")
  )
  restored_direct <- .fixed_clock_direct_restore_policy_1d(direct_frozen)
  expect_s3_class(
    restored_direct$prior_fits$offense,
    "empirical_challenge_margin_prior_1d_fit"
  )
  direct_value <- evaluate_fixed_clock_policy_1d(
    direct_frozen, confirmation, truth, scenario_ids = "middle",
    return_level = "game_role"
  )
  expect_true(all(is.finite(direct_value$game_role$captured_re)))

  public <- fit_fixed_clock_public_policy_1d(
    training, priors, stage_df = 2L,
    optimizer_control = list(maxit = 1L, reltol = 1e-5)
  )
  expect_match(public$information_regime, "raw-count empirical")
  public_frozen <- freeze_fixed_clock_policy_1d(
    public, union(public$training_games, unique(rows$game_pk)),
    cutoff = as.Date("2026-07-20")
  )
  restored_public <- .fixed_clock_public_restore_1d(public_frozen)
  expect_s3_class(
    restored_public$prior_fits$defense,
    "empirical_challenge_margin_prior_1d_fit"
  )
  public_value <- evaluate_fixed_clock_public_policy_1d(
    public_frozen, confirmation, truth
  )
  expect_true(all(is.finite(public_value$game_role$captured_re)))
})

test_that("alpha selection is development-only and uses proper game CRPS", {
  ledger <- empirical_prior_test_ledger_1d(4L, 48L)
  rows <- build_revealed_empirical_margin_prior_rows_1d(ledger)
  folds <- continuous_game_folds(rows$game_pk, folds = 2L, seed = 45L)
  selected <- select_revealed_empirical_margin_alpha_1d(
    rows, fold_assignment = folds, folds = 2L,
    alpha_grid = c(0, 50, 100)
  )

  expect_setequal(names(selected$selected_alpha), c("offense", "defense"))
  expect_true(all(selected$selected_alpha %in% c(0, 50, 100)))
  expect_true(all(is.finite(selected$metrics$mean_score)))
  expect_equal(selected$metrics[, sum(selected), by = role]$V1, c(1L, 1L))
  expect_match(selected$selection_rule, "CRPS")
})

test_that("whole-game bootstrap refits empirical bin masses cheaply", {
  ledger <- empirical_prior_test_ledger_1d(3L, 24L)
  rows <- build_revealed_empirical_margin_prior_rows_1d(ledger)
  weights <- data.table::data.table(
    game_pk = paste0("emp_", 1:3), bootstrap_weight = c(2L, 1L, 0L)
  )
  fits <- refit_fixed_clock_empirical_priors_1d(
    rows, game_weights = weights, alpha = 25,
    bin_width_inches = 0.01, fold_id = "bootstrap_1"
  )
  expect_setequal(names(fits), c("offense", "defense"))
  for (role_value in names(fits)) {
    expect_s3_class(
      fits[[role_value]], "empirical_challenge_margin_prior_1d_fit"
    )
    expect_equal(
      fits[[role_value]]$training_rows,
      rows[role == role_value & game_pk == "emp_1", .N] * 2L +
        rows[role == role_value & game_pk == "emp_2", .N]
    )
    expect_equal(fits[[role_value]]$bin_width_inches, 0.01)
  }
})

test_that("finite-distribution CRPS agrees with a hand calculation", {
  expect_equal(
    .empirical_margin_crps_vector_1d(
      center = c(-1, 1), weight = c(0.5, 0.5), observation = 0
    ),
    0.5,
    tolerance = 1e-14
  )
})
