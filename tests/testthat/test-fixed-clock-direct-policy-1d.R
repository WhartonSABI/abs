.fixed_clock_direct_test_cache_1d <- new.env(parent = emptyenv())

fixed_clock_direct_test_clock_1d <- function(prefix = "train", games = 3L) {
  data.table::rbindlist(lapply(seq_len(games), function(game_index) {
    data.table::data.table(
      game_pk = paste0(prefix, "_", game_index),
      team_id = paste0("team_", game_index),
      pitch_order = seq_len(7L),
      inning = c(1L, 2L, 4L, 6L, 8L, 9L, 9L),
      stage = c(0L, 8L, 18L, 30L, 42L, 52L, 58L),
      role = c(
        "offense", "defense", "offense", "defense",
        "offense", "defense", "offense"
      ),
      count_state = c("0-0", "1-1", "0-0", "1-1", "0-0", "1-1", "0-0"),
      stake_G = c(0.18, 0.28, 0.42, 0.31, 0.22, 0.14, 0),
      decision_mode = c(rep("structural", 5L), "descriptive_only", "structural")
    )
  }))
}

fixed_clock_direct_test_prior_1d <- function(role, training_games) {
  structure(list(
    components = 1L,
    weight = 1,
    mean = if (identical(role, "offense")) -0.15 else 0.10,
    sd = if (identical(role, "offense")) 1.10 else 1.25,
    context_column = "count_state",
    context_weights = matrix(
      1,
      nrow = 2L,
      dimnames = list(c("0-0", "1-1"), "component_1")
    ),
    fold_id = "fixed_clock_test",
    global_draw_id = 1L,
    training_games = sort(as.character(training_games)),
    training_fingerprint = paste0("fixed_clock_test_", role)
  ), class = "challenge_margin_prior_1d_fit")
}

fixed_clock_direct_test_priors_1d <- function(clock) {
  games <- unique(clock$game_pk)
  list(
    offense = fixed_clock_direct_test_prior_1d("offense", games),
    defense = fixed_clock_direct_test_prior_1d("defense", games)
  )
}

fixed_clock_direct_test_scenarios_1d <- function() {
  data.table::data.table(
    scenario_id = c("low", "high"),
    offense_kappa = c(0.35, 0.75),
    defense_kappa = c(0.35, 0.75)
  )
}

fixed_clock_direct_test_fit_1d <- function(
  compliance = "perfect", full_scenarios = FALSE
) {
  key <- paste(compliance, full_scenarios, sep = "__")
  if (exists(key, envir = .fixed_clock_direct_test_cache_1d, inherits = FALSE)) {
    return(get(key, envir = .fixed_clock_direct_test_cache_1d, inherits = FALSE))
  }
  clock <- fixed_clock_direct_test_clock_1d(
    games = if (isTRUE(full_scenarios)) 2L else 3L
  )
  fit <- fit_fixed_clock_direct_policy_1d(
    clock,
    fixed_clock_direct_test_priors_1d(clock),
    scenarios = if (isTRUE(full_scenarios)) {
      NULL
    } else {
      fixed_clock_direct_test_scenarios_1d()
    },
    effective_width = c(offense = 1.20, defense = 1.35),
    compliance = compliance,
    stage_df = if (isTRUE(full_scenarios)) 1L else 2L,
    ridge = 1e-4,
    initial_inventory = 2L,
    lookup_grid_step = 0.5,
    optimizer_control = list(maxit = 1L, reltol = 1e-6),
    progress = FALSE
  )
  assign(key, fit, envir = .fixed_clock_direct_test_cache_1d)
  fit
}

fixed_clock_direct_test_truth_1d <- function(clock) {
  out <- data.table::copy(clock)[, .(game_pk, team_id, pitch_order, role)]
  out[, role_margin_inches := rep(
    c(-0.90, 0.65, 0.35, -0.45, 1.10, 0.05, -0.20),
    length.out = .N
  )]
  out[]
}

fixed_clock_direct_scalar_trace_reference_1d <- function(
  clock, quantities, initial_inventory = 2L
) {
  n <- nrow(clock)
  inventory <- matrix(0, nrow = n, ncol = 3L)
  action <- success <- failure <- reward <- numeric(n)
  grant <- logical(n)
  groups <- split(
    seq_len(n), interaction(clock$game_pk, clock$team_id, drop = TRUE)
  )
  for (rows in groups) {
    mass <- c(0, 0, 0)
    mass[[initial_inventory + 1L]] <- 1
    previous_inning <- NA_integer_
    for (row in rows) {
      inning <- clock$inning[[row]]
      replenish <- (is.na(previous_inning) && inning > 9L) ||
        (!is.na(previous_inning) && inning > previous_inning && inning > 9L)
      if (replenish) {
        mass[[2L]] <- mass[[2L]] + mass[[1L]]
        mass[[1L]] <- 0
        grant[[row]] <- TRUE
      }
      inventory[row, ] <- mass
      action[[row]] <- mass[[2L]] *
        quantities$prior_action_probability_k1[[row]] + mass[[3L]] *
        quantities$prior_action_probability_k2[[row]]
      success[[row]] <- mass[[2L]] *
        quantities$prior_success_probability_k1[[row]] + mass[[3L]] *
        quantities$prior_success_probability_k2[[row]]
      failure[[row]] <- mass[[2L]] *
        quantities$prior_failure_probability_k1[[row]] + mass[[3L]] *
        quantities$prior_failure_probability_k2[[row]]
      reward[[row]] <- success[[row]] * clock$stake_G[[row]]
      old <- mass
      mass[[1L]] <- old[[1L]] + old[[2L]] *
        quantities$prior_failure_probability_k1[[row]]
      mass[[2L]] <- old[[2L]] * (
        1 - quantities$prior_failure_probability_k1[[row]]
      ) + old[[3L]] * quantities$prior_failure_probability_k2[[row]]
      mass[[3L]] <- old[[3L]] * (
        1 - quantities$prior_failure_probability_k2[[row]]
      )
      previous_inning <- inning
    }
  }
  list(
    total_expected_re = sum(reward),
    expected_re_per_team_game = sum(reward) / length(groups),
    inventory = inventory,
    action = action,
    success = success,
    failure = failure,
    reward = reward,
    grant = grant
  )
}

test_that("batched inventory recursion exactly matches the scalar recurrence", {
  clock <- data.table::rbindlist(list(
    data.table::data.table(
      game_pk = "g1", team_id = "a", pitch_order = 1:5,
      inning = c(8L, 10L, 10L, 11L, 11L), stake_G = seq(0.1, 0.5, 0.1)
    ),
    data.table::data.table(
      game_pk = "g2", team_id = "b", pitch_order = 1:4,
      inning = c(9L, 9L, 10L, 12L), stake_G = seq(0.15, 0.45, 0.1)
    )
  ))
  quantities <- data.table::data.table(
    prior_action_probability_k1 = seq(0.15, 0.55, length.out = nrow(clock)),
    prior_action_probability_k2 = seq(0.25, 0.65, length.out = nrow(clock))
  )
  quantities[, `:=`(
    prior_success_probability_k1 = prior_action_probability_k1 * 0.6,
    prior_success_probability_k2 = prior_action_probability_k2 * 0.7,
    prior_failure_probability_k1 = prior_action_probability_k1 * 0.4,
    prior_failure_probability_k2 = prior_action_probability_k2 * 0.3
  )]
  reference <- fixed_clock_direct_scalar_trace_reference_1d(clock, quantities)
  batched <- .fixed_clock_direct_trace_value_1d(
    clock, quantities, keep_rows = TRUE
  )
  expect_equal(batched$total_expected_re, reference$total_expected_re,
    tolerance = 1e-14
  )
  expect_equal(
    batched$expected_re_per_team_game,
    reference$expected_re_per_team_game,
    tolerance = 1e-14
  )
  expect_equal(
    unname(as.matrix(batched$rows[, .(
      inventory_probability_0,
      inventory_probability_1,
      inventory_probability_2
    )])),
    reference$inventory,
    tolerance = 1e-14
  )
  expect_equal(batched$rows$prior_expected_challenges, reference$action)
  expect_equal(batched$rows$prior_expected_successes, reference$success)
  expect_equal(batched$rows$prior_expected_failures, reference$failure)
  expect_equal(batched$rows$prior_expected_captured_re, reference$reward)
  expect_identical(batched$rows$extra_inning_replenishment, reference$grant)
})

test_that("direct fit is truth-free, maximin, and robustly refined once", {
  fit <- fixed_clock_direct_test_fit_1d()

  expect_s3_class(fit, "fixed_clock_direct_policy_1d")
  expect_identical(fit$schema, "fixed_clock_direct_policy_v1")
  expect_equal(length(fit$candidate_fits), 2L)
  expect_equal(nrow(fit$cross_evaluation), 4L)
  expect_equal(nrow(fit$candidate_summary), 2L)
  expect_equal(sum(fit$candidate_summary$maximin_selected), 1L)
  expect_identical(
    fit$selected_candidate_id,
    fit$candidate_summary[maximin_selected == TRUE, candidate_id]
  )
  expect_true(all(is.finite(
    fit$cross_evaluation$expected_re_per_team_game
  )))

  expect_identical(
    fit$robust_refinement$method,
    "one_step_binding_scenario_exchange"
  )
  expect_true(fit$robust_refinement$binding_scenario_id %in%
    fit$scenarios$scenario_id)
  expect_type(fit$robust_refinement$accepted, "logical")
  expect_length(fit$robust_refinement$accepted, 1L)
  if (isTRUE(fit$robust_refinement$accepted)) {
    expect_gte(
      fit$robust_refinement$proposal_score$
        worst_case_expected_re_per_team_game,
      fit$robust_refinement$baseline_score$
        worst_case_expected_re_per_team_game - 1e-12
    )
  }

  forbidden <- fixed_clock_policy_forbidden_columns_1d()
  expect_length(intersect(
    .fixed_clock_recursive_names_1d(fit), forbidden
  ), 0L)
  frozen <- freeze_fixed_clock_policy_1d(
    policy = fit,
    training_game_ids = fit$training_games,
    cutoff = as.Date("2026-07-20")
  )
  expect_s3_class(frozen, "fixed_clock_frozen_policy_1d")
  expect_true(validate_frozen_fixed_clock_policy_1d(frozen))

  leaked_clock <- fixed_clock_direct_test_clock_1d()
  leaked_clock[, role_margin_inches := 0]
  expect_error(
    fit_fixed_clock_direct_policy_1d(
      leaked_clock,
      fixed_clock_direct_test_priors_1d(leaked_clock),
      scenarios = fixed_clock_direct_test_scenarios_1d(),
      effective_width = c(offense = 1.2, defense = 1.35),
      progress = FALSE
    ),
    "confirmation truth"
  )
})

test_that("frozen evaluation obeys payoff, action, and inventory identities", {
  fit <- fixed_clock_direct_test_fit_1d()
  frozen <- freeze_fixed_clock_policy_1d(
    fit,
    training_game_ids = fit$training_games,
    cutoff = as.Date("2026-07-20")
  )
  clock <- fixed_clock_direct_test_clock_1d("confirm", games = 2L)
  truth <- fixed_clock_direct_test_truth_1d(clock)
  scored <- evaluate_fixed_clock_policy_1d(
    frozen,
    clock,
    truth,
    scenario_ids = fit$scenarios$scenario_id
  )
  summary_only <- evaluate_fixed_clock_policy_1d(
    frozen,
    clock,
    truth,
    scenario_ids = fit$scenarios$scenario_id,
    return_level = "game_role"
  )

  expect_s3_class(scored, "fixed_clock_policy_evaluation_1d")
  summary_game <- data.table::copy(summary_only$game_role)
  full_game <- data.table::copy(scored$game_role)
  data.table::setorder(summary_game, policy, game_pk, team_id, role)
  data.table::setorder(full_game, policy, game_pk, team_id, role)
  expect_equal(summary_game, full_game)
  expect_false(summary_only$detail_retained)
  expect_equal(nrow(summary_only$policy_actions), 0L)
  expect_equal(nrow(summary_only$replay), 0L)
  action <- scored$policy_actions
  expect_length(intersect(
    names(action),
    c(fixed_clock_policy_forbidden_columns_1d(), "role_margin_inches")
  ), 0L)
  expect_true(all(action$truth_used_by_action_rule == FALSE))
  expect_true(all(action$inventory_loss_k1 >= action$inventory_loss_k2))
  expect_true(all(action$inventory_loss_k2 >= 0))

  active <- action[eligible == TRUE & stake_G > 0]
  expect_equal(
    active$q_star_k1,
    active$inventory_loss_k1 / (active$stake_G + active$inventory_loss_k1),
    tolerance = 1e-12
  )
  expect_equal(
    active$q_star_k2,
    active$inventory_loss_k2 / (active$stake_G + active$inventory_loss_k2),
    tolerance = 1e-12
  )
  inactive <- action[eligible == FALSE | stake_G <= 0]
  expect_true(all(is.na(inactive$q_star_k1)))
  expect_true(all(is.na(inactive$q_star_k2)))
  expect_true(all(inactive$signal_threshold_k1_inches == Inf))
  expect_true(all(inactive$signal_threshold_k2_inches == Inf))
  expect_true(all(inactive$prior_action_probability_k0 == 0))
  expect_true(all(inactive$prior_action_probability_k1 == 0))
  expect_true(all(inactive$prior_action_probability_k2 == 0))

  replay <- scored$replay
  expect_true(all(replay$truth_used_by_decision_rule == FALSE))
  expect_true(all(replay$geometry_used_for_signal_integration == TRUE))
  expect_equal(
    replay$inventory_probability_0 +
      replay$inventory_probability_1 +
      replay$inventory_probability_2,
    rep(1, nrow(replay)),
    tolerance = 1e-12
  )
  expect_equal(
    replay$expected_challenges,
    replay$expected_successes + replay$expected_failures,
    tolerance = 1e-12
  )

  empty <- evaluate_fixed_clock_policy_1d(
    frozen,
    clock,
    truth,
    scenario_ids = "low",
    initial_inventory = 0L,
    evaluation_mode = "sensitivity",
    override_provenance = list(label = "zero-inventory acceptance test")
  )
  expect_equal(empty$replay$expected_challenges, rep(0, nrow(clock)))
  expect_identical(empty$evaluation_mode, "sensitivity")
  expect_true(nzchar(empty$override_sha256))
  expect_true(nzchar(empty$decision_spec_sha256))

  expect_error(
    evaluate_fixed_clock_policy_1d(
      frozen, clock, truth,
      scenario_ids = "low", compliance_override = "noisy"
    ),
    "not allowed in frozen mode"
  )
  expect_error(
    evaluate_fixed_clock_policy_1d(
      frozen, clock, truth,
      scenario_ids = "low", compliance_override = "noisy",
      evaluation_mode = "sensitivity"
    ),
    "override_provenance"
  )

  official <- data.table::copy(truth)
  official[, official_success := TRUE]
  expect_error(
    evaluate_fixed_clock_policy_1d(frozen, clock, official),
    "official"
  )
})

test_that("all 25 partial-identification scenarios are cross-evaluated", {
  fit <- fixed_clock_direct_test_fit_1d(full_scenarios = TRUE)

  expect_equal(nrow(fit$scenarios), 25L)
  expect_equal(length(fit$candidate_fits), 25L)
  expect_equal(nrow(fit$cross_evaluation), 25L * 25L)
  expect_equal(
    fit$cross_evaluation[, .N, by = candidate_id]$N,
    rep(25L, 25L)
  )
  expect_equal(
    fit$cross_evaluation[, .N, by = evaluation_scenario_id]$N,
    rep(25L, 25L)
  )
  expect_equal(sum(fit$candidate_summary$maximin_selected), 1L)
  expect_true(all(is.finite(
    fit$candidate_summary$worst_case_expected_re_per_team_game
  )))
})

test_that("perfect and noisy compliance use the intended action widths", {
  perfect <- fixed_clock_direct_test_fit_1d(compliance = "perfect")
  noisy <- fixed_clock_direct_test_fit_1d(compliance = "noisy")
  clock <- fixed_clock_direct_test_clock_1d("compliance", games = 1L)
  truth <- fixed_clock_direct_test_truth_1d(clock)
  freeze <- function(fit) {
    freeze_fixed_clock_policy_1d(
      fit,
      training_game_ids = fit$training_games,
      cutoff = as.Date("2026-07-20")
    )
  }
  perfect_score <- evaluate_fixed_clock_policy_1d(
    freeze(perfect), clock, truth, scenario_ids = "low"
  )
  noisy_score <- evaluate_fixed_clock_policy_1d(
    freeze(noisy), clock, truth, scenario_ids = "low"
  )
  perfect_action <- perfect_score$policy_actions[eligible & stake_G > 0]
  noisy_action <- noisy_score$policy_actions[eligible & stake_G > 0]

  expected_sensory <- ifelse(
    perfect_action$role == "offense", 1.20, 1.35
  ) * 0.35
  expected_effective <- ifelse(
    noisy_action$role == "offense", 1.20, 1.35
  )
  expect_equal(
    perfect_action$action_total_sigma_inches,
    expected_sensory,
    tolerance = 1e-12
  )
  expect_equal(
    noisy_action$action_total_sigma_inches,
    expected_effective,
    tolerance = 1e-12
  )
  expect_true(all(
    noisy_action$action_total_sigma_inches >
      perfect_action$action_total_sigma_inches
  ))
})

test_that("candidate-level fork parallelism is deterministic", {
  skip_on_os("windows")
  clock <- fixed_clock_direct_test_clock_1d("parallel", games = 2L)
  arguments <- list(
    clock_rows = clock,
    prior_fits = fixed_clock_direct_test_priors_1d(clock),
    scenarios = fixed_clock_direct_test_scenarios_1d(),
    effective_width = c(offense = 1.2, defense = 1.35),
    stage_df = 1L,
    lookup_grid_step = 0.5,
    optimizer_control = list(maxit = 1L, reltol = 1e-6),
    progress = FALSE
  )
  serial <- do.call(fit_fixed_clock_direct_policy_1d, c(arguments, workers = 1L))
  parallel <- do.call(
    fit_fixed_clock_direct_policy_1d, c(arguments, workers = 2L)
  )
  expect_identical(parallel$selected_candidate_id, serial$selected_candidate_id)
  expect_equal(parallel$candidate_summary, serial$candidate_summary,
    tolerance = 1e-12
  )
  expect_equal(parallel$cross_evaluation, serial$cross_evaluation,
    tolerance = 1e-12
  )
})
