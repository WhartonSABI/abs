revealed_policy_test_clock_1d <- function(game = "g1", team = "T1") {
  data.table::data.table(
    game_pk = game,
    team_id = team,
    pitch_order = 1:5,
    inning = c(9L, 9L, 10L, 10L, 11L),
    role = c("offense", "defense", "offense", "defense", "offense"),
    stake_G = c(1, 2, 3, 4, 5),
    challenged = c(1L, 1L, 1L, 0L, 1L),
    challenge_probability = c(0.8, 0.7, 0.6, 0.5, 0.9),
    normative_k1 = c(0.7, 0.8, 0.9, 0.4, 0.95),
    normative_k2 = c(0.6, 0.7, 0.85, 0.3, 0.90)
  )
}

revealed_policy_test_truth_1d <- function(game = "g1", team = "T1") {
  data.table::data.table(
    game_pk = game,
    team_id = team,
    pitch_order = 1:5,
    role = c("offense", "defense", "offense", "defense", "offense"),
    role_margin_inches = c(-1, 1, -0.5, 2, 1.5)
  )
}

test_that("shared replay loses inventory only on failure and replenishes extras", {
  clock <- revealed_policy_test_clock_1d()
  truth <- revealed_policy_test_truth_1d()
  probability <- clock[, .(
    game_pk, team_id, pitch_order,
    policy = "always",
    policy_family = "test",
    probability_k1 = 1,
    probability_k2 = 1,
    truth_used_by_action_rule = FALSE
  )]
  replay <- replay_revealed_policy_value_1d(
    clock, probability, truth, initial_inventory = 2L
  )

  expect_equal(replay$inventory_probability_2, c(1, 0, 0, 0, 0))
  expect_equal(replay$inventory_probability_1, c(0, 1, 1, 0, 1))
  expect_equal(replay$inventory_probability_0, c(0, 0, 0, 1, 0))
  expect_equal(replay$expected_challenges, c(1, 1, 1, 0, 1))
  expect_equal(replay$expected_successes, c(0, 1, 0, 0, 1))
  expect_equal(replay$expected_failures, c(1, 0, 1, 0, 0))
  expect_equal(sum(replay$expected_captured_re), 7)
  expect_true(replay$extra_inning_replenishment[[3L]])
  expect_true(replay$extra_inning_replenishment[[5L]])
  expect_equal(
    replay$expected_challenges,
    replay$expected_successes + replay$expected_failures,
    tolerance = 0
  )
  expect_equal(
    rowSums(as.matrix(replay[, .(
      inventory_probability_0,
      inventory_probability_1,
      inventory_probability_2
    )])),
    rep(1, nrow(replay)),
    tolerance = 1e-12
  )
  expect_true(all(
    replay$truth_used_by_decision_rule ==
      replay$truth_used_by_action_rule
  ))
  expect_false(any(replay$geometry_used_for_signal_integration))
  expect_true(all(
    replay$provenance_schema == "decision_rule_v2_legacy_compatible"
  ))
})

revealed_policy_two_game_inputs_1d <- function() {
  clock_one <- revealed_policy_test_clock_1d("g1", "T1")
  clock_two <- revealed_policy_test_clock_1d("g2", "T2")
  clock_two[, `:=`(
    challenged = c(0L, 1L, 0L, 1L, 0L),
    challenge_probability = rev(challenge_probability),
    normative_k1 = rev(normative_k1),
    normative_k2 = rev(normative_k2)
  )]
  truth_one <- revealed_policy_test_truth_1d("g1", "T1")
  truth_two <- revealed_policy_test_truth_1d("g2", "T2")
  truth_two[, role_margin_inches := -role_margin_inches]
  list(
    clock = data.table::rbindlist(list(clock_one, clock_two)),
    truth = data.table::rbindlist(list(truth_one, truth_two))
  )
}

test_that("comparator builder forms identical clocks for all policy families", {
  input <- revealed_policy_two_game_inputs_1d()
  replay <- build_revealed_policy_comparators_1d(
    input$clock,
    input$truth,
    fitted_policies = list(
      fitted_human_selection = "challenge_probability"
    ),
    normative_policies = list(
      normative_perception_policy = c("normative_k1", "normative_k2")
    )
  )

  expected_policies <- c(
    "observed", "fitted_human_selection", "no_challenges",
    "exact_location_oracle", "normative_perception_policy"
  )
  expect_setequal(unique(replay$policy), expected_policies)
  expect_equal(
    replay[, .N, by = policy]$N,
    rep(nrow(input$clock), length(expected_policies))
  )
  expect_true(all(
    replay[policy == "exact_location_oracle", truth_used_by_decision_rule]
  ))
  expect_false(any(
    replay[policy != "exact_location_oracle", truth_used_by_decision_rule]
  ))
  expect_equal(
    replay$truth_used_by_action_rule,
    replay$truth_used_by_decision_rule
  )
  expect_true(all(
    replay[
      policy_family %in% c("fitted_human_selection", "normative"),
      geometry_used_for_signal_integration
    ]
  ))
  expect_false(any(
    replay[
      !policy_family %in% c("fitted_human_selection", "normative"),
      geometry_used_for_signal_integration
    ]
  ))
  expect_true(all(
    replay$provenance_schema == "decision_rule_v2_explicit"
  ))
  expect_equal(
    replay[policy == "no_challenges", sum(expected_challenges)],
    0
  )
  expect_equal(
    replay[policy == "exact_location_oracle", sum(expected_failures)],
    0
  )
  expect_true(all(
    replay[policy == "exact_location_oracle" & probability_k1 == 1,
      geometry_success]
  ))
  regime <- attr(replay, "information_regime")
  expect_identical(
    regime$frozen_object,
    "OOF decision threshold/rule, not its evaluation probability"
  )
  expect_match(
    regime$pitch_conditional_evaluation_probability,
    "P\\(action \\| M\\)"
  )
})

test_that("policy summaries reconcile roles, observed gains, and bootstrap", {
  input <- revealed_policy_two_game_inputs_1d()
  replay <- build_revealed_policy_comparators_1d(
    input$clock,
    input$truth,
    fitted_policies = list(fitted_human_selection = "challenge_probability"),
    normative_policies = list(
      normative_perception_policy = c("normative_k1", "normative_k2")
    )
  )
  result <- summarize_revealed_policy_value_1d(
    replay, bootstrap_reps = 20L, seed = 17L
  )
  season <- result$season
  policies <- unique(replay$policy)

  expect_equal(nrow(season), length(policies) * 3L)
  expect_setequal(unique(season$role), c("offense", "defense", "combined"))
  for (policy_name in policies) {
    expect_equal(
      season[policy == policy_name & role == "combined", captured_re],
      season[policy == policy_name & role != "combined", sum(captured_re)],
      tolerance = 1e-12
    )
    expect_equal(
      season[policy == policy_name & role == "combined", attempts],
      season[policy == policy_name & role != "combined", sum(attempts)],
      tolerance = 1e-12
    )
  }
  expect_equal(
    season[policy == "observed", gain_over_observed_re],
    rep(0, 3L), tolerance = 0
  )
  expect_equal(
    season[policy == "no_challenges", captured_re],
    rep(0, 3L), tolerance = 0
  )
  expect_true(all(
    season$inventory_exhaustion_rate >= 0 &
      season$inventory_exhaustion_rate <= 1
  ))
  expect_equal(nrow(result$bootstrap), 20L * length(policies) * 3L)
  expect_equal(nrow(result$bootstrap_intervals), length(policies) * 3L)
  expect_true(all(c(
    "gain_over_observed_re_lower_95", "gain_over_observed_re_upper_95",
    "inventory_exhaustion_rate_lower_95"
  ) %in% names(result$season)))
})

test_that("official outcomes remain evaluation-only mismatch diagnostics", {
  input <- revealed_policy_two_game_inputs_1d()
  replay <- build_revealed_policy_comparators_1d(
    input$clock, input$truth,
    fitted_policies = list(fitted_human_selection = "challenge_probability")
  )
  truth <- normalize_revealed_policy_truth_1d(input$truth)
  labels <- merge(
    input$clock[challenged == 1L, .(
      game_pk, team_id, pitch_order, role
    )],
    truth[, .(game_pk, team_id, pitch_order, official_success = geometry_success)],
    by = c("game_pk", "team_id", "pitch_order")
  )
  labels[1L, official_success := !official_success]
  one <- evaluate_revealed_policy_official_outcomes_1d(replay, labels)
  changed <- data.table::copy(labels)
  changed[, official_success := !official_success]
  two <- evaluate_revealed_policy_official_outcomes_1d(replay, changed)

  expect_equal(one[role == "combined", official_geometry_mismatches], 1L)
  expect_false(identical(one$official_successes, two$official_successes))
  expect_true(isTRUE(attr(one, "evaluation_only")))
  expect_false(isTRUE(attr(one, "official_outcomes_used_for_policy")))

  bad <- data.table::copy(labels)
  bad[, challenge_outcome := "overturned"]
  expect_error(
    evaluate_revealed_policy_official_outcomes_1d(replay, bad),
    "evaluation-only allowlist"
  )
})

test_that("policy replay fails closed on truth leakage and incomplete actions", {
  clock <- revealed_policy_test_clock_1d()
  truth <- revealed_policy_test_truth_1d()
  probability <- clock[, .(
    game_pk, team_id, pitch_order,
    policy = "fitted",
    policy_family = "fitted_human_selection",
    probability_k1 = challenge_probability,
    probability_k2 = challenge_probability,
    truth_used_by_action_rule = FALSE
  )]
  leaked_clock <- data.table::copy(clock)
  leaked_clock[, official_success := TRUE]
  expect_error(
    replay_revealed_policy_value_1d(leaked_clock, probability, truth),
    "evaluation-only"
  )
  leaked_probability <- data.table::copy(probability)
  leaked_probability[, actual_wrong := FALSE]
  expect_error(
    replay_revealed_policy_value_1d(clock, leaked_probability, truth),
    "evaluation-only"
  )
  official_truth <- data.table::copy(truth)
  official_truth[, official_success := TRUE]
  expect_error(
    replay_revealed_policy_value_1d(clock, probability, official_truth),
    "evaluation-only official"
  )
  expect_error(
    replay_revealed_policy_value_1d(clock, probability[-1L], truth),
    "complete clock"
  )
  unauthorized <- data.table::copy(probability)
  unauthorized[, truth_used_by_action_rule := TRUE]
  expect_error(
    replay_revealed_policy_value_1d(clock, unauthorized, truth),
    "Only the explicitly labeled"
  )
})

test_that(paste(
  "frozen thresholds exclude truth and geometry enters only signal",
  "integration"
), {
  clock <- revealed_policy_test_clock_1d()
  truth <- revealed_policy_test_truth_1d()
  keys <- revealed_policy_value_key_columns_1d()
  frozen_rule <- clock[, ..keys]
  frozen_rule[, `:=`(
    signal_threshold_k1_inches = c(-0.25, 0.25, 0, 0.5, 0.75),
    signal_threshold_k2_inches = c(-0.5, 0, -0.25, 0.25, 0.5),
    sensory_sigma_inches = 0.75
  )]
  expect_silent(
    assert_revealed_policy_decision_rule_truth_free_1d(frozen_rule)
  )

  leaked_rule <- merge(frozen_rule, truth[, c(keys, "role_margin_inches"),
    with = FALSE
  ], by = keys)
  expect_error(
    assert_revealed_policy_decision_rule_truth_free_1d(leaked_rule),
    "held-out truth/geometry"
  )

  make_probability <- function(truth_rows) {
    truth_key <- do.call(paste, c(truth_rows[, ..keys], sep = "\r"))
    rule_key <- do.call(paste, c(frozen_rule[, ..keys], sep = "\r"))
    aligned_truth <- truth_rows[match(rule_key, truth_key)]
    out <- data.table::copy(frozen_rule)
    out[, `:=`(
      policy = "normative_signal",
      policy_family = "normative",
      probability_k1 = revealed_policy_signal_integration_probability_1d(
        aligned_truth$role_margin_inches,
        signal_threshold_k1_inches,
        sensory_sigma_inches
      ),
      probability_k2 = revealed_policy_signal_integration_probability_1d(
        aligned_truth$role_margin_inches,
        signal_threshold_k2_inches,
        sensory_sigma_inches
      ),
      truth_used_by_decision_rule = FALSE,
      geometry_used_for_signal_integration = TRUE
    )]
    out
  }

  probability <- make_probability(truth)
  permuted_truth <- data.table::copy(truth)
  permuted_truth[, role_margin_inches := rev(role_margin_inches)]
  permuted_probability <- make_probability(permuted_truth)
  expect_equal(
    probability$signal_threshold_k1_inches,
    permuted_probability$signal_threshold_k1_inches,
    tolerance = 0
  )
  expect_equal(
    probability$signal_threshold_k2_inches,
    permuted_probability$signal_threshold_k2_inches,
    tolerance = 0
  )
  expect_false(isTRUE(all.equal(
    probability$probability_k1,
    permuted_probability$probability_k1,
    tolerance = 0
  )))

  replay <- replay_revealed_policy_value_1d(clock, probability, truth)
  replay_permuted <- replay_revealed_policy_value_1d(
    clock, permuted_probability, permuted_truth
  )
  expect_false(any(replay$truth_used_by_decision_rule))
  expect_true(all(replay$geometry_used_for_signal_integration))
  expect_false(any(replay_permuted$truth_used_by_decision_rule))
  expect_true(all(replay_permuted$geometry_used_for_signal_integration))

  raw_geometry_payload <- data.table::copy(probability)
  raw_geometry_payload[, role_margin_inches := truth$role_margin_inches]
  expect_error(
    replay_revealed_policy_value_1d(
      clock, raw_geometry_payload, truth
    ),
    "completed evaluation probabilities, not raw held-out truth/geometry"
  )
})

test_that("decision-rule provenance is explicit and only the oracle uses truth", {
  clock <- revealed_policy_test_clock_1d()
  truth <- revealed_policy_test_truth_1d()
  probability <- clock[, .(
    game_pk, team_id, pitch_order,
    policy = "normative",
    policy_family = "normative",
    probability_k1 = normative_k1,
    probability_k2 = normative_k2,
    truth_used_by_decision_rule = FALSE,
    geometry_used_for_signal_integration = TRUE,
    truth_used_by_action_rule = FALSE
  )]

  conflicting <- data.table::copy(probability)
  conflicting[, truth_used_by_action_rule := TRUE]
  expect_error(
    normalize_revealed_policy_probabilities_1d(conflicting),
    "conflicts with legacy"
  )

  unauthorized <- data.table::copy(probability)
  unauthorized[, truth_used_by_decision_rule := TRUE]
  unauthorized[, truth_used_by_action_rule := TRUE]
  unauthorized[, geometry_used_for_signal_integration := FALSE]
  expect_error(
    normalize_revealed_policy_probabilities_1d(unauthorized),
    "Only the explicitly labeled exact-location oracle"
  )

  mislabeled_oracle <- data.table::copy(probability)
  mislabeled_oracle[, `:=`(
    policy = "exact_location_oracle",
    policy_family = "exact_location_oracle",
    geometry_used_for_signal_integration = FALSE
  )]
  expect_error(
    normalize_revealed_policy_probabilities_1d(mislabeled_oracle),
    "must be labeled consistently and declare"
  )

  oracle <- data.table::copy(mislabeled_oracle)
  oracle_probability <- as.numeric(
    normalize_revealed_policy_truth_1d(truth)$geometry_success &
      clock$stake_G > 0
  )
  oracle[, `:=`(
    probability_k1 = oracle_probability,
    probability_k2 = oracle_probability,
    truth_used_by_decision_rule = TRUE,
    truth_used_by_action_rule = TRUE
  )]
  expect_silent(replay_revealed_policy_value_1d(clock, oracle, truth))

  mixed_provenance <- data.table::copy(oracle)
  mixed_provenance[, geometry_used_for_signal_integration := TRUE]
  expect_error(
    normalize_revealed_policy_probabilities_1d(mixed_provenance),
    "cannot both be TRUE"
  )
})
