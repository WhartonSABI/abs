fixed_clock_test_ledger_1d <- function() {
  data.table::data.table(
    game_pk = c("d1", "d1", "d2", "c1", "c1", "c2"),
    game_date = as.Date(c(
      "2026-07-18", "2026-07-18", "2026-07-19",
      "2026-07-20", "2026-07-20", "2026-07-21"
    )),
    pitch_order = c(1L, 2L, 1L, 1L, 2L, 1L)
  )
}

fixed_clock_test_bootstrap_sources_1d <- function() {
  list(
    historical_re = paste0("h", 1:5),
    development = paste0("d", 1:4),
    confirmation = paste0("c", 1:3)
  )
}

test_that("the July 20 fixed clock has deterministic 1488/497 validation", {
  expectations <- fixed_clock_confirmation_snapshot_expectations_1d()
  expect_equal(expectations$games, c(1488L, 497L))
  expect_equal(expectations$tracked_pitches, c(218896L, 72633L))
  expect_equal(expectations$eligible_clock_pitches, c(218236L, 72346L))
  expect_equal(expectations$observed_challenges, c(6263L, 2275L))
  expect_equal(fixed_clock_confirmation_cutoff_1d(), as.Date("2026-07-20"))

  ledger <- data.table::data.table(
    game_pk = sprintf("snapshot_%04d", seq_len(1985L)),
    game_date = as.Date(c(
      rep("2026-07-19", 1488L), rep("2026-07-20", 497L)
    )),
    pitch_order = 1L
  )
  split <- fixed_clock_confirmation_split_1d(ledger)
  validation <- validate_fixed_clock_confirmation_split_1d(split)

  expect_s3_class(split, "fixed_clock_confirmation_split_1d")
  expect_equal(validation$games, c(1488L, 497L))
  expect_equal(split$summary$partition, c("development", "confirmation"))
  expect_equal(split$summary$rows, c(1488L, 497L))
  expect_true(all(split$development$game_date < split$cutoff))
  expect_true(all(split$confirmation$game_date >= split$cutoff))

  shuffled <- fixed_clock_confirmation_split_1d(ledger[sample(.N)])
  expect_identical(shuffled$split_sha256, split$split_sha256)
  expect_error(
    validate_fixed_clock_confirmation_split_1d(
      split, expected_development_games = 1487L
    ),
    "expected 1487 and 497"
  )
})

test_that("unavailable ABS games are absent from the fixed-clock split", {
  ledger <- data.table::data.table(
    game_pk = c("10", "10", "11", "12"),
    game_date = as.Date(c(
      "2026-07-19", "2026-07-19", "2026-07-19", "2026-07-20"
    )),
    abs_eligible = c(FALSE, FALSE, TRUE, TRUE)
  )
  exclusions <- data.table::data.table(
    game_pk = 10L,
    reason = "no_abs_infrastructure"
  )
  filtered <- exclude_fixed_clock_unavailable_games_1d(ledger, exclusions)
  split <- fixed_clock_confirmation_split_1d(filtered)

  expect_false("10" %in% split$game_split$game_pk)
  expect_equal(attr(filtered, "excluded_game_ids"), "10")
  expect_error(
    exclude_fixed_clock_unavailable_games_1d(
      ledger[game_pk != "10"], exclusions
    ),
    "absent from the pitch ledger"
  )
  bad <- data.table::copy(ledger)
  bad[game_pk == "10", abs_eligible := TRUE]
  expect_error(
    exclude_fixed_clock_unavailable_games_1d(bad, exclusions),
    "marked abs_eligible"
  )
})

test_that("fixed-clock split rejects ambiguous dates and detects mutation", {
  ledger <- fixed_clock_test_ledger_1d()
  split <- fixed_clock_confirmation_split_1d(ledger)
  expect_equal(
    validate_fixed_clock_confirmation_split_1d(
      split,
      expected_development_games = 2L,
      expected_confirmation_games = 2L,
      expected_development_rows = 3L,
      expected_confirmation_rows = 3L
    )$rows,
    c(3L, 3L)
  )

  ambiguous <- data.table::copy(ledger)
  ambiguous[game_pk == "d1" & pitch_order == 2L,
    game_date := as.Date("2026-07-19")]
  expect_error(
    fixed_clock_confirmation_split_1d(ambiguous),
    "more than one game date"
  )

  changed <- split
  changed$game_split[game_pk == "c2", game_date := as.Date("2026-07-22")]
  expect_error(
    validate_fixed_clock_confirmation_split_1d(
      changed, 2L, 2L, 3L, 3L
    ),
    "integrity hash"
  )
})

test_that("whole-game bootstrap coordinates IDs and is subset reproducible", {
  sources <- fixed_clock_test_bootstrap_sources_1d()
  set.seed(91)
  random_state <- .Random.seed
  plan <- fixed_clock_bootstrap_plan_1d(
    sources, replicate_ids = c(4L, 1L, 3L, 2L), seed = 1701L
  )
  expect_identical(.Random.seed, random_state)
  expect_equal(unique(plan$replicate), 1:4)
  expect_equal(
    plan[, data.table::uniqueN(replicate_seed), by = replicate]$V1,
    rep(1L, 4L)
  )
  expect_equal(
    plan[, data.table::uniqueN(source_seed), by = replicate]$V1,
    rep(3L, 4L)
  )
  expect_true(all(plan[, .(
    rows = .N,
    total_weight = sum(bootstrap_weight),
    expected = unique(source_games)
  ), by = .(replicate, source)][, rows == expected & total_weight == expected]))

  one <- fixed_clock_bootstrap_plan_1d(
    rev(sources), replicate_ids = 3L, seed = 1701L
  )
  expect_equal(plan[replicate == 3L], one)
  expect_error(
    fixed_clock_bootstrap_plan_1d(sources, replicate_ids = 1.5),
    "positive integers"
  )
})

test_that("bootstrap callbacks are seeded, restartable, and checkpointed", {
  sources <- fixed_clock_test_bootstrap_sources_1d()
  history_callback <- function(weights, replicate_id, seed, context) {
    list(
      total_weight = sum(weights$bootstrap_weight),
      draw = stats::runif(1L),
      multiplier = context$multiplier
    )
  }
  development_callback <- function(
    weights, historical_re_fit, replicate_id, seed, context
  ) {
    list(
      total_weight = sum(weights$bootstrap_weight),
      value = historical_re_fit$draw + stats::runif(1L),
      multiplier = context$multiplier
    )
  }
  score_callback <- function(
    weights, historical_re_fit, development_fit,
    replicate_id, seed, context
  ) {
    data.table::data.table(
      policy = "frozen",
      scenario_id = c("narrow", "wide"),
      score = development_fit$value + stats::runif(1L) + c(0, 1),
      confirmation_weight = sum(weights$bootstrap_weight),
      context_multiplier = context$multiplier
    )
  }
  checkpoint_dir <- file.path(tempdir(), paste0(
    "fixed-clock-checkpoints-", sample.int(100000000L, 1L)
  ))
  on.exit(unlink(checkpoint_dir, recursive = TRUE), add = TRUE)

  combined <- run_fixed_clock_confirmation_bootstrap_1d(
    sources,
    replicate_ids = c(3L, 1L),
    refit_historical_re = history_callback,
    refit_development = development_callback,
    score_confirmation = score_callback,
    seed = 811L,
    context = list(multiplier = 4),
    checkpoint_dir = checkpoint_dir,
    checkpoint_key = "synthetic-v1"
  )
  isolated <- run_fixed_clock_confirmation_bootstrap_1d(
    sources,
    replicate_ids = 3L,
    refit_historical_re = history_callback,
    refit_development = development_callback,
    score_confirmation = score_callback,
    seed = 811L,
    context = list(multiplier = 4),
    checkpoint_dir = NULL,
    checkpoint_key = "synthetic-v1"
  )

  expect_s3_class(combined, "fixed_clock_confirmation_bootstrap_1d")
  expect_equal(combined$replicate_ids, c(1L, 3L))
  expect_equal(combined$results[replicate == 3L], isolated$results)
  expect_equal(combined$results$confirmation_weight, rep(3L, 4L))
  expect_equal(combined$results$context_multiplier, rep(4, 4L))
  expect_equal(nrow(combined$plan), 6L)
  expect_equal(combined$timing$replicate, c(1L, 3L))
  expect_false(any(combined$timing$resumed))

  resumed <- run_fixed_clock_confirmation_bootstrap_1d(
    sources,
    replicate_ids = c(1L, 3L),
    refit_historical_re = history_callback,
    refit_development = development_callback,
    score_confirmation = score_callback,
    seed = 811L,
    context = list(multiplier = 4),
    checkpoint_dir = checkpoint_dir,
    checkpoint_key = "synthetic-v1",
    resume = TRUE
  )
  expect_equal(resumed$results, combined$results)
  expect_true(all(resumed$timing$resumed))
  expect_equal(
    list.files(checkpoint_dir, pattern = "[.]rds$"),
    c(
      "fixed_clock_bootstrap_00000001.rds",
      "fixed_clock_bootstrap_00000003.rds"
    )
  )
  changed_score_callback <- function(...) {
    stop("changed callback must invalidate the checkpoint")
  }
  expect_error(
    run_fixed_clock_confirmation_bootstrap_1d(
      sources,
      replicate_ids = 1L,
      refit_historical_re = history_callback,
      refit_development = development_callback,
      score_confirmation = changed_score_callback,
      seed = 811L,
      context = list(multiplier = 4),
      checkpoint_dir = checkpoint_dir,
      checkpoint_key = "synthetic-v1",
      resume = TRUE
    ),
    "fingerprint mismatch"
  )
})

test_that("summary-only bootstrap scoring weights completed game paths", {
  game_role <- function(policy, captured, family = policy) {
    out <- data.table::CJ(
      game_pk = c("g1", "g2"), role = c("offense", "defense")
    )
    out[, `:=`(
      team_id = paste0("team_", game_pk),
      policy = policy,
      policy_family = family,
      captured_re = rep(captured, length.out = .N),
      attempts = 1,
      successes = 0.6,
      failures = 0.4,
      exhausted_opportunity_mass = 0.2,
      opportunity_exposure = 10
    )]
    out[]
  }
  direct <- data.table::rbindlist(list(
    game_role("fixed_clock__s1", c(0.10, 0.20, 0.30, 0.40)),
    game_role("fixed_clock__s2", c(0.08, 0.16, 0.24, 0.32))
  ))
  public <- game_role("public_information_only", 0.12)
  comparator <- data.table::rbindlist(list(
    game_role("observed", 0.05),
    game_role("fitted_human_selection", 0.07),
    game_role("exact_location_oracle", 0.50)
  ))
  bellman <- data.table::rbindlist(list(
    game_role("bellman__s1", 0.18),
    game_role("bellman__s2", 0.15)
  ))
  procedure <- data.table::rbindlist(list(
    game_role("fixed_clock__s1", 0.14),
    game_role("fixed_clock__s2", 0.11)
  ))
  weights <- data.table::data.table(
    game_pk = c("g1", "g2"), bootstrap_weight = c(2L, 0L)
  )
  summary <- summarize_fixed_clock_bootstrap_evaluations_1d(
    direct, public, comparator, weights, c("s1", "s2"),
    bellman_game_role = bellman,
    procedure_game_role = procedure
  )

  expect_equal(
    summary[
      policy == "robust_signal_assisted" & scenario_id == "s1" &
        role == "combined",
      captured_re
    ],
    0.6,
    tolerance = 1e-12
  )
  expect_equal(
    summary[policy == "robust_signal_assisted" & role == "combined",
      unique(team_games)],
    2
  )
  expect_equal(
    summary[policy == "public_information_only" & role == "combined",
      unique(captured_re)],
    0.48,
    tolerance = 1e-12
  )
  expect_true(all(abs(summary$attempts - summary$successes -
    summary$failures) < 1e-12))
  expect_equal(
    summary[role == "combined", unique(observed_re)], 0.2,
    tolerance = 1e-12
  )

  invalid_weights <- data.table::copy(weights)
  invalid_weights[, bootstrap_weight := as.numeric(bootstrap_weight)]
  invalid_weights[1L, bootstrap_weight := 1.5]
  expect_error(
    summarize_fixed_clock_bootstrap_evaluations_1d(
      direct, public, comparator, invalid_weights, c("s1", "s2")
    ),
    "non-negative integers"
  )
  invalid_direct <- data.table::copy(direct)
  invalid_direct[1L, role := "combined"]
  expect_error(
    summarize_fixed_clock_bootstrap_evaluations_1d(
      invalid_direct, public, comparator, weights, c("s1", "s2")
    ),
    "invalid values"
  )
})

test_that("frozen-policy summaries report gains and scenario envelopes", {
  scores <- data.table::data.table(
    policy = "robust",
    scenario_id = rep(c("narrow", "wide"), each = 4L),
    role = "combined",
    game_pk = rep(c("g1", "g1", "g2", "g2"), 2L),
    team_id = "T",
    captured_re = c(0.2, 0.3, 0.1, 0.4, 0.1, 0.2, 0.1, 0.2),
    observed_re = 0.1,
    oracle_re = 0.5,
    attempts = 1,
    successes = c(1, 1, 0, 1, 1, 0, 0, 1)
  )
  summary <- summarize_fixed_clock_frozen_policy_1d(scores)

  expect_equal(summary$team_games, c(2, 2))
  expect_equal(
    summary[scenario_id == "narrow", gain_over_observed_re],
    0.6,
    tolerance = 1e-12
  )
  expect_equal(summary$share_of_oracle, c(0.5, 0.3))

  envelope <- fixed_clock_scenario_envelope_1d(summary)
  expect_equal(envelope$scenario_lower, 0.2, tolerance = 1e-12)
  expect_equal(envelope$scenario_upper, 0.6, tolerance = 1e-12)
  expect_equal(envelope$lower_scenario_id, "wide")
  expect_equal(envelope$upper_scenario_id, "narrow")

  draws <- data.table::data.table(
    replicate = 1:3,
    policy = "robust",
    role = "combined",
    scenario_lower = c(0.1, 0.2, -0.1),
    scenario_upper = c(0.6, 0.7, 0.5)
  )
  intervals <- summarize_fixed_clock_scenario_envelope_intervals_1d(
    draws, probabilities = c(0, 1)
  )
  expect_equal(intervals$scenario_lower_bound, -0.1)
  expect_equal(intervals$scenario_upper_bound, 0.7)
  expect_equal(intervals$probability_worst_case_positive, 2 / 3)
})

test_that("frozen artifacts and the leakage audit fail closed", {
  split <- fixed_clock_confirmation_split_1d(fixed_clock_test_ledger_1d())
  policy <- data.frame(
    count_state = c("0-0", "3-2"),
    signal_threshold = c(0.2, 0.8)
  )
  frozen <- freeze_fixed_clock_policy_1d(
    policy,
    training_game_ids = c("d2", "d1"),
    metadata = list(policy_family = "maximin")
  )
  expect_true(validate_frozen_fixed_clock_policy_1d(frozen))

  manifest <- build_fixed_clock_confirmation_manifest_1d(
    split,
    frozen,
    scenario_ids = c("wide", "narrow"),
    source_hashes = c(ledger = "ledger-sha", code = "code-sha"),
    seed = 71L
  )
  expect_true(validate_fixed_clock_confirmation_manifest_1d(
    manifest, split, frozen
  ))

  actions <- data.table::data.table(
    game_pk = c("c1", "c2"),
    team_id = "T",
    pitch_order = 1L,
    challenge_probability = c(0.2, 0.8)
  )
  truth <- actions[, .(game_pk, team_id, pitch_order)][,
    role_margin_inches := c(-0.4, 0.7)]
  audit <- audit_fixed_clock_confirmation_leakage_1d(
    split,
    frozen,
    manifest = manifest,
    confirmation_actions = actions,
    confirmation_truth = truth,
    fail = TRUE
  )
  expect_true(all(audit$passed))

  tampered_policy <- frozen
  tampered_policy$policy$signal_threshold[[1L]] <- 999
  expect_error(
    validate_frozen_fixed_clock_policy_1d(tampered_policy),
    "integrity hash"
  )
  expect_error(
    freeze_fixed_clock_policy_1d(
      data.frame(role_margin_inches = 1), training_game_ids = "d1"
    ),
    "truth/outcome fields"
  )

  tampered_manifest <- manifest
  tampered_manifest$payload$seed <- 72L
  expect_error(
    validate_fixed_clock_confirmation_manifest_1d(tampered_manifest),
    "integrity hash"
  )

  leaked_training <- freeze_fixed_clock_policy_1d(
    policy, training_game_ids = c("d1", "c1")
  )
  expect_error(
    audit_fixed_clock_confirmation_leakage_1d(
      split, leaked_training, fail = TRUE
    ),
    "training_games_development_only.*training_confirmation_disjoint"
  )

  expect_error(
    freeze_fixed_clock_policy_1d(
      list(
        training_games = "d1",
        prior_fits = list(offense = list(training_games = "d2"))
      ),
      training_game_ids = "d1"
    ),
    "omit embedded"
  )
})
