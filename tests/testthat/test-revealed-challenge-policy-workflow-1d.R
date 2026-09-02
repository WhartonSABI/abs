workflow_test_prior_1d <- function(fold, role, training_games) {
  structure(
    list(
      components = 1L,
      weight = 1,
      mean = if (role == "offense") -0.2 else 0.1,
      sd = 1.5,
      context_column = "count_state",
      context_weights = matrix(
        1, nrow = 1L,
        dimnames = list("0-0__fastball", "component_1")
      ),
      fold_id = fold,
      global_draw_id = 1L,
      training_games = training_games,
      training_fingerprint = paste("workflow", fold, role)
    ),
    class = "challenge_margin_prior_1d_fit"
  )
}

workflow_test_profile_inputs_1d <- function() {
  games <- paste0("wf", 1:8)
  folds <- data.table::data.table(
    game_pk = games,
    fold = rep(1:2, each = 4L)
  )
  rows <- data.table::rbindlist(lapply(seq_along(games), function(index) {
    data.table::data.table(
      row_id = paste(c("offense", "defense"), games[[index]], index, sep = "|"),
      game_pk = games[[index]],
      pitch_order = 2L * index + 0:1,
      fold = folds$fold[[index]],
      role = c("offense", "defense"),
      stage = 0L,
      count_state = "0-0__fastball",
      raw_count_state = "0-0",
      role_margin_inches = c(-1, 1) + (index - 4.5) / 4,
      challenged = as.integer(c(-1, 1) + (index - 4.5) / 4 > 0),
      stake_G = c(0.2, 0.25),
      inventory_before = 1L
    )
  }))
  priors <- lapply(1:2, function(fold_id) {
    training_games <- folds[folds$fold != fold_id, game_pk]
    list(
      offense = workflow_test_prior_1d(fold_id, "offense", training_games),
      defense = workflow_test_prior_1d(fold_id, "defense", training_games)
    )
  })
  widths <- data.table::CJ(
    fold = 1:2, role = c("offense", "defense"), sorted = TRUE
  )
  widths[, sigma_inches := ifelse(role == "offense", 2, 1.5)]
  scenarios <- data.table::data.table(
    scenario_id = c("zero", "noisy"),
    offense_kappa = c(0, 1),
    defense_kappa = c(0, 1)
  )
  states <- data.table::CJ(
    scenario_id = scenarios$scenario_id,
    fold = 1:2,
    role = c("offense", "defense"),
    stage = 0L,
    sorted = TRUE
  )
  states[, `:=`(
    marginal_re_1_to_0 = ifelse(scenario_id == "zero", 0.05, 0.08),
    marginal_re_2_to_1 = ifelse(scenario_id == "zero", 0.02, 0.03)
  )]
  direct <- rows[, .(
    game_pk, pitch_order, role, challenged,
    challenge_probability = pmin(
      0.99, pmax(0.01, stats::pnorm(role_margin_inches / 1.8))
    )
  )]
  list(
    rows = rows, priors = priors, widths = widths,
    scenarios = scenarios, states = states, direct = direct
  )
}

workflow_test_density_inputs_1d <- function(margins = NULL) {
  n <- 401L
  if (is.null(margins)) {
    margins <- stats::qnorm((seq_len(n) - 0.5) / n)
  }
  margins <- rep_len(as.numeric(margins), n)
  fits <- lapply(1:2, function(fold_id) {
    training_games <- paste0("density_train_", fold_id, "_", 1:2)
    role_fits <- lapply(c("offense", "defense"), function(role_value) {
      structure(list(
        components = 1L,
        weight = 1,
        mean = 0,
        sd = 1,
        context_column = "count_state",
        context_weights = matrix(
          1, nrow = 1L,
          dimnames = list("0-0__fastball", "component_1")
        ),
        fold_id = fold_id,
        global_draw_id = 1L,
        role = role_value,
        training_games = training_games
      ), class = "challenge_margin_prior_1d_fit")
    })
    names(role_fits) <- c("offense", "defense")
    role_fits
  })
  prior <- structure(
    list(primary_fits = fits),
    class = "revealed_challenge_prior_1d_crossfit"
  )
  rows <- data.table::rbindlist(lapply(1:2, function(fold_id) {
    data.table::rbindlist(lapply(c("offense", "defense"), function(role_value) {
      data.table::data.table(
        game_pk = paste0("density_", fold_id),
        pitch_order = seq_len(n) + if (role_value == "defense") n else 0L,
        fold = fold_id,
        role = role_value,
        count_state = "0-0",
        pitch_family_coarse = "fastball",
        role_margin_inches = margins,
        challenged = as.integer(margins > 0),
        challenge_probability = stats::pnorm(margins),
        selected_model = "synthetic"
      )
    }))
  }))
  list(prior = prior, rows = rows)
}

test_that("the workflow constructs all 25 canonical joint scenarios", {
  scenarios <- revealed_perception_joint_scenarios_1d()
  expect_equal(nrow(scenarios), 25L)
  expect_equal(data.table::uniqueN(scenarios$scenario_id), 25L)
  expect_setequal(scenarios$offense_kappa, c(0, 0.25, 0.5, 0.75, 1))
  expect_setequal(scenarios$defense_kappa, c(0, 0.25, 0.5, 0.75, 1))
})

test_that("joint profiling uses scenario-specific Bellman losses", {
  input <- workflow_test_profile_inputs_1d()
  result <- crossfit_revealed_joint_perception_profile_1d(
    input$rows,
    fold_prior_fits = input$priors,
    width_estimates = input$widths,
    scenario_state_values = input$states,
    scenarios = input$scenarios,
    direct_selection_oof = input$direct,
    lookup_grid_step = 0.02,
    keep_oof_profiles = TRUE,
    progress = FALSE
  )

  expect_s3_class(result, "revealed_joint_perception_profile_1d")
  expect_equal(nrow(result$scenario_metrics), 2L)
  expect_true(any(result$scenario_metrics$one_se_accepted))
  expect_equal(
    data.table::uniqueN(result$oof_profiles[, .(scenario_id, row_id)]),
    nrow(input$rows) * 2L
  )
  expect_true(all(result$oof_profiles[inventory_before == 1L,
    inventory_loss %in% c(0.05, 0.08)
  ]))
  expect_length(
    intersect(
      names(result$oof_profiles),
      revealed_perception_profile_outcome_columns_1d()
    ),
    0L
  )
})

test_that("policy thresholds preserve q-star identities and zero-width action", {
  replay <- data.table::data.table(
    game_pk = "g", team_id = "T", pitch_order = 1:2,
    fold = 1L, role = c("offense", "defense"), stage = 0L,
    scenario_id = "zero", offense_kappa = 0, defense_kappa = 0,
    stake_G = c(0.2, -0.1), count_state = "0-0__fastball",
    decision_mode = "structural",
    signal_threshold_k1_inches = c(0, Inf),
    signal_threshold_k2_inches = c(-Inf, Inf),
    marginal_inventory_re_k1 = c(0.1, 0.1),
    marginal_inventory_re_k2 = c(0, 0),
    context_fallback_k1 = FALSE, context_fallback_k2 = FALSE,
    exact_root_fallback_k1 = FALSE, exact_root_fallback_k2 = FALSE,
    role_margin_inches = c(-0.1, 0.1)
  )
  widths <- data.table::data.table(
    fold = 1L, role = c("offense", "defense"), sigma_inches = c(2, 1.5)
  )
  thresholds <- build_revealed_policy_thresholds_1d(replay, widths)
  expect_equal(
    thresholds[role == "offense" & inventory == 1L, q_star],
    0.1 / 0.3
  )
  expect_equal(
    thresholds[role == "offense" & inventory == 2L, q_star], 0
  )
  expect_equal(thresholds[role == "defense", unique(q_star)], 1)

  probability <- build_revealed_normative_probability_rows_1d(replay, widths)
  expect_equal(probability$probability_k1, c(0, 0))
  expect_equal(probability$probability_k2, c(1, 0))
})

test_that("snapshot IDs change with target hashes and attach to every row", {
  metadata <- data.table::data.table(
    name = c("pitch_ledger", "re_model"), data = c("aaa", "bbb")
  )
  one <- revealed_policy_snapshot_id_1d(metadata, c("g2", "g1"))
  metadata[name == "re_model", data := "changed"]
  two <- revealed_policy_snapshot_id_1d(metadata, c("g1", "g2"))
  expect_false(identical(one, two))
  attached <- attach_revealed_policy_snapshot_1d(
    data.table::data.table(value = 1:2), one, "aaa"
  )
  expect_true(all(attached$snapshot_id == one))
  expect_true(all(attached$pitch_ledger_target_hash == "aaa"))
})

test_that("accepted scenarios produce monotone subjective-belief envelopes", {
  input <- workflow_test_profile_inputs_1d()
  envelope <- build_revealed_subjective_belief_envelope_1d(
    input$priors,
    input$widths,
    input$scenarios,
    signal_grid = seq(-4, 4, by = 0.25)
  )
  expect_setequal(envelope$role, c("offense", "defense"))
  expect_true("all_counts" %in% envelope$count_state)
  expect_true(all(data.table::between(
    envelope$subjective_q_mean, 0, 1
  )))
  expect_true(all(envelope[order(perceived_margin_inches),
    all(diff(subjective_q_mean) >= -1e-10),
    by = .(role, count_state)
  ]$V1))
})

test_that("derived selected success is checked against direct OOF truth", {
  coherent <- workflow_test_density_inputs_1d()
  density <- build_revealed_challenge_margin_distributions_1d(
    coherent$prior,
    coherent$rows,
    grid_step = 0.05,
    direct_truth_tolerance = 0.03
  )
  expect_lt(max(density$direct_derived_success_absolute_error), 0.03)

  incoherent <- workflow_test_density_inputs_1d(seq(1, 3, length.out = 401L))
  expect_error(
    build_revealed_challenge_margin_distributions_1d(
      incoherent$prior,
      incoherent$rows,
      grid_step = 0.05,
      direct_truth_tolerance = 0.01
    ),
    "does not match direct OOF"
  )
})
