revealed_test_prior_1d <- function() {
  structure(
    list(
      components = 2L,
      weight = c(0.7, 0.3),
      mean = c(-1.5, 1.2),
      sd = c(1.1, 0.8),
      context_column = "count_state",
      context_weights = rbind(
        "0-0" = c(0.8, 0.2),
        "3-2" = c(0.4, 0.6)
      ),
      fold_id = "revealed_test_fold",
      global_draw_id = 9L
    ),
    class = "challenge_margin_prior_1d_fit"
  )
}

test_that("selected density normalizes and analytic probit mass matches numerics", {
  skip_if_not_installed("mvtnorm")
  prior <- revealed_test_prior_1d()
  margin <- seq(-10, 10, by = 0.0025)
  intercept <- c(-0.8, -0.2)
  slope <- c(0.75, 1.15)
  policy_weight <- c(0.35, 0.65)
  propensity <- rowSums(vapply(seq_along(intercept), function(index) {
    policy_weight[[index]] * stats::pnorm(
      intercept[[index]] + slope[[index]] * margin
    )
  }, numeric(length(margin))))
  grid <- data.table::data.table(
    fold = 1L,
    role = "offense",
    count_state = "0-0",
    margin_inches = margin,
    opportunity_density = challenge_margin_prior_density_1d(
      prior, margin, context = "0-0"
    ),
    challenge_probability = propensity
  )

  selected <- derive_revealed_challenge_margin_density_1d(grid)
  analytic <- analytic_revealed_challenge_probit_mass_1d(
    prior,
    intercept = intercept,
    slope = slope,
    policy_weight = policy_weight,
    context = "0-0",
    role = "offense",
    fold = 1L
  )
  comparison <- validate_revealed_challenge_analytic_numeric_mass_1d(
    selected$summary, analytic,
    action_tolerance = 2e-5,
    success_tolerance = 2e-5
  )

  expect_s3_class(selected, "revealed_challenge_margin_density_1d")
  expect_equal(selected$summary$selected_density_mass_numeric, 1,
    tolerance = 1e-12
  )
  expect_true(comparison$action_mass_within_tolerance)
  expect_true(comparison$success_mass_within_tolerance)
  expect_true(
    selected$summary$selected_success_rate_numeric > 0 &&
      selected$summary$selected_success_rate_numeric < 1
  )
  expect_true(all(selected$density$selected_density >= 0))
  expect_identical(selected$excluded_information,
    revealed_challenge_margin_outcome_columns_1d()
  )
})

test_that("density construction rejects leakage, missing mass, and empty selection", {
  margin <- seq(-1, 1, length.out = 101L)
  base <- data.table::data.table(
    fold = 1L,
    role = "offense",
    count_state = "0-0",
    margin_inches = margin,
    opportunity_density = stats::dnorm(margin),
    challenge_probability = 0.2
  )
  leaked <- data.table::copy(base)
  leaked[, official_success := TRUE]
  expect_error(
    derive_revealed_challenge_margin_density_1d(leaked),
    "evaluation-only"
  )
  expect_error(
    derive_revealed_challenge_margin_density_1d(base),
    "misses more than"
  )
  empty <- data.table::copy(base)
  empty[, `:=`(
    opportunity_density = stats::dnorm(margin) /
      .revealed_challenge_trapezoid_1d(margin, stats::dnorm(margin)),
    challenge_probability = 0
  )]
  expect_error(
    derive_revealed_challenge_margin_density_1d(empty),
    "no selected mass"
  )
})

revealed_test_oof_rows_1d <- function() {
  data.table::data.table(
    game_pk = rep(c("g1", "g2"), each = 6L),
    pitch_order = rep(1:6, 2L),
    fold = rep(c(1L, 2L), each = 6L),
    role = rep(c("offense", "defense"), each = 3L, times = 2L),
    count_state = rep(c("0-0", "0-0", "3-2"), 4L),
    role_margin_inches = c(
      -4, -0.5, 0, -3.5, 0, 2,
      -2, 0.3, 4, -0.2, 0, 3.5
    ),
    challenged = c(0L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L),
    challenge_probability = c(
      0.01, 0.18, 0.25, 0.02, 0.30, 0.75,
      0.08, 0.40, 0.90, 0.20, 0.35, 0.88
    ),
    inventory_before = 1L
  )
}

test_that("OOF evaluation obeys the propensity-weighted truth identity", {
  rows <- revealed_test_oof_rows_1d()
  result <- evaluate_revealed_challenge_margin_selection_1d(rows)
  overall <- result$diagnostics[aggregation == "overall"]
  truth <- revealed_challenge_geometry_success_1d(
    rows$role, rows$role_margin_inches
  )
  expected <- sum(rows$challenge_probability * truth) /
    sum(rows$challenge_probability)

  expect_s3_class(result, "revealed_challenge_margin_evaluation_1d")
  expect_equal(overall$predicted_selected_success_rate, expected,
    tolerance = 0
  )
  expect_equal(overall$propensity_weighted_truth_identity_error, 0,
    tolerance = 0
  )
  expect_setequal(
    unique(result$diagnostics$aggregation),
    c(
      "overall", "role", "count", "tail", "role_count", "role_tail",
      "role_count_tail"
    )
  )
  expect_true(all(c(
    "selected_margin_cdf_ks", "selected_margin_wasserstein_inches",
    "observed_selected_success_rate", "predicted_selected_success_rate"
  ) %in% names(result$diagnostics)))
  expect_false(any(grepl("official", names(result$diagnostics))))
  expect_equal(
    revealed_challenge_geometry_success_1d(
      c("offense", "defense"), c(0, 0)
    ),
    c(FALSE, TRUE)
  )
})

test_that("official results are evaluation-only mismatch diagnostics", {
  rows <- revealed_test_oof_rows_1d()
  labels <- rows[challenged == 1L, .(
    game_pk, pitch_order, role,
    official_success = revealed_challenge_geometry_success_1d(
      role, role_margin_inches
    )
  )]
  labels[1L, official_success := !official_success]
  one <- evaluate_revealed_challenge_margin_selection_1d(
    rows, official_labels = labels
  )
  changed <- data.table::copy(labels)
  changed[, official_success := !official_success]
  two <- evaluate_revealed_challenge_margin_selection_1d(
    rows, official_labels = changed
  )

  expect_equal(one$diagnostics, two$diagnostics, tolerance = 0)
  expect_equal(
    one$official_diagnostics[aggregation == "overall",
      official_geometry_mismatches],
    1L
  )
  expect_false(identical(
    one$official_diagnostics$official_successes,
    two$official_diagnostics$official_successes
  ))
  expect_identical(one$official_columns_used_for_fit, character())

  bad_labels <- data.table::copy(labels)
  bad_labels[, challenge_outcome := "overturned"]
  expect_error(
    evaluate_revealed_challenge_margin_selection_1d(
      rows, official_labels = bad_labels
    ),
    "evaluation-only allowlist"
  )
})

test_that("OOF rows fail closed on outcomes and unavailable inventory", {
  rows <- revealed_test_oof_rows_1d()
  leaked <- data.table::copy(rows)
  leaked[, actual_wrong := role_margin_inches > 0]
  expect_error(
    normalize_revealed_challenge_oof_rows_1d(leaked),
    "evaluation-only"
  )
  unavailable <- data.table::copy(rows)
  unavailable[1L, inventory_before := 0L]
  expect_error(
    normalize_revealed_challenge_oof_rows_1d(unavailable),
    "positive observed inventory"
  )
  bad_fold <- data.table::copy(rows)
  bad_fold[1L, fold := 2L]
  expect_error(
    normalize_revealed_challenge_oof_rows_1d(bad_fold),
    "exactly one OOF fold"
  )
})
