synthetic_challenge_margin_pitches <- function(
  games = 12L, pitches_per_game = 40L, seed = 20260825L
) {
  set.seed(seed)
  n <- games * pitches_per_game
  game_pk <- rep(sprintf("g%02d", seq_len(games)), each = pitches_per_game)
  mixture <- stats::rbinom(n, 1L, 0.22)
  margin <- stats::rnorm(
    n,
    mean = ifelse(mixture == 1L, 0.8, -2.2),
    sd = ifelse(mixture == 1L, 1.0, 1.8)
  )
  data.table::data.table(
    game_pk = game_pk,
    pitch_order = ave(seq_len(n), game_pk, FUN = seq_along),
    initial_call = "called_strike",
    tracking_available = TRUE,
    edge_distance_inches = margin,
    balls_before = rep(0:3, length.out = n),
    strikes_before = rep(0:2, length.out = n),
    pitch_family = rep(c("fastball", "breaking"), length.out = n),
    batter = rep(sprintf("b%02d", 1:8), length.out = n),
    challenged = stats::rbinom(n, 1L, 0.05),
    official_success = stats::rbinom(n, 1L, 0.5),
    challenge_outcome = rep(c("overturned", "upheld"), length.out = n),
    is_overturned = stats::rbinom(n, 1L, 0.5),
    abs_call = ifelse(margin > 0, "ball", "called_strike")
  )
}

manual_challenge_margin_prior_fit <- function(
  weight = 1, mean = 0, sd = 2, fold_id = "fold_2", global_draw_id = 17L
) {
  structure(
    list(
      components = length(weight),
      weight = weight,
      mean = mean,
      sd = sd,
      fold_id = fold_id,
      global_draw_id = global_draw_id
    ),
    class = "challenge_margin_prior_1d_fit"
  )
}

test_that("the called-strike margin prior uses an outcome/action-free allowlist", {
  pitches <- synthetic_challenge_margin_pitches(games = 3L)
  pitches <- data.table::rbindlist(list(
    pitches,
    data.table::data.table(
      game_pk = c("excluded_ball", "excluded_untracked", "excluded_missing"),
      pitch_order = 1L,
      initial_call = c("ball", "called_strike", "called_strike"),
      tracking_available = c(TRUE, FALSE, TRUE),
      edge_distance_inches = c(1, 1, NA_real_),
      challenged = 1L,
      official_success = TRUE,
      abs_call = "ball"
    )
  ), fill = TRUE)
  clean <- normalize_challenge_margin_prior_1d_input(pitches)

  forbidden <- c(
    "challenged", "official_success", "challenge_outcome", "is_overturned",
    "abs_call"
  )
  expect_length(intersect(names(clean), forbidden), 0L)
  expect_true(all(clean$initial_call == "called_strike"))
  expect_true(all(clean$tracking_available))
  expect_true(all(is.finite(clean$edge_distance_inches)))
  expect_false(any(grepl("^excluded_", clean$game_pk)))

  altered <- data.table::copy(pitches)
  altered[, `:=`(
    challenged = 1L - data.table::fcoalesce(as.integer(challenged), 0L),
    official_success = !data.table::fcoalesce(
      as.logical(official_success), FALSE
    ),
    is_overturned = !data.table::fcoalesce(as.logical(is_overturned), FALSE),
    abs_call = "deliberately_changed"
  )]
  clean_altered <- normalize_challenge_margin_prior_1d_input(altered)
  expect_equal(clean, clean_altered)

  altered[, `:=`(
    pitch_family = "deliberately_changed",
    batter = "deliberately_changed"
  )]
  fit_one <- fit_challenge_margin_prior_1d(pitches, components = 3L)
  fit_two <- fit_challenge_margin_prior_1d(altered, components = 3L)
  expect_equal(fit_one$weight, fit_two$weight)
  expect_equal(fit_one$mean, fit_two$mean)
  expect_equal(fit_one$sd, fit_two$sd)
  expect_equal(fit_one$context_weights, fit_two$context_weights)
  expect_identical(
    fit_one$training_fingerprint, fit_two$training_fingerprint
  )
  expect_true(all(fit_one$context_weight_table$context_column == "count_state"))
  expect_identical(fit_one$outcome_action_columns_used, character())
})

test_that("count context changes only shrunken weights, never shared shapes", {
  pitches <- synthetic_challenge_margin_pitches(games = 8L)
  pitches[balls_before == 0L, edge_distance_inches := edge_distance_inches - 2]
  pitches[balls_before == 3L, edge_distance_inches := edge_distance_inches + 2]

  contextual <- fit_challenge_margin_prior_1d(
    pitches, components = 3L, context_prior_strength = 40
  )
  league <- fit_challenge_margin_prior_1d(
    pitches, components = 3L, context_column = NULL
  )

  expect_equal(contextual$weight, league$weight, tolerance = 0)
  expect_equal(contextual$mean, league$mean, tolerance = 0)
  expect_equal(contextual$sd, league$sd, tolerance = 0)
  expect_equal(unname(rowSums(contextual$context_weights)), rep(1, 12))
  expect_identical(contextual$context_prior_strength, 40)
  expect_identical(
    contextual$context_weight_method,
    "soft_allocation_dirichlet_posterior_mean_toward_league"
  )

  table <- contextual$context_weight_table
  expected <- (
    table$context_exposure * table$unshrunk_weight +
      table$prior_strength * table$league_weight
  ) / (table$context_exposure + table$prior_strength)
  expect_equal(table$context_weight, expected, tolerance = 1e-12)
  distances <- table[, .(
    raw = sum(abs(unshrunk_weight - league_weight)),
    shrunk = sum(abs(context_weight - league_weight))
  ), by = context_value]
  expect_true(all(distances$shrunk <= distances$raw + 1e-12))
  expect_true(any(distances$shrunk < distances$raw - 1e-8))
})

test_that("contextual ball rates and unseen fallback use league-compatible APIs", {
  fit <- manual_challenge_margin_prior_fit(
    weight = c(0.5, 0.5), mean = c(-2, 2), sd = c(1, 1)
  )
  fit$context_column <- "count_state"
  fit$context_weights <- rbind(
    "0-0" = c(0.9, 0.1),
    "3-2" = c(0.1, 0.9)
  )
  component_rate <- stats::pnorm(fit$mean / fit$sd)
  expected <- as.numeric(fit$context_weights %*% component_rate)

  rates <- challenge_margin_prior_ball_rate_1d(
    fit, context = c("0-0", "3-2")
  )
  expect_equal(rates, expected, tolerance = 1e-12)
  expect_lt(rates[[1L]], rates[[2L]])
  expect_equal(
    challenge_margin_subjective_ball_probability_1d(
      fit, c(0, 0), Inf, context = c("0-0", "3-2")
    ),
    rates,
    tolerance = 0
  )

  league_rate <- challenge_margin_prior_ball_rate_1d(fit)
  expect_equal(
    challenge_margin_prior_ball_rate_1d(fit, context = "unseen"),
    league_rate,
    tolerance = 0
  )
  margins <- c(-1, 1)
  expect_equal(
    challenge_margin_prior_density_1d(
      fit, margins, context = rep("unseen", 2)
    ),
    challenge_margin_prior_density_1d(fit, margins),
    tolerance = 0
  )
  scored <- score_challenge_margin_subjective_q_1d(
    fit, private_margin_signal = 0, perception_sigma = Inf,
    context = "unseen"
  )
  expect_false(scored$context_seen_in_training)
  expect_true(scored$context_fallback)
  expect_equal(scored$prior_ball_rate, league_rate, tolerance = 0)
})

test_that("context weights are learned from requested training games only", {
  pitches <- synthetic_challenge_margin_pitches(games = 6L)
  training_games <- sprintf("g%02d", 1:4)
  altered <- data.table::copy(pitches)
  altered[!game_pk %in% training_games, `:=`(
    edge_distance_inches = edge_distance_inches + 100,
    balls_before = 3L,
    strikes_before = 2L
  )]
  one <- fit_challenge_margin_prior_1d(
    pitches, components = 3L, training_games = training_games
  )
  two <- fit_challenge_margin_prior_1d(
    altered, components = 3L, training_games = training_games
  )

  expect_equal(one$weight, two$weight, tolerance = 0)
  expect_equal(one$mean, two$mean, tolerance = 0)
  expect_equal(one$sd, two$sd, tolerance = 0)
  expect_equal(one$context_weights, two$context_weights, tolerance = 0)
})

test_that("deterministic EM produces ordered stable Gaussian mixtures", {
  pitches <- synthetic_challenge_margin_pitches(games = 10L)
  margin <- pitches$edge_distance_inches
  one <- fit_challenge_margin_gmm_1d(
    margin, components = 3L, tolerance = 1e-7
  )
  two <- fit_challenge_margin_gmm_1d(
    margin, components = 3L, tolerance = 1e-7
  )

  expect_true(one$converged)
  expect_equal(one$weight, two$weight, tolerance = 0)
  expect_equal(one$mean, two$mean, tolerance = 0)
  expect_equal(one$sd, two$sd, tolerance = 0)
  expect_equal(sum(one$weight), 1, tolerance = 1e-12)
  expect_true(all(one$weight > 0))
  expect_true(all(one$sd > 0))
  expect_true(all(diff(one$mean) >= 0))
  expect_true(is.finite(one$log_likelihood))
})

test_that("one-SE component selection chooses the smallest adequate mixture", {
  metrics <- data.table::data.table(
    components = c(1L, 3L, 6L),
    mean_log_score = c(-1.04, -1.02, -1.00),
    game_clustered_se = c(0.02, 0.03, 0.05),
    converged = TRUE
  )
  selected <- select_challenge_margin_component_count_1d(metrics)

  expect_identical(selected[selected == TRUE, components], 1L)
  expect_true(all(selected$one_se_threshold == -1.05))
  expect_true(all(selected$within_one_se))
})

test_that("nonconverged candidates cannot define the one-SE benchmark", {
  metrics <- data.table::data.table(
    components = c(1L, 3L, 6L),
    mean_log_score = c(-1.04, -1.02, -0.80),
    game_clustered_se = c(0.02, 0.03, 0.01),
    converged = c(TRUE, TRUE, FALSE)
  )
  selected <- select_challenge_margin_component_count_1d(metrics)

  expect_identical(selected[selected == TRUE, components], 1L)
  expect_equal(unique(selected$best_mean_log_score), -1.02)
  expect_equal(unique(selected$one_se_threshold), -1.05)
  expect_false(selected[components == 6L, eligible])
  expect_false(selected[components == 6L, within_one_se])
  expect_false(selected[components == 6L, selected])
})

test_that("component selection fails closed when no candidate converges", {
  metrics <- data.table::data.table(
    components = c(1L, 3L, 6L),
    mean_log_score = c(-1.04, -1.02, -0.80),
    game_clustered_se = c(0.02, 0.03, 0.01),
    converged = FALSE
  )
  expect_error(
    select_challenge_margin_component_count_1d(metrics),
    "No challenge-margin mixture candidate converged"
  )
})

test_that("EM defaults impose a practical transparent convergence budget", {
  defaults <- formals(fit_challenge_margin_gmm_1d)
  expect_identical(eval(defaults$tolerance), 1e-5)
  expect_identical(eval(defaults$max_iterations), 500L)

  fit <- fit_challenge_margin_gmm_1d(
    synthetic_challenge_margin_pitches(games = 10L)$edge_distance_inches,
    components = 6L
  )
  expect_true(fit$converged)
  expect_lte(fit$iterations, fit$maximum_iterations)
  expect_lte(
    fit$last_relative_log_likelihood_change,
    fit$convergence_tolerance
  )
})

test_that("candidate fitting separates games and preserves fold/draw IDs", {
  pitches <- synthetic_challenge_margin_pitches(games = 12L)
  fit_games <- sprintf("g%02d", 1:8)
  validation_games <- sprintf("g%02d", 9:12)
  selection <- select_challenge_margin_prior_1d(
    pitches,
    fit_games = fit_games,
    validation_games = validation_games,
    components = c(1L, 3L, 6L),
    fold_id = "outer_4",
    global_draw_id = 91L,
    tolerance = 1e-7
  )

  expect_length(intersect(
    selection$component_fit_games, selection$component_validation_games
  ), 0L)
  expect_true(selection$selected_components %in% c(1L, 3L, 6L))
  expect_setequal(selection$fit$training_games, c(fit_games, validation_games))
  expect_true(all(selection$candidate_metrics$fold_id == "outer_4"))
  expect_true(all(selection$candidate_metrics$global_draw_id == 91L))
  expect_identical(selection$fit$fold_id, "outer_4")
  expect_identical(selection$fit$global_draw_id, 91L)
  expect_error(
    select_challenge_margin_prior_1d(
      pitches, fit_games = fit_games, validation_games = fit_games[1:2]
    ),
    "disjoint"
  )
})

test_that("held-out location scores use game-clustered uncertainty", {
  scored <- data.table::data.table(
    game_pk = rep(c("a", "b", "c"), each = 3L),
    location_log_density = c(-1, -1.1, -0.9, -2, -2.1, -1.9, -1.4, -1.5, -1.6)
  )
  metric <- challenge_margin_game_clustered_log_score_1d(scored)

  expect_equal(metric$mean_log_score, mean(scored$location_log_density))
  expect_gt(metric$game_clustered_se, 0)
  expect_identical(metric$heldout_games, 3L)
  expect_identical(metric$heldout_rows, 9L)
})

test_that("analytic subjective q agrees with Gaussian conjugacy", {
  fit <- manual_challenge_margin_prior_fit(mean = -0.4, sd = 2.2)
  signal <- 0.7
  sigma <- 1.3
  prior_variance <- fit$sd^2
  signal_variance <- sigma^2
  posterior_variance <- prior_variance * signal_variance /
    (prior_variance + signal_variance)
  posterior_mean <- (
    signal_variance * fit$mean + prior_variance * signal
  ) / (prior_variance + signal_variance)
  expected <- stats::pnorm(posterior_mean / sqrt(posterior_variance))

  observed <- challenge_margin_subjective_ball_probability_1d(
    fit, signal, sigma
  )
  expect_equal(observed, expected, tolerance = 1e-12)
})

test_that("zero and diffuse perception recover geometry and the prior", {
  fit <- manual_challenge_margin_prior_fit(
    weight = c(0.7, 0.3), mean = c(-2, 1), sd = c(1.5, 0.8)
  )
  signals <- c(-3, -0.2, 0, 0.2, 4)
  deterministic <- challenge_margin_subjective_ball_probability_1d(
    fit, signals, 0
  )
  expect_identical(deterministic, as.numeric(signals > 0))

  prior_rate <- challenge_margin_prior_ball_rate_1d(fit)
  diffuse <- challenge_margin_subjective_ball_probability_1d(
    fit, signals, Inf
  )
  very_diffuse <- challenge_margin_subjective_ball_probability_1d(
    fit, signals, 1e8
  )
  expect_equal(diffuse, rep(prior_rate, length(signals)), tolerance = 0)
  expect_equal(very_diffuse, rep(prior_rate, length(signals)), tolerance = 1e-7)

  nearly_exact <- challenge_margin_subjective_ball_probability_1d(
    fit, c(-0.5, 0.5), 1e-6
  )
  expect_equal(nearly_exact, c(0, 1), tolerance = 1e-10)
})

test_that("subjective q is monotone in the private signed-margin signal", {
  fit <- manual_challenge_margin_prior_fit(
    weight = c(0.55, 0.30, 0.15),
    mean = c(-3, -0.5, 1.5),
    sd = c(1.2, 0.8, 1.0)
  )
  q <- challenge_margin_subjective_ball_probability_1d(
    fit, seq(-8, 8, length.out = 301L), 1.4
  )

  expect_true(all(is.finite(q)))
  expect_true(all(q >= 0 & q <= 1))
  expect_true(all(diff(q) >= -1e-12))
})

test_that("subjective scoring retains exact global draw and fold metadata", {
  fit <- manual_challenge_margin_prior_fit(
    fold_id = "fold_5", global_draw_id = 1234L
  )
  scored <- score_challenge_margin_subjective_q_1d(
    fit,
    private_margin_signal = c(-1, 0, 1),
    perception_sigma = 1.5,
    row_id = c("p1", "p2", "p3")
  )

  expect_identical(scored$row_id, c("p1", "p2", "p3"))
  expect_true(all(scored$fold_id == "fold_5"))
  expect_true(all(scored$global_draw_id == 1234L))
  expect_true(all(scored$prior_components == 1L))
})

test_that("choice-conditioned q integrates after signal-specific action", {
  fit <- manual_challenge_margin_prior_fit(mean = -0.5, sd = 2.5)
  increasing_action <- function(q, signal, row_index) {
    stats::plogis(-2 + 6 * q)
  }
  selected <- integrate_challenge_margin_choice_1d(
    fit,
    true_margin = c(-0.5, 0.5),
    perception_sigma = 1.2,
    action_function = increasing_action,
    quadrature_order = 21L
  )

  expect_true(all(data.table::between(
    selected$predicted_challenge_probability, 0, 1
  )))
  expect_true(all(selected$q_chosen > selected$q_signal_mean))
  expect_true(all(selected$fold_id == "fold_2"))
  expect_true(all(selected$global_draw_id == 17L))

  always <- integrate_challenge_margin_choice_1d(
    fit,
    true_margin = 0.25,
    perception_sigma = 1.2,
    action_function = function(q, signal, row_index) rep(1, length(q)),
    quadrature_order = 21L
  )
  expect_equal(always$predicted_challenge_probability, 1, tolerance = 1e-12)
  expect_equal(always$q_chosen, always$q_signal_mean, tolerance = 1e-12)
})

test_that("reduced-form composition preserves the exact probit structure", {
  fit <- manual_challenge_margin_prior_fit(
    weight = c(0.6, 0.4), mean = c(-1.5, 1), sd = c(1.2, 0.9)
  )
  margin <- c(-0.5, 0.8)
  threshold <- matrix(c(-1, 0, 1, -0.2, 0.5, 1.4), nrow = 2, byrow = TRUE)
  sigma <- matrix(c(0.7, 1.2, 2, 0.9, 1.4, 2.5), nrow = 2, byrow = TRUE)
  composed <- compose_challenge_margin_discrimination_1d(
    fit,
    true_margin = margin,
    threshold_draws = threshold,
    perception_sigma_draws = sigma,
    context = c("0-0", "3-2"),
    row_id = c("pitch_a", "pitch_b"),
    draw_id = c(101L, 205L, 999L)
  )
  expected <- stats::pnorm((
    matrix(margin, nrow = 2, ncol = 3) - threshold
  ) / sigma)

  expect_equal(
    composed$draws$predicted_challenge_probability,
    as.vector(t(expected)),
    tolerance = 1e-14
  )
  expect_identical(
    composed$draws$global_draw_id,
    rep(c(101L, 205L, 999L), 2L)
  )
  expect_identical(
    composed$draws$row_id,
    rep(c("pitch_a", "pitch_b"), each = 3L)
  )
  expect_true(all(composed$draws$numerical_status ==
    "adaptive_truncated_normal"))
  expect_true(all(data.table::between(composed$draws$q_chosen, 0, 1)))
  expect_equal(composed$summary$posterior_draws, rep(3L, 2L))
  expect_identical(
    composed$model,
    "reduced_form_latent_normal_signal_hard_threshold"
  )
})

test_that("adaptive truncated integration matches direct q_chosen integration", {
  fit <- manual_challenge_margin_prior_fit(
    weight = c(0.55, 0.30, 0.15),
    mean = c(-2.5, -0.3, 1.4),
    sd = c(1.2, 0.7, 1)
  )
  margin <- 0.2
  threshold <- 0.5
  sigma <- 1.4
  composed <- compose_challenge_margin_discrimination_1d(
    fit, margin, threshold, sigma, context = "0-0",
    relative_tolerance = 1e-10, absolute_tolerance = 1e-12
  )
  numerator <- stats::integrate(
    function(signal) {
      challenge_margin_subjective_ball_probability_1d(
        fit, signal, sigma, context = "0-0"
      ) * stats::dnorm(signal, margin, sigma)
    },
    lower = threshold, upper = Inf,
    rel.tol = 1e-11, abs.tol = 1e-13
  )$value
  denominator <- stats::pnorm((margin - threshold) / sigma)

  expect_equal(
    composed$draws$predicted_challenge_probability,
    denominator,
    tolerance = 1e-14
  )
  expect_equal(
    composed$draws$q_chosen,
    numerator / denominator,
    tolerance = 1e-9
  )
  expect_lt(composed$draws$numerical_absolute_error, 1e-9)

  ordered <- compose_challenge_margin_discrimination_1d(
    fit,
    true_margin = margin,
    threshold_draws = matrix(c(-1, 0, 1, 2), nrow = 1L),
    perception_sigma_draws = sigma,
    context = "0-0",
    draw_id = 1:4
  )$draws
  expect_true(all(diff(ordered$predicted_challenge_probability) < 0))
  expect_true(all(diff(ordered$q_chosen) > 0))
})

test_that("reduced-form composition has explicit zero and diffuse sigma limits", {
  fit <- manual_challenge_margin_prior_fit(
    weight = c(0.5, 0.5), mean = c(-2, 2), sd = c(1, 1)
  )
  fit$context_weights <- rbind(
    "0-0" = c(0.9, 0.1),
    "3-2" = c(0.1, 0.9)
  )
  exact <- compose_challenge_margin_discrimination_1d(
    fit,
    true_margin = c(0.5, -0.5),
    threshold_draws = 0,
    perception_sigma_draws = 0,
    context = c("0-0", "3-2")
  )$draws
  expect_identical(exact$predicted_challenge_probability, c(1, 0))
  expect_identical(exact$q_chosen, c(1, NA_real_))
  expect_true(all(exact$numerical_status == "analytic_zero_sigma_limit"))

  diffuse <- compose_challenge_margin_discrimination_1d(
    fit,
    true_margin = c(-4, 4),
    threshold_draws = c(-2, 2),
    perception_sigma_draws = Inf,
    context = c("0-0", "3-2")
  )$draws
  rates <- challenge_margin_prior_ball_rate_1d(
    fit, context = c("0-0", "3-2")
  )
  expect_identical(diffuse$predicted_challenge_probability, c(0.5, 0.5))
  expect_equal(diffuse$q_chosen, rates, tolerance = 0)
  expect_true(all(diffuse$numerical_status ==
    "analytic_infinite_sigma_limit"))

  expect_error(
    compose_challenge_margin_discrimination_1d(
      fit, true_margin = 0, threshold_draws = 0,
      perception_sigma_draws = 1
    ),
    "context must be supplied explicitly"
  )
})

test_that("prior scoring fingerprints data and can enforce game separation", {
  pitches <- synthetic_challenge_margin_pitches(games = 8L)
  training_games <- sprintf("g%02d", 1:5)
  heldout_games <- sprintf("g%02d", 6:8)
  fit <- fit_challenge_margin_prior_1d(
    pitches, components = 3L, training_games = training_games
  )
  heldout <- score_challenge_margin_prior_1d(
    fit,
    pitches[game_pk %in% heldout_games],
    require_game_separation = TRUE
  )
  metadata <- attr(heldout, "game_separation")

  expect_match(fit$training_fingerprint, "^[0-9a-f]{64}$")
  expect_true(metadata$required)
  expect_length(metadata$overlapping_games, 0L)
  expect_identical(
    unique(heldout$prior_training_fingerprint),
    fit$training_fingerprint
  )
  expect_match(unique(heldout$prior_scoring_fingerprint), "^[0-9a-f]{64}$")
  expect_false(identical(
    unique(heldout$prior_scoring_fingerprint), fit$training_fingerprint
  ))
  expect_error(
    score_challenge_margin_prior_1d(
      fit, pitches[game_pk %in% training_games],
      require_game_separation = TRUE
    ),
    "overlap"
  )
})

test_that("payoff inversion is row- and draw-aligned on posterior log odds", {
  fit <- manual_challenge_margin_prior_fit(
    weight = c(0.6, 0.4), mean = c(-1.5, 1), sd = c(1.2, 0.9)
  )
  fit$context_weights <- rbind(
    "0-0" = c(0.8, 0.2),
    "3-2" = c(0.3, 0.7)
  )
  sigma <- matrix(
    c(0.5, 1, 2, 0.7, 1.4, 2.8), nrow = 2L, byrow = TRUE
  )
  solved <- solve_challenge_margin_payoff_threshold_1d(
    fit,
    gain = c(1, 2),
    inventory_loss = c(1, 0.5),
    perception_sigma_draws = sigma,
    context = c("0-0", "3-2"),
    row_id = c("pitch_a", "pitch_b"),
    draw_id = c(11L, 22L, 33L)
  )

  expect_equal(dim(solved$threshold_draws), c(2L, 3L))
  expect_equal(solved$perception_sigma_draws, sigma, tolerance = 0)
  expect_identical(
    solved$draws$global_draw_id,
    rep(c(11L, 22L, 33L), 2L)
  )
  expect_identical(
    solved$draws$row_id,
    rep(c("pitch_a", "pitch_b"), each = 3L)
  )
  expect_true(all(solved$draws$root_status == "interior_log_odds_root"))
  expect_true(all(solved$draws$bracket_lower <
    solved$draws$threshold_inches))
  expect_true(all(solved$draws$bracket_upper >
    solved$draws$threshold_inches))
  expect_lt(max(solved$draws$inversion_absolute_error), 1e-9)
  expect_lt(max(abs(solved$draws$log_odds_residual)), 1e-8)

  recalculated <- mapply(
    function(signal, draw_sigma, count) {
      challenge_margin_subjective_ball_probability_1d(
        fit, signal, draw_sigma, context = count
      )
    },
    solved$draws$threshold_inches,
    solved$draws$perception_sigma,
    solved$draws$prior_context
  )
  expect_equal(recalculated, solved$draws$q_target, tolerance = 1e-9)

  composed <- compose_challenge_margin_discrimination_1d(
    fit,
    true_margin = c(-0.5, 0.8),
    threshold_draws = solved$threshold_draws,
    perception_sigma_draws = solved$perception_sigma_draws,
    context = solved$context,
    row_id = solved$row_id,
    draw_id = solved$global_draw_id
  )
  expected_probability <- stats::pnorm((
    matrix(c(-0.5, 0.8), nrow = 2L, ncol = 3L) -
      solved$threshold_draws
  ) / sigma)
  expect_equal(
    composed$draws$predicted_challenge_probability,
    as.vector(t(expected_probability)),
    tolerance = 1e-14
  )
})

test_that("payoff inversion remains stable for extreme interior costs", {
  fit <- manual_challenge_margin_prior_fit(
    weight = c(0.7, 0.3), mean = c(-2, 1), sd = c(1.5, 0.8)
  )
  solved <- solve_challenge_margin_payoff_threshold_1d(
    fit,
    gain = c(1e12, 1),
    inventory_loss = c(1, 1e12),
    perception_sigma_draws = 1.3,
    context = c("0-0", "0-0")
  )

  expect_true(all(is.finite(solved$draws$threshold_inches)))
  expect_lt(solved$draws$threshold_inches[[1L]],
    solved$draws$threshold_inches[[2L]])
  expect_lt(max(abs(solved$draws$log_odds_residual)), 1e-7)
  expect_lt(max(solved$draws$inversion_absolute_error), 1e-10)
  expect_true(any(solved$draws$bracket_expansions > 0L))
})

test_that("payoff and composition boundaries are exact", {
  fit <- manual_challenge_margin_prior_fit(
    weight = c(0.5, 0.5), mean = c(-2, 2), sd = c(1, 1)
  )
  fit$context_weights <- rbind(
    "low" = c(0.9, 0.1),
    "high" = c(0.1, 0.9)
  )
  solved <- solve_challenge_margin_payoff_threshold_1d(
    fit,
    gain = c(1, 0, 1, 1, 1),
    inventory_loss = c(0, 1, 1, 1, 1),
    perception_sigma_draws = c(1, 1, 0, Inf, Inf),
    context = c("low", "low", "low", "low", "high")
  )
  expect_identical(
    solved$draws$threshold_inches,
    c(-Inf, Inf, 0, Inf, -Inf)
  )
  expect_identical(
    solved$draws$root_status,
    c(
      "analytic_zero_failure_cost_always",
      "analytic_zero_gain_never",
      "analytic_zero_sigma_generalized_inverse",
      "analytic_diffuse_sigma_prior_not_above_threshold",
      "analytic_diffuse_sigma_prior_above_threshold"
    )
  )
  expect_identical(solved$draws$q_target, c(0, 1, 0.5, 0.5, 0.5))

  composed <- compose_challenge_margin_discrimination_1d(
    fit,
    true_margin = c(0, 0, 0.5, 0, 0),
    threshold_draws = solved$threshold_draws,
    perception_sigma_draws = solved$perception_sigma_draws,
    context = solved$context
  )$draws
  expect_identical(
    composed$predicted_challenge_probability,
    c(1, 0, 1, 0, 1)
  )
  expect_true(is.finite(composed$q_chosen[[1L]]))
  expect_true(is.na(composed$q_chosen[[2L]]))
  expect_identical(composed$q_chosen[[3L]], 1)
  expect_true(is.na(composed$q_chosen[[4L]]))
  expect_equal(
    composed$q_chosen[[5L]],
    challenge_margin_prior_ball_rate_1d(fit, context = "high"),
    tolerance = 0
  )
  expect_identical(
    composed$numerical_status,
    c(
      "adaptive_untruncated_normal_always_challenge",
      "analytic_never_challenge_threshold",
      "analytic_zero_sigma_limit",
      "analytic_never_challenge_threshold",
      "analytic_always_challenge_infinite_sigma"
    )
  )

  expect_error(
    solve_challenge_margin_payoff_threshold_1d(
      fit, gain = 0, inventory_loss = 0,
      perception_sigma_draws = 1, context = "low"
    ),
    "not both zero"
  )
  expect_error(
    solve_challenge_margin_payoff_threshold_1d(
      fit, gain = 1, inventory_loss = 1,
      perception_sigma_draws = 1
    ),
    "context must be supplied explicitly"
  )
})
