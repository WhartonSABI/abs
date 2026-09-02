synthetic_continuous_prior <- function() {
  list(
    weights = c(0.55, 0.45),
    means = rbind(c(0, 0), c(12, 2)),
    scale = rbind(c(5, 7), c(4, 5)),
    correlation = c(0.35, -0.25)
  )
}

synthetic_signal_batter_choices <- function(rows = 72L) {
  index <- seq_len(rows)
  data.table::data.table(
    game_pk = 1L + index %% 9L,
    at_bat_number = index,
    pitch_number = 1L,
    batter_id = paste0("b", 1L + index %% 4L),
    swing = index %% 2L,
    plate_x = 0.8 * sin(index / 7),
    plate_z = 2.5 + 0.9 * cos(index / 8),
    sz_top = 3.5,
    sz_bot = 1.5,
    balls = index %% 4L,
    strikes = index %% 3L,
    pitch_family = c("fastball", "breaking", "offspeed")[1L + index %% 3L],
    matchup = c("L-R", "R-L")[1L + index %% 2L]
  )
}

test_that("full component correlations survive signal conditioning", {
  posterior <- condition_continuous_pitch_prior_on_signal(
    synthetic_continuous_prior(), signal = c(7, -1), perception_sigma = 3
  )

  expect_equal(dim(posterior$covariance), c(2, 2, 2))
  expect_true(abs(posterior$covariance[1, 1, 2]) > 1e-6)
  expect_true(abs(posterior$covariance[2, 1, 2]) > 1e-6)
  expect_equal(sum(posterior$weights), 1, tolerance = 1e-12)
})

test_that("anisotropic signal axes enter full Gaussian conditioning", {
  prior <- list(
    weights = 1,
    means = matrix(c(0, 0), nrow = 1L),
    scale = matrix(c(5, 6), nrow = 1L),
    correlation = 0
  )
  posterior <- condition_continuous_pitch_prior_on_signal(
    prior, signal = c(1, -1), perception_sigma = c(4, 2)
  )

  expect_equal(
    diag(posterior$covariance[1, , ]),
    c(25 * 16 / (25 + 16), 36 * 4 / (36 + 4)),
    tolerance = 1e-12
  )
  expect_equal(posterior$covariance[1, 1, 2], 0, tolerance = 1e-12)
})

test_that("scalar and equal-axis perception inputs are identical", {
  arguments <- list(
    true_location = c(5, 0),
    prior = synthetic_continuous_prior(),
    initial_call = "called_strike",
    zone_half_height_inches = 10,
    take_likelihood_fn = constant_continuous_take_likelihood(0.8),
    gain = 1,
    inventory_loss = 0.3,
    decision_slope = 2,
    order = 7
  )
  isotropic_scalar <- do.call(
    integrate_continuous_signal_draw,
    c(arguments, list(perception_sigma = 3))
  )
  isotropic_axes <- do.call(
    integrate_continuous_signal_draw,
    c(arguments, list(perception_sigma = c(3, 3)))
  )

  expect_equal(isotropic_scalar, isotropic_axes, tolerance = 1e-12)
  expect_equal(isotropic_axes$perception_sigma_x_inches, 3)
  expect_equal(isotropic_axes$perception_sigma_z_inches, 3)
})

test_that("outer private-signal quadrature uses each anisotropic axis", {
  evaluated <- integrate_continuous_signal_draw(
    true_location = c(0, 0),
    perception_sigma = c(4, 2),
    prior = synthetic_continuous_prior(),
    initial_call = "called_strike",
    zone_half_height_inches = 10,
    take_likelihood_fn = constant_continuous_take_likelihood(1),
    gain = 1,
    inventory_loss = 0.3,
    order = 7,
    return_nodes = TRUE
  )
  nodes <- evaluated$nodes

  expect_equal(
    sum(nodes$unconditional_signal_weight * nodes$signal_x_inches^2),
    4^2,
    tolerance = 1e-10
  )
  expect_equal(
    sum(nodes$unconditional_signal_weight * nodes$signal_z_inches^2),
    2^2,
    tolerance = 1e-10
  )
  expect_equal(evaluated$summary$perception_sigma_x_inches, 4)
  expect_equal(evaluated$summary$perception_sigma_z_inches, 2)
})

test_that("batter draw adapter promotes shared anisotropy to signal axes", {
  bundle <- prepare_continuous_batter_perception(
    synthetic_signal_batter_choices(), anisotropy = "shared"
  )
  fit <- list(
    player_table = bundle$player_table,
    context_specification = list(categorical = list(), numeric = list()),
    sector_specification = bundle$sector_specification,
    anisotropy = "shared"
  )
  class(fit) <- "continuous_batter_perception_fit"
  context_columns <- 0L
  sector_columns <- bundle$data$S
  draws <- list(
    draw_id = 17L,
    mu_log_sigma = log(3),
    tau_log_sigma = 0.2,
    sigma_player = matrix(3, nrow = 1L, ncol = bundle$data$P),
    mu_threshold = 0,
    threshold_player = matrix(0, nrow = 1L, ncol = bundle$data$P),
    beta_context = matrix(0, nrow = 1L, ncol = context_columns),
    beta_sector = array(0, dim = c(1L, 3L, sector_columns)),
    lower_swing = matrix(0.1, nrow = 1L, ncol = 3L),
    upper_swing = matrix(0.9, nrow = 1L, ncol = 3L),
    log_anisotropy = log(2),
    anisotropy_ratio = 2
  )
  parameters <- continuous_batter_draw_signal_parameters(
    fit, bundle$rows[1L], draw_index = 1L, posterior_draws = draws
  )

  expect_equal(unname(parameters$perception_sigma), c(6, 1.5))
  expect_equal(parameters$perception_sigma_x_inches, 6)
  expect_equal(parameters$perception_sigma_z_inches, 1.5)
  expect_equal(parameters$anisotropy_ratio, 2)
  expect_equal(parameters$anisotropy_mode, "shared")

  fit$anisotropy <- "isotropic"
  draws$anisotropy_ratio <- NULL
  draws$log_anisotropy <- NULL
  isotropic <- continuous_batter_draw_signal_parameters(
    fit, bundle$rows[1L], draw_index = 1L, posterior_draws = draws
  )
  expect_length(isotropic$perception_sigma, 1L)
  expect_equal(isotropic$perception_sigma, 3)
  expect_equal(isotropic$perception_sigma_x_inches, 3)
  expect_equal(isotropic$perception_sigma_z_inches, 3)
})

test_that("the decision link is applied before integrating private signals", {
  evaluated <- integrate_continuous_signal_draw(
    true_location = c(9, 0),
    perception_sigma = 3,
    prior = synthetic_continuous_prior(),
    initial_call = "called_strike",
    zone_half_height_inches = 10,
    take_likelihood_fn = constant_continuous_take_likelihood(0.8),
    gain = 1,
    inventory_loss = 0.3,
    decision_slope = 5,
    order = 7,
    return_nodes = TRUE
  )
  result <- evaluated$summary
  action_of_average_q <- stats::plogis(
    5 * challenge_expected_utility(
      result$private_signal_q, gain = 1, inventory_loss = 0.3
    )
  )

  expect_gt(
    abs(result$predicted_challenge_probability - action_of_average_q),
    0.02
  )
  expect_equal(sum(evaluated$nodes$conditional_signal_weight), 1,
    tolerance = 1e-12
  )
  expect_gt(result$choice_conditioned_q, result$private_signal_q)
})

test_that("omega zero is exactly independent of the initial-call model", {
  forbidden_call_model <- function(x, z) {
    stop("the call cue was evaluated")
  }
  q <- continuous_signal_wrong_call_probability(
    prior = synthetic_continuous_prior(),
    signal = c(9, 1),
    perception_sigma = 3,
    initial_call = "called_strike",
    zone_half_height_inches = 10,
    omega = 0,
    call_likelihood_fn = forbidden_call_model,
    order = 7
  )

  expect_true(is.finite(q))
  expect_true(q >= 0 && q <= 1)
})

test_that("a constant call likelihood makes all trust weights equivalent", {
  arguments <- list(
    prior = synthetic_continuous_prior(),
    signal = c(9, 1),
    perception_sigma = 3,
    initial_call = "called_strike",
    zone_half_height_inches = 10,
    call_likelihood_fn = constant_continuous_call_likelihood(0.3),
    order = 11
  )
  q <- vapply(c(0, 0.5, 1), function(omega) {
    do.call(
      continuous_signal_wrong_call_probability,
      c(arguments, list(omega = omega))
    )
  }, numeric(1))

  expect_equal(q, rep(q[[1]], 3), tolerance = 1e-10)
})

test_that("unsupported non-Gaussian call updates fail explicitly", {
  unmarked_call_model <- function(x, z) stats::plogis(x)

  expect_error(
    continuous_signal_wrong_call_probability(
      prior = synthetic_continuous_prior(),
      signal = c(9, 1),
      perception_sigma = 3,
      initial_call = "called_strike",
      zone_half_height_inches = 10,
      omega = 0.5,
      call_likelihood_fn = unmarked_call_model,
      order = 7
    ),
    class = "continuous_integration_unavailable"
  )
})

test_that("zero perception sigma recovers deterministic rounded geometry", {
  outside <- continuous_signal_wrong_call_probability(
    prior = synthetic_continuous_prior(),
    signal = c(12, 10),
    perception_sigma = 0,
    initial_call = "called_strike",
    zone_half_height_inches = 10,
    omega = 0,
    order = 7
  )
  inside <- continuous_signal_wrong_call_probability(
    prior = synthetic_continuous_prior(),
    signal = c(0, 0),
    perception_sigma = 0,
    initial_call = "called_strike",
    zone_half_height_inches = 10,
    omega = 0,
    order = 7
  )

  expect_equal(outside, 1, tolerance = 1e-12)
  expect_equal(inside, 0, tolerance = 1e-12)
})

test_that("diffuse perception returns the contextual prior", {
  prior <- synthetic_continuous_prior()
  parsed <- continuous_prior_component_covariances(prior)
  prior_strike_probability <- sum(vapply(
    seq_along(parsed$weights),
    function(component) {
      parsed$weights[[component]] *
        continuous_gaussian_rounded_abs_expectation(
          parsed$means[component, ],
          parsed$covariance[component, , ],
          zone_half_height_inches = 10,
          order = 11
        )
    },
    numeric(1)
  ))
  diffuse_wrong_probability <- continuous_signal_wrong_call_probability(
    prior = prior,
    signal = c(0, 0),
    perception_sigma = 1e6,
    initial_call = "called_strike",
    zone_half_height_inches = 10,
    omega = 0,
    order = 11
  )

  expect_equal(
    diffuse_wrong_probability,
    1 - prior_strike_probability,
    tolerance = 1e-6
  )
  expect_lt(diffuse_wrong_probability, 0.9)
})

test_that("orders seven and eleven produce aligned numerical diagnostics", {
  specification <- list(
    global_draw_id = 91L,
    true_location = c(2, 0),
    perception_sigma = 1.25,
    prior = synthetic_continuous_prior(),
    initial_call = "called_strike",
    zone_half_height_inches = 10,
    take_likelihood_fn = constant_continuous_take_likelihood(0.8),
    gain = 1,
    inventory_loss = 0.3,
    decision_slope = 2
  )
  scored <- score_continuous_signal_draws(
    list(specification), omegas = 0, orders = c(7, 11)
  )

  expect_s3_class(scored, "continuous_signal_draw_scores")
  expect_equal(scored$posterior_draws$global_draw_id, 91L)
  expect_equal(scored$posterior_draws$quadrature_order, 11L)
  expect_true(scored$order_diagnostics$all_pass)
  expect_true(all(
    scored$order_diagnostics$by_metric$mean_absolute_difference < 0.001
  ))
  expect_true(all(
    scored$order_diagnostics$by_metric$p99_absolute_difference < 0.005
  ))
  expect_true(all(c(
    "private_signal_q_lower_95", "private_signal_q_upper_95",
    "choice_conditioned_q_lower_95", "choice_conditioned_q_upper_95"
  ) %in% names(scored$intervals)))
})
