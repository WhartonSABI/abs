# Continuous private-signal integration for the human challenge model.
#
# This file deliberately contains no spatial discretization. Signal integration
# uses Gauss--Hermite expectations under S | X. Location posteriors retain the
# full 2 x 2 covariance of every pitch-prior component. When the public-call
# cue makes that posterior non-Gaussian, its normalizing constant is evaluated
# with Gaussian--Hermite quadrature and its strike-region mass with nested
# Gauss--Legendre integration on conditional-normal CDF scales.

.signal_integral_unavailable <- function(message) {
  condition <- structure(
    list(message = message, call = NULL),
    class = c(
      "continuous_integration_unavailable", "error", "condition"
    )
  )
  stop(condition)
}

.signal_validate_probability <- function(value, name) {
  value <- as.numeric(value)
  if (!length(value) || anyNA(value) || any(!is.finite(value)) ||
      any(value < 0 | value > 1)) {
    stop(name, " must return finite probabilities", call. = FALSE)
  }
  value
}

.signal_validate_location <- function(value, name) {
  value <- as.numeric(value)
  if (length(value) != 2L || anyNA(value) || any(!is.finite(value))) {
    stop(name, " must contain two finite coordinates", call. = FALSE)
  }
  value
}

.signal_symmetrize <- function(value) {
  (value + t(value)) / 2
}

.signal_psd_matrix <- function(value, name, strictly_positive = FALSE) {
  value <- as.matrix(value)
  if (!identical(dim(value), c(2L, 2L)) || anyNA(value) ||
      any(!is.finite(value))) {
    stop(name, " must be one finite 2 by 2 matrix", call. = FALSE)
  }
  value <- .signal_symmetrize(value)
  eigenvalues <- eigen(value, symmetric = TRUE, only.values = TRUE)$values
  tolerance <- 1e-9 * max(1, max(abs(value)))
  if (min(eigenvalues) < -tolerance ||
      (isTRUE(strictly_positive) && min(eigenvalues) <= tolerance)) {
    qualifier <- if (strictly_positive) "positive definite" else "positive semidefinite"
    stop(name, " must be ", qualifier, call. = FALSE)
  }
  value
}

continuous_prior_component_covariances <- function(
  prior,
  correlation_tolerance = 1e-8
) {
  if (!is.list(prior) || is.null(prior$weights) || is.null(prior$means)) {
    stop("prior must contain weights and means", call. = FALSE)
  }
  weights <- as.numeric(prior$weights)
  if (!length(weights) || anyNA(weights) || any(!is.finite(weights)) ||
      any(weights < 0) || sum(weights) <= 0) {
    stop("prior weights must be finite, nonnegative, and have positive mass",
      call. = FALSE
    )
  }
  components <- length(weights)
  means <- as.matrix(prior$means)
  if (!identical(dim(means), c(components, 2L)) || anyNA(means) ||
      any(!is.finite(means))) {
    stop("prior means must have one finite two-coordinate row per component",
      call. = FALSE
    )
  }

  covariance <- prior$covariance
  if (!is.null(covariance)) {
    if (components == 1L && is.matrix(covariance)) {
      covariance <- array(covariance, dim = c(1L, 2L, 2L))
    }
    if (!is.array(covariance) ||
        !identical(dim(covariance), c(components, 2L, 2L))) {
      stop("prior covariance must have dimensions components by 2 by 2",
        call. = FALSE
      )
    }
  } else {
    scale <- prior$scale
    if (is.null(scale)) scale <- prior$component_sd
    if (is.null(scale)) scale <- prior$sd
    if (is.null(scale)) {
      stop("prior must contain covariance or component scale", call. = FALSE)
    }
    scale <- .continuous_as_k_by_two(
      scale, components, "prior scale", constraint = "positive"
    )
    correlation <- prior$correlation
    if (is.null(correlation)) correlation <- prior$rho
    if (is.null(correlation)) correlation <- rep(0, components)
    correlation <- rep_len(as.numeric(correlation), components)
    if (anyNA(correlation) || any(!is.finite(correlation)) ||
        any(abs(correlation) >= 1)) {
      stop("prior correlations must be finite and lie strictly between -1 and 1",
        call. = FALSE
      )
    }
    correlation <- pmax(-1 + correlation_tolerance,
      pmin(1 - correlation_tolerance, correlation)
    )
    covariance <- array(NA_real_, dim = c(components, 2L, 2L))
    for (component in seq_len(components)) {
      covariance[component, , ] <- matrix(c(
        scale[component, 1L]^2,
        correlation[[component]] * prod(scale[component, ]),
        correlation[[component]] * prod(scale[component, ]),
        scale[component, 2L]^2
      ), nrow = 2L)
    }
  }
  for (component in seq_len(components)) {
    covariance[component, , ] <- .signal_psd_matrix(
      covariance[component, , ],
      paste0("prior covariance component ", component),
      strictly_positive = TRUE
    )
  }
  list(
    weights = weights / sum(weights),
    means = means,
    covariance = covariance
  )
}

.signal_log_mvn2 <- function(value, mean, covariance) {
  value <- .signal_validate_location(value, "value")
  mean <- .signal_validate_location(mean, "mean")
  covariance <- .signal_psd_matrix(
    covariance, "covariance", strictly_positive = TRUE
  )
  factor <- chol(covariance)
  centered <- value - mean
  standardized <- backsolve(factor, centered, transpose = TRUE)
  -log(2 * pi) - sum(log(diag(factor))) -
    0.5 * sum(standardized^2)
}

# Condition a full-covariance Gaussian-mixture prior on
# S | X ~ N_2(X, diag(sigma_x^2, sigma_z^2)). A scalar sigma is isotropic.
# Two zero axes are treated as an exactly observed private signal and therefore
# yield a point-mass posterior at that signal.
condition_continuous_pitch_prior_on_signal <- function(
  prior,
  signal,
  perception_sigma
) {
  parsed <- continuous_prior_component_covariances(prior)
  signal <- .signal_validate_location(signal, "signal")
  perception_sigma <- as.numeric(perception_sigma)
  if (length(perception_sigma) == 1L) {
    perception_sigma <- rep(perception_sigma, 2L)
  }
  if (length(perception_sigma) != 2L || anyNA(perception_sigma) ||
      any(!is.finite(perception_sigma)) || any(perception_sigma < 0)) {
    stop("perception_sigma must contain one or two nonnegative finite values",
      call. = FALSE
    )
  }
  if (xor(perception_sigma[[1L]] == 0, perception_sigma[[2L]] == 0)) {
    .signal_integral_unavailable(
      "partially degenerate perception covariance is not supported"
    )
  }

  components <- length(parsed$weights)
  posterior_mean <- matrix(NA_real_, nrow = components, ncol = 2L)
  posterior_covariance <- array(NA_real_, dim = c(components, 2L, 2L))
  log_component_mass <- numeric(components)
  signal_covariance <- diag(perception_sigma^2, 2L)

  for (component in seq_len(components)) {
    prior_covariance <- parsed$covariance[component, , ]
    marginal_covariance <- prior_covariance + signal_covariance
    log_component_mass[[component]] <- log(parsed$weights[[component]]) +
      .signal_log_mvn2(
        signal, parsed$means[component, ], marginal_covariance
      )
    if (all(perception_sigma == 0)) {
      posterior_mean[component, ] <- signal
      posterior_covariance[component, , ] <- matrix(0, 2L, 2L)
    } else {
      kalman_gain <- prior_covariance %*% solve(marginal_covariance)
      posterior_mean[component, ] <- parsed$means[component, ] +
        as.numeric(kalman_gain %*% (signal - parsed$means[component, ]))
      covariance <- prior_covariance - kalman_gain %*% prior_covariance
      covariance <- .signal_symmetrize(covariance)
      decomposition <- eigen(covariance, symmetric = TRUE)
      decomposition$values <- pmax(decomposition$values, 0)
      posterior_covariance[component, , ] <-
        decomposition$vectors %*% diag(decomposition$values, 2L) %*%
        t(decomposition$vectors)
    }
  }

  log_evidence <- .continuous_log_sum_exp(log_component_mass)
  posterior_weights <- exp(log_component_mass - log_evidence)
  structure(
    list(
      weights = posterior_weights / sum(posterior_weights),
      means = posterior_mean,
      covariance = posterior_covariance,
      log_evidence = log_evidence,
      signal = signal,
      perception_sigma = perception_sigma
    ),
    class = "continuous_full_signal_posterior"
  )
}

.signal_call_values <- function(fn, x, z) {
  values <- fn(x, z)
  if (length(values) == 1L) values <- rep(values, length(x))
  if (length(values) != length(x)) {
    stop("call likelihood must return one value per location", call. = FALSE)
  }
  .signal_validate_probability(values, "call likelihood")
}

as_continuous_call_likelihood <- function(fn, label = "continuous call cue") {
  if (!is.function(fn)) stop("fn must be a function", call. = FALSE)
  attr(fn, "continuous_quadrature_supported") <- TRUE
  attr(fn, "continuous_call_label") <- as.character(label)[[1L]]
  fn
}

constant_continuous_call_likelihood <- function(probability) {
  probability <- .signal_validate_probability(
    probability, "constant call likelihood"
  )
  if (length(probability) != 1L) {
    stop("constant call likelihood must be scalar", call. = FALSE)
  }
  as_continuous_call_likelihood(
    function(x, z) rep(probability, length(x)),
    label = "constant call likelihood"
  )
}

.signal_require_supported_call_likelihood <- function(fn) {
  if (!is.function(fn) ||
      !identical(attr(fn, "continuous_quadrature_supported"), TRUE)) {
    .signal_integral_unavailable(paste(
      "a call-informed posterior requires a continuous call-likelihood",
      "evaluator explicitly marked as quadrature-supported"
    ))
  }
  invisible(TRUE)
}

.signal_matrix_square_root <- function(covariance) {
  covariance <- .signal_psd_matrix(covariance, "covariance")
  decomposition <- eigen(covariance, symmetric = TRUE)
  decomposition$vectors %*% diag(
    sqrt(pmax(decomposition$values, 0)), 2L
  )
}

# E[f(X)] for a possibly correlated bivariate Gaussian.
continuous_gaussian_expectation_gh <- function(
  mean,
  covariance,
  fn,
  order = 11L
) {
  mean <- .signal_validate_location(mean, "mean")
  covariance <- .signal_psd_matrix(covariance, "covariance")
  if (!is.function(fn)) stop("fn must be a function", call. = FALSE)
  if (max(abs(covariance)) <= 1e-14) {
    return(.signal_call_values(fn, mean[[1L]], mean[[2L]])[[1L]])
  }
  rule <- bivariate_gauss_hermite_rule(order, mean = c(0, 0), sd = c(1, 1))
  square_root <- .signal_matrix_square_root(covariance)
  location <- cbind(rule$x, rule$z) %*% t(square_root)
  location <- sweep(location, 2L, mean, "+")
  values <- .signal_call_values(fn, location[, 1L], location[, 2L])
  sum(rule$weight * values)
}

.signal_integrate_probability_interval <- function(
  lower_probability,
  upper_probability,
  inverse_cdf,
  fn,
  order
) {
  width <- upper_probability - lower_probability
  if (!is.finite(width) || width <= .Machine$double.eps) return(0)
  rule <- gauss_legendre_rule(
    order, lower = lower_probability, upper = upper_probability
  )
  sum(rule$weight * fn(inverse_cdf(rule$node)))
}

# E[f(X) 1{X is in the exact rounded ABS region}]. Integration occurs on
# normal-CDF scales, which remains stable for tight or diffuse Gaussians and
# does not introduce spatial cells.
continuous_gaussian_rounded_abs_expectation <- function(
  mean,
  covariance,
  zone_half_height_inches,
  fn = NULL,
  order = 11L,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5
) {
  mean <- .signal_validate_location(mean, "mean")
  covariance <- .signal_psd_matrix(covariance, "covariance")
  order <- .continuous_validate_order(order)
  .continuous_validate_scalar(
    zone_half_height_inches, "zone_half_height_inches", lower = 0
  )
  if (!is.null(fn) && !is.function(fn)) {
    stop("fn must be NULL or a function", call. = FALSE)
  }
  evaluate <- if (is.null(fn)) {
    function(x, z) rep(1, length(x))
  } else {
    function(x, z) .signal_call_values(fn, x, z)
  }

  variance_x <- covariance[1L, 1L]
  variance_z <- covariance[2L, 2L]
  tolerance <- 1e-14
  if (max(abs(covariance)) <= tolerance) {
    inside <- centered_abs_edge_distance_inches(
      mean[[1L]], mean[[2L]], zone_half_height_inches,
      ball_radius_inches, plate_half_width_inches
    ) <= 0
    return(if (inside) evaluate(mean[[1L]], mean[[2L]])[[1L]] else 0)
  }

  horizontal_outer <- plate_half_width_inches + ball_radius_inches
  vertical_outer <- zone_half_height_inches + ball_radius_inches

  if (variance_x <= tolerance) {
    limit <- rounded_abs_vertical_limit_inches(
      mean[[1L]], zone_half_height_inches, ball_radius_inches,
      plate_half_width_inches
    )
    if (is.na(limit)) return(0)
    sd_z <- sqrt(max(variance_z, 0))
    if (sd_z <= tolerance) {
      inside <- abs(mean[[2L]]) <= limit
      return(if (inside) evaluate(mean[[1L]], mean[[2L]])[[1L]] else 0)
    }
    lower <- stats::pnorm(-limit, mean[[2L]], sd_z)
    upper <- stats::pnorm(limit, mean[[2L]], sd_z)
    return(.signal_integrate_probability_interval(
      lower, upper,
      function(probability) stats::qnorm(probability, mean[[2L]], sd_z),
      function(z) evaluate(rep(mean[[1L]], length(z)), z), order
    ))
  }

  if (variance_z <= tolerance) {
    vertical_overhang <- max(abs(mean[[2L]]) - zone_half_height_inches, 0)
    if (vertical_overhang > ball_radius_inches) return(0)
    limit <- plate_half_width_inches + sqrt(max(
      ball_radius_inches^2 - vertical_overhang^2, 0
    ))
    sd_x <- sqrt(variance_x)
    lower <- stats::pnorm(-limit, mean[[1L]], sd_x)
    upper <- stats::pnorm(limit, mean[[1L]], sd_x)
    return(.signal_integrate_probability_interval(
      lower, upper,
      function(probability) stats::qnorm(probability, mean[[1L]], sd_x),
      function(x) evaluate(x, rep(mean[[2L]], length(x))), order
    ))
  }

  sd_x <- sqrt(variance_x)
  sd_z <- sqrt(variance_z)
  correlation <- covariance[1L, 2L] / (sd_x * sd_z)
  correlation <- pmax(-1 + 1e-10, pmin(1 - 1e-10, correlation))
  conditional_sd <- sd_z * sqrt(max(1 - correlation^2, 0))
  x_lower_probability <- stats::pnorm(
    -horizontal_outer, mean[[1L]], sd_x
  )
  x_upper_probability <- stats::pnorm(
    horizontal_outer, mean[[1L]], sd_x
  )
  if (x_upper_probability - x_lower_probability <= .Machine$double.eps) {
    return(0)
  }
  x_rule <- gauss_legendre_rule(
    order, x_lower_probability, x_upper_probability
  )
  x <- stats::qnorm(x_rule$node, mean[[1L]], sd_x)
  z_limit <- rounded_abs_vertical_limit_inches(
    x, zone_half_height_inches, ball_radius_inches,
    plate_half_width_inches
  )
  total <- 0
  for (index in seq_along(x)) {
    if (is.na(z_limit[[index]])) next
    conditional_mean <- mean[[2L]] +
      correlation * sd_z / sd_x * (x[[index]] - mean[[1L]])
    if (conditional_sd <= tolerance) {
      if (abs(conditional_mean) <= z_limit[[index]]) {
        total <- total + x_rule$weight[[index]] *
          evaluate(x[[index]], conditional_mean)[[1L]]
      }
      next
    }
    z_lower_probability <- stats::pnorm(
      -z_limit[[index]], conditional_mean, conditional_sd
    )
    z_upper_probability <- stats::pnorm(
      z_limit[[index]], conditional_mean, conditional_sd
    )
    inner <- .signal_integrate_probability_interval(
      z_lower_probability,
      z_upper_probability,
      function(probability) {
        stats::qnorm(probability, conditional_mean, conditional_sd)
      },
      function(z) evaluate(rep(x[[index]], length(z)), z),
      order
    )
    total <- total + x_rule$weight[[index]] * inner
  }
  pmax(0, pmin(1, total))
}

continuous_signal_wrong_call_probability <- function(
  prior,
  signal,
  perception_sigma,
  initial_call,
  zone_half_height_inches,
  omega = 0,
  call_likelihood_fn = NULL,
  order = 11L,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5,
  return_diagnostics = FALSE
) {
  if (length(initial_call) != 1L || is.na(initial_call) ||
      !initial_call %in% c("ball", "called_strike")) {
    stop("initial_call must be 'ball' or 'called_strike'", call. = FALSE)
  }
  omega <- as.numeric(omega)
  if (length(omega) != 1L || !is.finite(omega) || omega < 0 || omega > 1.5) {
    stop("omega must be one finite value in [0, 1.5]", call. = FALSE)
  }
  posterior <- condition_continuous_pitch_prior_on_signal(
    prior, signal, perception_sigma
  )
  components <- length(posterior$weights)

  if (omega == 0) {
    component_denominator <- rep(1, components)
    component_strike_mass <- vapply(seq_len(components), function(component) {
      continuous_gaussian_rounded_abs_expectation(
        posterior$means[component, ],
        posterior$covariance[component, , ],
        zone_half_height_inches = zone_half_height_inches,
        order = order,
        ball_radius_inches = ball_radius_inches,
        plate_half_width_inches = plate_half_width_inches
      )
    }, numeric(1))
    call_mode <- "omitted"
  } else {
    .signal_require_supported_call_likelihood(call_likelihood_fn)
    weighted_call_likelihood <- function(x, z) {
      .signal_call_values(call_likelihood_fn, x, z)^omega
    }
    attr(weighted_call_likelihood, "continuous_quadrature_supported") <- TRUE
    component_denominator <- vapply(
      seq_len(components),
      function(component) {
        continuous_gaussian_expectation_gh(
          posterior$means[component, ],
          posterior$covariance[component, , ],
          weighted_call_likelihood,
          order = order
        )
      },
      numeric(1)
    )
    component_strike_mass <- vapply(
      seq_len(components),
      function(component) {
        continuous_gaussian_rounded_abs_expectation(
          posterior$means[component, ],
          posterior$covariance[component, , ],
          zone_half_height_inches = zone_half_height_inches,
          fn = weighted_call_likelihood,
          order = order,
          ball_radius_inches = ball_radius_inches,
          plate_half_width_inches = plate_half_width_inches
        )
      },
      numeric(1)
    )
    call_mode <- "continuous_nested_quadrature"
  }

  denominator <- sum(posterior$weights * component_denominator)
  if (!is.finite(denominator) || denominator <= .Machine$double.xmin) {
    .signal_integral_unavailable(
      "the call-updated location posterior has no numerically resolvable mass"
    )
  }
  strike_probability <- sum(
    posterior$weights * component_strike_mass
  ) / denominator
  strike_probability <- pmax(0, pmin(1, strike_probability))
  wrong_probability <- if (initial_call == "ball") {
    strike_probability
  } else {
    1 - strike_probability
  }
  if (!isTRUE(return_diagnostics)) return(as.numeric(wrong_probability))
  list(
    wrong_call_probability = as.numeric(wrong_probability),
    strike_probability = as.numeric(strike_probability),
    normalizing_constant = denominator,
    component_normalizing_constant = component_denominator,
    component_strike_mass = component_strike_mass,
    posterior_component_weights = posterior$weights,
    quadrature_order = as.integer(order),
    call_update_mode = call_mode
  )
}

# A conditional take kernel corresponding to the fitted lapse-rate threshold
# model. The private signal is the perceived location: inside signals swing at
# the upper lapse rate and outside signals at the lower lapse rate.
make_continuous_batter_take_likelihood <- function(
  threshold_inches,
  context_shift = 0,
  lower_swing,
  upper_swing,
  zone_half_height_inches,
  sector_coefficients = NULL,
  sector_specification = NULL,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5
) {
  scalars <- c(threshold_inches, context_shift, lower_swing, upper_swing)
  if (anyNA(scalars) || any(!is.finite(scalars)) || lower_swing < 0 ||
      upper_swing > 1 || lower_swing > upper_swing) {
    stop("batter take-kernel parameters are invalid", call. = FALSE)
  }
  if (xor(is.null(sector_coefficients), is.null(sector_specification))) {
    stop("sector coefficients and specification must be supplied together",
      call. = FALSE
    )
  }
  if (!is.null(sector_coefficients)) {
    sector_coefficients <- as.numeric(sector_coefficients)
    expected <- length(sector_specification$column_names)
    if (length(sector_coefficients) != expected ||
        any(!is.finite(sector_coefficients))) {
      stop("sector coefficients do not match their specification",
        call. = FALSE
      )
    }
  }
  function(x, z) {
    edge <- centered_abs_edge_distance_inches(
      x, z, zone_half_height_inches,
      ball_radius_inches, plate_half_width_inches
    )
    sector_shift <- 0
    if (!is.null(sector_coefficients)) {
      sector <- score_continuous_sector_basis(
        x, z, edge, sector_specification
      )
      sector_shift <- as.numeric(sector %*% sector_coefficients)
    }
    perceived_inside <- edge <= threshold_inches + context_shift + sector_shift
    swing_probability <- lower_swing +
      (upper_swing - lower_swing) * as.numeric(perceived_inside)
    pmax(0, pmin(1, 1 - swing_probability))
  }
}

constant_continuous_take_likelihood <- function(probability = 1) {
  probability <- .signal_validate_probability(
    probability, "constant take likelihood"
  )
  if (length(probability) != 1L) {
    stop("constant take likelihood must be scalar", call. = FALSE)
  }
  function(x, z) rep(probability, length(x))
}

# Evaluate one pitch under one globally aligned posterior draw.
integrate_continuous_signal_draw <- function(
  true_location,
  perception_sigma,
  prior,
  initial_call,
  zone_half_height_inches,
  take_likelihood_fn,
  gain,
  inventory_loss,
  linear_predictor = 0,
  decision_slope = 1,
  sampling_offset = 0,
  omega = 0,
  call_likelihood_fn = NULL,
  order = 11L,
  global_draw_id = 1L,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5,
  return_nodes = FALSE
) {
  true_location <- .signal_validate_location(true_location, "true_location")
  perception_sigma <- as.numeric(perception_sigma)
  if (length(perception_sigma) == 1L) {
    perception_sigma_axes <- rep(perception_sigma, 2L)
  } else {
    perception_sigma_axes <- perception_sigma
  }
  if (length(perception_sigma_axes) != 2L ||
      anyNA(perception_sigma_axes) ||
      any(!is.finite(perception_sigma_axes)) ||
      any(perception_sigma_axes < 0)) {
    stop(
      "perception_sigma must be one isotropic scale or two nonnegative axes",
      call. = FALSE
    )
  }
  if (xor(
    perception_sigma_axes[[1L]] == 0,
    perception_sigma_axes[[2L]] == 0
  )) {
    .signal_integral_unavailable(
      "partially degenerate perception covariance is not supported"
    )
  }
  if (!is.function(take_likelihood_fn)) {
    stop("take_likelihood_fn must be a function", call. = FALSE)
  }
  order <- .continuous_validate_order(order)
  signal_rule <- bivariate_gauss_hermite_rule(
    order, mean = true_location, sd = perception_sigma_axes
  )
  take_probability <- take_likelihood_fn(signal_rule$x, signal_rule$z)
  if (length(take_probability) == 1L) {
    take_probability <- rep(take_probability, nrow(signal_rule))
  }
  take_probability <- .signal_validate_probability(
    take_probability, "take likelihood"
  )
  if (length(take_probability) != nrow(signal_rule)) {
    stop("take likelihood must return one value per signal node", call. = FALSE)
  }
  take_mass <- sum(signal_rule$weight * take_probability)
  if (!is.finite(take_mass) || take_mass <= .Machine$double.xmin) {
    .signal_integral_unavailable(
      "the fitted batter model assigns no resolvable probability to a take"
    )
  }
  conditioned_weight <- as.numeric(condition_signal_weights_on_take(
    matrix(signal_rule$weight, nrow = 1L),
    matrix(take_probability, nrow = 1L)
  ))

  q_signal <- numeric(nrow(signal_rule))
  call_normalizer <- numeric(nrow(signal_rule))
  for (node in seq_len(nrow(signal_rule))) {
    evaluated <- continuous_signal_wrong_call_probability(
      prior = prior,
      signal = c(signal_rule$x[[node]], signal_rule$z[[node]]),
      perception_sigma = perception_sigma_axes,
      initial_call = initial_call,
      zone_half_height_inches = zone_half_height_inches,
      omega = omega,
      call_likelihood_fn = call_likelihood_fn,
      order = order,
      ball_radius_inches = ball_radius_inches,
      plate_half_width_inches = plate_half_width_inches,
      return_diagnostics = TRUE
    )
    q_signal[[node]] <- evaluated$wrong_call_probability
    call_normalizer[[node]] <- evaluated$normalizing_constant
  }
  integrated <- integrate_human_action(
    q_signal = matrix(q_signal, nrow = 1L),
    signal_weights = matrix(conditioned_weight, nrow = 1L),
    gain = gain,
    inventory_loss = inventory_loss,
    linear_predictor = linear_predictor,
    decision_slope = decision_slope,
    sampling_offset = sampling_offset
  )
  action_at_signal <- stats::plogis(
    linear_predictor + sampling_offset + decision_slope *
      challenge_expected_utility(q_signal, gain, inventory_loss)
  )
  summary <- data.table::data.table(
    global_draw_id = as.integer(global_draw_id),
    omega = as.numeric(omega),
    quadrature_order = as.integer(order),
    perception_sigma_x_inches = perception_sigma_axes[[1L]],
    perception_sigma_z_inches = perception_sigma_axes[[2L]],
    private_signal_q = integrated$marginal_signal_q,
    predicted_challenge_probability = integrated$challenge_probability,
    choice_conditioned_q = integrated$choice_conditioned_q,
    take_probability_given_true_location = take_mass,
    effective_signal_nodes = 1 / sum(conditioned_weight^2),
    minimum_signal_q = min(q_signal),
    maximum_signal_q = max(q_signal),
    minimum_call_normalizer = min(call_normalizer),
    maximum_call_normalizer = max(call_normalizer),
    call_update_mode = if (omega == 0) {
      "omitted"
    } else {
      "continuous_nested_quadrature"
    },
    numerical_status = "available"
  )
  if (!isTRUE(return_nodes)) return(summary[])
  list(
    summary = summary[],
    nodes = data.table::data.table(
      signal_x_inches = signal_rule$x,
      signal_z_inches = signal_rule$z,
      unconditional_signal_weight = signal_rule$weight,
      take_probability = take_probability,
      conditional_signal_weight = conditioned_weight,
      private_signal_q = q_signal,
      challenge_probability_at_signal = action_at_signal,
      call_normalizer = call_normalizer
    )
  )
}

.signal_numeric_summary <- function(value, prefix) {
  value <- as.numeric(value)
  stats::setNames(
    list(
      mean(value),
      stats::median(value),
      stats::quantile(value, 0.025, names = FALSE),
      stats::quantile(value, 0.975, names = FALSE)
    ),
    paste0(prefix, c("mean", "median", "lower_95", "upper_95"))
  )
}

summarize_continuous_signal_draws <- function(draws) {
  x <- data.table::copy(data.table::as.data.table(draws))
  required <- c(
    "global_draw_id", "omega", "private_signal_q",
    "predicted_challenge_probability", "choice_conditioned_q"
  )
  stop_if_missing_columns(x, required, "continuous signal draws")
  if (!nrow(x)) stop("continuous signal draws are empty", call. = FALSE)
  x[, c(
    .signal_numeric_summary(private_signal_q, "private_signal_q_"),
    .signal_numeric_summary(
      predicted_challenge_probability, "predicted_challenge_probability_"
    ),
    .signal_numeric_summary(choice_conditioned_q, "choice_conditioned_q_")
  ), by = omega]
}

continuous_signal_order_diagnostics <- function(
  estimates,
  lower_order = 7L,
  higher_order = 11L,
  mean_tolerance = 0.001,
  p99_tolerance = 0.005
) {
  x <- data.table::copy(data.table::as.data.table(estimates))
  required <- c(
    "global_draw_id", "omega", "quadrature_order", "private_signal_q",
    "predicted_challenge_probability", "choice_conditioned_q"
  )
  stop_if_missing_columns(x, required, "continuous signal estimates")
  lower <- x[quadrature_order == lower_order]
  higher <- x[quadrature_order == higher_order]
  if (!nrow(lower) || !nrow(higher)) {
    stop("both requested quadrature orders must be present", call. = FALSE)
  }
  joined <- merge(
    lower,
    higher,
    by = c("global_draw_id", "omega"),
    suffixes = c("_lower", "_higher")
  )
  if (!nrow(joined)) stop("quadrature orders have no aligned draws", call. = FALSE)
  metrics <- c(
    "private_signal_q", "predicted_challenge_probability",
    "choice_conditioned_q"
  )
  by_metric <- data.table::rbindlist(lapply(metrics, function(metric) {
    error <- abs(
      joined[[paste0(metric, "_lower")]] -
        joined[[paste0(metric, "_higher")]]
    )
    data.table::data.table(
      metric = metric,
      mean_absolute_difference = mean(error),
      p99_absolute_difference = stats::quantile(
        error, 0.99, names = FALSE
      ),
      maximum_absolute_difference = max(error),
      aligned_draws = length(error)
    )
  }))
  by_metric[, `:=`(
    lower_order = as.integer(lower_order),
    higher_order = as.integer(higher_order),
    mean_tolerance = mean_tolerance,
    p99_tolerance = p99_tolerance,
    mean_pass = mean_absolute_difference < mean_tolerance,
    p99_pass = p99_absolute_difference < p99_tolerance
  )]
  list(
    by_draw = joined[],
    by_metric = by_metric[],
    all_pass = all(by_metric$mean_pass & by_metric$p99_pass)
  )
}

# Score aligned posterior-draw specifications. Each specification is a named
# list of arguments to integrate_continuous_signal_draw() and must carry a
# unique global_draw_id. Fixed call-trust variants and orders are expanded here
# while the component draw mapping remains unchanged.
score_continuous_signal_draws <- function(
  draw_specifications,
  omegas = continuous_call_trust_candidates(),
  orders = c(7L, 11L)
) {
  if (!is.list(draw_specifications) || !length(draw_specifications) ||
      !all(vapply(draw_specifications, is.list, logical(1)))) {
    stop("draw_specifications must be a nonempty list of argument lists",
      call. = FALSE
    )
  }
  global_ids <- vapply(draw_specifications, function(specification) {
    value <- as.integer(specification$global_draw_id)
    if (length(value) != 1L) return(NA_integer_)
    value
  }, integer(1))
  if (anyNA(global_ids) || anyDuplicated(global_ids)) {
    stop("draw specifications require unique, nonmissing global draw IDs",
      call. = FALSE
    )
  }
  omegas <- as.numeric(omegas)
  if (!length(omegas) || anyNA(omegas) || any(!is.finite(omegas)) ||
      any(omegas < 0 | omegas > 1.5)) {
    stop("omegas must lie in [0, 1.5]", call. = FALSE)
  }
  orders <- vapply(orders, .continuous_validate_order, integer(1))
  estimates <- data.table::rbindlist(lapply(draw_specifications, function(specification) {
    data.table::rbindlist(lapply(omegas, function(omega) {
      data.table::rbindlist(lapply(orders, function(order) {
        arguments <- specification
        arguments$omega <- omega
        arguments$order <- order
        arguments$return_nodes <- FALSE
        do.call(integrate_continuous_signal_draw, arguments)
      }))
    }))
  }))
  highest <- max(orders)
  result <- list(
    posterior_draws = estimates[quadrature_order == highest],
    intervals = summarize_continuous_signal_draws(
      estimates[quadrature_order == highest]
    ),
    all_orders = estimates[]
  )
  if (length(unique(orders)) >= 2L) {
    sorted <- sort(unique(orders))
    result$order_diagnostics <- continuous_signal_order_diagnostics(
      estimates,
      lower_order = sorted[[length(sorted) - 1L]],
      higher_order = sorted[[length(sorted)]]
    )
  }
  class(result) <- "continuous_signal_draw_scores"
  result
}

# Extract one contextual Gaussian-mixture draw without discarding rho.
continuous_pitch_prior_draw_parameters <- function(
  fit,
  pitch,
  draw_index,
  posterior_draws = NULL
) {
  if (!inherits(fit, "continuous_pitch_prior_fit")) {
    stop("fit must be a continuous_pitch_prior_fit", call. = FALSE)
  }
  x <- normalize_continuous_pitch_prior_input(pitch)
  if (nrow(x) != 1L) stop("pitch must normalize to exactly one row", call. = FALSE)
  context <- score_continuous_design(x, fit$context_specification)
  if (is.null(posterior_draws)) {
    posterior_draws <- draws_continuous_pitch_prior(fit)
  }
  draw_index <- as.integer(draw_index)
  if (length(draw_index) != 1L || is.na(draw_index) || draw_index < 1L ||
      draw_index > length(posterior_draws$draw_id)) {
    stop("invalid pitch-prior draw index", call. = FALSE)
  }
  eta <- numeric(fit$components)
  if (fit$components > 1L) {
    eta[seq_len(fit$components - 1L)] <-
      posterior_draws$weight_intercept[draw_index, ]
    if (ncol(context)) {
      eta[seq_len(fit$components - 1L)] <-
        eta[seq_len(fit$components - 1L)] + as.numeric(
          context %*% posterior_draws$weight_context[draw_index, , ]
        )
    }
    pitcher_index <- match(
      as.character(x$pitcher_id), as.character(fit$pitcher_table$pitcher_id)
    )
    if (!is.na(pitcher_index)) {
      eta[seq_len(fit$components - 1L)] <-
        eta[seq_len(fit$components - 1L)] +
        posterior_draws$pitcher_effect[draw_index, pitcher_index, ]
    }
  }
  component_mean <- matrix(
    posterior_draws$component_mean[draw_index, , ],
    nrow = fit$components,
    ncol = 2L
  )
  component_scale <- matrix(
    posterior_draws$component_scale[draw_index, , ],
    nrow = fit$components,
    ncol = 2L
  )
  list(
    weights = continuous_softmax(eta),
    means = component_mean,
    scale = component_scale,
    correlation = posterior_draws$component_rho[draw_index, ],
    prior_draw_id = posterior_draws$draw_id[[draw_index]]
  )
}

# Extract the fitted sigma and the signal-specific take kernel for one batter
# draw. Challenge labels and outcomes are not inputs to this constructor.
continuous_batter_draw_signal_parameters <- function(
  fit,
  pitch,
  draw_index,
  posterior_draws = NULL
) {
  if (!inherits(fit, "continuous_batter_perception_fit")) {
    stop("fit must be a continuous_batter_perception_fit", call. = FALSE)
  }
  features <- continuous_batter_scoring_features(pitch, fit)
  x <- features$rows
  if (nrow(x) != 1L) stop("pitch must normalize to exactly one row", call. = FALSE)
  if (is.null(posterior_draws)) {
    posterior_draws <- draws_continuous_batter_perception(fit)
  }
  draw_index <- as.integer(draw_index)
  if (length(draw_index) != 1L || is.na(draw_index) || draw_index < 1L ||
      draw_index > length(posterior_draws$draw_id)) {
    stop("invalid batter-perception draw index", call. = FALSE)
  }
  player_index <- match(
    as.character(x$batter_id), as.character(fit$player_table$batter_id)
  )
  if (is.na(player_index)) {
    sigma <- exp(
      posterior_draws$mu_log_sigma[[draw_index]] +
        0.5 * posterior_draws$tau_log_sigma[[draw_index]]^2
    )
    threshold <- posterior_draws$mu_threshold[[draw_index]]
    fallback <- TRUE
  } else {
    sigma <- posterior_draws$sigma_player[draw_index, player_index]
    threshold <- posterior_draws$threshold_player[draw_index, player_index]
    fallback <- FALSE
  }
  sigma <- as.numeric(sigma)
  if (length(sigma) != 1L || !is.finite(sigma) || sigma <= 0) {
    stop("invalid batter-perception sigma draw", call. = FALSE)
  }
  anisotropy_ratio <- if (!is.null(posterior_draws$anisotropy_ratio)) {
    as.numeric(posterior_draws$anisotropy_ratio[[draw_index]])
  } else if (!is.null(posterior_draws$log_anisotropy)) {
    exp(as.numeric(posterior_draws$log_anisotropy[[draw_index]]))
  } else {
    1
  }
  if (length(anisotropy_ratio) != 1L || !is.finite(anisotropy_ratio) ||
      anisotropy_ratio <= 0) {
    stop("invalid batter-perception anisotropy draw", call. = FALSE)
  }
  declared_anisotropy_mode <- if (is.null(fit$anisotropy)) {
    "isotropic"
  } else {
    as.character(fit$anisotropy)[[1L]]
  }
  anisotropy_mode <- if (abs(anisotropy_ratio - 1) > 1e-12) {
    "shared"
  } else {
    declared_anisotropy_mode
  }
  sigma_axes <- c(
    horizontal = as.numeric(sigma) * anisotropy_ratio,
    vertical = as.numeric(sigma) / anisotropy_ratio
  )
  perception_sigma <- if (
    identical(anisotropy_mode, "isotropic") &&
      abs(anisotropy_ratio - 1) <= 1e-12
  ) {
    as.numeric(sigma)
  } else {
    sigma_axes
  }
  context_shift <- if (ncol(features$context)) {
    as.numeric(features$context %*% posterior_draws$beta_context[draw_index, ])
  } else {
    0
  }
  strike_group <- as.integer(x$strikes) + 1L
  zone_half_height <- 6 * (x$sz_top - x$sz_bot)
  take_likelihood <- make_continuous_batter_take_likelihood(
    threshold_inches = threshold,
    context_shift = context_shift,
    lower_swing = posterior_draws$lower_swing[draw_index, strike_group],
    upper_swing = posterior_draws$upper_swing[draw_index, strike_group],
    zone_half_height_inches = zone_half_height,
    sector_coefficients = posterior_draws$beta_sector[
      draw_index, strike_group,
    ],
    sector_specification = fit$sector_specification
  )
  list(
    perception_sigma = perception_sigma,
    perception_sigma_x_inches = unname(sigma_axes[["horizontal"]]),
    perception_sigma_z_inches = unname(sigma_axes[["vertical"]]),
    anisotropy_ratio = anisotropy_ratio,
    anisotropy_mode = anisotropy_mode,
    threshold_inches = as.numeric(threshold),
    context_shift = as.numeric(context_shift),
    take_likelihood_fn = take_likelihood,
    batter_fallback = fallback,
    signal_draw_id = posterior_draws$draw_id[[draw_index]]
  )
}

# Construct h(c | x) for one call-model draw and one pitch context. The returned
# function is explicitly marked as safe for continuous quadrature.
continuous_call_cue_draw_likelihood <- function(
  fit,
  pitch,
  draw_index,
  initial_call = NULL,
  posterior_draws = NULL
) {
  if (!inherits(fit, "continuous_call_cue_fit")) {
    stop("fit must be a continuous_call_cue_fit", call. = FALSE)
  }
  features <- continuous_call_cue_scoring_features(pitch, fit)
  x <- features$rows
  if (nrow(x) != 1L) stop("pitch must normalize to exactly one row", call. = FALSE)
  if (is.null(initial_call)) initial_call <- x$initial_call[[1L]]
  if (!initial_call %in% c("ball", "called_strike")) {
    stop("initial_call must be 'ball' or 'called_strike'", call. = FALSE)
  }
  if (is.null(posterior_draws)) posterior_draws <- draws_continuous_call_cue(fit)
  draw_index <- as.integer(draw_index)
  if (length(draw_index) != 1L || is.na(draw_index) || draw_index < 1L ||
      draw_index > length(posterior_draws$draw_id)) {
    stop("invalid initial-call draw index", call. = FALSE)
  }
  context_effect <- if (ncol(features$context)) {
    as.numeric(features$context %*% posterior_draws$beta_context[draw_index, ])
  } else {
    0
  }
  umpire_index <- match(
    as.character(x$umpire_id), as.character(fit$umpire_table$umpire_id)
  )
  catcher_index <- match(
    as.character(x$catcher_id), as.character(fit$catcher_table$catcher_id)
  )
  umpire_effect <- if (is.na(umpire_index)) 0 else
    posterior_draws$umpire_effect[draw_index, umpire_index]
  catcher_effect <- if (is.na(catcher_index)) 0 else
    posterior_draws$catcher_effect[draw_index, catcher_index]
  zone_half_height <- 6 * (x$sz_top - x$sz_bot)
  likelihood <- function(horizontal_inches, vertical_inches) {
    edge <- centered_abs_edge_distance_inches(
      horizontal_inches, vertical_inches, zone_half_height
    )
    edge_basis <- score_continuous_edge_basis(edge, fit$edge_specification)
    residual_basis <- score_continuous_call_residual_basis(
      horizontal_inches, vertical_inches, edge_basis,
      fit$residual_specification
    )
    linear_predictor <- posterior_draws$intercept[[draw_index]] +
      as.numeric(edge_basis %*% posterior_draws$beta_edge[draw_index, ]) +
      as.numeric(
        residual_basis %*% posterior_draws$beta_residual[draw_index, ]
      ) + context_effect + umpire_effect + catcher_effect
    called_strike_probability <- stats::plogis(linear_predictor)
    if (initial_call == "called_strike") {
      called_strike_probability
    } else {
      1 - called_strike_probability
    }
  }
  likelihood <- as_continuous_call_likelihood(
    likelihood, label = "fitted continuous initial-call cue"
  )
  attr(likelihood, "call_draw_id") <- posterior_draws$draw_id[[draw_index]]
  likelihood
}
