# Continuous geometry and quadrature utilities for the human-perception model.
#
# All public functions in this file use physical inches. Horizontal coordinates
# are measured from the center of home plate and vertical coordinates from the
# midpoint of the batter-specific strike zone. The ABS strike region is the
# rectangle for a ball center, dilated by the configured ball radius; its four
# corners are therefore circular rather than square.

.continuous_validate_scalar <- function(x, name, lower = -Inf, strict = FALSE) {
  if (length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(name, " must be one finite scalar", call. = FALSE)
  }
  bad <- if (strict) x <= lower else x < lower
  if (bad) {
    relation <- if (strict) "greater than" else "at least"
    stop(name, " must be ", relation, " ", lower, call. = FALSE)
  }
  invisible(x)
}

.continuous_validate_order <- function(order) {
  .continuous_validate_scalar(order, "order", lower = 1)
  if (order != as.integer(order)) {
    stop("order must be a positive integer", call. = FALSE)
  }
  as.integer(order)
}

.continuous_probability <- function(x) {
  pmin(1, pmax(0, x))
}

.continuous_recycle <- function(..., names) {
  values <- list(...)
  lengths <- vapply(values, length, integer(1))
  size <- max(lengths)
  invalid <- lengths != 1L & lengths != size
  if (any(invalid)) {
    stop(
      paste(names[invalid], collapse = ", "),
      " must be scalar or have the common output length",
      call. = FALSE
    )
  }
  lapply(values, rep_len, length.out = size)
}

# Convert Statcast feet to the centered, physical-inch coordinate system.
center_abs_coordinates_inches <- function(plate_x, plate_z, sz_top, sz_bot) {
  values <- .continuous_recycle(
    plate_x, plate_z, sz_top, sz_bot,
    names = c("plate_x", "plate_z", "sz_top", "sz_bot")
  )
  plate_x <- values[[1L]]
  plate_z <- values[[2L]]
  sz_top <- values[[3L]]
  sz_bot <- values[[4L]]

  data.frame(
    x_inches = 12 * plate_x,
    z_inches = 12 * (plate_z - (sz_top + sz_bot) / 2),
    zone_half_height_inches = 6 * (sz_top - sz_bot),
    stringsAsFactors = FALSE
  )
}

# Signed distance from a centered point to the rounded ABS boundary. Negative
# values are strikes, zero is on the strike boundary, and positive values are
# balls. This is the inch-coordinate equivalent of abs_edge_distance_inches().
centered_abs_edge_distance_inches <- function(
  x_inches,
  z_inches,
  zone_half_height_inches,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5
) {
  values <- .continuous_recycle(
    x_inches, z_inches, zone_half_height_inches,
    ball_radius_inches, plate_half_width_inches,
    names = c(
      "x_inches", "z_inches", "zone_half_height_inches",
      "ball_radius_inches", "plate_half_width_inches"
    )
  )
  x_inches <- values[[1L]]
  z_inches <- values[[2L]]
  zone_half_height_inches <- values[[3L]]
  ball_radius_inches <- values[[4L]]
  plate_half_width_inches <- values[[5L]]

  invalid_geometry <- !is.na(zone_half_height_inches) &
    (zone_half_height_inches < 0 | ball_radius_inches < 0 |
      plate_half_width_inches <= 0)
  if (any(invalid_geometry)) {
    stop("zone dimensions and ball radius must define a non-negative region",
      call. = FALSE
    )
  }

  qx <- abs(x_inches) - plate_half_width_inches
  qz <- abs(z_inches) - zone_half_height_inches
  center_to_rectangle <- sqrt(pmax(qx, 0)^2 + pmax(qz, 0)^2) +
    pmin(pmax(qx, qz), 0)
  center_to_rectangle - ball_radius_inches
}

classify_centered_abs_call <- function(
  x_inches,
  z_inches,
  zone_half_height_inches,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5
) {
  distance <- centered_abs_edge_distance_inches(
    x_inches = x_inches,
    z_inches = z_inches,
    zone_half_height_inches = zone_half_height_inches,
    ball_radius_inches = ball_radius_inches,
    plate_half_width_inches = plate_half_width_inches
  )
  ifelse(
    is.na(distance),
    NA_character_,
    ifelse(distance <= 0, "called_strike", "ball")
  )
}

# Vertical half-extent of the exact rounded strike region at horizontal x.
rounded_abs_vertical_limit_inches <- function(
  x_inches,
  zone_half_height_inches,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5
) {
  .continuous_validate_scalar(
    zone_half_height_inches, "zone_half_height_inches", lower = 0
  )
  .continuous_validate_scalar(
    ball_radius_inches, "ball_radius_inches", lower = 0
  )
  .continuous_validate_scalar(
    plate_half_width_inches, "plate_half_width_inches", lower = 0,
    strict = TRUE
  )
  overhang <- pmax(abs(x_inches) - plate_half_width_inches, 0)
  inside <- overhang <= ball_radius_inches
  limit <- rep(NA_real_, length(x_inches))
  limit[inside] <- zone_half_height_inches + sqrt(pmax(
    ball_radius_inches^2 - overhang[inside]^2,
    0
  ))
  limit
}

# Gauss-Legendre rule for an ordinary integral on [lower, upper].
gauss_legendre_rule <- function(order, lower = -1, upper = 1) {
  order <- .continuous_validate_order(order)
  .continuous_validate_scalar(lower, "lower")
  .continuous_validate_scalar(upper, "upper")
  if (lower > upper) stop("lower must not exceed upper", call. = FALSE)

  if (order == 1L) {
    nodes <- 0
    weights <- 2
  } else {
    index <- seq_len(order - 1L)
    off_diagonal <- index / sqrt(4 * index^2 - 1)
    jacobi <- matrix(0, nrow = order, ncol = order)
    jacobi[cbind(index, index + 1L)] <- off_diagonal
    jacobi[cbind(index + 1L, index)] <- off_diagonal
    decomposition <- eigen(jacobi, symmetric = TRUE)
    ordering <- order(decomposition$values)
    nodes <- decomposition$values[ordering]
    weights <- 2 * decomposition$vectors[1L, ordering]^2
  }

  midpoint <- (lower + upper) / 2
  half_width <- (upper - lower) / 2
  data.frame(
    node = midpoint + half_width * nodes,
    weight = half_width * weights,
    stringsAsFactors = FALSE
  )
}

integrate_gauss_legendre <- function(fn, lower, upper, order = 11L) {
  if (!is.function(fn)) stop("fn must be a function", call. = FALSE)
  rule <- gauss_legendre_rule(order, lower = lower, upper = upper)
  values <- fn(rule$node)
  if (length(values) == 1L) values <- rep(values, nrow(rule))
  if (length(values) != nrow(rule) || anyNA(values) ||
      any(!is.finite(values))) {
    stop("fn must return one finite value per quadrature node", call. = FALSE)
  }
  sum(rule$weight * values)
}

# Gauss-Hermite rule parameterized as an expectation under N(mean, sd^2).
# Thus the returned weights sum to one rather than sqrt(pi).
gauss_hermite_normal_rule <- function(order, mean = 0, sd = 1) {
  order <- .continuous_validate_order(order)
  .continuous_validate_scalar(mean, "mean")
  .continuous_validate_scalar(sd, "sd", lower = 0)

  if (order == 1L) {
    nodes <- 0
    weights <- 1
  } else {
    index <- seq_len(order - 1L)
    off_diagonal <- sqrt(index)
    jacobi <- matrix(0, nrow = order, ncol = order)
    jacobi[cbind(index, index + 1L)] <- off_diagonal
    jacobi[cbind(index + 1L, index)] <- off_diagonal
    decomposition <- eigen(jacobi, symmetric = TRUE)
    ordering <- order(decomposition$values)
    nodes <- decomposition$values[ordering]
    weights <- decomposition$vectors[1L, ordering]^2
  }

  data.frame(
    node = mean + sd * nodes,
    weight = weights / sum(weights),
    stringsAsFactors = FALSE
  )
}

bivariate_gauss_hermite_rule <- function(
  order,
  mean = c(0, 0),
  sd = c(1, 1)
) {
  order <- .continuous_validate_order(order)
  if (length(mean) != 2L || anyNA(mean) || any(!is.finite(mean))) {
    stop("mean must contain two finite coordinates", call. = FALSE)
  }
  if (length(sd) == 1L) sd <- rep(sd, 2L)
  if (length(sd) != 2L || anyNA(sd) || any(!is.finite(sd)) || any(sd < 0)) {
    stop("sd must contain one or two finite non-negative values", call. = FALSE)
  }
  x_rule <- gauss_hermite_normal_rule(order, mean[[1L]], sd[[1L]])
  z_rule <- gauss_hermite_normal_rule(order, mean[[2L]], sd[[2L]])
  indices <- expand.grid(
    x_index = seq_len(order),
    z_index = seq_len(order),
    KEEP.OUT.ATTRS = FALSE
  )
  data.frame(
    x = x_rule$node[indices$x_index],
    z = z_rule$node[indices$z_index],
    weight = x_rule$weight[indices$x_index] *
      z_rule$weight[indices$z_index],
    stringsAsFactors = FALSE
  )
}

integrate_bivariate_normal_gh <- function(
  fn,
  mean = c(0, 0),
  sd = c(1, 1),
  order = 11L
) {
  if (!is.function(fn)) stop("fn must be a function", call. = FALSE)
  rule <- bivariate_gauss_hermite_rule(order, mean = mean, sd = sd)
  values <- fn(rule$x, rule$z)
  if (length(values) == 1L) values <- rep(values, nrow(rule))
  if (length(values) != nrow(rule) || anyNA(values) ||
      any(!is.finite(values))) {
    stop("fn must return one finite value per quadrature node", call. = FALSE)
  }
  sum(rule$weight * values)
}

# Numerically stable P(lower <= X <= upper) for a univariate normal.
.continuous_normal_interval_probability <- function(lower, upper, mean, sd) {
  if (sd == 0) return(as.numeric(mean >= lower && mean <= upper))
  if (upper <= mean) {
    probability <- stats::pnorm(upper, mean, sd) -
      stats::pnorm(lower, mean, sd)
  } else if (lower >= mean) {
    probability <- stats::pnorm(lower, mean, sd, lower.tail = FALSE) -
      stats::pnorm(upper, mean, sd, lower.tail = FALSE)
  } else {
    probability <- stats::pnorm(upper, mean, sd) -
      stats::pnorm(lower, mean, sd)
  }
  .continuous_probability(probability)
}

.continuous_rounded_cap_mass <- function(
  lower_x,
  upper_x,
  mean,
  sd,
  zone_half_height_inches,
  ball_radius_inches,
  plate_half_width_inches,
  order
) {
  if (lower_x >= mean[[1L]]) {
    probability_lower <- stats::pnorm(
      upper_x, mean[[1L]], sd[[1L]], lower.tail = FALSE
    )
    probability_upper <- stats::pnorm(
      lower_x, mean[[1L]], sd[[1L]], lower.tail = FALSE
    )
    inverse_probability <- function(probability) {
      stats::qnorm(
        probability, mean[[1L]], sd[[1L]], lower.tail = FALSE
      )
    }
  } else {
    probability_lower <- stats::pnorm(lower_x, mean[[1L]], sd[[1L]])
    probability_upper <- stats::pnorm(upper_x, mean[[1L]], sd[[1L]])
    inverse_probability <- function(probability) {
      stats::qnorm(probability, mean[[1L]], sd[[1L]])
    }
  }
  probability_width <- probability_upper - probability_lower
  if (!is.finite(probability_width) ||
      probability_width <= .Machine$double.xmin) {
    return(0)
  }

  # Integrating on the CDF scale cancels the narrow or diffuse horizontal
  # normal density and keeps the rule stable as sd_x approaches zero.
  rule <- gauss_legendre_rule(order, probability_lower, probability_upper)
  x <- inverse_probability(rule$node)
  z_limit <- rounded_abs_vertical_limit_inches(
    x,
    zone_half_height_inches = zone_half_height_inches,
    ball_radius_inches = ball_radius_inches,
    plate_half_width_inches = plate_half_width_inches
  )
  z_probability <- vapply(
    z_limit,
    function(limit) {
      .continuous_normal_interval_probability(
        -limit, limit, mean[[2L]], sd[[2L]]
      )
    },
    numeric(1)
  )
  sum(rule$weight * z_probability)
}

# Probability that a diagonal bivariate normal falls in the exact rounded ABS
# strike region. sd may be isotropic (length one) or diagonal (length two).
rounded_abs_normal_mass <- function(
  mean = c(0, 0),
  sd = c(1, 1),
  zone_half_height_inches,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5,
  order = 11L
) {
  order <- .continuous_validate_order(order)
  if (length(mean) != 2L || anyNA(mean) || any(!is.finite(mean))) {
    stop("mean must contain two finite coordinates", call. = FALSE)
  }
  if (length(sd) == 1L) sd <- rep(sd, 2L)
  if (length(sd) != 2L || anyNA(sd) || any(!is.finite(sd)) || any(sd < 0)) {
    stop("sd must contain one or two finite non-negative values", call. = FALSE)
  }
  .continuous_validate_scalar(
    zone_half_height_inches, "zone_half_height_inches", lower = 0
  )
  .continuous_validate_scalar(
    ball_radius_inches, "ball_radius_inches", lower = 0
  )
  .continuous_validate_scalar(
    plate_half_width_inches, "plate_half_width_inches", lower = 0,
    strict = TRUE
  )

  if (all(sd == 0)) {
    return(as.numeric(centered_abs_edge_distance_inches(
      mean[[1L]], mean[[2L]], zone_half_height_inches,
      ball_radius_inches, plate_half_width_inches
    ) <= 0))
  }

  horizontal_outer <- plate_half_width_inches + ball_radius_inches
  vertical_outer <- zone_half_height_inches + ball_radius_inches

  if (sd[[1L]] == 0) {
    z_limit <- rounded_abs_vertical_limit_inches(
      mean[[1L]], zone_half_height_inches, ball_radius_inches,
      plate_half_width_inches
    )
    if (is.na(z_limit)) return(0)
    return(.continuous_normal_interval_probability(
      -z_limit, z_limit, mean[[2L]], sd[[2L]]
    ))
  }

  if (sd[[2L]] == 0) {
    vertical_overhang <- pmax(abs(mean[[2L]]) - zone_half_height_inches, 0)
    if (vertical_overhang > ball_radius_inches) return(0)
    x_limit <- plate_half_width_inches + sqrt(pmax(
      ball_radius_inches^2 - vertical_overhang^2,
      0
    ))
    return(.continuous_normal_interval_probability(
      -x_limit, x_limit, mean[[1L]], sd[[1L]]
    ))
  }

  # The central rectangle has an analytic product mass. Only the two rounded
  # side caps require quadrature, split at their geometry breakpoints.
  central_mass <- .continuous_normal_interval_probability(
    -plate_half_width_inches,
    plate_half_width_inches,
    mean[[1L]],
    sd[[1L]]
  ) * .continuous_normal_interval_probability(
    -vertical_outer,
    vertical_outer,
    mean[[2L]],
    sd[[2L]]
  )

  left_cap <- .continuous_rounded_cap_mass(
    -horizontal_outer, -plate_half_width_inches,
    mean, sd, zone_half_height_inches, ball_radius_inches,
    plate_half_width_inches, order
  )
  right_cap <- .continuous_rounded_cap_mass(
    plate_half_width_inches, horizontal_outer,
    mean, sd, zone_half_height_inches, ball_radius_inches,
    plate_half_width_inches, order
  )
  .continuous_probability(central_mass + left_cap + right_cap)
}

.continuous_as_k_by_two <- function(
  value,
  components,
  name,
  constraint = c("none", "nonnegative", "positive")
) {
  constraint <- match.arg(constraint)
  if (is.matrix(value) || is.data.frame(value)) {
    output <- as.matrix(value)
    if (!identical(dim(output), c(components, 2L))) {
      stop(name, " must have one row per component and two columns",
        call. = FALSE
      )
    }
  } else {
    value <- as.numeric(value)
    if (length(value) == 1L) {
      output <- matrix(value, nrow = components, ncol = 2L)
    } else if (length(value) == 2L) {
      output <- matrix(rep(value, components), nrow = components, byrow = TRUE)
    } else if (length(value) == 2L * components) {
      output <- matrix(value, nrow = components, byrow = TRUE)
    } else {
      stop(name, " must provide one or two values per component", call. = FALSE)
    }
  }
  if (anyNA(output) || any(!is.finite(output))) {
    stop(name, " must contain only finite values", call. = FALSE)
  }
  if (constraint != "none") {
    invalid <- if (constraint == "nonnegative") output < 0 else output <= 0
    if (any(invalid)) {
      qualifier <- if (constraint == "nonnegative") "non-negative" else "positive"
      stop(name, " must contain only ", qualifier, " values", call. = FALSE)
    }
  }
  output
}

.continuous_log_sum_exp <- function(x) {
  maximum <- max(x)
  if (!is.finite(maximum)) return(maximum)
  maximum + log(sum(exp(x - maximum)))
}

# Closed-form posterior for a diagonal Gaussian-mixture location prior and a
# private signal S | X ~ N_2(X, diag(perception_sd^2)).
gaussian_mixture_signal_posterior <- function(
  weights,
  means,
  component_sd,
  signal,
  perception_sd
) {
  weights <- as.numeric(weights)
  if (!length(weights) || anyNA(weights) || any(!is.finite(weights)) ||
      any(weights < 0) || sum(weights) <= 0) {
    stop("weights must be finite, non-negative, and have positive sum",
      call. = FALSE
    )
  }
  weights <- weights / sum(weights)
  components <- length(weights)
  means <- .continuous_as_k_by_two(
    means, components, "means", constraint = "none"
  )
  component_sd <- .continuous_as_k_by_two(
    component_sd, components, "component_sd", constraint = "positive"
  )
  if (length(signal) != 2L || anyNA(signal) || any(!is.finite(signal))) {
    stop("signal must contain two finite coordinates", call. = FALSE)
  }
  if (length(perception_sd) == 1L) perception_sd <- rep(perception_sd, 2L)
  if (length(perception_sd) != 2L || anyNA(perception_sd) ||
      any(!is.finite(perception_sd)) || any(perception_sd < 0)) {
    stop("perception_sd must contain one or two finite non-negative values",
      call. = FALSE
    )
  }

  prior_variance <- component_sd^2
  signal_variance <- perception_sd^2
  marginal_variance <- sweep(prior_variance, 2L, signal_variance, "+")
  marginal_sd <- sqrt(marginal_variance)
  centered_signal <- sweep(means, 2L, signal, "-")
  log_density <- rowSums(
    -0.5 * log(2 * pi) - log(marginal_sd) -
      0.5 * (centered_signal / marginal_sd)^2
  )
  log_terms <- log(weights) + log_density
  log_evidence <- .continuous_log_sum_exp(log_terms)
  posterior_weights <- exp(log_terms - log_evidence)

  gain <- sweep(prior_variance, 2L, signal_variance, "+")
  gain <- prior_variance / gain
  signal_matrix <- matrix(signal, nrow = components, ncol = 2L, byrow = TRUE)
  posterior_means <- means + gain * (signal_matrix - means)
  posterior_variance <- prior_variance * (1 - gain)

  structure(
    list(
      weights = posterior_weights / sum(posterior_weights),
      means = posterior_means,
      sd = sqrt(pmax(posterior_variance, 0)),
      log_evidence = log_evidence,
      signal = as.numeric(signal),
      perception_sd = perception_sd
    ),
    class = "continuous_signal_posterior"
  )
}

gaussian_mixture_signal_log_density <- function(
  weights,
  means,
  component_sd,
  signal,
  perception_sd
) {
  gaussian_mixture_signal_posterior(
    weights, means, component_sd, signal, perception_sd
  )$log_evidence
}

gaussian_mixture_abs_probability <- function(
  posterior,
  zone_half_height_inches,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5,
  order = 11L
) {
  required <- c("weights", "means", "sd")
  if (!is.list(posterior) || !all(required %in% names(posterior))) {
    stop("posterior must contain weights, means, and sd", call. = FALSE)
  }
  components <- length(posterior$weights)
  means <- as.matrix(posterior$means)
  component_sd <- as.matrix(posterior$sd)
  if (!identical(dim(means), c(components, 2L)) ||
      !identical(dim(component_sd), c(components, 2L))) {
    stop("posterior means and sd must have one row per component",
      call. = FALSE
    )
  }
  masses <- vapply(seq_len(components), function(component) {
    rounded_abs_normal_mass(
      mean = means[component, ],
      sd = component_sd[component, ],
      zone_half_height_inches = zone_half_height_inches,
      ball_radius_inches = ball_radius_inches,
      plate_half_width_inches = plate_half_width_inches,
      order = order
    )
  }, numeric(1))
  .continuous_probability(sum(posterior$weights * masses))
}

gaussian_mixture_wrong_call_probability <- function(
  posterior,
  initial_call,
  zone_half_height_inches,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5,
  order = 11L
) {
  if (length(initial_call) != 1L || is.na(initial_call) ||
      !initial_call %in% c("ball", "called_strike")) {
    stop("initial_call must be either 'ball' or 'called_strike'",
      call. = FALSE
    )
  }
  strike_probability <- gaussian_mixture_abs_probability(
    posterior = posterior,
    zone_half_height_inches = zone_half_height_inches,
    ball_radius_inches = ball_radius_inches,
    plate_half_width_inches = plate_half_width_inches,
    order = order
  )
  if (initial_call == "ball") {
    strike_probability
  } else {
    1 - strike_probability
  }
}

quadrature_order_comparison <- function(
  evaluator,
  orders = c(7L, 11L),
  tolerance = 0.001
) {
  if (!is.function(evaluator)) stop("evaluator must be a function", call. = FALSE)
  if (length(orders) != 2L) stop("orders must contain exactly two orders",
    call. = FALSE
  )
  orders <- vapply(orders, .continuous_validate_order, integer(1))
  if (orders[[1L]] >= orders[[2L]]) {
    stop("orders must be strictly increasing", call. = FALSE)
  }
  .continuous_validate_scalar(tolerance, "tolerance", lower = 0)
  estimates <- vapply(orders, evaluator, numeric(1))
  if (anyNA(estimates) || any(!is.finite(estimates))) {
    stop("evaluator must return one finite estimate per order", call. = FALSE)
  }
  difference <- abs(diff(estimates))
  data.frame(
    lower_order = orders[[1L]],
    higher_order = orders[[2L]],
    lower_estimate = estimates[[1L]],
    higher_estimate = estimates[[2L]],
    absolute_difference = difference,
    tolerance = tolerance,
    converged = difference <= tolerance,
    stringsAsFactors = FALSE
  )
}

compare_rounded_abs_mass_orders <- function(
  mean = c(0, 0),
  sd = c(1, 1),
  zone_half_height_inches,
  ball_radius_inches = 1.45,
  plate_half_width_inches = 8.5,
  orders = c(7L, 11L),
  tolerance = 0.001
) {
  quadrature_order_comparison(
    evaluator = function(order) {
      rounded_abs_normal_mass(
        mean = mean,
        sd = sd,
        zone_half_height_inches = zone_half_height_inches,
        ball_radius_inches = ball_radius_inches,
        plate_half_width_inches = plate_half_width_inches,
        order = order
      )
    },
    orders = orders,
    tolerance = tolerance
  )
}

compare_bivariate_gh_orders <- function(
  fn,
  mean = c(0, 0),
  sd = c(1, 1),
  orders = c(7L, 11L),
  tolerance = 0.001
) {
  quadrature_order_comparison(
    evaluator = function(order) {
      integrate_bivariate_normal_gh(fn, mean = mean, sd = sd, order = order)
    },
    orders = orders,
    tolerance = tolerance
  )
}
