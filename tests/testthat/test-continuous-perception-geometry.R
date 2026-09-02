test_that("centered inch geometry exactly reproduces repository ABS distances", {
  plate_x <- c(-0.9, -17 / 24, 0, 17 / 24 + 0.1, 1.2)
  plate_z <- c(1.1, 1.5, 2.4, 3.2, 3.8)
  sz_top <- c(3.2, 3.1, 3.3, 3.1, 3.4)
  sz_bot <- c(1.4, 1.5, 1.6, 1.5, 1.55)
  centered <- center_abs_coordinates_inches(
    plate_x, plate_z, sz_top, sz_bot
  )

  expect_equal(
    centered_abs_edge_distance_inches(
      centered$x_inches,
      centered$z_inches,
      centered$zone_half_height_inches
    ),
    abs_edge_distance_inches(plate_x, plate_z, sz_top, sz_bot),
    tolerance = 1e-12
  )

  half_height <- 10
  corner <- c(8.5, half_height) + 1.45 / sqrt(2)
  expect_equal(
    centered_abs_edge_distance_inches(corner[[1L]], corner[[2L]], half_height),
    0,
    tolerance = 1e-12
  )
  expect_identical(
    classify_centered_abs_call(corner[[1L]], corner[[2L]], half_height),
    "called_strike"
  )
  expect_gt(
    centered_abs_edge_distance_inches(
      corner[[1L]] + 0.01, corner[[2L]] + 0.01, half_height
    ),
    0
  )
})

test_that("rounded vertical limits describe the exact dilated rectangle", {
  half_height <- 10
  radius <- 1.45
  limits <- rounded_abs_vertical_limit_inches(
    c(0, 8.5, 8.5 + radius / sqrt(2), 8.5 + radius, 10.01),
    zone_half_height_inches = half_height,
    ball_radius_inches = radius
  )

  expect_equal(limits[[1L]], half_height + radius)
  expect_equal(limits[[2L]], half_height + radius)
  expect_equal(limits[[3L]], half_height + radius / sqrt(2), tolerance = 1e-12)
  expect_equal(limits[[4L]], half_height, tolerance = 1e-7)
  expect_true(is.na(limits[[5L]]))
})

test_that("Legendre and normal-Hermite rules reproduce polynomial moments", {
  legendre <- gauss_legendre_rule(7L, lower = -1, upper = 1)
  expect_equal(sum(legendre$weight), 2, tolerance = 1e-14)
  expect_equal(sum(legendre$weight * legendre$node^12), 2 / 13,
    tolerance = 1e-13
  )
  expect_equal(
    integrate_gauss_legendre(function(x) x^6, -2, 3, order = 7L),
    (3^7 - (-2)^7) / 7,
    tolerance = 1e-11
  )

  hermite <- gauss_hermite_normal_rule(7L)
  expect_equal(sum(hermite$weight), 1, tolerance = 1e-14)
  expect_equal(sum(hermite$weight * hermite$node^2), 1, tolerance = 1e-13)
  expect_equal(sum(hermite$weight * hermite$node^4), 3, tolerance = 1e-12)

  bivariate <- bivariate_gauss_hermite_rule(
    7L, mean = c(1, -2), sd = c(2, 3)
  )
  expect_equal(sum(bivariate$weight), 1, tolerance = 1e-14)
  expect_equal(sum(bivariate$weight * bivariate$x), 1, tolerance = 1e-13)
  expect_equal(sum(bivariate$weight * bivariate$z), -2, tolerance = 1e-13)
  expect_equal(
    integrate_bivariate_normal_gh(
      function(x, z) x^2 + z^2,
      mean = c(1, -2), sd = c(2, 3), order = 7L
    ),
    18,
    tolerance = 1e-12
  )
})

test_that("rounded normal mass matches analytic rectangles and reference integration", {
  mean <- c(3.1, 8.7)
  sd <- c(4.2, 2.3)
  half_height <- 10
  rectangular <- rounded_abs_normal_mass(
    mean, sd, half_height, ball_radius_inches = 0, order = 11L
  )
  expected_rectangle <- diff(stats::pnorm(c(-8.5, 8.5), mean[[1L]], sd[[1L]])) *
    diff(stats::pnorm(
      c(-half_height, half_height), mean[[2L]], sd[[2L]]
    ))
  expect_equal(rectangular, expected_rectangle, tolerance = 1e-13)

  radius <- 1.45
  reference <- stats::integrate(
    function(x) {
      limit <- rounded_abs_vertical_limit_inches(
        x, half_height, radius, plate_half_width_inches = 8.5
      )
      stats::dnorm(x, mean[[1L]], sd[[1L]]) *
        (stats::pnorm(limit, mean[[2L]], sd[[2L]]) -
          stats::pnorm(-limit, mean[[2L]], sd[[2L]]))
    },
    lower = -8.5 - radius,
    upper = 8.5 + radius,
    subdivisions = 1000L,
    rel.tol = 1e-11
  )$value
  rounded <- rounded_abs_normal_mass(
    mean, sd, half_height, ball_radius_inches = radius, order = 31L
  )
  expect_equal(rounded, reference, tolerance = 1e-7)
  expect_equal(
    rounded,
    rounded_abs_normal_mass(-mean, sd, half_height, radius, order = 31L),
    tolerance = 1e-13
  )
  expect_true(rounded >= 0 && rounded <= 1)
})

test_that("rounded-region quadrature agrees with high-precision Monte Carlo", {
  set.seed(20260825L)
  draws <- 750000L
  location <- cbind(
    stats::rnorm(draws, mean = 7.8, sd = 4.1),
    stats::rnorm(draws, mean = 8.9, sd = 3.4)
  )
  monte_carlo <- mean(centered_abs_edge_distance_inches(
    location[, 1L], location[, 2L], zone_half_height_inches = 10
  ) <= 0)
  quadrature <- rounded_abs_normal_mass(
    mean = c(7.8, 8.9), sd = c(4.1, 3.4),
    zone_half_height_inches = 10, order = 64L
  )
  expect_equal(quadrature, monte_carlo, tolerance = 0.002)
})

test_that("rounded normal mass is stable for zero and nearly-zero sigma", {
  half_height <- 10
  expect_equal(rounded_abs_normal_mass(c(0, 0), 0, half_height), 1)
  expect_equal(rounded_abs_normal_mass(c(30, 30), 0, half_height), 0)
  expect_equal(
    rounded_abs_normal_mass(c(0, 0), c(1e-8, 1e-8), half_height),
    1,
    tolerance = 1e-12
  )
  expect_equal(
    rounded_abs_normal_mass(c(30, 30), c(1e-8, 1e-8), half_height),
    0,
    tolerance = 1e-12
  )

  vertical_only <- rounded_abs_normal_mass(
    mean = c(0, 11), sd = c(0, 2), zone_half_height_inches = half_height
  )
  expect_equal(
    vertical_only,
    diff(stats::pnorm(c(-11.45, 11.45), mean = 11, sd = 2)),
    tolerance = 1e-13
  )
  horizontal_only <- rounded_abs_normal_mass(
    mean = c(9, 0), sd = c(2, 0), zone_half_height_inches = half_height
  )
  expect_equal(
    horizontal_only,
    diff(stats::pnorm(c(-9.95, 9.95), mean = 9, sd = 2)),
    tolerance = 1e-13
  )

  extreme_cases <- list(
    list(mean = c(-40, 40), sd = c(1e-4, 1e-4)),
    list(mean = c(40, -40), sd = c(0.05, 1000)),
    list(mean = c(-25, 8.5), sd = c(0.2, 3)),
    list(mean = c(25, -8.5), sd = c(0.2, 3))
  )
  extreme_masses <- vapply(extreme_cases, function(parameters) {
    rounded_abs_normal_mass(
      parameters$mean, parameters$sd, half_height, order = 11L
    )
  }, numeric(1))
  expect_true(all(is.finite(extreme_masses)))
  expect_true(all(extreme_masses >= 0 & extreme_masses <= 1))
})

test_that("Gaussian-mixture signal posterior follows diagonal conjugate algebra", {
  posterior <- gaussian_mixture_signal_posterior(
    weights = 1,
    means = c(-1, 2),
    component_sd = c(3, 4),
    signal = c(2, -2),
    perception_sd = 2
  )
  expected_gain <- c(9 / 13, 16 / 20)
  expect_equal(
    as.numeric(posterior$means[1, ]),
    c(-1, 2) + expected_gain * (c(2, -2) - c(-1, 2)),
    tolerance = 1e-13
  )
  expect_equal(
    as.numeric(posterior$sd[1, ]),
    sqrt(c(9, 16) * (1 - expected_gain)),
    tolerance = 1e-13
  )
  expect_equal(posterior$weights, 1)
  expect_true(is.finite(posterior$log_evidence))

  mixture <- gaussian_mixture_signal_posterior(
    weights = c(0.25, 0.75),
    means = rbind(c(-8, 0), c(8, 0)),
    component_sd = rbind(c(2, 3), c(2, 3)),
    signal = c(7, 0),
    perception_sd = c(1.5, 1.5)
  )
  expect_equal(sum(mixture$weights), 1, tolerance = 1e-14)
  expect_gt(mixture$weights[[2L]], 0.999)
})

test_that("zero and diffuse perception have the correct posterior limits", {
  weights <- c(0.35, 0.65)
  means <- rbind(c(-15, 0), c(2, 0))
  component_sd <- rbind(c(2, 2), c(4, 5))
  half_height <- 10

  exact_signal <- gaussian_mixture_signal_posterior(
    weights, means, component_sd,
    signal = c(20, 20), perception_sd = 0
  )
  expect_equal(exact_signal$means[, 1], rep(20, 2), tolerance = 1e-13)
  expect_equal(exact_signal$means[, 2], rep(20, 2), tolerance = 1e-13)
  expect_equal(exact_signal$sd, matrix(0, 2, 2), tolerance = 1e-13)
  expect_equal(
    gaussian_mixture_abs_probability(exact_signal, half_height),
    0
  )

  diffuse_signal <- gaussian_mixture_signal_posterior(
    weights, means, component_sd,
    signal = c(5, -4), perception_sd = 1e8
  )
  prior <- list(weights = weights, means = means, sd = component_sd)
  prior_strike_probability <- gaussian_mixture_abs_probability(prior, half_height)
  diffuse_strike_probability <- gaussian_mixture_abs_probability(
    diffuse_signal, half_height
  )
  expect_equal(diffuse_signal$weights, weights, tolerance = 1e-12)
  expect_equal(diffuse_signal$means, means, tolerance = 1e-12)
  expect_equal(diffuse_signal$sd, component_sd, tolerance = 1e-12)
  expect_equal(
    diffuse_strike_probability, prior_strike_probability, tolerance = 1e-12
  )
  expect_gt(diffuse_strike_probability, 0)
  expect_lt(diffuse_strike_probability, 1)
})

test_that("wrong-call probabilities are complements and remain bounded", {
  posterior <- gaussian_mixture_signal_posterior(
    weights = c(0.5, 0.5),
    means = rbind(c(-10, 0), c(0, 0)),
    component_sd = rbind(c(3, 3), c(3, 3)),
    signal = c(-4, 1),
    perception_sd = c(4, 4)
  )
  ball_wrong <- gaussian_mixture_wrong_call_probability(
    posterior, "ball", zone_half_height_inches = 10
  )
  strike_wrong <- gaussian_mixture_wrong_call_probability(
    posterior, "called_strike", zone_half_height_inches = 10
  )
  expect_equal(ball_wrong + strike_wrong, 1, tolerance = 1e-14)
  expect_true(all(c(ball_wrong, strike_wrong) >= 0))
  expect_true(all(c(ball_wrong, strike_wrong) <= 1))
})

test_that("seven-versus-eleven convergence helpers expose numerical diagnostics", {
  mass_comparison <- compare_rounded_abs_mass_orders(
    mean = c(8.8, 10.2),
    sd = c(3.5, 2.8),
    zone_half_height_inches = 10,
    orders = c(7L, 11L),
    tolerance = 0.001
  )
  expect_identical(mass_comparison$lower_order, 7L)
  expect_identical(mass_comparison$higher_order, 11L)
  expect_true(mass_comparison$converged)
  expect_lte(mass_comparison$absolute_difference, 0.001)

  hermite_comparison <- compare_bivariate_gh_orders(
    function(x, z) stats::plogis(0.2 + 0.1 * x - 0.08 * z),
    mean = c(1, -1),
    sd = c(2, 2.5),
    orders = c(7L, 11L),
    tolerance = 0.001
  )
  expect_true(hermite_comparison$converged)
  expect_lte(hermite_comparison$absolute_difference, 0.001)
})
