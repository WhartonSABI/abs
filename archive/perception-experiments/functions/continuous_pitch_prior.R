continuous_pitch_prior_allowed_columns <- function() {
  c(
    "game_pk", "at_bat_number", "pitch_number", "pitcher_id", "pitcher",
    "plate_x", "plate_z", "sz_top", "sz_bot", "balls", "strikes",
    "pitch_family", "pitch_type", "matchup", "stand", "p_throws"
  )
}

normalize_continuous_pitch_prior_input <- function(pitches) {
  x <- continuous_allowlist(
    pitches, continuous_pitch_prior_allowed_columns(),
    "continuous pitch-prior input"
  )
  if (!"pitcher_id" %in% names(x) && "pitcher" %in% names(x)) {
    x[, pitcher_id := as.character(pitcher)]
  }
  if (!"pitch_family" %in% names(x) && "pitch_type" %in% names(x)) {
    x[, pitch_family := swing_take_pitch_family(pitch_type)]
  }
  if (!"matchup" %in% names(x) && all(c("stand", "p_throws") %in% names(x))) {
    x[, matchup := paste0(
      data.table::fcoalesce(as.character(stand), "U"), "-",
      data.table::fcoalesce(as.character(p_throws), "U")
    )]
  }
  stop_if_missing_columns(
    x,
    c(
      "pitcher_id", "plate_x", "plate_z", "sz_top", "sz_bot", "balls",
      "strikes", "pitch_family", "matchup"
    ),
    "continuous pitch-prior input"
  )
  x[, `:=`(
    pitcher_id = as.character(pitcher_id),
    pitch_family = as.character(pitch_family),
    matchup = as.character(matchup),
    count_state = paste0(balls, "-", strikes),
    x_location_inches = 12 * plate_x,
    z_location_inches = 12 * (plate_z - (sz_bot + sz_top) / 2)
  )]
  x <- x[
    !is.na(pitcher_id) & nzchar(pitcher_id) &
      is.finite(x_location_inches) & is.finite(z_location_inches) &
      balls %in% 0:3 & strikes %in% 0:2 &
      !is.na(pitch_family) & !is.na(matchup)
  ]
  if (!nrow(x)) stop("No eligible continuous pitch-prior rows remain")
  x[]
}

initialize_continuous_pitch_mixture <- function(
  location, components, seed = 42L
) {
  location <- as.matrix(location)
  components <- as.integer(components)
  if (!components %in% c(1L, 3L, 6L)) {
    stop("components must be one of 1, 3, or 6")
  }
  if (nrow(location) < max(20L, 4L * components)) {
    stop("Too few pitches to initialize the requested mixture")
  }
  if (components == 1L) {
    cluster <- rep.int(1L, nrow(location))
    center <- matrix(colMeans(location), nrow = 1L)
  } else {
    if (nrow(unique(data.frame(location))) < components) {
      stop("Pitch locations contain fewer unique points than components")
    }
    set.seed(seed)
    fitted <- stats::kmeans(
      location, centers = components, iter.max = 100L, nstart = 10L
    )
    cluster <- fitted$cluster
    center <- fitted$centers
  }
  order <- order(center[, 2L], center[, 1L])
  center <- center[order, , drop = FALSE]
  relabel <- match(cluster, order)
  global_scale <- apply(location, 2L, stats::sd)
  global_scale[!is.finite(global_scale) | global_scale < 1] <- 6
  scale <- t(vapply(seq_len(components), function(component) {
    value <- location[relabel == component, , drop = FALSE]
    result <- if (nrow(value) >= 3L) {
      apply(value, 2L, stats::sd)
    } else {
      global_scale
    }
    result[!is.finite(result) | result < 1] <- global_scale[
      !is.finite(result) | result < 1
    ]
    pmax(1, result)
  }, numeric(2L)))
  list(center = center, scale = scale, cluster = relabel)
}

prepare_continuous_pitch_prior <- function(
  pitches, components = 6L, seed = 42L, include_pitch_family = TRUE
) {
  x <- normalize_continuous_pitch_prior_input(pitches)
  pitcher_table <- x[, .(training_pitches = .N), by = pitcher_id]
  data.table::setorder(pitcher_table, pitcher_id)
  pitcher_table[, pitcher_index := .I]
  x[pitcher_table, on = "pitcher_id", pitcher_index := i.pitcher_index]
  categorical <- c(
    "count_state", if (isTRUE(include_pitch_family)) "pitch_family", "matchup"
  )
  context <- fit_continuous_design(x, categorical = categorical)
  location <- cbind(
    x_location_inches = x$x_location_inches,
    z_location_inches = x$z_location_inches
  )
  initialization <- initialize_continuous_pitch_mixture(
    location, components, seed
  )
  stan_data <- list(
    N = nrow(x),
    P = nrow(pitcher_table),
    D = ncol(context$matrix),
    K = as.integer(components),
    pitcher = as.integer(x$pitcher_index),
    location = location,
    X = context$matrix,
    anchor_mean = initialization$center,
    anchor_scale = initialization$scale
  )
  list(
    data = stan_data,
    rows = x,
    pitcher_table = pitcher_table,
    context_specification = context$specification,
    initialization = initialization,
    components = as.integer(components),
    include_pitch_family = isTRUE(include_pitch_family),
    information_set = c(
      "tracked_location", "pitcher", "count", "pitch_family",
      "handedness_matchup"
    )
  )
}

default_continuous_pitch_prior_stan_file <- function() {
  file.path(
    continuous_model_root(), "scripts", "stan", "continuous_pitch_prior.stan"
  )
}

fit_continuous_pitch_prior <- function(
  pitches, components = 6L, include_pitch_family = TRUE,
  backend = c("cmdstanr", "rstan"), chains = 4L,
  parallel_chains = chains, iter_warmup = 1000L, iter_sampling = 1000L,
  seed = 42L, adapt_delta = 0.95, max_treedepth = 12L,
  refresh = 100L, stan_file = NULL, file = NULL, force_refit = FALSE
) {
  if (!is.null(file) && file.exists(file) && !isTRUE(force_refit)) {
    cached <- readRDS(file)
    if (inherits(cached, "continuous_pitch_prior_fit")) return(cached)
  }
  backend <- match.arg(backend)
  bundle <- prepare_continuous_pitch_prior(
    pitches, components, seed, include_pitch_family
  )
  if (is.null(stan_file)) stan_file <- default_continuous_pitch_prior_stan_file()
  stan_fit <- continuous_stan_fit(
    stan_file, bundle$data, backend = backend, chains = chains,
    parallel_chains = parallel_chains, iter_warmup = iter_warmup,
    iter_sampling = iter_sampling, seed = seed, adapt_delta = adapt_delta,
    max_treedepth = max_treedepth, refresh = refresh
  )
  fit <- list(
    stan_fit = stan_fit,
    components = bundle$components,
    pitcher_table = bundle$pitcher_table,
    context_specification = bundle$context_specification,
    initialization = bundle$initialization,
    include_pitch_family = bundle$include_pitch_family,
    training_games = sort(unique(as.character(bundle$rows$game_pk))),
    training_rows = nrow(bundle$rows),
    backend = backend,
    information_set = bundle$information_set
  )
  class(fit) <- "continuous_pitch_prior_fit"
  if (!is.null(file)) saveRDS(fit, file)
  fit
}

draws_continuous_pitch_prior <- function(fit, ndraws = NULL, seed = 42L) {
  if (!inherits(fit, "continuous_pitch_prior_fit")) {
    stop("fit must be a continuous_pitch_prior_fit")
  }
  variables <- c("component_mean", "component_scale", "component_rho")
  if (fit$components > 1L) {
    variables <- c(
      "weight_intercept", "weight_context", "pitcher_effect", variables
    )
  }
  draws <- continuous_thin_draws(
    continuous_draw_matrix(fit$stan_fit, variables), ndraws, seed
  )
  components <- fit$components
  context_columns <- sum(vapply(
    fit$context_specification$categorical,
    function(entry) length(entry$levels) - 1L, integer(1L)
  )) + length(fit$context_specification$numeric)
  list(
    draw_id = seq_len(nrow(draws)),
    weight_intercept = continuous_extract_vector(
      draws, "weight_intercept", components - 1L
    ),
    weight_context = continuous_extract_matrix(
      draws, "weight_context", context_columns, components - 1L
    ),
    pitcher_effect = continuous_extract_matrix(
      draws, "pitcher_effect", nrow(fit$pitcher_table), components - 1L
    ),
    component_mean = continuous_extract_matrix(
      draws, "component_mean", components, 2L
    ),
    component_scale = continuous_extract_matrix(
      draws, "component_scale", components, 2L
    ),
    component_rho = continuous_extract_vector(
      draws, "component_rho", components
    )
  )
}

continuous_bivariate_normal_density <- function(
  x, mean, scale, correlation
) {
  zx <- (x[, 1L] - mean[[1L]]) / scale[[1L]]
  zz <- (x[, 2L] - mean[[2L]]) / scale[[2L]]
  one_minus_rho2 <- pmax(1e-8, 1 - correlation^2)
  exponent <- -(
    zx^2 - 2 * correlation * zx * zz + zz^2
  ) / (2 * one_minus_rho2)
  exp(exponent) / (
    2 * pi * scale[[1L]] * scale[[2L]] * sqrt(one_minus_rho2)
  )
}

score_continuous_pitch_prior <- function(
  fit, pitches, ndraws = 400L, seed = 42L, return_draws = FALSE
) {
  if (!inherits(fit, "continuous_pitch_prior_fit")) {
    stop("fit must be a continuous_pitch_prior_fit")
  }
  x <- normalize_continuous_pitch_prior_input(pitches)
  context <- score_continuous_design(x, fit$context_specification)
  location <- cbind(x$x_location_inches, x$z_location_inches)
  pitcher <- match(as.character(x$pitcher_id), fit$pitcher_table$pitcher_id)
  posterior <- draws_continuous_pitch_prior(fit, ndraws, seed)
  density <- matrix(NA_real_, nrow(x), length(posterior$draw_id))
  for (draw in seq_along(posterior$draw_id)) {
    component_density <- vapply(seq_len(fit$components), function(component) {
      continuous_bivariate_normal_density(
        location,
        posterior$component_mean[draw, component, ],
        posterior$component_scale[draw, component, ],
        posterior$component_rho[draw, component]
      )
    }, numeric(nrow(x)))
    if (is.null(dim(component_density))) {
      component_density <- matrix(component_density, ncol = 1L)
    }
    for (row in seq_len(nrow(x))) {
      eta <- numeric(fit$components)
      if (fit$components > 1L) {
        eta[seq_len(fit$components - 1L)] <-
          posterior$weight_intercept[draw, ] +
          if (ncol(context)) {
            as.numeric(
              context[row, , drop = FALSE] %*%
                posterior$weight_context[draw, , ]
            )
          } else {
            numeric(fit$components - 1L)
          }
        if (!is.na(pitcher[[row]])) {
          eta[seq_len(fit$components - 1L)] <-
            eta[seq_len(fit$components - 1L)] +
            posterior$pitcher_effect[draw, pitcher[[row]], ]
        }
      }
      density[row, draw] <- sum(
        continuous_softmax(eta) * component_density[row, ]
      )
    }
  }
  density <- pmax(density, .Machine$double.xmin)
  result <- data.table::copy(x)
  result <- cbind(
    result,
    continuous_probability_summary(density, "location_density_")
  )
  result[, `:=`(
    log_posterior_predictive_density = log(rowMeans(density)),
    mean_log_density = rowMeans(log(density)),
    pitcher_seen_in_training = !is.na(pitcher),
    mixture_components = fit$components
  )]
  if (isTRUE(return_draws)) {
    return(list(
      summary = result,
      draw_id = posterior$draw_id,
      location_density = density
    ))
  }
  result[]
}
