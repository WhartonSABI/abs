add_swing_take_2d_coordinates <- function(choices) {
  x <- data.table::copy(data.table::as.data.table(choices))
  stop_if_missing_columns(
    x, c("plate_x", "plate_z", "sz_bot", "sz_top"),
    "2D swing/take choices"
  )
  x[, `:=`(
    x_location_inches = 12 * plate_x,
    z_location_inches = 12 * (plate_z - (sz_bot + sz_top) / 2),
    zone_height_inches = 12 * (sz_top - sz_bot)
  )]
  if (any(!is.finite(x$x_location_inches)) ||
      any(!is.finite(x$z_location_inches)) ||
      any(!is.finite(x$zone_height_inches)) ||
      any(x$zone_height_inches <= 0)) {
    stop("2D swing/take coordinates contain invalid values")
  }
  x[]
}

raw_swing_take_2d_rbf <- function(
  x_location_inches, z_location_inches, centers, bandwidth
) {
  centers <- data.table::as.data.table(centers)
  result <- vapply(seq_len(nrow(centers)), function(index) {
    exp(-(
      (x_location_inches - centers$x_center[[index]])^2 +
        (z_location_inches - centers$z_center[[index]])^2
    ) / (2 * bandwidth^2))
  }, numeric(length(x_location_inches)))
  if (is.null(dim(result))) result <- matrix(result, ncol = 1L)
  colnames(result) <- paste0("rbf_", seq_len(ncol(result)))
  storage.mode(result) <- "double"
  result
}

fit_swing_take_2d_basis <- function(
  choices, centers = NULL, bandwidth = 6.5
) {
  x <- add_swing_take_2d_coordinates(choices)
  if (is.null(centers)) {
    centers <- data.table::CJ(
      x_center = c(-12, -4, 4, 12),
      z_center = c(-12, -4, 4, 12)
    )
  }
  centers <- data.table::as.data.table(centers)
  stop_if_missing_columns(
    centers, c("x_center", "z_center"), "2D RBF centers"
  )
  bandwidth <- as.numeric(bandwidth)
  if (!is.finite(bandwidth) || bandwidth <= 0) {
    stop("2D RBF bandwidth must be positive")
  }
  raw <- raw_swing_take_2d_rbf(
    x$x_location_inches, x$z_location_inches, centers, bandwidth
  )
  projections <- array(
    NA_real_, dim = c(3L, 2L, ncol(raw)),
    dimnames = list(
      strikes = as.character(0:2),
      projection = c("intercept", "edge_distance"),
      basis = colnames(raw)
    )
  )
  scales <- matrix(
    NA_real_, nrow = 3L, ncol = ncol(raw),
    dimnames = list(strikes = as.character(0:2), basis = colnames(raw))
  )
  basis <- matrix(0, nrow = nrow(x), ncol = ncol(raw))
  colnames(basis) <- colnames(raw)
  for (strike in 0:2) {
    rows <- which(x$strikes == strike)
    if (length(rows) < ncol(raw) + 2L) {
      stop("Too few training rows for the ", strike, "-strike 2D surface")
    }
    anchor <- cbind(intercept = 1, edge_distance = x$edge_distance_inches[rows])
    ridge <- diag(c(0, 1e-10), nrow = 2L)
    projection <- solve(
      crossprod(anchor) + ridge,
      crossprod(anchor, raw[rows, , drop = FALSE])
    )
    residual <- raw[rows, , drop = FALSE] - anchor %*% projection
    column_scale <- apply(residual, 2L, stats::sd)
    column_scale[!is.finite(column_scale) | column_scale < 1e-6] <- 1
    projections[strike + 1L, , ] <- projection
    scales[strike + 1L, ] <- column_scale
    basis[rows, ] <- sweep(residual, 2L, column_scale, "/")
  }
  specification <- list(
    centers = centers,
    bandwidth = bandwidth,
    projections = projections,
    scales = scales,
    column_names = colnames(raw),
    average_zone_height_inches = mean(x$zone_height_inches)
  )
  list(basis = basis, coordinates = x, specification = specification)
}

score_swing_take_2d_basis <- function(choices, specification) {
  x <- add_swing_take_2d_coordinates(choices)
  raw <- raw_swing_take_2d_rbf(
    x$x_location_inches, x$z_location_inches,
    specification$centers, specification$bandwidth
  )
  raw <- raw[, specification$column_names, drop = FALSE]
  basis <- matrix(0, nrow = nrow(x), ncol = ncol(raw))
  colnames(basis) <- colnames(raw)
  for (strike in 0:2) {
    rows <- which(x$strikes == strike)
    if (!length(rows)) next
    anchor <- cbind(intercept = 1, edge_distance = x$edge_distance_inches[rows])
    projection <- specification$projections[strike + 1L, , , drop = TRUE]
    residual <- raw[rows, , drop = FALSE] - anchor %*% projection
    basis[rows, ] <- sweep(
      residual, 2L, specification$scales[strike + 1L, ], "/"
    )
  }
  list(basis = basis, coordinates = x)
}

fit_swing_take_2d_feasibility <- function(train_choices) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("fit_swing_take_2d_feasibility() requires mgcv")
  }
  x <- add_swing_take_2d_coordinates(train_choices)
  x[, `:=`(
    count_state = factor(count_state),
    pitch_family = factor(pitch_family),
    matchup = factor(matchup)
  )]
  fit <- mgcv::bam(
    swing ~ count_state + pitch_family + matchup +
      s(
        x_location_inches, z_location_inches,
        by = factor(strikes), k = 30, bs = "tp"
      ),
    data = x,
    family = stats::binomial(),
    method = "fREML",
    discrete = TRUE
  )
  fit
}

score_swing_take_2d_feasibility <- function(fit, validation_choices) {
  x <- add_swing_take_2d_coordinates(validation_choices)
  for (column in c("count_state", "pitch_family", "matchup")) {
    training_levels <- levels(fit$model[[column]])
    fallback <- names(sort(table(fit$model[[column]]), decreasing = TRUE))[[1L]]
    value <- as.character(x[[column]])
    value[!value %in% training_levels] <- fallback
    x[, (column) := factor(value, levels = training_levels)]
  }
  x[, prediction_2d_flexible := pmin(
    1 - 1e-9,
    pmax(1e-9, as.numeric(stats::predict(fit, newdata = x, type = "response")))
  )]
  x[]
}

prepare_swing_take_2d_stan_data <- function(
  choices, basis_specification = NULL
) {
  x <- data.table::copy(data.table::as.data.table(choices))
  required <- c(
    "game_pk", "batter_id", "swing", "edge_distance_inches", "balls",
    "strikes", "pitch_family", "matchup", "plate_x", "plate_z",
    "sz_bot", "sz_top"
  )
  stop_if_missing_columns(x, required, "hierarchical 2D swing/take choices")
  x <- x[
    swing %in% 0:1 & is.finite(edge_distance_inches) &
      balls %in% 0:3 & strikes %in% 0:2 & !is.na(batter_id)
  ]
  if (!nrow(x)) stop("No eligible 2D swing/take rows remain")
  x[, batter_id := as.character(batter_id)]
  players <- x[, .(
    training_opportunities = .N,
    training_swings = sum(swing),
    training_swing_rate = mean(swing)
  ), by = batter_id]
  data.table::setorder(players, batter_id)
  players[, player_index := .I]
  x[players, on = "batter_id", player_index := i.player_index]

  context <- swing_take_context_matrix(x)
  spatial <- if (is.null(basis_specification)) {
    fit_swing_take_2d_basis(x)
  } else {
    scored <- score_swing_take_2d_basis(x, basis_specification)
    list(
      basis = scored$basis, coordinates = scored$coordinates,
      specification = basis_specification
    )
  }
  x <- spatial$coordinates
  lower_target <- c(0.08, 0.18, 0.35)
  upper_target <- c(0.70, 0.88, 0.97)
  conditional_range <- (upper_target - lower_target) / (1 - lower_target)
  stan_data <- list(
    N = nrow(x),
    P = nrow(players),
    K = ncol(context$matrix),
    S = ncol(spatial$basis),
    swings = as.integer(x$swing),
    trials = rep.int(1L, nrow(x)),
    player = as.integer(x$player_index),
    strike_group = as.integer(x$strikes) + 1L,
    edge_distance = as.numeric(x$edge_distance_inches),
    X = context$matrix,
    spatial_basis = spatial$basis,
    lower_prior_mean = stats::qlogis(lower_target),
    range_prior_mean = stats::qlogis(conditional_range)
  )
  list(
    data = stan_data,
    choices = x,
    player_table = players,
    context_specification = context$specification,
    basis_specification = spatial$specification
  )
}

default_swing_take_perception_2d_stan_file <- function() {
  root <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd()),
    error = function(e) getwd()
  )
  file.path(
    root, "scripts", "stan", "hierarchical_swing_take_perception_2d.stan"
  )
}

fit_hierarchical_swing_take_perception_2d <- function(
  choices, chains = 2L, cores = chains, iter = 2000L,
  warmup = floor(iter / 2), seed = 42L, adapt_delta = 0.97,
  max_treedepth = 12L, stan_file = NULL, file = NULL,
  refresh = 100L, force_refit = FALSE
) {
  if (!is.null(file) && file.exists(file) && !isTRUE(force_refit)) {
    cached <- readRDS(file)
    if (inherits(cached, "swing_take_perception_2d_fit")) return(cached)
  }
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("fit_hierarchical_swing_take_perception_2d() requires rstan")
  }
  if (is.null(stan_file)) stan_file <- default_swing_take_perception_2d_stan_file()
  if (!file.exists(stan_file)) stop("2D perception Stan file not found: ", stan_file)
  bundle <- prepare_swing_take_2d_stan_data(choices)
  rstan::rstan_options(auto_write = TRUE)
  model <- rstan::stan_model(file = stan_file, auto_write = TRUE)
  stan_fit <- rstan::sampling(
    model,
    data = bundle$data,
    chains = as.integer(chains),
    cores = min(as.integer(cores), as.integer(chains)),
    iter = as.integer(iter),
    warmup = as.integer(warmup),
    init = 0,
    seed = as.integer(seed),
    refresh = as.integer(refresh),
    control = list(
      adapt_delta = as.numeric(adapt_delta),
      max_treedepth = as.integer(max_treedepth)
    )
  )
  fit <- list(
    stan_fit = stan_fit,
    player_table = bundle$player_table,
    context_specification = bundle$context_specification,
    basis_specification = bundle$basis_specification,
    training_games = sort(unique(as.character(choices$game_pk))),
    training_pitches = nrow(choices),
    controls = list(
      chains = chains, cores = cores, iter = iter, warmup = warmup,
      seed = seed, adapt_delta = adapt_delta, max_treedepth = max_treedepth
    )
  )
  class(fit) <- "swing_take_perception_2d_fit"
  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(fit, file)
  }
  fit
}

extract_swing_take_2d_parameter <- function(
  draw_matrix, parameter, first_dimension, second_dimension
) {
  columns <- grep(paste0("^", parameter, "\\["), colnames(draw_matrix))
  if (!length(columns)) stop("Posterior is missing parameter ", parameter)
  names <- colnames(draw_matrix)[columns]
  first <- as.integer(sub(
    ".*\\[([0-9]+),([0-9]+)\\].*", "\\1", names
  ))
  second <- as.integer(sub(
    ".*\\[([0-9]+),([0-9]+)\\].*", "\\2", names
  ))
  result <- array(
    NA_real_,
    dim = c(nrow(draw_matrix), first_dimension, second_dimension)
  )
  for (column in seq_along(columns)) {
    result[, first[[column]], second[[column]]] <-
      draw_matrix[, columns[[column]]]
  }
  if (anyNA(result)) stop(parameter, " posterior has missing matrix entries")
  result
}

swing_take_2d_parameter_draws <- function(fit, ndraws = NULL, seed = 42L) {
  if (!inherits(fit, "swing_take_perception_2d_fit")) {
    stop("fit must be a swing_take_perception_2d_fit")
  }
  parameters <- c(
    "mu_log_sigma", "tau_log_sigma", "sigma_player", "mu_threshold",
    "tau_threshold", "threshold_player", "beta", "spatial_shift_beta",
    "lower_swing", "upper_swing"
  )
  draws <- as.matrix(fit$stan_fit, pars = parameters)
  if (!is.null(ndraws) && nrow(draws) > as.integer(ndraws)) {
    set.seed(seed)
    draws <- draws[sample.int(nrow(draws), as.integer(ndraws)), , drop = FALSE]
  }
  list(
    mu_log_sigma = as.numeric(draws[, "mu_log_sigma"]),
    tau_log_sigma = as.numeric(draws[, "tau_log_sigma"]),
    sigma_player = swing_take_extract_indexed(
      draws, "sigma_player", nrow(fit$player_table)
    ),
    mu_threshold = as.numeric(draws[, "mu_threshold"]),
    tau_threshold = as.numeric(draws[, "tau_threshold"]),
    threshold_player = swing_take_extract_indexed(
      draws, "threshold_player", nrow(fit$player_table)
    ),
    beta = swing_take_extract_indexed(
      draws, "beta", length(fit$context_specification$column_names)
    ),
    spatial_shift_beta = extract_swing_take_2d_parameter(
      draws, "spatial_shift_beta", 3L,
      length(fit$basis_specification$column_names)
    ),
    lower_swing = swing_take_extract_indexed(draws, "lower_swing", 3L),
    upper_swing = swing_take_extract_indexed(draws, "upper_swing", 3L)
  )
}

score_hierarchical_swing_take_2d <- function(
  fit, choices, ndraws = 300L, seed = 42L
) {
  if (!inherits(fit, "swing_take_perception_2d_fit")) {
    stop("fit must be a swing_take_perception_2d_fit")
  }
  x <- data.table::copy(data.table::as.data.table(choices))
  context <- swing_take_context_matrix(x, fit$context_specification)$matrix
  spatial <- score_swing_take_2d_basis(x, fit$basis_specification)
  x <- spatial$coordinates
  player_index <- match(as.character(x$batter_id), fit$player_table$batter_id)
  new_players <- sort(unique(as.character(x$batter_id[is.na(player_index)])))
  new_index <- match(as.character(x$batter_id), new_players)
  strike_group <- as.integer(x$strikes) + 1L
  draws <- swing_take_2d_parameter_draws(fit, ndraws = ndraws, seed = seed)
  draw_count <- length(draws$mu_log_sigma)
  probability_sum <- sigma_sum <- threshold_sum <- numeric(nrow(x))
  set.seed(seed + 1L)
  strike_rows <- lapply(1:3, function(index) which(strike_group == index))
  for (draw in seq_len(draw_count)) {
    sigma <- threshold <- numeric(nrow(x))
    seen <- !is.na(player_index)
    if (any(seen)) {
      sigma[seen] <- draws$sigma_player[draw, player_index[seen]]
      threshold[seen] <- draws$threshold_player[draw, player_index[seen]]
    }
    if (length(new_players)) {
      sigma_new <- exp(
        draws$mu_log_sigma[[draw]] + draws$tau_log_sigma[[draw]] *
          stats::rnorm(length(new_players))
      )
      threshold_new <- draws$mu_threshold[[draw]] +
        draws$tau_threshold[[draw]] * stats::rnorm(length(new_players))
      sigma[!seen] <- sigma_new[new_index[!seen]]
      threshold[!seen] <- threshold_new[new_index[!seen]]
    }
    spatial_shift <- numeric(nrow(x))
    for (strike in 1:3) {
      rows <- strike_rows[[strike]]
      if (length(rows)) {
        spatial_shift[rows] <- as.numeric(
          spatial$basis[rows, , drop = FALSE] %*%
            draws$spatial_shift_beta[draw, strike, ]
        )
      }
    }
    margin <- threshold + as.numeric(context %*% draws$beta[draw, ]) +
      spatial_shift - x$edge_distance_inches
    perceived_swing_side <- stats::pnorm(margin / sigma)
    lower <- draws$lower_swing[draw, strike_group]
    upper <- draws$upper_swing[draw, strike_group]
    probability_sum <- probability_sum +
      lower + (upper - lower) * perceived_swing_side
    sigma_sum <- sigma_sum + sigma
    threshold_sum <- threshold_sum + threshold
  }
  x[, `:=`(
    prediction_hierarchical_2d = pmin(
      1 - 1e-9, pmax(1e-9, probability_sum / draw_count)
    ),
    sigma_2d_posterior_mean = sigma_sum / draw_count,
    threshold_2d_posterior_mean = threshold_sum / draw_count,
    batter_seen_in_2d_training = !is.na(player_index)
  )]
  x[]
}

swing_take_2d_fit_summaries <- function(fit, ndraws = NULL, seed = 42L) {
  draws <- swing_take_2d_parameter_draws(fit, ndraws = ndraws, seed = seed)
  population_median <- exp(draws$mu_log_sigma)
  population_mean <- exp(
    draws$mu_log_sigma + 0.5 * draws$tau_log_sigma^2
  )
  population_sd <- sqrt(
    (exp(draws$tau_log_sigma^2) - 1) *
      exp(2 * draws$mu_log_sigma + draws$tau_log_sigma^2)
  )
  population <- data.table::as.data.table(c(
    summarize_swing_take_draws(population_mean, "population_sigma_mean_"),
    summarize_swing_take_draws(population_median, "population_sigma_median_"),
    summarize_swing_take_draws(population_sd, "between_batter_sigma_sd_"),
    summarize_swing_take_draws(draws$tau_log_sigma, "tau_log_sigma_")
  ))
  players <- data.table::rbindlist(lapply(
    seq_len(nrow(fit$player_table)), function(index) {
      data.table::as.data.table(c(
        as.list(fit$player_table[index]),
        summarize_swing_take_draws(draws$sigma_player[, index], "sigma_"),
        summarize_swing_take_draws(
          draws$threshold_player[, index], "threshold_"
        )
      ))
    }
  ), fill = TRUE)
  strategy_limits <- data.table::rbindlist(lapply(1:3, function(index) {
    data.table::as.data.table(c(
      list(strikes = index - 1L),
      summarize_swing_take_draws(
        draws$lower_swing[, index], "lower_swing_"
      ),
      summarize_swing_take_draws(
        draws$upper_swing[, index], "upper_swing_"
      )
    ))
  }), fill = TRUE)
  surface_norm <- sapply(1:3, function(strike) {
    sqrt(rowMeans(draws$spatial_shift_beta[, strike, ]^2))
  })
  correlation_targets <- cbind(
    mu_threshold = draws$mu_threshold,
    tau_threshold = draws$tau_threshold,
    draws$beta,
    surface_norm,
    draws$lower_swing,
    draws$upper_swing
  )
  correlations <- vapply(
    seq_len(ncol(correlation_targets)),
    function(index) stats::cor(population_median, correlation_targets[, index]),
    numeric(1L)
  )
  identifiability <- data.table::data.table(
    paired_parameter = colnames(correlation_targets),
    sigma_posterior_correlation = correlations,
    tolerance = 0.8,
    pass = abs(correlations) < 0.8
  )
  list(
    population = population,
    players = players,
    strategy_limits = strategy_limits,
    identifiability = identifiability
  )
}

validate_hierarchical_swing_take_2d <- function(
  scored_choices, minimum_cell_pitches = 100L,
  weighted_mae_tolerance = 0.03, p90_tolerance = 0.06
) {
  x <- data.table::copy(data.table::as.data.table(scored_choices))
  x[, prediction_hierarchical := prediction_hierarchical_2d]
  one_dimensional <- validate_hierarchical_swing_take(x)
  probability <- pmin(1 - 1e-9, pmax(1e-9, x$prediction_hierarchical_2d))
  x[, loss_2d_hierarchical__ := -(
    swing * log(probability) + (1 - swing) * log1p(-probability)
  )]
  if ("prediction_2d_flexible" %in% names(x)) {
    flexible <- pmin(1 - 1e-9, pmax(1e-9, x$prediction_2d_flexible))
    x[, loss_2d_flexible__ := -(
      swing * log(flexible) + (1 - swing) * log1p(-flexible)
    )]
  } else {
    x[, loss_2d_flexible__ := NA_real_]
  }
  breaks <- seq(-18, 18, by = 3)
  x[, `:=`(
    x_cell__ = cut(
      x_location_inches, breaks, include.lowest = TRUE, right = FALSE
    ),
    z_cell__ = cut(
      z_location_inches, breaks, include.lowest = TRUE, right = FALSE
    )
  )]
  midpoint <- head(breaks, -1L) + 1.5
  x_midpoint <- data.table::data.table(
    x_cell__ = levels(x$x_cell__), x_midpoint = midpoint
  )
  z_midpoint <- data.table::data.table(
    z_cell__ = levels(x$z_cell__), z_midpoint = midpoint
  )
  cells <- x[!is.na(x_cell__) & !is.na(z_cell__), .(
    pitches = .N,
    observed_swing_rate = mean(swing),
    predicted_swing_rate = mean(prediction_hierarchical_2d)
  ), by = .(strikes, x_cell__, z_cell__)]
  cells <- merge(cells, x_midpoint, by = "x_cell__", all.x = TRUE)
  cells <- merge(cells, z_midpoint, by = "z_cell__", all.x = TRUE)
  cells[, `:=`(
    error = predicted_swing_rate - observed_swing_rate,
    absolute_error = abs(predicted_swing_rate - observed_swing_rate)
  )]
  included <- cells[pitches >= as.integer(minimum_cell_pitches)]
  weighted_mae <- stats::weighted.mean(included$absolute_error, included$pitches)
  p90_error <- as.numeric(stats::quantile(
    included$absolute_error, 0.90, names = FALSE
  ))
  maximum_error <- max(included$absolute_error)
  by_game <- x[, .(
    pitches = .N,
    hierarchical_2d_log_loss = mean(loss_2d_hierarchical__),
    flexible_2d_log_loss = mean(loss_2d_flexible__, na.rm = TRUE)
  ), by = game_pk]
  two_dimensional_metrics <- data.table::data.table(
    validation_pitches = nrow(x),
    populated_2d_cells = nrow(included),
    hierarchical_2d_log_loss = mean(x$loss_2d_hierarchical__),
    flexible_2d_log_loss = mean(x$loss_2d_flexible__, na.rm = TRUE),
    weighted_2d_cell_mae = weighted_mae,
    weighted_mae_tolerance = weighted_mae_tolerance,
    p90_2d_cell_error = p90_error,
    p90_tolerance = p90_tolerance,
    maximum_2d_cell_error = maximum_error,
    two_dimensional_pass = weighted_mae <= weighted_mae_tolerance &&
      p90_error <= p90_tolerance
  )
  metrics <- cbind(
    one_dimensional$metrics,
    two_dimensional_metrics[, !c("validation_pitches")]
  )
  metrics[, predictive_pass := predictive_pass & two_dimensional_pass]
  list(
    metrics = metrics,
    by_game = by_game,
    distance_curve = one_dimensional$curve,
    cells = cells
  )
}

plot_swing_take_2d_validation <- function(validation) {
  cells <- data.table::copy(validation$cells)
  long <- data.table::rbindlist(list(
    cells[, .(
      strikes, x_midpoint, z_midpoint, pitches,
      value = observed_swing_rate, panel = "Observed swing rate"
    )],
    cells[, .(
      strikes, x_midpoint, z_midpoint, pitches,
      value = predicted_swing_rate, panel = "Predicted swing rate"
    )]
  ))
  long[, strikes_label := paste0(strikes, ifelse(strikes == 1L, " strike", " strikes"))]
  ggplot2::ggplot(long, ggplot2::aes(x_midpoint, z_midpoint, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.15) +
    ggplot2::facet_grid(strikes_label ~ panel) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_gradient2(
      low = "#2457A6", mid = "white", high = "#B40426",
      midpoint = 0.5, limits = c(0, 1),
      labels = function(value) paste0(round(100 * value), "%")
    ) +
    ggplot2::labs(
      title = "Observed vs. Predicted Swing Decisions in Two Dimensions",
      subtitle = "Held-out games; location is measured in inches from the zone center",
      x = "Horizontal location (inches)",
      y = "Vertical location (inches)",
      fill = "Swing rate"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

plot_swing_take_2d_error <- function(validation) {
  cells <- data.table::copy(validation$cells)
  cells[, strikes_label := paste0(
    strikes, ifelse(strikes == 1L, " strike", " strikes")
  )]
  ggplot2::ggplot(cells, ggplot2::aes(x_midpoint, z_midpoint, fill = error)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.15) +
    ggplot2::facet_wrap(~strikes_label) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_gradient2(
      low = "#2457A6", mid = "white", high = "#B40426",
      midpoint = 0, limits = c(-0.15, 0.15),
      oob = scales::squish,
      labels = function(value) paste0(round(100 * value), "%")
    ) +
    ggplot2::labs(
      title = "Where Does the 2D Perception Model Miss?",
      subtitle = "Red means predicted swing rate is too high; blue means too low",
      x = "Horizontal location (inches)",
      y = "Vertical location (inches)",
      fill = "Prediction error"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}
