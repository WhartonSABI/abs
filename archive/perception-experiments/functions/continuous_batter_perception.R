continuous_batter_allowed_columns <- function() {
  c(
    "game_pk", "at_bat_number", "pitch_number", "batter_id", "batter",
    "swing", "description", "plate_x", "plate_z", "sz_top", "sz_bot",
    "balls", "strikes", "pitch_family", "pitch_type", "matchup", "stand",
    "p_throws"
  )
}

continuous_abs_outward_normal <- function(
  x_inches, z_inches, zone_half_height_inches,
  plate_half_width_inches = 8.5
) {
  lengths <- c(
    length(x_inches), length(z_inches), length(zone_half_height_inches),
    length(plate_half_width_inches)
  )
  size <- max(lengths)
  if (any(lengths != 1L & lengths != size)) {
    stop("Rounded-boundary normal inputs must be scalar or row-aligned")
  }
  values <- lapply(
    list(
      x_inches, z_inches, zone_half_height_inches,
      plate_half_width_inches
    ),
    rep_len, length.out = size
  )
  x_inches <- as.numeric(values[[1L]])
  z_inches <- as.numeric(values[[2L]])
  zone_half_height_inches <- as.numeric(values[[3L]])
  plate_half_width_inches <- as.numeric(values[[4L]])
  if (
    any(!is.finite(x_inches)) || any(!is.finite(z_inches)) ||
      any(!is.finite(zone_half_height_inches)) ||
      any(zone_half_height_inches <= 0) ||
      any(!is.finite(plate_half_width_inches)) ||
      any(plate_half_width_inches <= 0)
  ) {
    stop("Rounded-boundary normal inputs must define finite positive zones")
  }

  qx <- abs(x_inches) - plate_half_width_inches
  qz <- abs(z_inches) - zone_half_height_inches
  sign_x <- ifelse(x_inches < 0, -1, 1)
  sign_z <- ifelse(z_inches < 0, -1, 1)
  normal_x <- normal_z <- numeric(size)

  corner <- qx > 0 & qz > 0
  if (any(corner)) {
    radius <- sqrt(qx[corner]^2 + qz[corner]^2)
    normal_x[corner] <- sign_x[corner] * qx[corner] / radius
    normal_z[corner] <- sign_z[corner] * qz[corner] / radius
  }
  remaining <- !corner
  horizontal <- remaining & qx >= qz
  vertical <- remaining & !horizontal
  normal_x[horizontal] <- sign_x[horizontal]
  normal_z[vertical] <- sign_z[vertical]

  norm <- sqrt(normal_x^2 + normal_z^2)
  if (any(abs(norm - 1) > 1e-10)) {
    stop("Rounded-boundary normals were not unit length")
  }
  data.table::data.table(normal_x = normal_x, normal_z = normal_z)
}

continuous_normal_transition_sd <- function(
  sigma_inches, normal_x, normal_z, anisotropy_ratio = 1
) {
  lengths <- c(
    length(sigma_inches), length(normal_x), length(normal_z),
    length(anisotropy_ratio)
  )
  size <- max(lengths)
  if (any(lengths != 1L & lengths != size)) {
    stop("Transition-width inputs must be scalar or row-aligned")
  }
  sigma_inches <- rep_len(as.numeric(sigma_inches), size)
  normal_x <- rep_len(as.numeric(normal_x), size)
  normal_z <- rep_len(as.numeric(normal_z), size)
  anisotropy_ratio <- rep_len(as.numeric(anisotropy_ratio), size)
  if (
    any(!is.finite(sigma_inches)) || any(sigma_inches <= 0) ||
      any(!is.finite(normal_x)) || any(!is.finite(normal_z)) ||
      any(!is.finite(anisotropy_ratio)) || any(anisotropy_ratio <= 0)
  ) {
    stop("Transition widths require positive scales and finite normals")
  }
  sigma_inches * sqrt(
    normal_x^2 * anisotropy_ratio^2 +
      normal_z^2 / anisotropy_ratio^2
  )
}

normalize_continuous_batter_input <- function(choices, require_swing = TRUE) {
  x <- continuous_allowlist(
    choices, continuous_batter_allowed_columns(),
    "continuous batter-perception input"
  )
  if (!"batter_id" %in% names(x) && "batter" %in% names(x)) {
    x[, batter_id := as.character(batter)]
  }
  if (!"swing" %in% names(x) && "description" %in% names(x)) {
    x[, swing := classify_swing_take(description)]
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
  required <- c(
    "batter_id", "plate_x", "plate_z", "sz_top", "sz_bot", "balls",
    "strikes", "pitch_family", "matchup"
  )
  if (isTRUE(require_swing)) required <- c(required, "swing")
  stop_if_missing_columns(x, required, "continuous batter-perception input")
  x[, `:=`(
    batter_id = as.character(batter_id),
    pitch_family = as.character(pitch_family),
    matchup = as.character(matchup),
    edge_distance_inches = abs_edge_distance_inches(
      plate_x, plate_z, sz_top, sz_bot
    ),
    x_location_inches = 12 * plate_x,
    z_location_inches = 12 * (plate_z - (sz_bot + sz_top) / 2),
    zone_half_height_inches = 6 * (sz_top - sz_bot),
    count_state = paste0(balls, "-", strikes)
  )]
  eligible <-
    !is.na(x$batter_id) & nzchar(x$batter_id) &
    is.finite(x$edge_distance_inches) &
    is.finite(x$x_location_inches) & is.finite(x$z_location_inches) &
    is.finite(x$zone_half_height_inches) & x$zone_half_height_inches > 0 &
    x$balls %in% 0:3 & x$strikes %in% 0:2 &
    !is.na(x$pitch_family) & !is.na(x$matchup)
  if (isTRUE(require_swing)) eligible <- eligible & x$swing %in% 0:1
  x <- x[eligible]
  if (!nrow(x)) stop("No eligible continuous batter-perception rows remain")
  normal <- continuous_abs_outward_normal(
    x$x_location_inches, x$z_location_inches, x$zone_half_height_inches
  )
  x[, `:=`(
    normal_x = normal$normal_x,
    normal_z = normal$normal_z
  )]
  x[]
}

raw_continuous_sector_basis <- function(x_inches, z_inches, harmonics = 2L) {
  harmonics <- as.integer(harmonics)
  if (length(harmonics) != 1L || harmonics < 1L) {
    stop("harmonics must be a positive integer")
  }
  angle <- atan2(z_inches, x_inches)
  result <- do.call(cbind, lapply(seq_len(harmonics), function(harmonic) {
    cbind(
      cos(harmonic * angle),
      sin(harmonic * angle)
    )
  }))
  colnames(result) <- unlist(lapply(seq_len(harmonics), function(harmonic) {
    c(paste0("sector_cos_", harmonic), paste0("sector_sin_", harmonic))
  }))
  storage.mode(result) <- "double"
  result
}

fit_continuous_sector_basis <- function(
  x_inches, z_inches, edge_distance_inches, harmonics = 2L
) {
  raw <- raw_continuous_sector_basis(x_inches, z_inches, harmonics)
  anchor <- cbind(intercept = 1, edge_distance = edge_distance_inches)
  projection <- solve(
    crossprod(anchor) + diag(c(1e-10, 1e-10), 2L),
    crossprod(anchor, raw)
  )
  residual <- raw - anchor %*% projection
  scale <- apply(residual, 2L, stats::sd)
  scale[!is.finite(scale) | scale < 1e-6] <- 1
  basis <- sweep(residual, 2L, scale, "/")
  list(
    basis = basis,
    specification = list(
      harmonics = as.integer(harmonics),
      projection = projection,
      scale = scale,
      column_names = colnames(raw)
    )
  )
}

score_continuous_sector_basis <- function(
  x_inches, z_inches, edge_distance_inches, specification
) {
  raw <- raw_continuous_sector_basis(
    x_inches, z_inches, specification$harmonics
  )
  raw <- raw[, specification$column_names, drop = FALSE]
  anchor <- cbind(intercept = 1, edge_distance = edge_distance_inches)
  residual <- raw - anchor %*% specification$projection
  basis <- sweep(residual, 2L, specification$scale, "/")
  storage.mode(basis) <- "double"
  basis
}

prepare_continuous_batter_perception <- function(
  choices, harmonics = 2L,
  anisotropy = c("isotropic", "shared")
) {
  anisotropy <- match.arg(anisotropy)
  x <- normalize_continuous_batter_input(choices, require_swing = TRUE)
  player_table <- x[, .(
    training_opportunities = .N,
    training_swings = sum(swing),
    training_swing_rate = mean(swing)
  ), by = batter_id]
  data.table::setorder(player_table, batter_id)
  player_table[, player_index := .I]
  x[player_table, on = "batter_id", player_index := i.player_index]

  context <- fit_continuous_design(
    x, categorical = c("count_state", "pitch_family", "matchup")
  )
  sector <- fit_continuous_sector_basis(
    x$x_location_inches, x$z_location_inches,
    x$edge_distance_inches, harmonics = harmonics
  )
  lower_target <- c(0.08, 0.18, 0.35)
  upper_target <- c(0.70, 0.88, 0.97)
  conditional_range <- (upper_target - lower_target) / (1 - lower_target)
  stan_data <- list(
    N = nrow(x),
    P = nrow(player_table),
    K = ncol(context$matrix),
    S = ncol(sector$basis),
    swing = as.integer(x$swing),
    player = as.integer(x$player_index),
    strike_group = as.integer(x$strikes) + 1L,
    edge_distance = as.numeric(x$edge_distance_inches),
    normal_x = as.numeric(x$normal_x),
    normal_z = as.numeric(x$normal_z),
    estimate_anisotropy = as.integer(anisotropy == "shared"),
    X = context$matrix,
    sector_basis = sector$basis,
    lower_prior_mean = stats::qlogis(lower_target),
    range_prior_mean = stats::qlogis(conditional_range)
  )
  list(
    data = stan_data,
    rows = x,
    player_table = player_table,
    context_specification = context$specification,
    sector_specification = sector$specification,
    anisotropy = anisotropy,
    information_set = c(
      "swing", "tracked_location", "batter", "count", "pitch_family",
      "handedness_matchup"
    )
  )
}

default_continuous_batter_stan_file <- function() {
  file.path(
    continuous_model_root(), "scripts", "stan",
    "continuous_batter_perception.stan"
  )
}

fit_continuous_batter_perception <- function(
  choices, harmonics = 2L, backend = c("cmdstanr", "rstan"),
  chains = 4L, parallel_chains = chains, iter_warmup = 1000L,
  iter_sampling = 1000L, seed = 42L, adapt_delta = 0.97,
  max_treedepth = 12L, refresh = 100L, stan_file = NULL, file = NULL,
  force_refit = FALSE, anisotropy = c("isotropic", "shared")
) {
  anisotropy <- match.arg(anisotropy)
  if (!is.null(file) && file.exists(file) && !isTRUE(force_refit)) {
    cached <- readRDS(file)
    cached_anisotropy <- if (is.null(cached$anisotropy)) {
      "isotropic"
    } else {
      cached$anisotropy
    }
    if (
      inherits(cached, "continuous_batter_perception_fit") &&
        identical(cached_anisotropy, anisotropy)
    ) {
      return(cached)
    }
  }
  backend <- match.arg(backend)
  bundle <- prepare_continuous_batter_perception(
    choices, harmonics, anisotropy
  )
  if (is.null(stan_file)) stan_file <- default_continuous_batter_stan_file()
  stan_fit <- continuous_stan_fit(
    stan_file, bundle$data, backend = backend, chains = chains,
    parallel_chains = parallel_chains, iter_warmup = iter_warmup,
    iter_sampling = iter_sampling, seed = seed, adapt_delta = adapt_delta,
    max_treedepth = max_treedepth, refresh = refresh
  )
  fit <- list(
    stan_fit = stan_fit,
    player_table = bundle$player_table,
    context_specification = bundle$context_specification,
    sector_specification = bundle$sector_specification,
    training_games = sort(unique(as.character(bundle$rows$game_pk))),
    training_rows = nrow(bundle$rows),
    backend = backend,
    anisotropy = bundle$anisotropy,
    information_set = bundle$information_set
  )
  class(fit) <- "continuous_batter_perception_fit"
  if (!is.null(file)) saveRDS(fit, file)
  fit
}

draws_continuous_batter_perception <- function(
  fit, ndraws = NULL, seed = 42L
) {
  if (!inherits(fit, "continuous_batter_perception_fit")) {
    stop("fit must be a continuous_batter_perception_fit")
  }
  variables <- c(
    "mu_log_sigma", "tau_log_sigma", "sigma_player", "mu_threshold",
    "tau_threshold", "threshold_player", "beta_context", "beta_sector",
    "lower_swing", "upper_swing", "log_anisotropy", "anisotropy_ratio"
  )
  legacy_variables <- setdiff(
    variables, c("log_anisotropy", "anisotropy_ratio")
  )
  draw_matrix <- tryCatch(
    continuous_draw_matrix(fit$stan_fit, variables),
    error = function(error) continuous_draw_matrix(
      fit$stan_fit, legacy_variables
    )
  )
  draws <- continuous_thin_draws(draw_matrix, ndraws, seed)
  has_anisotropy <- all(
    c("log_anisotropy", "anisotropy_ratio") %in% colnames(draws)
  )
  list(
    draw_id = seq_len(nrow(draws)),
    mu_log_sigma = continuous_extract_scalar(draws, "mu_log_sigma"),
    tau_log_sigma = continuous_extract_scalar(draws, "tau_log_sigma"),
    sigma_player = continuous_extract_vector(
      draws, "sigma_player", nrow(fit$player_table)
    ),
    mu_threshold = continuous_extract_scalar(draws, "mu_threshold"),
    tau_threshold = continuous_extract_scalar(draws, "tau_threshold"),
    threshold_player = continuous_extract_vector(
      draws, "threshold_player", nrow(fit$player_table)
    ),
    beta_context = continuous_extract_vector(
      draws, "beta_context",
      sum(vapply(
        fit$context_specification$categorical,
        function(entry) length(entry$levels) - 1L, integer(1L)
      )) + length(fit$context_specification$numeric)
    ),
    beta_sector = continuous_extract_matrix(
      draws, "beta_sector", 3L,
      length(fit$sector_specification$column_names)
    ),
    lower_swing = continuous_extract_vector(draws, "lower_swing", 3L),
    upper_swing = continuous_extract_vector(draws, "upper_swing", 3L),
    log_anisotropy = if (has_anisotropy) {
      continuous_extract_scalar(draws, "log_anisotropy")
    } else {
      rep.int(0, nrow(draws))
    },
    anisotropy_ratio = if (has_anisotropy) {
      continuous_extract_scalar(draws, "anisotropy_ratio")
    } else {
      rep.int(1, nrow(draws))
    }
  )
}

continuous_batter_scoring_features <- function(choices, fit) {
  x <- normalize_continuous_batter_input(choices, require_swing = FALSE)
  context <- score_continuous_design(x, fit$context_specification)
  sector <- score_continuous_sector_basis(
    x$x_location_inches, x$z_location_inches, x$edge_distance_inches,
    fit$sector_specification
  )
  list(rows = x, context = context, sector = sector)
}

score_continuous_batter_perception <- function(
  fit, choices, ndraws = 400L, seed = 42L,
  new_player = c("population_mean", "sample"), return_draws = FALSE
) {
  if (!inherits(fit, "continuous_batter_perception_fit")) {
    stop("fit must be a continuous_batter_perception_fit")
  }
  new_player <- match.arg(new_player)
  features <- continuous_batter_scoring_features(choices, fit)
  x <- features$rows
  posterior <- draws_continuous_batter_perception(fit, ndraws, seed)
  player <- match(as.character(x$batter_id), fit$player_table$batter_id)
  unseen_names <- sort(unique(as.character(x$batter_id[is.na(player)])))
  unseen <- match(as.character(x$batter_id), unseen_names)
  strike <- as.integer(x$strikes) + 1L
  probability <- matrix(NA_real_, nrow(x), length(posterior$draw_id))
  sigma_used <- threshold_used <- probability
  transition_sd <- probability
  set.seed(seed + 1L)
  for (draw in seq_along(posterior$draw_id)) {
    sigma <- threshold <- numeric(nrow(x))
    seen <- !is.na(player)
    sigma[seen] <- posterior$sigma_player[draw, player[seen]]
    threshold[seen] <- posterior$threshold_player[draw, player[seen]]
    if (any(!seen)) {
      if (new_player == "sample") {
        sigma_new <- exp(
          posterior$mu_log_sigma[[draw]] +
            posterior$tau_log_sigma[[draw]] * stats::rnorm(length(unseen_names))
        )
        threshold_new <- posterior$mu_threshold[[draw]] +
          posterior$tau_threshold[[draw]] * stats::rnorm(length(unseen_names))
        sigma[!seen] <- sigma_new[unseen[!seen]]
        threshold[!seen] <- threshold_new[unseen[!seen]]
      } else {
        sigma[!seen] <- exp(
          posterior$mu_log_sigma[[draw]] +
            0.5 * posterior$tau_log_sigma[[draw]]^2
        )
        threshold[!seen] <- posterior$mu_threshold[[draw]]
      }
    }
    context_shift <- if (ncol(features$context)) {
      as.numeric(features$context %*% posterior$beta_context[draw, ])
    } else {
      numeric(nrow(x))
    }
    sector_shift <- numeric(nrow(x))
    for (group in 1:3) {
      rows <- which(strike == group)
      if (length(rows)) {
        sector_shift[rows] <- as.numeric(
          features$sector[rows, , drop = FALSE] %*%
            posterior$beta_sector[draw, group, ]
        )
      }
    }
    local_sd <- continuous_normal_transition_sd(
      sigma, x$normal_x, x$normal_z,
      posterior$anisotropy_ratio[[draw]]
    )
    inside_probability <- stats::pnorm(
      (threshold + context_shift + sector_shift - x$edge_distance_inches) /
        local_sd
    )
    lower <- posterior$lower_swing[draw, strike]
    upper <- posterior$upper_swing[draw, strike]
    probability[, draw] <- pmin(
      1 - 1e-9,
      pmax(1e-9, lower + (upper - lower) * inside_probability)
    )
    sigma_used[, draw] <- sigma
    transition_sd[, draw] <- local_sd
    threshold_used[, draw] <- threshold
  }
  result <- data.table::copy(x)
  result <- cbind(
    result,
    continuous_probability_summary(probability, "swing_probability_"),
    continuous_probability_summary(sigma_used, "sigma_inches_"),
    continuous_probability_summary(
      transition_sd, "normal_transition_sd_inches_"
    ),
    continuous_probability_summary(threshold_used, "threshold_inches_")
  )
  result[, `:=`(
    batter_seen_in_training = !is.na(player),
    anisotropy_mode = if (is.null(fit$anisotropy)) {
      "isotropic"
    } else {
      fit$anisotropy
    },
    anisotropy_ratio_mean = mean(posterior$anisotropy_ratio),
    anisotropy_ratio_lower_95 = as.numeric(stats::quantile(
      posterior$anisotropy_ratio, 0.025, names = FALSE
    )),
    anisotropy_ratio_upper_95 = as.numeric(stats::quantile(
      posterior$anisotropy_ratio, 0.975, names = FALSE
    ))
  )]
  if (isTRUE(return_draws)) {
    return(list(
      summary = result,
      draw_id = posterior$draw_id,
      swing_probability = probability,
      sigma_inches = sigma_used,
      normal_transition_sd_inches = transition_sd,
      anisotropy_ratio = posterior$anisotropy_ratio,
      threshold_inches = threshold_used
    ))
  }
  result[]
}

expand_continuous_batter_simulation_parameter <- function(
  value, batter_id, label
) {
  original_names <- names(value)
  value <- as.numeric(value)
  if (length(value) == 1L) return(rep.int(value, length(batter_id)))
  if (length(value) == length(batter_id)) return(value)
  if (!is.null(original_names)) {
    matched <- match(as.character(batter_id), original_names)
    if (!anyNA(matched)) return(value[matched])
  }
  stop(label, " must be scalar, row-aligned, or named by batter")
}

simulate_continuous_batter_anisotropy <- function(
  choices, sigma_inches = 3, anisotropy_ratio = 1,
  threshold_inches = 0, lower_swing = c(0.08, 0.18, 0.35),
  upper_swing = c(0.70, 0.88, 0.97), seed = 42L
) {
  x <- normalize_continuous_batter_input(choices, require_swing = FALSE)
  sigma <- expand_continuous_batter_simulation_parameter(
    sigma_inches, x$batter_id, "sigma_inches"
  )
  threshold <- expand_continuous_batter_simulation_parameter(
    threshold_inches, x$batter_id, "threshold_inches"
  )
  anisotropy_ratio <- as.numeric(anisotropy_ratio)
  if (
    length(anisotropy_ratio) != 1L || !is.finite(anisotropy_ratio) ||
      anisotropy_ratio <= 0
  ) {
    stop("anisotropy_ratio must be one positive number")
  }
  if (
    length(lower_swing) != 3L || length(upper_swing) != 3L ||
      any(!is.finite(lower_swing)) || any(!is.finite(upper_swing)) ||
      any(lower_swing < 0) || any(upper_swing > 1) ||
      any(lower_swing >= upper_swing)
  ) {
    stop("Swing lapse limits must define three increasing probability ranges")
  }
  local_sd <- continuous_normal_transition_sd(
    sigma, x$normal_x, x$normal_z, anisotropy_ratio
  )
  strike_group <- as.integer(x$strikes) + 1L
  perceived_inside <- stats::pnorm(
    (threshold - x$edge_distance_inches) / local_sd
  )
  probability <- lower_swing[strike_group] +
    (upper_swing[strike_group] - lower_swing[strike_group]) *
      perceived_inside
  set.seed(seed)
  x[, `:=`(
    swing_probability = probability,
    swing = stats::rbinom(.N, 1L, probability),
    simulation_sigma_inches = sigma,
    simulation_threshold_inches = threshold,
    simulation_anisotropy_ratio = anisotropy_ratio,
    simulation_normal_transition_sd_inches = local_sd
  )]
  x[]
}

validate_continuous_batter_anisotropy_recovery <- function(
  fit_or_draws, true_anisotropy_ratio, ndraws = NULL, seed = 42L,
  maximum_absolute_log_error = log(1.25)
) {
  truth <- as.numeric(true_anisotropy_ratio)
  if (length(truth) != 1L || !is.finite(truth) || truth <= 0) {
    stop("true_anisotropy_ratio must be one positive number")
  }
  if (
    length(maximum_absolute_log_error) != 1L ||
      !is.finite(maximum_absolute_log_error) ||
      maximum_absolute_log_error < 0
  ) {
    stop("maximum_absolute_log_error must be one nonnegative number")
  }
  draws <- if (inherits(fit_or_draws, "continuous_batter_perception_fit")) {
    draws_continuous_batter_perception(
      fit_or_draws, ndraws = ndraws, seed = seed
    )$anisotropy_ratio
  } else {
    as.numeric(fit_or_draws)
  }
  if (!length(draws) || any(!is.finite(draws)) || any(draws <= 0)) {
    stop("Anisotropy recovery requires positive finite posterior draws")
  }
  interval <- as.numeric(stats::quantile(
    draws, c(0.025, 0.975), names = FALSE
  ))
  median <- stats::median(draws)
  log_error <- abs(log(median) - log(truth))
  direction_recovered <- if (truth > 1) {
    median > 1
  } else if (truth < 1) {
    median < 1
  } else {
    interval[[1L]] <= 1 && interval[[2L]] >= 1
  }
  coverage <- interval[[1L]] <= truth && interval[[2L]] >= truth
  data.table::data.table(
    true_anisotropy_ratio = truth,
    posterior_mean = mean(draws),
    posterior_median = median,
    lower_95 = interval[[1L]],
    upper_95 = interval[[2L]],
    absolute_log_error = log_error,
    maximum_absolute_log_error = maximum_absolute_log_error,
    interval_covers_truth = coverage,
    direction_recovered = direction_recovered,
    recovery_pass = coverage && direction_recovered &&
      log_error <= maximum_absolute_log_error
  )
}

compare_continuous_batter_anisotropy_heldout <- function(
  isotropic_scored, anisotropic_scored,
  probability_column = "swing_probability_mean"
) {
  isotropic <- data.table::copy(data.table::as.data.table(isotropic_scored))
  anisotropic <- data.table::copy(
    data.table::as.data.table(anisotropic_scored)
  )
  required <- c("game_pk", "swing", probability_column)
  stop_if_missing_columns(isotropic, required, "isotropic held-out scores")
  stop_if_missing_columns(
    anisotropic, required, "anisotropic held-out scores"
  )
  if (
    nrow(isotropic) != nrow(anisotropic) ||
      !identical(as.character(isotropic$game_pk),
        as.character(anisotropic$game_pk)) ||
      !identical(as.integer(isotropic$swing),
        as.integer(anisotropic$swing))
  ) {
    stop("Held-out isotropic and anisotropic rows are not aligned")
  }
  keys <- intersect(
    c("game_pk", "at_bat_number", "pitch_number"),
    intersect(names(isotropic), names(anisotropic))
  )
  if (length(keys) && !identical(isotropic[, ..keys], anisotropic[, ..keys])) {
    stop("Held-out pitch keys are not aligned")
  }
  epsilon <- 1e-9
  probability_isotropic <- pmin(
    1 - epsilon, pmax(epsilon, as.numeric(isotropic[[probability_column]]))
  )
  probability_anisotropic <- pmin(
    1 - epsilon,
    pmax(epsilon, as.numeric(anisotropic[[probability_column]]))
  )
  observed <- as.integer(isotropic$swing)
  loss_isotropic <- -(
    observed * log(probability_isotropic) +
      (1 - observed) * log1p(-probability_isotropic)
  )
  loss_anisotropic <- -(
    observed * log(probability_anisotropic) +
      (1 - observed) * log1p(-probability_anisotropic)
  )
  comparison <- data.table::data.table(
    game_pk = isotropic$game_pk,
    improvement = loss_isotropic - loss_anisotropic
  )
  by_game <- comparison[, .(
    pitches = .N,
    mean_log_loss_improvement = mean(improvement)
  ), by = game_pk]
  improvement <- mean(by_game$mean_log_loss_improvement)
  clustered_se <- if (nrow(by_game) >= 2L) {
    stats::sd(by_game$mean_log_loss_improvement) / sqrt(nrow(by_game))
  } else {
    NA_real_
  }
  list(
    metrics = data.table::data.table(
      heldout_pitches = nrow(comparison),
      heldout_games = nrow(by_game),
      isotropic_log_loss = mean(loss_isotropic),
      anisotropic_log_loss = mean(loss_anisotropic),
      log_loss_improvement = improvement,
      game_clustered_se = clustered_se,
      one_clustered_se_pass = is.finite(clustered_se) &&
        improvement >= clustered_se
    ),
    by_game = by_game
  )
}

gate_continuous_batter_anisotropy <- function(
  heldout_validation, recovery_validation,
  minimum_clustered_standard_errors = 1
) {
  heldout <- if (is.list(heldout_validation)) {
    data.table::as.data.table(heldout_validation$metrics)
  } else {
    data.table::as.data.table(heldout_validation)
  }
  recovery <- data.table::as.data.table(recovery_validation)
  stop_if_missing_columns(
    heldout, c("log_loss_improvement", "game_clustered_se"),
    "anisotropy held-out validation"
  )
  stop_if_missing_columns(
    recovery, "recovery_pass", "anisotropy recovery validation"
  )
  if (nrow(heldout) != 1L || nrow(recovery) != 1L) {
    stop("Anisotropy gates require one held-out and one recovery summary")
  }
  minimum_clustered_standard_errors <- as.numeric(
    minimum_clustered_standard_errors
  )
  if (
    length(minimum_clustered_standard_errors) != 1L ||
      !is.finite(minimum_clustered_standard_errors) ||
      minimum_clustered_standard_errors < 0
  ) {
    stop("minimum_clustered_standard_errors must be nonnegative")
  }
  predictive_pass <-
    is.finite(heldout$game_clustered_se) &&
    heldout$log_loss_improvement >=
      minimum_clustered_standard_errors * heldout$game_clustered_se
  recovery_pass <- isTRUE(recovery$recovery_pass)
  data.table::data.table(
    candidate = "shared_horizontal_vertical_anisotropy",
    log_loss_improvement = heldout$log_loss_improvement[[1L]],
    game_clustered_se = heldout$game_clustered_se[[1L]],
    required_standard_errors = minimum_clustered_standard_errors,
    predictive_pass = predictive_pass,
    recovery_pass = recovery_pass,
    promote_shared_anisotropy = predictive_pass && recovery_pass,
    selected_model = if (predictive_pass && recovery_pass) {
      "shared"
    } else {
      "isotropic"
    }
  )
}
