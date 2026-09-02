continuous_call_cue_allowed_columns <- function() {
  c(
    "game_pk", "at_bat_number", "pitch_number", "initial_call", "swing",
    "description", "plate_x", "plate_z", "sz_top", "sz_bot", "balls",
    "strikes", "pitch_family", "pitch_type", "matchup", "stand", "p_throws",
    "umpire_id", "umpire_name", "catcher_id", "fielder_2"
  )
}

normalize_continuous_call_cue_input <- function(pitches) {
  x <- continuous_allowlist(
    pitches, continuous_call_cue_allowed_columns(),
    "continuous initial-call input"
  )
  if (!"umpire_id" %in% names(x) && "umpire_name" %in% names(x)) {
    x[, umpire_id := as.character(umpire_name)]
  }
  if (!"catcher_id" %in% names(x) && "fielder_2" %in% names(x)) {
    x[, catcher_id := as.character(fielder_2)]
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
  if (!"swing" %in% names(x) && "description" %in% names(x)) {
    x[, swing := classify_swing_take(description)]
  }
  stop_if_missing_columns(
    x,
    c(
      "initial_call", "plate_x", "plate_z", "sz_top", "sz_bot", "balls",
      "strikes", "pitch_family", "matchup", "umpire_id", "catcher_id"
    ),
    "continuous initial-call input"
  )
  if ("swing" %in% names(x)) x <- x[is.na(swing) | swing == 0L]
  x <- x[initial_call %in% c("ball", "called_strike")]
  x[, `:=`(
    called_strike = as.integer(initial_call == "called_strike"),
    umpire_id = as.character(umpire_id),
    catcher_id = as.character(catcher_id),
    pitch_family = as.character(pitch_family),
    matchup = as.character(matchup),
    count_state = paste0(balls, "-", strikes),
    edge_distance_inches = abs_edge_distance_inches(
      plate_x, plate_z, sz_top, sz_bot
    ),
    x_location_inches = 12 * plate_x,
    z_location_inches = 12 * (plate_z - (sz_bot + sz_top) / 2)
  )]
  x <- x[
    !is.na(umpire_id) & nzchar(umpire_id) &
      !is.na(catcher_id) & nzchar(catcher_id) &
      is.finite(edge_distance_inches) &
      is.finite(x_location_inches) & is.finite(z_location_inches) &
      balls %in% 0:3 & strikes %in% 0:2 &
      !is.na(pitch_family) & !is.na(matchup)
  ]
  if (!nrow(x)) stop("No eligible continuous initial-call rows remain")
  x[]
}

fit_continuous_edge_basis <- function(edge_distance, degrees_freedom = 6L) {
  degrees_freedom <- as.integer(degrees_freedom)
  if (degrees_freedom < 3L) stop("degrees_freedom must be at least three")
  basis <- splines::ns(
    edge_distance, df = degrees_freedom, intercept = FALSE
  )
  specification <- list(
    knots = attr(basis, "knots"),
    boundary_knots = attr(basis, "Boundary.knots"),
    intercept = FALSE,
    column_names = paste0("edge_spline_", seq_len(ncol(basis)))
  )
  colnames(basis) <- specification$column_names
  storage.mode(basis) <- "double"
  list(basis = basis, specification = specification)
}

score_continuous_edge_basis <- function(edge_distance, specification) {
  basis <- splines::ns(
    edge_distance,
    knots = specification$knots,
    Boundary.knots = specification$boundary_knots,
    intercept = specification$intercept
  )
  colnames(basis) <- specification$column_names
  storage.mode(basis) <- "double"
  basis
}

raw_continuous_call_rbf <- function(x_inches, z_inches, centers, bandwidth) {
  centers <- as.matrix(centers)
  result <- vapply(seq_len(nrow(centers)), function(index) {
    exp(-(
      (x_inches - centers[index, 1L])^2 +
        (z_inches - centers[index, 2L])^2
    ) / (2 * bandwidth^2))
  }, numeric(length(x_inches)))
  if (is.null(dim(result))) result <- matrix(result, ncol = 1L)
  colnames(result) <- paste0("call_rbf_", seq_len(ncol(result)))
  storage.mode(result) <- "double"
  result
}

fit_continuous_call_residual_basis <- function(
  x_inches, z_inches, edge_basis, rank = 12L, seed = 42L
) {
  location <- cbind(x_inches, z_inches)
  unique_locations <- nrow(unique(data.frame(location)))
  rank <- min(as.integer(rank), unique_locations, nrow(location))
  if (rank < 1L) stop("At least one residual-basis center is required")
  if (rank == 1L) {
    centers <- matrix(colMeans(location), nrow = 1L)
    bandwidth <- mean(apply(location, 2L, stats::sd))
  } else {
    set.seed(seed)
    centers <- stats::kmeans(
      location, centers = rank, nstart = 10L, iter.max = 100L
    )$centers
    centers <- centers[order(centers[, 2L], centers[, 1L]), , drop = FALSE]
    distances <- as.matrix(stats::dist(centers))
    diag(distances) <- Inf
    bandwidth <- stats::median(apply(distances, 1L, min))
  }
  if (!is.finite(bandwidth) || bandwidth < 1) bandwidth <- 6
  raw <- raw_continuous_call_rbf(
    x_inches, z_inches, centers, bandwidth
  )
  anchor <- cbind(intercept = 1, edge_basis)
  projection <- solve(
    crossprod(anchor) + diag(1e-8, ncol(anchor)),
    crossprod(anchor, raw)
  )
  residual <- raw - anchor %*% projection
  scale <- apply(residual, 2L, stats::sd)
  scale[!is.finite(scale) | scale < 1e-6] <- 1
  basis <- sweep(residual, 2L, scale, "/")
  list(
    basis = basis,
    specification = list(
      centers = centers,
      bandwidth = bandwidth,
      projection = projection,
      scale = scale,
      column_names = colnames(raw)
    )
  )
}

score_continuous_call_residual_basis <- function(
  x_inches, z_inches, edge_basis, specification
) {
  raw <- raw_continuous_call_rbf(
    x_inches, z_inches, specification$centers, specification$bandwidth
  )
  raw <- raw[, specification$column_names, drop = FALSE]
  anchor <- cbind(intercept = 1, edge_basis)
  residual <- raw - anchor %*% specification$projection
  basis <- sweep(residual, 2L, specification$scale, "/")
  storage.mode(basis) <- "double"
  basis
}

prepare_continuous_call_cue <- function(
  pitches, edge_degrees_freedom = 6L, residual_rank = 12L, seed = 42L
) {
  x <- normalize_continuous_call_cue_input(pitches)
  umpire_table <- x[, .(training_calls = .N), by = umpire_id]
  catcher_table <- x[, .(training_calls = .N), by = catcher_id]
  data.table::setorder(umpire_table, umpire_id)
  data.table::setorder(catcher_table, catcher_id)
  umpire_table[, umpire_index := .I]
  catcher_table[, catcher_index := .I]
  x[umpire_table, on = "umpire_id", umpire_index := i.umpire_index]
  x[catcher_table, on = "catcher_id", catcher_index := i.catcher_index]

  context <- fit_continuous_design(
    x, categorical = c("count_state", "pitch_family", "matchup")
  )
  edge <- fit_continuous_edge_basis(
    x$edge_distance_inches, edge_degrees_freedom
  )
  residual <- fit_continuous_call_residual_basis(
    x$x_location_inches, x$z_location_inches, edge$basis,
    residual_rank, seed
  )
  stan_data <- list(
    N = nrow(x),
    U = nrow(umpire_table),
    C = nrow(catcher_table),
    E = ncol(edge$basis),
    R = ncol(residual$basis),
    K = ncol(context$matrix),
    called_strike = as.integer(x$called_strike),
    umpire = as.integer(x$umpire_index),
    catcher = as.integer(x$catcher_index),
    edge_basis = edge$basis,
    residual_basis = residual$basis,
    X = context$matrix
  )
  list(
    data = stan_data,
    rows = x,
    umpire_table = umpire_table,
    catcher_table = catcher_table,
    context_specification = context$specification,
    edge_specification = edge$specification,
    residual_specification = residual$specification,
    information_set = c(
      "initial_call", "tracked_location", "umpire", "catcher", "count",
      "pitch_family", "handedness_matchup"
    )
  )
}

default_continuous_call_cue_stan_file <- function() {
  file.path(
    continuous_model_root(), "scripts", "stan", "continuous_call_cue.stan"
  )
}

fit_continuous_call_cue <- function(
  pitches, edge_degrees_freedom = 6L, residual_rank = 12L,
  backend = c("cmdstanr", "rstan"), chains = 4L,
  parallel_chains = chains, iter_warmup = 1000L, iter_sampling = 1000L,
  seed = 42L, adapt_delta = 0.97, max_treedepth = 12L,
  refresh = 100L, stan_file = NULL, file = NULL, force_refit = FALSE
) {
  if (!is.null(file) && file.exists(file) && !isTRUE(force_refit)) {
    cached <- readRDS(file)
    if (inherits(cached, "continuous_call_cue_fit")) return(cached)
  }
  backend <- match.arg(backend)
  bundle <- prepare_continuous_call_cue(
    pitches, edge_degrees_freedom, residual_rank, seed
  )
  if (is.null(stan_file)) stan_file <- default_continuous_call_cue_stan_file()
  stan_fit <- continuous_stan_fit(
    stan_file, bundle$data, backend = backend, chains = chains,
    parallel_chains = parallel_chains, iter_warmup = iter_warmup,
    iter_sampling = iter_sampling, seed = seed, adapt_delta = adapt_delta,
    max_treedepth = max_treedepth, refresh = refresh
  )
  fit <- list(
    stan_fit = stan_fit,
    umpire_table = bundle$umpire_table,
    catcher_table = bundle$catcher_table,
    context_specification = bundle$context_specification,
    edge_specification = bundle$edge_specification,
    residual_specification = bundle$residual_specification,
    training_games = sort(unique(as.character(bundle$rows$game_pk))),
    training_rows = nrow(bundle$rows),
    backend = backend,
    information_set = bundle$information_set
  )
  class(fit) <- "continuous_call_cue_fit"
  if (!is.null(file)) saveRDS(fit, file)
  fit
}

draws_continuous_call_cue <- function(fit, ndraws = NULL, seed = 42L) {
  if (!inherits(fit, "continuous_call_cue_fit")) {
    stop("fit must be a continuous_call_cue_fit")
  }
  variables <- c(
    "intercept", "beta_edge", "beta_residual", "beta_context",
    "sigma_umpire", "sigma_catcher", "umpire_effect", "catcher_effect"
  )
  draws <- continuous_thin_draws(
    continuous_draw_matrix(fit$stan_fit, variables), ndraws, seed
  )
  context_columns <- sum(vapply(
    fit$context_specification$categorical,
    function(entry) length(entry$levels) - 1L, integer(1L)
  )) + length(fit$context_specification$numeric)
  list(
    draw_id = seq_len(nrow(draws)),
    intercept = continuous_extract_scalar(draws, "intercept"),
    beta_edge = continuous_extract_vector(
      draws, "beta_edge", length(fit$edge_specification$column_names)
    ),
    beta_residual = continuous_extract_vector(
      draws, "beta_residual", length(fit$residual_specification$column_names)
    ),
    beta_context = continuous_extract_vector(
      draws, "beta_context", context_columns
    ),
    sigma_umpire = continuous_extract_scalar(draws, "sigma_umpire"),
    sigma_catcher = continuous_extract_scalar(draws, "sigma_catcher"),
    umpire_effect = continuous_extract_vector(
      draws, "umpire_effect", nrow(fit$umpire_table)
    ),
    catcher_effect = continuous_extract_vector(
      draws, "catcher_effect", nrow(fit$catcher_table)
    )
  )
}

continuous_call_cue_scoring_features <- function(pitches, fit) {
  x <- normalize_continuous_call_cue_input(pitches)
  context <- score_continuous_design(x, fit$context_specification)
  edge <- score_continuous_edge_basis(
    x$edge_distance_inches, fit$edge_specification
  )
  residual <- score_continuous_call_residual_basis(
    x$x_location_inches, x$z_location_inches, edge,
    fit$residual_specification
  )
  list(rows = x, context = context, edge = edge, residual = residual)
}

score_continuous_call_cue <- function(
  fit, pitches, ndraws = 400L, seed = 42L,
  new_level = c("population_mean", "sample"), return_draws = FALSE
) {
  if (!inherits(fit, "continuous_call_cue_fit")) {
    stop("fit must be a continuous_call_cue_fit")
  }
  new_level <- match.arg(new_level)
  features <- continuous_call_cue_scoring_features(pitches, fit)
  x <- features$rows
  posterior <- draws_continuous_call_cue(fit, ndraws, seed)
  umpire <- match(as.character(x$umpire_id), fit$umpire_table$umpire_id)
  catcher <- match(as.character(x$catcher_id), fit$catcher_table$catcher_id)
  umpire_unseen <- match(
    as.character(x$umpire_id),
    sort(unique(as.character(x$umpire_id[is.na(umpire)])))
  )
  catcher_unseen <- match(
    as.character(x$catcher_id),
    sort(unique(as.character(x$catcher_id[is.na(catcher)])))
  )
  probability <- matrix(NA_real_, nrow(x), length(posterior$draw_id))
  set.seed(seed + 1L)
  for (draw in seq_along(posterior$draw_id)) {
    umpire_effect <- catcher_effect <- numeric(nrow(x))
    seen_umpire <- !is.na(umpire)
    seen_catcher <- !is.na(catcher)
    umpire_effect[seen_umpire] <-
      posterior$umpire_effect[draw, umpire[seen_umpire]]
    catcher_effect[seen_catcher] <-
      posterior$catcher_effect[draw, catcher[seen_catcher]]
    if (new_level == "sample") {
      if (any(!seen_umpire)) {
        values <- stats::rnorm(
          max(umpire_unseen, na.rm = TRUE),
          0, posterior$sigma_umpire[[draw]]
        )
        umpire_effect[!seen_umpire] <- values[umpire_unseen[!seen_umpire]]
      }
      if (any(!seen_catcher)) {
        values <- stats::rnorm(
          max(catcher_unseen, na.rm = TRUE),
          0, posterior$sigma_catcher[[draw]]
        )
        catcher_effect[!seen_catcher] <- values[catcher_unseen[!seen_catcher]]
      }
    }
    linear_predictor <- posterior$intercept[[draw]] +
      as.numeric(features$edge %*% posterior$beta_edge[draw, ]) +
      as.numeric(features$residual %*% posterior$beta_residual[draw, ]) +
      umpire_effect + catcher_effect
    if (ncol(features$context)) {
      linear_predictor <- linear_predictor + as.numeric(
        features$context %*% posterior$beta_context[draw, ]
      )
    }
    probability[, draw] <- stats::plogis(linear_predictor)
  }
  observed_call_probability <- probability
  ball_rows <- which(x$initial_call == "ball")
  if (length(ball_rows)) {
    observed_call_probability[ball_rows, ] <-
      1 - probability[ball_rows, , drop = FALSE]
  }
  result <- data.table::copy(x)
  result <- cbind(
    result,
    continuous_probability_summary(
      probability, "called_strike_probability_"
    ),
    continuous_probability_summary(
      observed_call_probability, "initial_call_probability_"
    )
  )
  result[, `:=`(
    umpire_seen_in_training = !is.na(umpire),
    catcher_seen_in_training = !is.na(catcher)
  )]
  if (isTRUE(return_draws)) {
    return(list(
      summary = result,
      draw_id = posterior$draw_id,
      called_strike_probability = probability,
      initial_call_probability = observed_call_probability
    ))
  }
  result[]
}
