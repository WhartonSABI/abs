continuous_call_trust_fixed_omegas <- function() c(0, 0.5, 1)

continuous_call_trust_forbidden_outcomes <- function() {
  c(
    "actual_wrong", "call_wrong", "abs_call", "challenge_outcome",
    "is_overturned", "overturned", "policy_success", "challenge_success",
    "final_call", "official_abs_call", "official_outcome", "official_success"
  )
}

default_continuous_call_trust_stan_file <- function() {
  root <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd()),
    error = function(e) getwd()
  )
  file.path(root, "scripts", "stan", "continuous_human_decision_trust.stan")
}

continuous_call_trust_stage_id <- function(omega) {
  value <- format(as.numeric(omega), trim = TRUE, scientific = FALSE)
  paste0("omega_", gsub("\\.", "p", value))
}

validate_continuous_call_trust_grid <- function(
  q_grid, omega_grid, signal_weights, n_rows = NULL
) {
  omega_grid <- as.numeric(omega_grid)
  if (length(omega_grid) < 3L || any(!is.finite(omega_grid)) ||
      any(diff(omega_grid) <= 0)) {
    stop("omega_grid must contain at least three finite, strictly increasing knots")
  }
  if (abs(omega_grid[[1L]]) > 1e-12 ||
      abs(tail(omega_grid, 1L) - 1.5) > 1e-12) {
    stop("omega_grid must span exactly [0, 1.5]")
  }

  q <- as.array(q_grid)
  dimensions <- dim(q)
  if (is.null(dimensions)) stop("q_grid must be a matrix or a three-dimensional array")
  if (length(dimensions) == 2L) {
    q <- array(q, dim = c(dimensions[[1L]], 1L, dimensions[[2L]]))
    dimensions <- dim(q)
  }
  if (length(dimensions) != 3L) {
    stop("q_grid dimensions must be decision row x signal x omega knot")
  }
  if (dimensions[[3L]] != length(omega_grid)) {
    stop("The final q_grid dimension must equal length(omega_grid)")
  }
  if (!is.null(n_rows) && dimensions[[1L]] != as.integer(n_rows)) {
    stop("The first q_grid dimension does not match the decision rows")
  }
  if (any(!is.finite(q)) || any(q < 0 | q > 1)) {
    stop("Every precomputed q value must be finite and in [0, 1]")
  }

  n <- dimensions[[1L]]
  signals <- dimensions[[2L]]
  weights <- as.matrix(signal_weights)
  if (is.null(dim(signal_weights)) || nrow(weights) == 1L) {
    values <- as.numeric(signal_weights)
    if (length(values) != signals) {
      stop("A signal-weight vector must have one value per signal")
    }
    weights <- matrix(rep(values, each = n), nrow = n, ncol = signals)
  }
  if (!identical(dim(weights), c(n, signals))) {
    stop("signal_weights must be an N x S matrix or a length-S vector")
  }
  if (any(!is.finite(weights)) || any(weights < 0)) {
    stop("Signal weights must be finite and nonnegative")
  }
  totals <- rowSums(weights)
  if (any(!is.finite(totals)) || any(totals <= 0)) {
    stop("Every decision row must have positive total signal weight")
  }
  weights <- weights / totals

  list(
    q_grid = q,
    omega_grid = omega_grid,
    signal_weights = weights,
    N = n,
    S = signals,
    G = length(omega_grid)
  )
}

# q(omega) is approximated by a continuous piecewise-linear interpolant over
# precomputed signal-specific probabilities. The only non-smooth points are the
# fixed knots, a measure-zero set for the continuous omega posterior. A dense
# grid plus continuous_call_trust_grid_convergence() controls this approximation.
continuous_call_trust_interpolate_validated <- function(validated, omega) {
  omega <- as.numeric(omega)
  if (length(omega) != 1L || !is.finite(omega) ||
      omega < validated$omega_grid[[1L]] ||
      omega > tail(validated$omega_grid, 1L)) {
    stop("omega must be one finite value inside the precomputed grid")
  }
  lower <- findInterval(omega, validated$omega_grid, all.inside = TRUE)
  lower <- min(lower, validated$G - 1L)
  fraction <- (omega - validated$omega_grid[[lower]]) /
    (validated$omega_grid[[lower + 1L]] - validated$omega_grid[[lower]])
  value <- (1 - fraction) * validated$q_grid[, , lower, drop = FALSE] +
    fraction * validated$q_grid[, , lower + 1L, drop = FALSE]
  matrix(value, nrow = validated$N, ncol = validated$S)
}

continuous_call_trust_interpolate <- function(q_grid, omega_grid, omega) {
  validated <- validate_continuous_call_trust_grid(
    q_grid,
    omega_grid,
    signal_weights = rep(
      1,
      if (length(dim(q_grid)) == 3L) dim(q_grid)[[2L]] else 1L
    )
  )
  continuous_call_trust_interpolate_validated(validated, omega)
}

continuous_call_trust_weighted_quantile <- function(x, weight, probability) {
  keep <- is.finite(x) & is.finite(weight) & weight > 0
  x <- x[keep]
  weight <- weight[keep]
  if (!length(x)) return(NA_real_)
  ordering <- order(x)
  x <- x[ordering]
  weight <- weight[ordering]
  x[[which(cumsum(weight) / sum(weight) >= probability)[[1L]]]]
}

continuous_call_trust_grid_convergence <- function(
  q_grid, omega_grid, signal_weights, coarse_stride = 2L,
  mean_tolerance = 1e-4, p99_tolerance = 1e-3
) {
  validated <- validate_continuous_call_trust_grid(
    q_grid, omega_grid, signal_weights
  )
  coarse_stride <- as.integer(coarse_stride)
  if (coarse_stride < 2L || validated$G < 5L) {
    stop("Grid convergence needs at least five knots and coarse_stride >= 2")
  }
  coarse_index <- unique(c(
    seq.int(1L, validated$G, by = coarse_stride), validated$G
  ))
  if (length(coarse_index) < 3L) stop("The thinned comparison grid is too small")
  check_index <- setdiff(seq_len(validated$G), coarse_index)
  if (!length(check_index)) stop("The comparison leaves no held-out omega knots")
  coarse_q <- validated$q_grid[, , coarse_index, drop = FALSE]
  coarse_omega <- validated$omega_grid[coarse_index]

  by_omega <- data.table::rbindlist(lapply(check_index, function(index) {
    estimate <- continuous_call_trust_interpolate(
      coarse_q, coarse_omega, validated$omega_grid[[index]]
    )
    truth <- validated$q_grid[, , index]
    error <- abs(estimate - truth)
    weight <- validated$signal_weights
    data.table::data.table(
      omega = validated$omega_grid[[index]],
      weighted_mean_absolute_error = sum(error * weight) / sum(weight),
      weighted_p99_absolute_error = continuous_call_trust_weighted_quantile(
        as.numeric(error), as.numeric(weight), 0.99
      ),
      maximum_absolute_error = max(error)
    )
  }))
  summary <- data.table::data.table(
    knots = validated$G,
    comparison_knots = length(coarse_index),
    evaluated_knots = length(check_index),
    maximum_weighted_mean_absolute_error = max(
      by_omega$weighted_mean_absolute_error
    ),
    maximum_weighted_p99_absolute_error = max(
      by_omega$weighted_p99_absolute_error
    ),
    mean_tolerance = as.numeric(mean_tolerance),
    p99_tolerance = as.numeric(p99_tolerance)
  )
  summary[, pass :=
    maximum_weighted_mean_absolute_error <= mean_tolerance &
      maximum_weighted_p99_absolute_error <= p99_tolerance]
  list(summary = summary, by_omega = by_omega, coarse_index = coarse_index)
}

continuous_call_trust_assert_no_leakage <- function(
  decision_rows, covariates = character(), mapped_columns = character()
) {
  forbidden <- continuous_call_trust_forbidden_outcomes()
  leaked_predictors <- intersect(
    c(as.character(covariates), as.character(mapped_columns)), forbidden
  )
  if (length(leaked_predictors)) {
    stop(
      "Outcome fields cannot enter the decision likelihood: ",
      paste(leaked_predictors, collapse = ", ")
    )
  }
  # Extra columns in decision_rows are deliberately ignored. Returning the
  # forbidden names makes that quarantine auditable without making canonical
  # ledger input inconvenient.
  intersect(names(decision_rows), forbidden)
}

prepare_continuous_call_trust_data <- function(
  decision_rows, q_grid, omega_grid, signal_weights,
  covariates = character(), fixed_omega = NULL,
  outcome_col = "challenged", player_col = "batter_id",
  team_col = "bat_team_id", gain_col = "stake_G",
  inventory_loss_col = "inventory_loss", cluster_col = "game_pk",
  offset_col = "sampling_offset",
  omega_prior_mean = 0.5, omega_prior_sd = 0.5
) {
  x <- data.table::copy(data.table::as.data.table(decision_rows))
  required <- c(
    outcome_col, player_col, team_col, gain_col, inventory_loss_col,
    cluster_col, covariates
  )
  stop_if_missing_columns(x, required, "continuous call-trust decisions")
  excluded_outcome_columns <- continuous_call_trust_assert_no_leakage(
    x,
    covariates,
    mapped_columns = c(
      outcome_col, player_col, team_col, gain_col, inventory_loss_col,
      offset_col, cluster_col
    )
  )
  grid <- validate_continuous_call_trust_grid(
    q_grid, omega_grid, signal_weights, n_rows = nrow(x)
  )
  y <- as.integer(x[[outcome_col]])
  if (anyNA(y) || any(!y %in% 0:1)) {
    stop("The challenge decision outcome must contain only zero and one")
  }
  player_id <- as.character(x[[player_col]])
  team_id <- as.character(x[[team_col]])
  if (anyNA(player_id) || any(!nzchar(player_id)) ||
      anyNA(team_id) || any(!nzchar(team_id))) {
    stop("Every decision needs nonmissing player and team identifiers")
  }
  player_table <- data.table::data.table(player_id = sort(unique(player_id)))
  team_table <- data.table::data.table(team_id = sort(unique(team_id)))
  player_table[, player_index := .I]
  team_table[, team_index := .I]
  player <- match(player_id, player_table$player_id)
  team <- match(team_id, team_table$team_id)
  gain <- as.numeric(x[[gain_col]])
  inventory_loss <- as.numeric(x[[inventory_loss_col]])
  if (any(!is.finite(gain)) || any(gain < 0) ||
      any(!is.finite(inventory_loss)) || any(inventory_loss < 0)) {
    stop("Decision gains and inventory losses must be finite and nonnegative")
  }

  if (length(covariates)) {
    X <- as.matrix(x[, ..covariates])
    storage.mode(X) <- "double"
    if (any(!is.finite(X))) stop("Decision covariates must be finite numeric values")
  } else {
    X <- matrix(numeric(), nrow = nrow(x), ncol = 0L)
  }
  offset <- if (offset_col %in% names(x)) as.numeric(x[[offset_col]]) else {
    rep(0, nrow(x))
  }
  if (any(!is.finite(offset))) stop("Decision offsets must be finite")

  estimate_omega <- is.null(fixed_omega)
  if (!estimate_omega) {
    fixed_omega <- as.numeric(fixed_omega)
    if (length(fixed_omega) != 1L || !is.finite(fixed_omega) ||
        fixed_omega < 0 || fixed_omega > 1.5) {
      stop("fixed_omega must be one value in [0, 1.5]")
    }
  } else {
    fixed_omega <- 0
  }
  if (!is.finite(omega_prior_mean) || omega_prior_mean < 0 ||
      omega_prior_mean > 1.5 || !is.finite(omega_prior_sd) ||
      omega_prior_sd <= 0) {
    stop("The omega prior must have a mean in [0, 1.5] and positive scale")
  }

  # R arrays are column-major: n + (s - 1) * N is the signal-row index used
  # by the Stan program, so this reshape is lossless and requires no copying by
  # decision in Stan.
  q_flat <- matrix(
    grid$q_grid,
    nrow = grid$N * grid$S,
    ncol = grid$G
  )
  safe_data <- list(
    N = grid$N,
    S = grid$S,
    G = grid$G,
    P = nrow(player_table),
    T = nrow(team_table),
    K = ncol(X),
    challenged = y,
    player = as.integer(player),
    team = as.integer(team),
    gain = gain,
    inventory_loss = inventory_loss,
    X = X,
    sampling_offset = offset,
    omega_grid = grid$omega_grid,
    q_grid = q_flat,
    signal_weight = grid$signal_weights,
    estimate_omega = as.integer(estimate_omega),
    omega_fixed = as.numeric(fixed_omega),
    omega_prior_mean = as.numeric(omega_prior_mean),
    omega_prior_sd = as.numeric(omega_prior_sd)
  )
  if (length(intersect(names(safe_data), continuous_call_trust_forbidden_outcomes()))) {
    stop("Internal error: an outcome field entered the Stan data")
  }
  retained_decision_columns <- unique(c(
    outcome_col, player_col, team_col, gain_col, inventory_loss_col,
    cluster_col, covariates,
    if (offset_col %in% names(x)) offset_col else character()
  ))
  safe_decisions <- x[, ..retained_decision_columns]
  safe_decisions[, call_trust_row_id__ := .I]
  list(
    data = safe_data,
    decisions = safe_decisions,
    player_table = player_table,
    team_table = team_table,
    covariate_names = as.character(covariates),
    outcome_col = outcome_col,
    player_col = player_col,
    team_col = team_col,
    gain_col = gain_col,
    inventory_loss_col = inventory_loss_col,
    cluster_col = cluster_col,
    offset_col = offset_col,
    excluded_outcome_columns = excluded_outcome_columns,
    fixed_omega = if (estimate_omega) NULL else fixed_omega
  )
}

fit_continuous_call_trust <- function(
  bundle, backend = c("cmdstanr", "rstan"), chains = 4L,
  parallel_chains = chains, iter_warmup = 1000L, iter_sampling = 1000L,
  seed = 20260825L, adapt_delta = 0.97, max_treedepth = 12L,
  stan_file = NULL, refresh = 0L
) {
  if (!is.list(bundle) || is.null(bundle$data) || is.null(bundle$decisions)) {
    stop("bundle must come from prepare_continuous_call_trust_data()")
  }
  backend <- match.arg(backend)
  if (is.null(stan_file)) stan_file <- default_continuous_call_trust_stan_file()
  if (!file.exists(stan_file)) stop("Continuous call-trust Stan file not found: ", stan_file)
  if (as.integer(chains) < 1L || as.integer(iter_warmup) < 1L ||
      as.integer(iter_sampling) < 1L) {
    stop("Invalid call-trust sampling controls")
  }
  stan_fit <- continuous_stan_fit(
    stan_file = stan_file,
    data = bundle$data,
    backend = backend,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    seed = seed,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh
  )
  fit <- list(
    fit = stan_fit,
    data = bundle$data,
    player_table = bundle$player_table,
    team_table = bundle$team_table,
    covariate_names = bundle$covariate_names,
    outcome_col = bundle$outcome_col,
    player_col = bundle$player_col,
    team_col = bundle$team_col,
    gain_col = bundle$gain_col,
    inventory_loss_col = bundle$inventory_loss_col,
    cluster_col = bundle$cluster_col,
    offset_col = bundle$offset_col,
    omega_grid = bundle$data$omega_grid,
    fixed_omega = bundle$fixed_omega,
    training_games = sort(unique(as.character(
      bundle$decisions[[bundle$cluster_col]]
    ))),
    controls = list(
      backend = backend, chains = chains, parallel_chains = parallel_chains,
      iter_warmup = iter_warmup, iter_sampling = iter_sampling, seed = seed,
      adapt_delta = adapt_delta, max_treedepth = max_treedepth
    )
  )
  class(fit) <- "continuous_call_trust_fit"
  fit
}

continuous_call_trust_draw_matrix <- function(fit, ndraws = NULL, seed = 42L) {
  if (inherits(fit, "continuous_call_trust_fit")) {
    draws <- continuous_draw_matrix(
      fit$fit,
      variables = c(
        "omega_shared", "mu_player", "tau_player", "alpha_player",
        "tau_team", "alpha_team", "decision_slope", "gamma"
      )
    )
  } else {
    draws <- continuous_draw_matrix(fit)
  }
  if (!is.null(ndraws) && nrow(draws) > as.integer(ndraws)) {
    set.seed(seed)
    draws <- draws[sample.int(nrow(draws), as.integer(ndraws)), , drop = FALSE]
  }
  draws
}

continuous_call_trust_diagnostics <- function(fit) {
  if (!inherits(fit, "continuous_call_trust_fit")) {
    stop("fit must be a continuous_call_trust_fit")
  }
  diagnostic <- continuous_cmdstan_diagnostics(fit$fit)
  diagnostic[, pass :=
    is.finite(max_rhat) & max_rhat <= 1.01 &
      is.finite(min_bulk_ess) & min_bulk_ess >= 400 &
      is.finite(divergences) & divergences == 0]
  diagnostic[]
}

continuous_call_trust_extract_indexed <- function(draws, parameter, expected) {
  columns <- grep(paste0("^", parameter, "\\["), colnames(draws))
  if (!length(columns) && expected == 0L) {
    return(matrix(numeric(), nrow = nrow(draws), ncol = 0L))
  }
  if (!length(columns)) stop("Posterior is missing ", parameter)
  indices <- as.integer(sub(".*\\[([0-9]+)\\].*", "\\1", colnames(draws)[columns]))
  columns <- columns[order(indices)]
  result <- draws[, columns, drop = FALSE]
  if (ncol(result) != expected) stop(parameter, " posterior has the wrong size")
  result
}

score_continuous_call_trust <- function(
  fit, decision_rows, q_grid, signal_weights,
  omega_grid = fit$omega_grid, ndraws = 500L, seed = 42L
) {
  if (!inherits(fit, "continuous_call_trust_fit")) {
    stop("fit must be a continuous_call_trust_fit")
  }
  x <- data.table::copy(data.table::as.data.table(decision_rows))
  required <- c(
    fit$player_col, fit$team_col, fit$gain_col, fit$inventory_loss_col,
    fit$cluster_col, fit$covariate_names
  )
  stop_if_missing_columns(x, required, "continuous call-trust scoring rows")
  continuous_call_trust_assert_no_leakage(
    x,
    fit$covariate_names,
    mapped_columns = c(
      fit$player_col, fit$team_col, fit$gain_col,
      fit$inventory_loss_col, fit$offset_col, fit$cluster_col
    )
  )
  grid <- validate_continuous_call_trust_grid(
    q_grid, omega_grid, signal_weights, n_rows = nrow(x)
  )
  if (!identical(as.numeric(omega_grid), as.numeric(fit$omega_grid))) {
    stop("Scoring omega_grid does not match the fitted grid")
  }
  covariate_names <- fit$covariate_names
  X <- if (length(covariate_names)) {
    value <- as.matrix(x[, ..covariate_names])
    storage.mode(value) <- "double"
    value
  } else matrix(numeric(), nrow = nrow(x), ncol = 0L)
  offset <- if (fit$offset_col %in% names(x)) {
    as.numeric(x[[fit$offset_col]])
  } else rep(0, nrow(x))
  if (any(!is.finite(X)) || any(!is.finite(offset))) {
    stop("Scoring covariates and offsets must be finite")
  }
  gain <- as.numeric(x[[fit$gain_col]])
  inventory_loss <- as.numeric(x[[fit$inventory_loss_col]])
  if (any(!is.finite(gain)) || any(gain < 0) ||
      any(!is.finite(inventory_loss)) || any(inventory_loss < 0)) {
    stop("Scoring gains and inventory losses must be finite and nonnegative")
  }
  player_id <- as.character(x[[fit$player_col]])
  team_id <- as.character(x[[fit$team_col]])
  player_index <- match(player_id, fit$player_table$player_id)
  team_index <- match(team_id, fit$team_table$team_id)
  new_players <- sort(unique(player_id[is.na(player_index)]))
  new_teams <- sort(unique(team_id[is.na(team_index)]))
  new_player_index <- match(player_id, new_players)
  new_team_index <- match(team_id, new_teams)

  draws <- continuous_call_trust_draw_matrix(fit, ndraws, seed)
  required_scalars <- c(
    "mu_player", "tau_player", "tau_team", "decision_slope", "omega_shared"
  )
  if (!all(required_scalars %in% colnames(draws))) {
    stop("Call-trust posterior is missing scoring parameters")
  }
  player_effect <- continuous_call_trust_extract_indexed(
    draws, "alpha_player", nrow(fit$player_table)
  )
  team_effect <- continuous_call_trust_extract_indexed(
    draws, "alpha_team", nrow(fit$team_table)
  )
  gamma <- continuous_call_trust_extract_indexed(
    draws, "gamma", length(fit$covariate_names)
  )
  probability_sum <- probability_square_sum <- numeric(nrow(x))
  draw_probabilities <- matrix(NA_real_, nrow = nrow(draws), ncol = nrow(x))
  draw_choice_q <- matrix(NA_real_, nrow = nrow(draws), ncol = nrow(x))
  set.seed(seed + 1L)
  for (draw in seq_len(nrow(draws))) {
    p_effect <- t_effect <- numeric(nrow(x))
    known_player <- !is.na(player_index)
    known_team <- !is.na(team_index)
    p_effect[known_player] <- player_effect[draw, player_index[known_player]]
    t_effect[known_team] <- team_effect[draw, team_index[known_team]]
    if (length(new_players)) {
      fresh <- stats::rnorm(
        length(new_players), draws[draw, "mu_player"],
        draws[draw, "tau_player"]
      )
      p_effect[!known_player] <- fresh[new_player_index[!known_player]]
    }
    if (length(new_teams)) {
      fresh <- stats::rnorm(length(new_teams), 0, draws[draw, "tau_team"])
      t_effect[!known_team] <- fresh[new_team_index[!known_team]]
    }
    base <- p_effect + t_effect + offset
    if (ncol(X)) base <- base + as.numeric(X %*% gamma[draw, ])
    q <- continuous_call_trust_interpolate_validated(
      grid, draws[draw, "omega_shared"]
    )
    q <- pmin(1, pmax(0, q))
    utility <- q * gain - (1 - q) * inventory_loss
    eta <- matrix(base, nrow = nrow(x), ncol = grid$S) +
      draws[draw, "decision_slope"] * utility
    action <- stats::plogis(eta)
    probability <- rowSums(grid$signal_weights * action)
    draw_probabilities[draw, ] <- probability
    draw_choice_q[draw, ] <- rowSums(grid$signal_weights * action * q) /
      pmax(probability, 1e-12)
    probability_sum <- probability_sum + probability
    probability_square_sum <- probability_square_sum + probability^2
  }
  mean_probability <- probability_sum / nrow(draws)
  x[, `:=`(
    p_challenge_call_trust = mean_probability,
    p_challenge_call_trust_sd = sqrt(pmax(
      probability_square_sum / nrow(draws) - mean_probability^2, 0
    )),
    p_challenge_call_trust_lower = apply(
      draw_probabilities, 2L, stats::quantile, 0.025, names = FALSE
    ),
    p_challenge_call_trust_upper = apply(
      draw_probabilities, 2L, stats::quantile, 0.975, names = FALSE
    ),
    q_chosen_call_trust_mean = colMeans(draw_choice_q),
    q_chosen_call_trust_lower = apply(
      draw_choice_q, 2L, stats::quantile, 0.025, names = FALSE
    ),
    q_chosen_call_trust_upper = apply(
      draw_choice_q, 2L, stats::quantile, 0.975, names = FALSE
    ),
    omega_posterior_mean = mean(draws[, "omega_shared"]),
    player_seen_in_call_trust = !is.na(player_index),
    team_seen_in_call_trust = !is.na(team_index)
  )]
  x[]
}

continuous_call_trust_crossfit_plan <- function(
  decision_rows, folds = 5L, seed = 42L,
  fixed_omegas = continuous_call_trust_fixed_omegas(),
  cluster_col = "game_pk"
) {
  x <- data.table::as.data.table(decision_rows)
  stop_if_missing_columns(x, cluster_col, "call-trust crossfit decisions")
  games <- sort(unique(as.character(x[[cluster_col]])))
  folds <- as.integer(folds)
  if (folds < 2L || length(games) < folds) {
    stop("Call-trust cross-fitting needs at least one game per fold")
  }
  fixed_omegas <- sort(unique(as.numeric(fixed_omegas)))
  if (any(!is.finite(fixed_omegas)) || any(fixed_omegas < 0 | fixed_omegas > 1.5)) {
    stop("Fixed staging omegas must be in [0, 1.5]")
  }
  set.seed(seed)
  assignments <- data.table::data.table(
    game_key__ = sample(games),
    fold = rep(seq_len(folds), length.out = length(games))
  )
  data.table::setorder(assignments, game_key__)
  stages <- data.table::CJ(fold = seq_len(folds), omega = fixed_omegas)
  stages[, `:=`(
    stage_id = continuous_call_trust_stage_id(omega),
    stage_type = "fixed"
  )]
  estimated_stages <- data.table::data.table(
    fold = seq_len(folds),
    omega = NA_real_,
    stage_id = "omega_estimated",
    stage_type = "estimated"
  )
  list(
    assignments = assignments,
    stages = stages,
    estimated_stages = estimated_stages,
    all_stages = data.table::rbindlist(list(stages, estimated_stages))
  )
}

continuous_call_trust_fold_slice <- function(
  decision_rows, q_grid, omega_grid, signal_weights, assignments,
  fold, split = c("train", "test"), cluster_col = "game_pk"
) {
  split <- match.arg(split)
  x <- data.table::as.data.table(decision_rows)
  stop_if_missing_columns(x, cluster_col, "call-trust fold rows")
  grid <- validate_continuous_call_trust_grid(
    q_grid, omega_grid, signal_weights,
    n_rows = nrow(x)
  )
  assignment <- data.table::as.data.table(assignments)
  stop_if_missing_columns(assignment, c("game_key__", "fold"), "call-trust assignments")
  row_fold <- assignment$fold[match(as.character(x[[cluster_col]]), assignment$game_key__)]
  if (anyNA(row_fold)) stop("Some decision games are missing a fold assignment")
  keep <- if (split == "train") row_fold != as.integer(fold) else {
    row_fold == as.integer(fold)
  }
  indices <- which(keep)
  list(
    decisions = data.table::copy(x[indices]),
    q_grid = grid$q_grid[indices, , , drop = FALSE],
    signal_weights = grid$signal_weights[indices, , drop = FALSE],
    row_index = indices,
    games = sort(unique(as.character(x[[cluster_col]][indices]))),
    fold = as.integer(fold),
    split = split
  )
}

prepare_continuous_call_trust_fold_bundle <- function(
  decision_rows, q_grid, omega_grid, signal_weights, assignments,
  fold, fixed_omega = NULL, covariates = character(), cluster_col = "game_pk", ...
) {
  x <- data.table::as.data.table(decision_rows)
  assignment <- data.table::as.data.table(assignments)
  row_fold <- assignment$fold[match(as.character(x[[cluster_col]]), assignment$game_key__)]
  if (anyNA(row_fold)) stop("Some decision games are missing a fold assignment")
  indices <- which(row_fold != as.integer(fold))
  prepared <- validate_continuous_call_trust_grid(
    q_grid, omega_grid, signal_weights, n_rows = nrow(x)
  )
  bundle <- prepare_continuous_call_trust_data(
    x[indices],
    prepared$q_grid[indices, , , drop = FALSE],
    omega_grid,
    prepared$signal_weights[indices, , drop = FALSE],
    covariates = covariates,
    fixed_omega = fixed_omega,
    cluster_col = cluster_col,
    ...
  )
  attr(bundle, "heldout_fold") <- as.integer(fold)
  attr(bundle, "training_row_index") <- indices
  bundle
}

prepare_continuous_call_trust_stage_bundle <- function(
  decision_rows, q_grid, omega_grid, signal_weights, assignments,
  stage, covariates = character(), cluster_col = "game_pk", ...
) {
  specification <- data.table::as.data.table(stage)
  stop_if_missing_columns(
    specification,
    c("fold", "omega", "stage_id", "stage_type"),
    "continuous call-trust stage"
  )
  if (nrow(specification) != 1L ||
      !specification$stage_type[[1L]] %in% c("fixed", "estimated")) {
    stop("A call-trust stage must be one fixed or estimated row")
  }
  fixed_omega <- if (specification$stage_type[[1L]] == "fixed") {
    as.numeric(specification$omega[[1L]])
  } else {
    NULL
  }
  bundle <- prepare_continuous_call_trust_fold_bundle(
    decision_rows = decision_rows,
    q_grid = q_grid,
    omega_grid = omega_grid,
    signal_weights = signal_weights,
    assignments = assignments,
    fold = specification$fold[[1L]],
    fixed_omega = fixed_omega,
    covariates = covariates,
    cluster_col = cluster_col,
    ...
  )
  attr(bundle, "call_trust_stage_id") <- specification$stage_id[[1L]]
  attr(bundle, "call_trust_stage_type") <- specification$stage_type[[1L]]
  bundle
}

continuous_call_trust_log_loss <- function(y, probability) {
  probability <- pmin(1 - 1e-12, pmax(1e-12, as.numeric(probability)))
  -(as.numeric(y) * log(probability) +
      (1 - as.numeric(y)) * log1p(-probability))
}

continuous_call_trust_crossfit_metrics <- function(
  predictions, model_col = "model", row_col = "row_id",
  cluster_col = "game_pk", outcome_col = "challenged",
  probability_col = "p_challenge", baseline_model = "omega_0"
) {
  x <- data.table::copy(data.table::as.data.table(predictions))
  required <- c(model_col, row_col, cluster_col, outcome_col, probability_col)
  stop_if_missing_columns(x, required, "call-trust crossfit predictions")
  model_row_key <- c(model_col, row_col)
  if (anyDuplicated(x[, ..model_row_key])) {
    stop("Crossfit predictions duplicate a model-row pair")
  }
  probability <- as.numeric(x[[probability_col]])
  y <- as.integer(x[[outcome_col]])
  if (any(!is.finite(probability)) || any(probability < 0 | probability > 1) ||
      anyNA(y) || any(!y %in% 0:1)) {
    stop("Crossfit predictions contain invalid outcomes or probabilities")
  }
  x[, loss__ := continuous_call_trust_log_loss(y, probability)]
  by_cluster <- x[, .(
    rows = .N,
    log_loss = mean(loss__)
  ), by = c(model_col, cluster_col)]
  summary <- by_cluster[, .(
    clusters = .N,
    rows = sum(rows),
    log_loss = stats::weighted.mean(log_loss, rows),
    cluster_mean_log_loss = mean(log_loss),
    cluster_se = stats::sd(log_loss) / sqrt(.N)
  ), by = model_col]

  baseline <- x[get(model_col) == baseline_model, .(
    row_key__ = as.character(get(row_col)),
    cluster_key__ = as.character(get(cluster_col)),
    baseline_loss__ = loss__
  )]
  if (!nrow(baseline)) stop("Crossfit predictions are missing baseline model ", baseline_model)
  comparison_parts <- lapply(setdiff(unique(as.character(x[[model_col]])), baseline_model), function(model) {
    # Keep the comparison value under a name that cannot collide with the
    # conventional `model` column inside data.table's evaluation environment.
    candidate_value <- model
    candidate <- x[get(model_col) == candidate_value, .(
      row_key__ = as.character(get(row_col)),
      cluster_key__ = as.character(get(cluster_col)),
      candidate_loss__ = loss__
    )]
    paired <- merge(
      baseline, candidate,
      by = c("row_key__", "cluster_key__"), all = FALSE
    )
    if (nrow(paired) != nrow(baseline) || nrow(paired) != nrow(candidate)) {
      stop("Models do not score the same held-out rows: ", model)
    }
    paired[, improvement__ := baseline_loss__ - candidate_loss__]
    cluster <- paired[, .(improvement = mean(improvement__)), by = cluster_key__]
    se <- stats::sd(cluster$improvement) / sqrt(nrow(cluster))
    standardized_improvement <- if (!is.finite(se)) {
      NA_real_
    } else if (se > 0) {
      mean(cluster$improvement) / se
    } else if (mean(cluster$improvement) > 0) {
      Inf
    } else {
      0
    }
    data.table::data.table(
      model = model,
      baseline_model = baseline_model,
      clusters = nrow(cluster),
      improvement_vs_baseline = mean(cluster$improvement),
      clustered_se = se,
      improvement_in_se = standardized_improvement,
      one_se_improvement = !is.na(standardized_improvement) &&
        standardized_improvement >= 1
    )
  })
  comparisons <- data.table::rbindlist(comparison_parts, fill = TRUE)
  list(summary = summary, comparisons = comparisons, by_cluster = by_cluster)
}

continuous_call_trust_identifiability <- function(
  fit_or_draws, correlation_tolerance = 0.8,
  include_varying_effects = TRUE
) {
  draws <- continuous_call_trust_draw_matrix(fit_or_draws)
  if (!"omega_shared" %in% colnames(draws)) {
    stop("Posterior draws are missing omega_shared")
  }
  base_patterns <- c(
    "^mu_player$", "^tau_player$", "^tau_team$", "^decision_slope$",
    "^gamma\\["
  )
  if (isTRUE(include_varying_effects)) {
    base_patterns <- c(base_patterns, "^alpha_player\\[", "^alpha_team\\[")
  }
  decision_columns <- unique(unlist(lapply(
    base_patterns, function(pattern) grep(pattern, colnames(draws), value = TRUE)
  )))
  omega <- as.numeric(draws[, "omega_shared"])
  fixed <- !is.finite(stats::sd(omega)) || stats::sd(omega) < 1e-12
  if (!length(decision_columns)) stop("No decision parameters found for identifiability")
  correlations <- vapply(decision_columns, function(column) {
    if (fixed || stats::sd(draws[, column]) < 1e-12) NA_real_ else {
      stats::cor(omega, draws[, column])
    }
  }, numeric(1L))
  table <- data.table::data.table(
    decision_parameter = decision_columns,
    omega_correlation = correlations,
    absolute_correlation = abs(correlations),
    tolerance = as.numeric(correlation_tolerance)
  )
  table[, pass := is.na(absolute_correlation) | absolute_correlation < tolerance]
  max_abs <- if (all(is.na(table$absolute_correlation))) {
    NA_real_
  } else max(table$absolute_correlation, na.rm = TRUE)
  list(
    table = table,
    summary = data.table::data.table(
      omega_fixed = fixed,
      maximum_absolute_correlation = max_abs,
      tolerance = as.numeric(correlation_tolerance),
      pass = if (fixed) NA else all(table$pass)
    )
  )
}

continuous_call_trust_grid_gate <- function(grid_convergence) {
  if (is.null(grid_convergence)) return(FALSE)
  value <- if (is.list(grid_convergence) &&
      !is.null(grid_convergence$summary)) {
    grid_convergence$summary
  } else {
    grid_convergence
  }
  if (is.logical(value) && length(value) == 1L && !is.na(value)) {
    return(isTRUE(value))
  }
  value <- data.table::as.data.table(value)
  if (!"pass" %in% names(value) || nrow(value) != 1L) return(FALSE)
  isTRUE(value$pass[[1L]])
}

continuous_call_trust_promotion_gate <- function(
  crossfit_metrics, fit_or_draws,
  grid_convergence = NULL,
  estimated_model = "omega_estimated", baseline_model = "omega_0",
  minimum_improvement_se = 1,
  interval_probability = 0.95, maximum_interval_width = 0.75,
  boundary_margin = 0.02, correlation_tolerance = 0.8
) {
  comparisons <- if (is.list(crossfit_metrics) &&
      !is.null(crossfit_metrics$comparisons)) {
    data.table::as.data.table(crossfit_metrics$comparisons)
  } else data.table::as.data.table(crossfit_metrics)
  baseline_value <- as.character(baseline_model)
  estimated_value <- as.character(estimated_model)
  row <- comparisons[
    comparisons$model == estimated_value &
      comparisons$baseline_model == baseline_value
  ]
  if (nrow(row) != 1L) {
    stop("Promotion needs one estimated-vs-omega-zero comparison")
  }
  draws <- continuous_call_trust_draw_matrix(fit_or_draws)
  if (!"omega_shared" %in% colnames(draws)) stop("Posterior is missing omega_shared")
  omega <- as.numeric(draws[, "omega_shared"])
  alpha <- (1 - as.numeric(interval_probability)) / 2
  interval <- stats::quantile(omega, c(alpha, 1 - alpha), names = FALSE)
  width <- diff(interval)
  informative <- is.finite(width) && width <= maximum_interval_width &&
    interval[[1L]] >= boundary_margin && interval[[2L]] <= 1.5 - boundary_margin
  identifiability <- continuous_call_trust_identifiability(
    draws, correlation_tolerance = correlation_tolerance
  )
  max_correlation <- identifiability$summary$maximum_absolute_correlation[[1L]]
  correlation_pass <- isTRUE(identifiability$summary$pass[[1L]])
  grid_pass <- continuous_call_trust_grid_gate(grid_convergence)
  improvement_se <- row$improvement_in_se[[1L]]
  improvement_pass <- !is.na(improvement_se) &&
    improvement_se >= as.numeric(minimum_improvement_se)
  pass <- improvement_pass && informative && correlation_pass && grid_pass
  median_omega <- stats::median(omega)
  data.table::data.table(
    estimated_model = estimated_model,
    baseline_model = baseline_model,
    improvement_vs_omega_zero = row$improvement_vs_baseline[[1L]],
    clustered_se = row$clustered_se[[1L]],
    improvement_in_se = improvement_se,
    minimum_improvement_se = as.numeric(minimum_improvement_se),
    improvement_pass = improvement_pass,
    omega_mean = mean(omega),
    omega_median = median_omega,
    omega_lower = interval[[1L]],
    omega_upper = interval[[2L]],
    omega_interval_width = width,
    maximum_interval_width = as.numeric(maximum_interval_width),
    informative_interval_pass = informative,
    grid_convergence_pass = grid_pass,
    maximum_absolute_decision_correlation = max_correlation,
    correlation_tolerance = as.numeric(correlation_tolerance),
    identifiability_pass = correlation_pass,
    pass = pass,
    selected_omega = if (pass) median_omega else 0,
    status = if (pass) "estimated_omega_promoted" else "default_omega_zero"
  )
}
