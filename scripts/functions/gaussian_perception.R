perception_role_levels <- function() c("batter", "catcher", "pitcher")

clamp_perception_probability <- function(p, eps = 1e-9) {
  p <- as.numeric(p)
  if (any(!is.finite(p))) stop("Perception probabilities must be finite")
  if (length(eps) != 1L || !is.finite(eps) || eps <= 0 || eps >= 0.5) {
    stop("eps must be one number strictly between zero and 0.5")
  }
  pmin(1 - eps, pmax(eps, p))
}

perception_blur <- function(p_hat, sigma, spatial_scale, eps = 1e-9) {
  n <- max(length(p_hat), length(sigma), length(spatial_scale))
  p_hat <- rep_len(as.numeric(p_hat), n)
  sigma <- rep_len(as.numeric(sigma), n)
  spatial_scale <- rep_len(as.numeric(spatial_scale), n)
  if (any(!is.finite(sigma)) || any(sigma < 0)) {
    stop("sigma must be finite and nonnegative")
  }
  if (any(!is.finite(spatial_scale)) || any(spatial_scale <= 0)) {
    stop("spatial_scale must be finite and positive")
  }
  z <- stats::qnorm(clamp_perception_probability(p_hat, eps))
  out <- stats::pnorm(z / sqrt(1 + (sigma / spatial_scale)^2))
  pmin(1, pmax(0, out))
}

perception_adverse_margin <- function(initial_call, edge_distance_inches) {
  initial_call <- as.character(initial_call)
  edge_distance_inches <- as.numeric(edge_distance_inches)
  out <- rep(NA_real_, length(initial_call))
  out[initial_call == "called_strike"] <- edge_distance_inches[initial_call == "called_strike"]
  out[initial_call == "ball"] <- -edge_distance_inches[initial_call == "ball"]
  out
}

estimate_perception_spatial_scale <- function(choices, eps = 1e-6) {
  x <- data.table::copy(data.table::as.data.table(choices))
  stop_if_missing_columns(
    x, c("p_hat", "initial_call", "edge_distance_inches"),
    "perception spatial-scale data"
  )
  if (!"adverse_margin" %in% names(x)) {
    x[, adverse_margin := perception_adverse_margin(initial_call, edge_distance_inches)]
  }
  x[, p_clip__ := clamp_perception_probability(p_hat, eps)]
  x[, `:=`(
    latent_z__ = stats::qnorm(p_clip__),
    scale_weight__ = p_clip__ * (1 - p_clip__)
  )]
  result <- lapply(c("ball", "called_strike"), function(call) {
    d <- x[
      initial_call == call & is.finite(adverse_margin) & is.finite(latent_z__) &
        is.finite(scale_weight__) & scale_weight__ > 0
    ]
    if (nrow(d) < 20L) stop("Too few rows to estimate spatial scale for ", call)
    model <- stats::lm(
      latent_z__ ~ adverse_margin,
      data = d,
      weights = scale_weight__
    )
    slope <- unname(stats::coef(model)[["adverse_margin"]])
    if (!is.finite(slope) || slope <= 0) {
      stop("Estimated p_hat-to-distance slope is not positive for ", call)
    }
    data.table::data.table(
      initial_call = call,
      n = nrow(d),
      intercept = unname(stats::coef(model)[["(Intercept)"]]),
      slope_per_inch = slope,
      spatial_scale = 1 / slope,
      weighted_r_squared = summary(model)$r.squared
    )
  })
  data.table::rbindlist(result)
}

expand_perception_choices <- function(
  mdp_opportunities, available_only = TRUE, exclude_preempted = TRUE
) {
  x <- data.table::copy(data.table::as.data.table(mdp_opportunities))
  required <- c(
    "game_pk", "pitch_order", "initial_call", "edge_distance_inches",
    "p_hat", "stake_G", "batter_id", "pitcher_id", "fielder_2",
    "challenge_occurred", "challenger_role", "challenger_player_id"
  )
  stop_if_missing_columns(x, required, "perception MDP opportunities")
  if (!"bat_team_challenges_before" %in% names(x)) {
    if (!"adverse_challenges_before" %in% names(x)) {
      stop("Perception opportunities need batting-team inventory")
    }
    x[, bat_team_challenges_before := adverse_challenges_before]
  }
  if (!"fld_team_challenges_before" %in% names(x)) {
    if (!"adverse_challenges_before" %in% names(x)) {
      stop("Perception opportunities need fielding-team inventory")
    }
    x[, fld_team_challenges_before := adverse_challenges_before]
  }
  if (!"actual_wrong" %in% names(x)) {
    if ("call_wrong" %in% names(x)) {
      x[, actual_wrong := as.logical(call_wrong)]
    } else if ("abs_call" %in% names(x)) {
      x[, actual_wrong := initial_call != abs_call]
    } else {
      x[, actual_wrong := NA]
    }
  }

  make_rows <- function(d, observer_role, player_column, inventory_column) {
    out <- data.table::copy(d)
    out[, `:=`(
      role = observer_role,
      player_id = as.character(get(player_column)),
      inventory_before = as.integer(get(inventory_column))
    )]
    out[, challenged :=
      challenge_occurred %in% TRUE & challenger_role == observer_role &
        as.character(challenger_player_id) == player_id]
    out
  }

  batter <- make_rows(
    x[initial_call == "called_strike"], "batter", "batter_id",
    "bat_team_challenges_before"
  )
  catcher <- make_rows(
    x[initial_call == "ball"], "catcher", "fielder_2",
    "fld_team_challenges_before"
  )
  pitcher <- make_rows(
    x[initial_call == "ball"], "pitcher", "pitcher_id",
    "fld_team_challenges_before"
  )
  # Once one defensive observer challenges, the teammate no longer has an
  # independently observed pass/challenge decision on that pitch.
  if (isTRUE(exclude_preempted)) {
    catcher <- catcher[!(challenge_occurred %in% TRUE & challenger_role == "pitcher")]
    pitcher <- pitcher[!(challenge_occurred %in% TRUE & challenger_role == "catcher")]
  }

  out <- data.table::rbindlist(list(batter, catcher, pitcher), fill = TRUE)
  out <- out[!is.na(player_id) & nzchar(player_id) & is.finite(inventory_before)]
  if (isTRUE(available_only)) out <- out[inventory_before > 0L]
  out[, `:=`(
    player_role_id = paste(role, player_id, sep = ":"),
    adverse_margin = perception_adverse_margin(initial_call, edge_distance_inches),
    challenged = as.integer(challenged)
  )]
  key <- c("game_pk", "pitch_order", "role", "player_role_id")
  if (anyDuplicated(out[, ..key])) {
    stop("Perception preparation produced duplicate pitch-player decisions")
  }
  data.table::setorder(out, game_pk, pitch_order, role)
  out[]
}

prepare_perception_choices <- function(
  ledger, re_model, p_col = "p_hat", min_coverage = 0.99,
  available_only = TRUE, exclude_preempted = TRUE
) {
  x <- data.table::copy(data.table::as.data.table(ledger))
  stop_if_missing_columns(x, p_col, "perception ledger")
  opportunities <- prepare_mdp_opportunities(
    x, re_model, p_col = p_col, min_coverage = min_coverage
  )
  out <- expand_perception_choices(
    opportunities,
    available_only = available_only,
    exclude_preempted = exclude_preempted
  )
  attr(out, "coverage") <- attr(opportunities, "coverage")
  out[]
}

deterministic_perception_folds <- function(games, folds = 5L, seed = 42L) {
  games <- sort(unique(as.character(games)))
  folds <- as.integer(folds)
  if (folds < 2L || length(games) < folds) {
    stop("Perception cross-fitting needs at least one game per fold")
  }
  set.seed(seed)
  shuffled <- sample(games)
  data.table::data.table(
    game_key__ = shuffled,
    fold = rep(seq_len(folds), length.out = length(shuffled))
  )[order(game_key__)]
}

# Rare challenges make the full decision table unnecessarily expensive for a
# screening fit. Keep every challenge, sample passes within player, and attach
# the standard case-control log-odds correction. The correction lets the
# fitted intercepts continue to describe the original (not sampled) attempt
# rate while preserving the probability/eyesight relationship.
subsample_perception_pilot <- function(
  choices, pass_fraction = 0.05, min_passes_per_player = 10L, seed = 42L
) {
  x <- data.table::copy(data.table::as.data.table(choices))
  stop_if_missing_columns(
    x, c("player_role_id", "challenged", "role"),
    "perception pilot choices"
  )
  pass_fraction <- as.numeric(pass_fraction)
  min_passes_per_player <- as.integer(min_passes_per_player)
  if (!is.finite(pass_fraction) || pass_fraction <= 0 || pass_fraction > 1) {
    stop("pass_fraction must be in (0, 1]")
  }
  if (min_passes_per_player < 1L) {
    stop("min_passes_per_player must be positive")
  }
  x[, challenged := as.integer(challenged)]
  x[, row_id__ := .I]
  pass_plan <- x[challenged == 0L, .(available_passes = .N), by = player_role_id]
  pass_plan[, selected_passes := pmin(
    available_passes,
    pmax(min_passes_per_player, ceiling(available_passes * pass_fraction))
  )]
  pass_plan[, pass_inclusion_probability := selected_passes / available_passes]
  x[pass_plan, on = "player_role_id", `:=`(
    pass_inclusion_probability = i.pass_inclusion_probability,
    available_passes = i.available_passes,
    selected_passes = i.selected_passes
  )]
  # A player with challenges but no passes needs no sampling correction.
  x[is.na(pass_inclusion_probability), `:=`(
    pass_inclusion_probability = 1,
    available_passes = 0L,
    selected_passes = 0L
  )]
  set.seed(seed)
  selected_pass_ids <- x[challenged == 0L, {
    take <- selected_passes[[1L]]
    .(row_id__ = if (take >= .N) row_id__ else sample(row_id__, take))
  }, by = player_role_id]$row_id__
  out <- x[challenged == 1L | row_id__ %in% selected_pass_ids]
  # In a case-control sample, sample odds equal population odds multiplied by
  # P(select | challenge) / P(select | pass) = 1 / pass inclusion probability.
  out[, sampling_offset := -log(pass_inclusion_probability)]
  if (any(!is.finite(out$sampling_offset)) || any(out$sampling_offset < 0)) {
    stop("Pilot case-control offsets are invalid")
  }
  original_manifest <- x[, .(
    original_rows = .N,
    original_challenges = sum(challenged),
    original_passes = sum(challenged == 0L)
  ), by = role]
  selected_manifest <- out[, .(
    selected_rows = .N,
    selected_challenges = sum(challenged),
    selected_passes = sum(challenged == 0L),
    median_pass_inclusion = stats::median(pass_inclusion_probability)
  ), by = role]
  manifest <- merge(
    original_manifest, selected_manifest,
    by = "role", all = TRUE, sort = FALSE
  )
  manifest[, retained_fraction := selected_rows / original_rows]
  data.table::setorder(out, row_id__)
  out[, row_id__ := NULL]
  attr(out, "sampling_manifest") <- manifest
  out[]
}

prepare_perception_stan_data <- function(choices, spatial_scale = NULL) {
  x <- data.table::copy(data.table::as.data.table(choices))
  required <- c(
    "game_pk", "role", "player_id", "player_role_id", "challenged", "p_hat",
    "stake_G", "initial_call"
  )
  stop_if_missing_columns(x, required, "perception choices")
  x[, `:=`(
    role = as.character(role),
    player_id = as.character(player_id),
    player_role_id = as.character(player_role_id),
    initial_call = as.character(initial_call)
  )]
  if (is.null(spatial_scale)) spatial_scale <- estimate_perception_spatial_scale(x)
  if (!"sampling_offset" %in% names(x)) x[, sampling_offset := 0]
  if (any(!is.finite(x$sampling_offset))) {
    stop("Perception sampling offsets must be finite")
  }
  scale_table <- data.table::as.data.table(spatial_scale)[, .(
    initial_call = as.character(initial_call),
    spatial_scale = as.numeric(spatial_scale)
  )]
  if (anyDuplicated(scale_table$initial_call)) stop("Spatial-scale calls are duplicated")
  x <- merge(x, scale_table, by = "initial_call", all.x = TRUE, sort = FALSE)
  if (any(!is.finite(x$spatial_scale)) || any(x$spatial_scale <= 0)) {
    stop("Every perception choice needs a positive spatial scale")
  }
  roles <- perception_role_levels()
  if (any(!x$role %in% roles)) stop("Unknown perception role")
  players <- unique(x[, .(player_role_id, role, player_id = as.character(player_id))])
  if (players[, any(data.table::uniqueN(role) != 1L), by = player_role_id]$V1 |> any()) {
    stop("A player-role identifier maps to more than one role")
  }
  data.table::setorder(players, role, player_role_id)
  players[, `:=`(
    player_index = seq_len(.N),
    role_index = match(role, roles)
  )]
  x[players, on = "player_role_id", `:=`(
    player_index = i.player_index,
    role_index = i.role_index
  )]
  positive_ev <- x$p_hat * pmax(x$stake_G, 0)
  ev_reference <- stats::median(positive_ev[is.finite(positive_ev) & positive_ev > 0])
  if (!is.finite(ev_reference) || ev_reference <= 0) {
    stop("Cannot construct a positive expected-value reference")
  }
  player_counts <- x[, .(
    opportunities = .N,
    challenges = sum(challenged)
  ), by = .(player_role_id, role, player_id)]
  players <- merge(
    players, player_counts,
    by = c("player_role_id", "role", "player_id"), all.x = TRUE, sort = FALSE
  )
  data.table::setorder(players, player_index)
  stan_data <- list(
    N = nrow(x),
    P = nrow(players),
    R = length(roles),
    y = as.integer(x$challenged),
    player = as.integer(x$player_index),
    role_of_player = as.integer(players$role_index),
    tracking_z = stats::qnorm(clamp_perception_probability(x$p_hat)),
    spatial_scale = as.numeric(x$spatial_scale),
    stake_G_positive = pmax(as.numeric(x$stake_G), 0),
    sampling_offset = as.numeric(x$sampling_offset),
    ev_reference = as.numeric(ev_reference),
    epsilon_ev = as.numeric(ev_reference * 1e-8)
  )
  list(
    data = stan_data,
    choices = x,
    player_table = players,
    role_levels = roles,
    spatial_scale = data.table::as.data.table(spatial_scale),
    ev_reference = ev_reference
  )
}

default_perception_stan_file <- function(
  model_variant = c("fixed_slope", "role_sensitivity")
) {
  model_variant <- match.arg(model_variant)
  root <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd()),
    error = function(e) getwd()
  )
  filename <- if (model_variant == "fixed_slope") {
    "hierarchical_gaussian_perception.stan"
  } else {
    "hierarchical_gaussian_perception_sensitivity.stan"
  }
  file.path(root, "scripts", "stan", filename)
}

fit_hierarchical_perception <- function(
  choices, spatial_scale = NULL, chains = 2L, cores = chains,
  iter = 2000L, warmup = floor(iter / 2), seed = 42L,
  adapt_delta = 0.95, max_treedepth = 12L,
  model_variant = c("fixed_slope", "role_sensitivity"),
  stan_file = NULL, file = NULL,
  refresh = 100L, force_refit = FALSE
) {
  model_variant <- match.arg(model_variant)
  if (is.null(stan_file)) {
    stan_file <- default_perception_stan_file(model_variant)
  }
  if (!is.null(file) && file.exists(file) && !isTRUE(force_refit)) {
    cached <- readRDS(file)
    if (inherits(cached, "gaussian_perception_fit")) return(cached)
  }
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("fit_hierarchical_perception() requires the rstan package")
  }
  if (!file.exists(stan_file)) stop("Perception Stan file not found: ", stan_file)
  chains <- as.integer(chains)
  cores <- as.integer(cores)
  iter <- as.integer(iter)
  warmup <- as.integer(warmup)
  if (chains < 1L || iter < 2L || warmup < 1L || warmup >= iter) {
    stop("Invalid perception sampling controls")
  }
  bundle <- prepare_perception_stan_data(choices, spatial_scale)
  rstan::rstan_options(auto_write = TRUE)
  model <- rstan::stan_model(file = stan_file, auto_write = TRUE)
  stan_fit <- rstan::sampling(
    model,
    data = bundle$data,
    chains = chains,
    cores = min(cores, chains),
    iter = iter,
    warmup = warmup,
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
    role_levels = bundle$role_levels,
    spatial_scale = bundle$spatial_scale,
    ev_reference = bundle$ev_reference,
    model_variant = model_variant,
    training_games = sort(unique(as.character(choices$game_pk))),
    controls = list(
      chains = chains, iter = iter, warmup = warmup, seed = seed,
      adapt_delta = adapt_delta, max_treedepth = max_treedepth
    )
  )
  class(fit) <- "gaussian_perception_fit"
  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(fit, file)
  }
  fit
}

extract_indexed_parameter <- function(draw_matrix, parameter, expected = NULL) {
  pattern <- paste0("^", parameter, "\\[")
  columns <- grep(pattern, colnames(draw_matrix))
  if (!length(columns)) stop("Posterior is missing parameter ", parameter)
  indices <- as.integer(sub(".*\\[([0-9]+)\\].*", "\\1", colnames(draw_matrix)[columns]))
  columns <- columns[order(indices)]
  out <- draw_matrix[, columns, drop = FALSE]
  if (!is.null(expected) && ncol(out) != expected) {
    stop(parameter, " posterior has the wrong number of levels")
  }
  out
}

perception_parameter_draws <- function(fit, ndraws = NULL, seed = 42L) {
  if (!inherits(fit, "gaussian_perception_fit")) {
    stop("fit must be a gaussian_perception_fit")
  }
  model_variant <- if (is.null(fit$model_variant)) {
    "fixed_slope"
  } else {
    fit$model_variant
  }
  parameters <- c(
    "mu_log_sigma", "tau_log_sigma", "sigma_player",
    "mu_alpha", "tau_alpha", "alpha_player"
  )
  if (model_variant == "role_sensitivity") {
    parameters <- c(parameters, "beta_role")
  }
  matrix <- as.matrix(fit$stan_fit, pars = parameters)
  if (!is.null(ndraws) && nrow(matrix) > as.integer(ndraws)) {
    set.seed(seed)
    matrix <- matrix[sample.int(nrow(matrix), as.integer(ndraws)), , drop = FALSE]
  }
  beta_role <- if (model_variant == "role_sensitivity") {
    extract_indexed_parameter(matrix, "beta_role", length(fit$role_levels))
  } else {
    matrix(1, nrow = nrow(matrix), ncol = length(fit$role_levels))
  }
  list(
    mu_log_sigma = extract_indexed_parameter(
      matrix, "mu_log_sigma", length(fit$role_levels)
    ),
    tau_log_sigma = as.numeric(matrix[, "tau_log_sigma"]),
    sigma_player = extract_indexed_parameter(
      matrix, "sigma_player", nrow(fit$player_table)
    ),
    mu_alpha = extract_indexed_parameter(matrix, "mu_alpha", length(fit$role_levels)),
    tau_alpha = as.numeric(matrix[, "tau_alpha"]),
    alpha_player = extract_indexed_parameter(
      matrix, "alpha_player", nrow(fit$player_table)
    ),
    beta_role = beta_role
  )
}

add_perception_spatial_scale <- function(x, spatial_scale) {
  d <- data.table::copy(data.table::as.data.table(x))
  if ("spatial_scale" %in% names(d)) d[, spatial_scale := NULL]
  scale_table <- data.table::as.data.table(spatial_scale)[, .(
    initial_call = as.character(initial_call),
    spatial_scale = as.numeric(spatial_scale)
  )]
  d <- merge(d, scale_table, by = "initial_call", all.x = TRUE, sort = FALSE)
  if (any(!is.finite(d$spatial_scale)) || any(d$spatial_scale <= 0)) {
    stop("Scoring rows are missing a positive perception spatial scale")
  }
  d
}

score_perception <- function(fit, opportunities, ndraws = 500L, seed = 42L) {
  if (!inherits(fit, "gaussian_perception_fit")) {
    stop("fit must be a gaussian_perception_fit")
  }
  x <- add_perception_spatial_scale(opportunities, fit$spatial_scale)
  required <- c(
    "role", "player_role_id", "p_hat", "stake_G", "spatial_scale"
  )
  stop_if_missing_columns(x, required, "perception scoring opportunities")
  role_index <- match(x$role, fit$role_levels)
  if (anyNA(role_index)) stop("Scoring data contain an unknown perception role")
  player_index <- match(x$player_role_id, fit$player_table$player_role_id)
  new_table <- unique(x[is.na(player_index), .(player_role_id, role)])
  data.table::setorder(new_table, role, player_role_id)
  new_index <- match(x$player_role_id, new_table$player_role_id)
  draws <- perception_parameter_draws(fit, ndraws = ndraws, seed = seed)
  draw_count <- nrow(draws$mu_log_sigma)
  sum_p <- sum_challenge <- sum_challenge_zero <- sum_sigma <- numeric(nrow(x))
  stake_positive <- pmax(as.numeric(x$stake_G), 0)
  tracking_z <- stats::qnorm(clamp_perception_probability(x$p_hat))
  set.seed(seed + 1L)
  for (draw in seq_len(draw_count)) {
    sigma <- alpha <- numeric(nrow(x))
    known <- which(!is.na(player_index))
    if (length(known)) {
      sigma[known] <- draws$sigma_player[draw, player_index[known]]
      alpha[known] <- draws$alpha_player[draw, player_index[known]]
    }
    if (nrow(new_table)) {
      new_role <- match(new_table$role, fit$role_levels)
      new_sigma <- exp(
        draws$mu_log_sigma[draw, new_role] +
          draws$tau_log_sigma[[draw]] * stats::rnorm(nrow(new_table))
      )
      new_alpha <- draws$mu_alpha[draw, new_role] +
        draws$tau_alpha[[draw]] * stats::rnorm(nrow(new_table))
      missing <- which(is.na(player_index))
      sigma[missing] <- new_sigma[new_index[missing]]
      alpha[missing] <- new_alpha[new_index[missing]]
    }
    perceived <- stats::pnorm(
      tracking_z / sqrt(1 + (sigma / x$spatial_scale)^2)
    )
    decision_beta <- draws$beta_role[draw, role_index]
    eta <- alpha + decision_beta * log(
      pmax(perceived * stake_positive, fit$ev_reference * 1e-8) /
        fit$ev_reference
    )
    eta_zero <- alpha + decision_beta * log(
      pmax(x$p_hat * stake_positive, fit$ev_reference * 1e-8) /
        fit$ev_reference
    )
    sum_p <- sum_p + perceived
    sum_challenge <- sum_challenge + stats::plogis(eta)
    sum_challenge_zero <- sum_challenge_zero + stats::plogis(eta_zero)
    sum_sigma <- sum_sigma + sigma
  }
  x[, `:=`(
    p_perceived = sum_p / draw_count,
    challenge_probability = sum_challenge / draw_count,
    challenge_probability_sigma0 = sum_challenge_zero / draw_count,
    sigma_posterior_mean = sum_sigma / draw_count,
    player_seen_in_training = !is.na(player_index)
  )]
  probabilities <- unlist(x[, .(
    p_perceived, challenge_probability, challenge_probability_sigma0
  )], use.names = FALSE)
  if (any(!is.finite(probabilities)) || any(probabilities < 0 | probabilities > 1)) {
    stop("Perception scoring produced invalid probabilities")
  }
  x[]
}

summarize_draw_vector <- function(x, prefix = "") {
  quantiles <- stats::quantile(
    x, c(0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975),
    names = FALSE, na.rm = TRUE
  )
  values <- c(
    mean = mean(x), lower_95 = quantiles[[1L]], lower_80 = quantiles[[2L]],
    lower_50 = quantiles[[3L]], median = quantiles[[4L]],
    upper_50 = quantiles[[5L]], upper_80 = quantiles[[6L]],
    upper_95 = quantiles[[7L]]
  )
  names(values) <- paste0(prefix, names(values))
  as.list(values)
}

summarize_perception_fit <- function(fit, ndraws = NULL, seed = 42L) {
  draws <- perception_parameter_draws(fit, ndraws = ndraws, seed = seed)
  roles <- fit$role_levels
  population <- lapply(seq_along(roles), function(index) {
    mu <- draws$mu_log_sigma[, index]
    tau <- draws$tau_log_sigma
    median_sigma <- exp(mu)
    mean_sigma <- exp(mu + 0.5 * tau^2)
    sd_sigma <- sqrt((exp(tau^2) - 1) * exp(2 * mu + tau^2))
    data.table::as.data.table(c(
      list(role = roles[[index]]),
      summarize_draw_vector(mean_sigma, "population_sigma_"),
      summarize_draw_vector(median_sigma, "population_median_sigma_"),
      summarize_draw_vector(sd_sigma, "between_player_sigma_sd_")
    ))
  })
  population <- data.table::rbindlist(population, fill = TRUE)
  player <- lapply(seq_len(nrow(fit$player_table)), function(index) {
    data.table::as.data.table(c(
      as.list(fit$player_table[index]),
      summarize_draw_vector(draws$sigma_player[, index], "sigma_"),
      summarize_draw_vector(draws$alpha_player[, index], "trigger_alpha_")
    ))
  })
  player <- data.table::rbindlist(player, fill = TRUE)
  ordering <- data.table::data.table(
    comparison = "catcher_sigma_below_batter_sigma",
    posterior_probability = mean(
      exp(draws$mu_log_sigma[, match("catcher", roles)]) <
        exp(draws$mu_log_sigma[, match("batter", roles)])
    )
  )
  spread <- data.table::as.data.table(summarize_draw_vector(
    draws$tau_log_sigma, "tau_log_sigma_"
  ))
  sensitivity <- data.table::rbindlist(lapply(seq_along(roles), function(index) {
    data.table::as.data.table(c(
      list(role = roles[[index]]),
      summarize_draw_vector(draws$beta_role[, index], "decision_beta_")
    ))
  }), fill = TRUE)
  sensitivity_variant <- identical(fit$model_variant, "role_sensitivity")
  identifiability <- data.table::rbindlist(lapply(seq_along(roles), function(index) {
    correlation <- if (sensitivity_variant) {
      stats::cor(exp(draws$mu_log_sigma[, index]), draws$beta_role[, index])
    } else {
      NA_real_
    }
    data.table::data.table(
      role = roles[[index]],
      sigma_beta_posterior_correlation = correlation,
      interpretable = if (sensitivity_variant) abs(correlation) < 0.8 else NA
    )
  }))
  list(
    population = population,
    players = player,
    ordering = ordering,
    spread = spread,
    sensitivity = sensitivity,
    identifiability = identifiability
  )
}

perception_fit_diagnostics <- function(fit) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    stop("perception_fit_diagnostics() requires the posterior package")
  }
  summary <- posterior::summarise_draws(
    posterior::as_draws_array(fit$stan_fit), "rhat", "ess_bulk", "ess_tail"
  )
  sampler <- rstan::get_sampler_params(fit$stan_fit, inc_warmup = FALSE)
  divergences <- sum(vapply(
    sampler, function(x) sum(x[, "divergent__"] > 0), numeric(1L)
  ))
  max_rhat <- max(summary$rhat, na.rm = TRUE)
  min_bulk <- min(summary$ess_bulk, na.rm = TRUE)
  min_tail <- min(summary$ess_tail, na.rm = TRUE)
  data.table::data.table(
    max_rhat = max_rhat,
    min_bulk_ess = min_bulk,
    min_tail_ess = min_tail,
    divergences = divergences,
    pass = max_rhat <= 1.01 & min_bulk >= 400 & min_tail >= 400 & divergences == 0
  )
}

weighted_bin_rate <- function(x, weight_column) {
  weight <- as.numeric(x[[weight_column]])
  data.table::data.table(
    n = nrow(x),
    effective_n = sum(weight),
    rate = if (sum(weight) > 0) stats::weighted.mean(x$actual_wrong, weight) else NA_real_
  )
}

validate_flat55 <- function(
  fit, heldout_choices = NULL, min_bin_challenges = 50L,
  tolerance = 0.05, slope_tolerance = 0.01,
  bootstrap_reps = 500L, seed = 42L,
  primary_roles = c("batter", "catcher")
) {
  if (inherits(fit, "gaussian_perception_fit")) {
    if (is.null(heldout_choices)) {
      stop("Supply held-out choices with a gaussian_perception_fit")
    }
    scored <- if ("challenge_probability" %in% names(heldout_choices)) {
      heldout_choices
    } else {
      score_perception(fit, heldout_choices, seed = seed)
    }
  } else {
    scored <- if (is.null(heldout_choices)) fit else heldout_choices
  }
  x <- data.table::copy(data.table::as.data.table(scored))
  required <- c(
    "game_pk", "role", "challenged", "actual_wrong", "edge_distance_inches",
    "challenge_probability", "challenge_probability_sigma0"
  )
  stop_if_missing_columns(x, required, "flat-55 validation data")
  if (anyNA(x$actual_wrong)) stop("Flat-55 validation requires held-out ABS truth")
  x <- x[
    role %in% primary_roles & is.finite(edge_distance_inches) &
      edge_distance_inches >= -6 & edge_distance_inches <= 6
  ]
  breaks <- seq(-6, 6, by = 1)
  x[, edge_bin := cut(
    edge_distance_inches, breaks = breaks, include.lowest = TRUE,
    right = FALSE
  )]
  x <- x[!is.na(edge_bin)]
  levels_table <- data.table::data.table(
    edge_bin = levels(x$edge_bin),
    midpoint = head(breaks, -1L) + 0.5
  )
  actual <- x[challenged == 1L, .(
    observed_challenges = .N,
    observed_overturn_rate = mean(actual_wrong)
  ), by = edge_bin]
  predicted <- x[, .(
    predicted_challenges = sum(challenge_probability),
    predicted_overturn_rate = stats::weighted.mean(
      actual_wrong, challenge_probability
    )
  ), by = edge_bin]
  curve <- merge(levels_table, actual, by = "edge_bin", all.x = TRUE)
  curve <- merge(curve, predicted, by = "edge_bin", all.x = TRUE)
  curve[, difference := predicted_overturn_rate - observed_overturn_rate]
  included <- curve[
    is.finite(observed_challenges) & observed_challenges >= min_bin_challenges &
      is.finite(predicted_overturn_rate)
  ]
  if (nrow(included) < 2L) stop("Too few populated bins for flat-55 validation")
  slope_model <- stats::lm(
    predicted_overturn_rate ~ midpoint,
    data = included,
    weights = predicted_challenges
  )
  predicted_slope <- unname(stats::coef(slope_model)[["midpoint"]])

  games <- sort(unique(x$game_pk))
  bootstrap_reps <- as.integer(bootstrap_reps)
  slopes <- rep(NA_real_, bootstrap_reps)
  set.seed(seed)
  for (replicate in seq_len(bootstrap_reps)) {
    sampled <- sample(games, length(games), replace = TRUE)
    game_weight <- data.table::data.table(game_pk = sampled)[, .(game_weight = .N), by = game_pk]
    draw <- merge(x, game_weight, by = "game_pk", all = FALSE)
    draw[, prediction_weight := challenge_probability * game_weight]
    draw_curve <- draw[, .(
      predicted_challenges = sum(prediction_weight),
      predicted_overturn_rate = stats::weighted.mean(actual_wrong, prediction_weight)
    ), by = edge_bin]
    draw_curve <- merge(levels_table, draw_curve, by = "edge_bin")
    draw_curve <- draw_curve[edge_bin %in% included$edge_bin]
    if (nrow(draw_curve) >= 2L && all(is.finite(draw_curve$predicted_overturn_rate))) {
      slopes[[replicate]] <- unname(stats::coef(stats::lm(
        predicted_overturn_rate ~ midpoint,
        data = draw_curve,
        weights = predicted_challenges
      ))[["midpoint"]])
    }
  }
  slope_interval <- stats::quantile(
    slopes, c(0.025, 0.975), na.rm = TRUE, names = FALSE
  )

  probability <- clamp_perception_probability(x$challenge_probability)
  probability_zero <- clamp_perception_probability(x$challenge_probability_sigma0)
  y <- as.numeric(x$challenged)
  x[, `:=`(
    loss_model__ = -(y * log(probability) + (1 - y) * log(1 - probability)),
    loss_sigma0__ = -(y * log(probability_zero) + (1 - y) * log(1 - probability_zero))
  )]
  paired <- x[, .(
    loss_difference = mean(loss_model__ - loss_sigma0__)
  ), by = game_pk]
  loss_difference <- mean(paired$loss_difference)
  loss_difference_se <- stats::sd(paired$loss_difference) / sqrt(nrow(paired))
  bin_pass <- all(abs(included$difference) <= tolerance)
  slope_pass <- abs(predicted_slope) <= slope_tolerance &&
    slope_interval[[1L]] <= 0 && slope_interval[[2L]] >= 0
  loss_pass <- loss_difference <= loss_difference_se
  gate <- data.table::data.table(
    included_bins = nrow(included),
    max_absolute_bin_error = max(abs(included$difference)),
    bin_tolerance = tolerance,
    predicted_slope_per_inch = predicted_slope,
    slope_lower_95 = slope_interval[[1L]],
    slope_upper_95 = slope_interval[[2L]],
    slope_tolerance = slope_tolerance,
    challenge_log_loss = mean(x$loss_model__),
    sigma0_challenge_log_loss = mean(x$loss_sigma0__),
    paired_log_loss_difference = loss_difference,
    paired_log_loss_difference_se = loss_difference_se,
    bin_pass = bin_pass,
    slope_pass = slope_pass,
    log_loss_pass = loss_pass,
    pass = bin_pass & slope_pass & loss_pass
  )
  role_summary <- x[, .(
    opportunities = .N,
    actual_attempts = sum(challenged),
    predicted_attempts = sum(challenge_probability),
    actual_attempt_rate = mean(challenged),
    predicted_attempt_rate = mean(challenge_probability),
    actual_success_rate = if (sum(challenged)) mean(actual_wrong[challenged == 1L]) else NA_real_,
    predicted_selected_success_rate = stats::weighted.mean(
      actual_wrong, challenge_probability
    ),
    mean_edge_distance_actual = if (sum(challenged)) {
      mean(edge_distance_inches[challenged == 1L])
    } else NA_real_,
    mean_edge_distance_predicted = stats::weighted.mean(
      edge_distance_inches, challenge_probability
    )
  ), by = role]
  location_summary <- x[, .(
    opportunities = .N,
    actual_attempts = sum(challenged),
    predicted_attempts = sum(challenge_probability),
    actual_success_rate = if (sum(challenged)) {
      mean(actual_wrong[challenged == 1L])
    } else NA_real_,
    predicted_selected_success_rate = stats::weighted.mean(
      actual_wrong, challenge_probability
    )
  ), by = .(role, edge_bin)]
  timing_summary <- data.table::data.table()
  if ("inning" %in% names(x)) {
    x[, inning_group__ := data.table::fifelse(
      inning >= 10L, "10+", as.character(inning)
    )]
    timing_summary <- x[, .(
      opportunities = .N,
      actual_attempts = sum(challenged),
      predicted_attempts = sum(challenge_probability),
      actual_attempt_rate = mean(challenged),
      predicted_attempt_rate = mean(challenge_probability)
    ), by = .(role, inning_group = inning_group__)]
  }
  list(
    gate = gate,
    curve = curve[order(midpoint)],
    role_summary = role_summary,
    location_summary = location_summary,
    timing_summary = timing_summary,
    bootstrap_slopes = data.table::data.table(replicate = seq_along(slopes), slope = slopes)
  )
}

plot_flat55_validation <- function(validation) {
  curve <- data.table::copy(validation$curve)
  long <- data.table::rbindlist(list(
    curve[, .(midpoint, rate = observed_overturn_rate, source = "Actual challenges")],
    curve[, .(midpoint, rate = predicted_overturn_rate, source = "Perception model")]
  ))
  ggplot2::ggplot(long[is.finite(rate)], ggplot2::aes(
    midpoint, rate, color = source, linetype = source
  )) +
    ggplot2::geom_hline(yintercept = 0.55, color = "grey65", linetype = "dotted") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      x = "Signed ball-edge distance from the ABS boundary (inches)",
      y = "Overturn rate",
      color = NULL, linetype = NULL,
      title = "Does the Perception Model Reproduce the Flat Overturn Curve?",
      subtitle = "Model fitting uses challenge/pass choices; overturn outcomes are held out"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom")
}

prepare_sharp_car_input <- function(pitches, call_type, bin_width = 1.5) {
  if (!call_type %in% c("ball", "called_strike")) {
    stop("call_type must be ball or called_strike")
  }
  x <- assign_joint_perception_cells(pitches, bin_width)
  x <- x[as.character(initial_call) == call_type]
  if (!nrow(x)) stop("No training pitches for ", call_type)
  all_cells <- unique(x[, .(ix, iz, cell)])
  cells <- largest_rook_component(all_cells)
  data.table::setorder(cells, ix, iz)
  x <- x[cell %in% cells$cell]
  adjacency <- joint_rook_adjacency(cells)
  aggregated <- x[, .(
    n = .N,
    wrong = sum(call_wrong)
  ), by = .(cell, umpire, catcher)]
  aggregated[, `:=`(
    cell = factor(as.character(cell), levels = cells$cell),
    umpire = factor(umpire),
    catcher = factor(catcher)
  )]
  list(
    data = aggregated,
    M = adjacency,
    cell_table = cells,
    call_type = call_type,
    bin_width = as.numeric(bin_width),
    pitch_count = nrow(x),
    dropped_disconnected = nrow(all_cells) - nrow(cells)
  )
}

fit_sharp_car_model <- function(
  input, chains = 4L, cores = chains, iter = 8000L, warmup = 4000L,
  seed = 42L, file = NULL, refresh = 100L, force_refit = FALSE
) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("fit_sharp_car_model() requires the brms package")
  }
  formula <- brms::bf(
    wrong | trials(n) ~ car(M, gr = cell, type = "esicar") +
      (1 | umpire) + (1 | catcher)
  )
  priors <- c(
    brms::prior(student_t(3, 0, 10), class = "sdcar"),
    brms::prior(normal(0, 0.2), class = "sd")
  )
  arguments <- list(
    formula = formula,
    data = input$data,
    data2 = list(M = input$M),
    family = stats::binomial(),
    prior = priors,
    control = list(adapt_delta = 0.95),
    chains = as.integer(chains),
    cores = min(as.integer(cores), as.integer(chains)),
    iter = as.integer(iter),
    warmup = as.integer(warmup),
    init = 0,
    seed = as.integer(seed),
    refresh = as.integer(refresh)
  )
  if (!is.null(file)) {
    arguments$file <- sub("\\.rds$", "", file)
    arguments$file_refit <- if (isTRUE(force_refit)) "always" else "on_change"
  }
  fit <- do.call(brms::brm, arguments)
  attr(fit, "sharp_car_spec") <- list(
    call_type = input$call_type,
    bin_width = input$bin_width,
    cell_table = input$cell_table
  )
  if (!is.null(file)) {
    path <- if (grepl("\\.rds$", file)) file else paste0(file, ".rds")
    saveRDS(fit, path)
  }
  fit
}

score_sharp_car_model <- function(
  fit, pitches, ndraws = 500L, chunk_size = 5000L, seed = 42L
) {
  spec <- attr(fit, "sharp_car_spec")
  if (is.null(spec$call_type) || is.null(spec$cell_table)) {
    stop("Sharp CAR fit is missing its scoring specification")
  }
  x <- map_to_joint_training_cells(pitches, spec$cell_table, spec$bin_width)
  x <- x[as.character(initial_call) == spec$call_type]
  x[, n := 1L]
  prediction <- numeric(nrow(x))
  chunks <- split(seq_len(nrow(x)), ceiling(seq_len(nrow(x)) / as.integer(chunk_size)))
  set.seed(seed)
  for (indices in chunks) {
    draws <- brms::posterior_epred(
      fit,
      newdata = x[indices],
      ndraws = as.integer(ndraws),
      allow_new_levels = TRUE,
      sample_new_levels = "gaussian"
    )
    prediction[indices] <- colMeans(draws)
  }
  x[, p_hat := prediction]
  if (any(!is.finite(x$p_hat)) || any(!data.table::between(x$p_hat, 0, 1))) {
    stop("Sharp CAR scoring produced invalid probabilities")
  }
  x[]
}

score_sharp_car_pair <- function(
  ball_fit, strike_fit, ledger, ndraws = 500L, seed = 42L
) {
  original <- data.table::copy(data.table::as.data.table(ledger))
  ball <- score_sharp_car_model(ball_fit, original, ndraws = ndraws, seed = seed)
  strike <- score_sharp_car_model(
    strike_fit, original, ndraws = ndraws, seed = seed + 1L
  )
  probabilities <- data.table::rbindlist(list(
    ball[, .(game_pk, pitch_order, p_hat)],
    strike[, .(game_pk, pitch_order, p_hat)]
  ))
  if (anyDuplicated(probabilities[, .(game_pk, pitch_order)])) {
    stop("Sharp CAR pair produced duplicate pitch probabilities")
  }
  if ("p_hat" %in% names(original)) original[, p_hat := NULL]
  merge(
    original, probabilities,
    by = c("game_pk", "pitch_order"), all.x = TRUE, sort = FALSE
  )
}

score_saved_sharp_car_pair <- function(
  ball_fit, strike_fit, ledger, ndraws = 500L, seed = 42L
) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("score_saved_sharp_car_pair() requires the brms package")
  }
  original <- data.table::copy(data.table::as.data.table(ledger))
  assigned <- assign_joint_perception_cells(original, 1.5)
  score_fit_data <- function(fit, call, draw_seed) {
    d <- data.table::copy(data.table::as.data.table(fit$data))
    set.seed(draw_seed)
    expected_count <- stats::fitted(
      fit, ndraws = as.integer(ndraws)
    )[, "Estimate"]
    d[, p_hat := expected_count / n]
    unique(d[, .(
      cell = as.character(cell),
      umpire = as.character(umpire),
      catcher = as.character(catcher),
      initial_call = call,
      p_hat
    )])
  }
  probability_table <- data.table::rbindlist(list(
    score_fit_data(ball_fit, "ball", seed),
    score_fit_data(strike_fit, "called_strike", seed + 1L)
  ))
  assigned[, `:=`(
    cell = as.character(cell),
    umpire = as.character(umpire),
    catcher = as.character(catcher),
    initial_call = as.character(initial_call)
  )]
  assigned <- merge(
    assigned, probability_table,
    by = c("cell", "umpire", "catcher", "initial_call"),
    all.x = TRUE, sort = FALSE
  )
  probabilities <- assigned[, .(game_pk, pitch_order, p_hat)]
  if (anyDuplicated(probabilities[, .(game_pk, pitch_order)])) {
    stop("Saved CAR scoring produced duplicate pitch probabilities")
  }
  if ("p_hat" %in% names(original)) original[, p_hat := NULL]
  merge(
    original, probabilities,
    by = c("game_pk", "pitch_order"), all.x = TRUE, sort = FALSE
  )
}

perception_choices_to_pitch_probabilities <- function(scored_choices) {
  x <- data.table::as.data.table(scored_choices)
  stop_if_missing_columns(
    x, c("game_pk", "pitch_order", "initial_call", "role", "p_hat", "p_perceived"),
    "scored perception choices"
  )
  primary <- x[
    (initial_call == "called_strike" & role == "batter") |
      (initial_call == "ball" & role == "catcher")
  ]
  out <- unique(primary[, .(
    game_pk, pitch_order, p_hat = as.numeric(p_hat),
    p_perceived = as.numeric(p_perceived),
    observer_role = role,
    observer_player_id = player_id
  )])
  if (anyDuplicated(out[, .(game_pk, pitch_order)])) {
    stop("Primary perception scoring produced duplicate pitch probabilities")
  }
  out[]
}

merge_perception_probabilities <- function(ledger, scored_choices) {
  x <- data.table::copy(data.table::as.data.table(ledger))
  probabilities <- perception_choices_to_pitch_probabilities(scored_choices)
  if ("p_hat" %in% names(x)) x[, p_hat := NULL]
  if ("p_perceived" %in% names(x)) x[, p_perceived := NULL]
  merge(x, probabilities, by = c("game_pk", "pitch_order"), all.x = TRUE, sort = FALSE)
}

crossfit_perception_mdp <- function(
  fold_objects, re_model, validation = NULL, prior_n = 30,
  tol = 1e-10, max_iter = 10000L, bootstrap_reps = 500L, seed = 42L
) {
  if (is.null(validation)) {
    heldout <- data.table::rbindlist(lapply(fold_objects, `[[`, "test_scored"), fill = TRUE)
    if ("inventory_before" %in% names(heldout)) {
      heldout <- heldout[inventory_before > 0L]
    }
    if (all(c("challenge_occurred", "challenger_role", "role") %in% names(heldout))) {
      heldout <- heldout[!(
        challenge_occurred %in% TRUE &
          ((role == "catcher" & challenger_role == "pitcher") |
            (role == "pitcher" & challenger_role == "catcher"))
      )]
    }
    validation <- validate_flat55(heldout, bootstrap_reps = bootstrap_reps, seed = seed)
  }
  if (!isTRUE(validation$gate$pass[[1L]])) {
    return(list(
      status = "flat55_gate_failed", validation = validation,
      evaluation = data.table::data.table(), decomposition = data.table::data.table()
    ))
  }
  replay_parts <- list()
  cursor <- 0L
  for (fold_id in seq_along(fold_objects)) {
    item <- fold_objects[[fold_id]]
    train_games <- unique(as.character(item$train_ledger$game_pk))
    test_games <- unique(as.character(item$test_ledger$game_pk))
    if (length(intersect(train_games, test_games))) stop("Perception MDP fold leakage")
    train_ledger <- merge_perception_probabilities(item$train_ledger, item$train_scored)
    test_ledger <- merge_perception_probabilities(item$test_ledger, item$test_scored)
    train_tracking <- prepare_mdp_opportunities(train_ledger, re_model, p_col = "p_hat")
    test_tracking <- prepare_mdp_opportunities(test_ledger, re_model, p_col = "p_hat")
    train_human <- prepare_mdp_opportunities(train_ledger, re_model, p_col = "p_perceived")
    test_human <- prepare_mdp_opportunities(test_ledger, re_model, p_col = "p_perceived")
    tracking_fit <- fit_challenge_mdp(
      train_tracking, prior_n = prior_n, tol = tol, max_iter = max_iter
    )
    human_fit <- fit_challenge_mdp(
      train_human, prior_n = prior_n, tol = tol, max_iter = max_iter
    )
    policies <- list(
      tracking_mdp = replay_challenge_policy(test_tracking, tracking_fit, "mdp"),
      human_eyes_mdp = replay_challenge_policy(test_human, human_fit, "mdp"),
      observed = replay_challenge_policy(test_tracking, policy = "observed"),
      never = replay_challenge_policy(test_tracking, policy = "never")
    )
    for (name in names(policies)) {
      cursor <- cursor + 1L
      policies[[name]][, `:=`(policy = name, fold = fold_id)]
      replay_parts[[cursor]] <- policies[[name]]
    }
  }
  replays <- data.table::rbindlist(replay_parts, fill = TRUE)
  behavior <- replays[, .(
    opportunities = .N,
    attempts = sum(policy_challenge),
    challenge_rate = mean(policy_challenge),
    successes = sum(policy_success),
    success_rate = if (sum(policy_challenge)) {
      sum(policy_success) / sum(policy_challenge)
    } else NA_real_,
    captured_re = sum(captured_re),
    zero_inventory_rate = mean(inventory_before_policy == 0L)
  ), by = .(policy, role)]
  evaluation <- summarize_mdp_evaluation(
    replays, reps = bootstrap_reps, seed = seed
  )
  summary <- evaluation$summary
  lookup <- stats::setNames(summary$total_re, summary$policy)
  mean_lookup <- stats::setNames(summary$mean_re_team_game, summary$policy)
  decomposition <- data.table::data.table(
    component = c("perception_cost", "strategy_cost"),
    total_re = c(
      lookup[["tracking_mdp"]] - lookup[["human_eyes_mdp"]],
      lookup[["human_eyes_mdp"]] - lookup[["observed"]]
    ),
    mean_re_team_game = c(
      mean_lookup[["tracking_mdp"]] - mean_lookup[["human_eyes_mdp"]],
      mean_lookup[["human_eyes_mdp"]] - mean_lookup[["observed"]]
    )
  )
  if (!is.null(evaluation$bootstrap) && nrow(evaluation$bootstrap)) {
    wide <- data.table::dcast(
      evaluation$bootstrap, replicate ~ policy,
      value.var = "mean_re_team_game"
    )
    season_team_games <- data.table::uniqueN(
      evaluation$team_game, by = c("game_pk", "team_id")
    )
    draws <- data.table::rbindlist(list(
      data.table::data.table(
        component = "perception_cost",
        value = (wide$tracking_mdp - wide$human_eyes_mdp) * season_team_games
      ),
      data.table::data.table(
        component = "strategy_cost",
        value = (wide$human_eyes_mdp - wide$observed) * season_team_games
      )
    ))
    intervals <- draws[, .(
      lower_95 = stats::quantile(value, 0.025, names = FALSE),
      upper_95 = stats::quantile(value, 0.975, names = FALSE)
    ), by = component]
    decomposition <- merge(decomposition, intervals, by = "component", all.x = TRUE)
  }
  list(
    status = "complete", validation = validation,
    evaluation = summary, decomposition = decomposition,
    role_evaluation = behavior,
    replays = replays, team_game = evaluation$team_game,
    bootstrap = evaluation$bootstrap
  )
}
