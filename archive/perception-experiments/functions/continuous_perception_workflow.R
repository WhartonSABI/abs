# Cross-fitting, model-selection, diagnostics, and artifact helpers for the
# continuous human-perception analysis. Heavy posterior objects remain in the
# targets store; only compact summaries are written to data/processed.

continuous_perception_profile_from_env <- function() {
  value <- Sys.getenv("ABS_PERCEPTION_PROFILE", unset = "pilot")
  continuous_perception_profile(value)
}

continuous_cmdstan_version <- function() "2.39.0"

continuous_cmdstan_path <- function() {
  file.path(
    continuous_model_root(), "data", "processed", "stan",
    paste0("cmdstan-", continuous_cmdstan_version())
  )
}

configure_continuous_cmdstan <- function(require_install = TRUE) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("The perception workflow requires locked cmdstanr")
  }
  path <- continuous_cmdstan_path()
  if (!dir.exists(path)) {
    if (isTRUE(require_install)) {
      stop(
        "Pinned CmdStan ", continuous_cmdstan_version(),
        " is missing. Run `make perception-setup`."
      )
    }
    return(FALSE)
  }
  cmdstanr::set_cmdstan_path(path)
  installed <- as.character(cmdstanr::cmdstan_version())
  if (!identical(installed, continuous_cmdstan_version())) {
    stop("CmdStan version mismatch: expected ", continuous_cmdstan_version(),
      ", found ", installed)
  }
  TRUE
}

read_continuous_parquet <- function(path, label = basename(path)) {
  if (length(path) != 1L || !file.exists(path)) {
    stop("Missing ", label, ": ", path)
  }
  data.table::as.data.table(arrow::read_parquet(path))
}

continuous_training_games <- function(folds, heldout_fold) {
  x <- data.table::as.data.table(folds)
  validate_continuous_game_folds(x, max(x$fold))
  as.character(x[fold != as.integer(heldout_fold), game_pk])
}

continuous_heldout_games <- function(folds, heldout_fold) {
  x <- data.table::as.data.table(folds)
  validate_continuous_game_folds(x, max(x$fold))
  as.character(x[fold == as.integer(heldout_fold), game_pk])
}

assert_continuous_fold_separation <- function(training_games, heldout_games) {
  overlap <- intersect(as.character(training_games), as.character(heldout_games))
  if (length(overlap)) {
    stop("Continuous-perception training and held-out games overlap")
  }
  invisible(TRUE)
}

continuous_pilot_sample <- function(rows, fraction = 0.20, seed = 20260825L,
                                    strata = NULL) {
  x <- data.table::copy(data.table::as.data.table(rows))
  fraction <- as.numeric(fraction)
  if (!is.finite(fraction) || fraction <= 0 || fraction > 1) {
    stop("Pilot fraction must lie in (0, 1]")
  }
  if (fraction == 1 || !nrow(x)) return(x[])
  if (is.null(strata)) {
    candidates <- intersect(c("batter_id", "pitcher_id"), names(x))
    strata <- if (length(candidates)) candidates[[1L]] else NULL
  }
  if (!length(strata) || !strata %in% names(x)) strata <- NULL
  set.seed(seed)
  if (is.null(strata)) {
    return(x[sample.int(.N, max(1L, floor(.N * fraction)))])
  }
  x[, row_sample__ := stats::runif(.N)]
  sampled <- x[order(row_sample__), head(.SD, max(1L, floor(.N * fraction))),
    by = strata]
  sampled[, row_sample__ := NULL]
  sampled[]
}

continuous_case_control_decision_sample <- function(
  rows, zero_fraction = 0.25, seed = 20260825L
) {
  x <- prepare_continuous_decision_rows(rows)
  zero_fraction <- as.numeric(zero_fraction)
  if (!is.finite(zero_fraction) || zero_fraction <= 0 || zero_fraction > 1) {
    stop("zero_fraction must lie in (0, 1]")
  }
  if (zero_fraction == 1) return(x[])
  set.seed(seed)
  x[, retained__ := challenged == 1L | stats::runif(.N) <= zero_fraction]
  sampled <- x[retained__ == TRUE]
  sampled[, `:=`(
    sampling_offset = log(1 / zero_fraction),
    retained__ = NULL
  )]
  sampled[]
}

continuous_game_clustered_mean <- function(game_pk, value) {
  x <- data.table::data.table(
    game_pk = as.character(game_pk), value = as.numeric(value)
  )
  x <- x[is.finite(value) & !is.na(game_pk)]
  if (!nrow(x)) {
    return(data.table::data.table(mean = NA_real_, game_se = NA_real_, games = 0L))
  }
  by_game <- x[, .(value = mean(value)), by = game_pk]
  data.table::data.table(
    mean = mean(by_game$value),
    game_se = if (nrow(by_game) > 1L) {
      stats::sd(by_game$value) / sqrt(nrow(by_game))
    } else {
      NA_real_
    },
    games = nrow(by_game)
  )
}

select_continuous_pitch_mixture_one_se <- function(scores) {
  x <- data.table::copy(data.table::as.data.table(scores))
  stop_if_missing_columns(
    x, c("components", "game_pk", "log_score"),
    "continuous pitch-mixture selection scores"
  )
  if (!all(unique(x$components) %in% c(1L, 3L, 6L))) {
    stop("Pitch-mixture candidates must have 1, 3, or 6 components")
  }
  per_game <- x[is.finite(log_score), .(log_score = mean(log_score)),
    by = .(components = as.integer(components), game_pk)]
  summary <- per_game[, .(
    mean_log_score = mean(log_score),
    game_se = if (.N > 1L) stats::sd(log_score) / sqrt(.N) else NA_real_,
    games = .N
  ), by = components][order(components)]
  if (!nrow(summary)) stop("No finite pitch-mixture scores are available")
  best <- summary[which.max(mean_log_score)]
  tolerance <- if (is.finite(best$game_se)) best$game_se else 0
  eligible <- summary[mean_log_score >= best$mean_log_score - tolerance]
  chosen <- min(eligible$components)
  list(
    components = as.integer(chosen),
    scores = summary,
    best_components = as.integer(best$components),
    one_se_floor = best$mean_log_score - tolerance
  )
}

continuous_inner_folds <- function(game_pk, outer_fold, folds = 3L,
                                   seed = 20260825L) {
  continuous_game_folds(
    game_pk, folds = folds, seed = seed + 1000L * as.integer(outer_fold)
  )
}

fit_continuous_pitch_prior_selection <- function(
  rows, outer_fold, profile, backend = "cmdstanr", inner_folds = 3L
) {
  x <- data.table::as.data.table(rows)
  fold_table <- continuous_inner_folds(
    x$game_pk, outer_fold, inner_folds, profile$seed
  )
  scores <- list()
  score_index <- 0L
  for (components in c(1L, 3L, 6L)) {
    for (inner_fold in seq_len(inner_folds)) {
      training_games <- fold_table[fold != inner_fold, game_pk]
      validation_games <- fold_table[fold == inner_fold, game_pk]
      train <- x[as.character(game_pk) %in% training_games]
      validation <- x[as.character(game_pk) %in% validation_games]
      fit <- fit_continuous_pitch_prior(
        train, components = components, backend = backend,
        chains = profile$chains,
        parallel_chains = profile$parallel_chains,
        iter_warmup = profile$iter_warmup,
        iter_sampling = profile$iter_sampling,
        seed = profile$seed + 10000L * outer_fold + 100L * components + inner_fold,
        refresh = 0L
      )
      scored <- score_continuous_pitch_prior(
        fit, validation, ndraws = profile$posterior_draws,
        seed = profile$seed + inner_fold
      )
      score_index <- score_index + 1L
      scores[[score_index]] <- scored[, .(
        components = as.integer(components), game_pk,
        log_score = log_posterior_predictive_density
      )]
    }
  }
  select_continuous_pitch_mixture_one_se(
    data.table::rbindlist(scores, use.names = TRUE)
  )
}

fit_continuous_perception_fold <- function(
  fold, folds, location_rows, swing_rows, call_rows, profile,
  backend = "cmdstanr"
) {
  fold <- as.integer(fold)
  training_games <- continuous_training_games(folds, fold)
  heldout_games <- continuous_heldout_games(folds, fold)
  assert_continuous_fold_separation(training_games, heldout_games)

  location_train <- data.table::as.data.table(location_rows)[
    as.character(game_pk) %in% training_games
  ]
  swing_train <- data.table::as.data.table(swing_rows)[
    as.character(game_pk) %in% training_games
  ]
  call_train <- data.table::as.data.table(call_rows)[
    as.character(game_pk) %in% training_games
  ]
  assert_perception_fit_is_label_free(names(location_train))
  assert_perception_fit_is_label_free(names(swing_train))
  assert_perception_fit_is_label_free(names(call_train))

  if (profile$name == "pilot") {
    location_train <- continuous_pilot_sample(
      location_train, 0.20, profile$seed + fold, "pitcher_id"
    )
    swing_train <- continuous_pilot_sample(
      swing_train, 0.20, profile$seed + fold, "batter_id"
    )
    call_train <- continuous_pilot_sample(
      call_train, 0.20, profile$seed + fold, "umpire_id"
    )
  }

  mixture <- fit_continuous_pitch_prior_selection(
    location_train, fold, profile, backend = backend,
    inner_folds = if (profile$name == "pilot") 2L else 3L
  )
  signal_fit <- fit_continuous_batter_perception(
    swing_train, anisotropy = "isotropic", backend = backend,
    chains = profile$chains,
    parallel_chains = profile$parallel_chains,
    iter_warmup = profile$iter_warmup,
    iter_sampling = profile$iter_sampling,
    seed = profile$seed + 20000L + fold, refresh = 0L
  )
  signal_anisotropic_fit <- fit_continuous_batter_perception(
    swing_train, anisotropy = "shared", backend = backend,
    chains = profile$chains, parallel_chains = profile$parallel_chains,
    iter_warmup = profile$iter_warmup,
    iter_sampling = profile$iter_sampling,
    seed = profile$seed + 25000L + fold, refresh = 0L
  )
  prior_fit <- fit_continuous_pitch_prior(
    location_train, components = mixture$components, backend = backend,
    chains = profile$chains, parallel_chains = profile$parallel_chains,
    iter_warmup = profile$iter_warmup,
    iter_sampling = profile$iter_sampling,
    seed = profile$seed + 30000L + fold, refresh = 0L
  )
  prior_no_family_fit <- fit_continuous_pitch_prior(
    location_train, components = mixture$components,
    include_pitch_family = FALSE, backend = backend,
    chains = profile$chains, parallel_chains = profile$parallel_chains,
    iter_warmup = profile$iter_warmup,
    iter_sampling = profile$iter_sampling,
    seed = profile$seed + 40000L + fold, refresh = 0L
  )
  call_fit <- fit_continuous_call_cue(
    call_train, backend = backend, chains = profile$chains,
    parallel_chains = profile$parallel_chains,
    iter_warmup = profile$iter_warmup,
    iter_sampling = profile$iter_sampling,
    seed = profile$seed + 50000L + fold, refresh = 0L
  )
  structure(list(
    fold = fold,
    training_games = training_games,
    heldout_games = heldout_games,
    mixture_selection = mixture,
    signal_fit = signal_fit,
    signal_isotropic_fit = signal_fit,
    signal_anisotropic_fit = signal_anisotropic_fit,
    prior_fit = prior_fit,
    prior_no_family_fit = prior_no_family_fit,
    call_fit = call_fit,
    profile = profile$name
  ), class = "continuous_perception_fold_fit")
}

score_continuous_anisotropy_fold <- function(
  fold_fit, swing_rows, folds, profile
) {
  heldout_games <- continuous_heldout_games(folds, fold_fit$fold)
  heldout <- data.table::as.data.table(swing_rows)[
    as.character(game_pk) %in% heldout_games
  ]
  isotropic <- score_continuous_batter_perception(
    fold_fit$signal_isotropic_fit, heldout,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 26000L + fold_fit$fold
  )
  anisotropic <- score_continuous_batter_perception(
    fold_fit$signal_anisotropic_fit, heldout,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 27000L + fold_fit$fold
  )
  compared <- compare_continuous_batter_anisotropy_heldout(
    isotropic, anisotropic
  )
  compared$by_game[, fold := fold_fit$fold]
  list(
    fold = fold_fit$fold,
    metrics = compared$metrics,
    by_game = compared$by_game
  )
}

run_continuous_anisotropy_recovery <- function(
  swing_rows, profile, backend = "cmdstanr", true_ratio = 1.5
) {
  x <- continuous_pilot_sample(
    swing_rows, if (profile$name == "pilot") 0.10 else 0.25,
    seed = profile$seed + 28000L, strata = "batter_id"
  )
  simulated <- simulate_continuous_batter_anisotropy(
    x, sigma_inches = 3, anisotropy_ratio = true_ratio,
    seed = profile$seed + 28001L
  )
  fit <- fit_continuous_batter_perception(
    simulated, anisotropy = "shared", backend = backend,
    chains = profile$chains, parallel_chains = profile$parallel_chains,
    iter_warmup = profile$iter_warmup,
    iter_sampling = profile$iter_sampling,
    seed = profile$seed + 28002L, refresh = 0L
  )
  list(
    fit = fit,
    validation = validate_continuous_batter_anisotropy_recovery(
      fit, true_anisotropy_ratio = true_ratio,
      ndraws = profile$posterior_draws,
      seed = profile$seed + 28003L
    )
  )
}

aggregate_continuous_anisotropy_gate <- function(
  fold_scores, recovery_validation
) {
  if (!is.list(fold_scores) || !length(fold_scores)) {
    stop("Anisotropy gate requires held-out fold scores")
  }
  by_game <- data.table::rbindlist(
    lapply(fold_scores, `[[`, "by_game"), fill = TRUE
  )
  metrics <- data.table::data.table(
    log_loss_improvement = mean(by_game$mean_log_loss_improvement),
    game_clustered_se = stats::sd(by_game$mean_log_loss_improvement) /
      sqrt(nrow(by_game))
  )
  gate_continuous_batter_anisotropy(
    metrics, recovery_validation
  )
}

apply_continuous_anisotropy_gate <- function(fold_fit, gate) {
  gate <- data.table::as.data.table(gate)
  if (nrow(gate) != 1L) stop("Anisotropy gate must contain one row")
  selected <- if (isTRUE(gate$promote_shared_anisotropy)) {
    fold_fit$signal_anisotropic_fit
  } else {
    fold_fit$signal_isotropic_fit
  }
  fold_fit$signal_fit <- selected
  fold_fit$anisotropy_gate <- gate
  fold_fit
}

score_continuous_batter_sigma_zero <- function(
  fit, choices, ndraws = 400L, seed = 20260825L
) {
  features <- continuous_batter_scoring_features(choices, fit)
  x <- features$rows
  posterior <- draws_continuous_batter_perception(fit, ndraws, seed)
  player <- match(as.character(x$batter_id), fit$player_table$batter_id)
  strike <- as.integer(x$strikes) + 1L
  probability <- matrix(NA_real_, nrow(x), length(posterior$draw_id))
  for (draw in seq_along(posterior$draw_id)) {
    threshold <- rep(posterior$mu_threshold[[draw]], nrow(x))
    seen <- !is.na(player)
    threshold[seen] <- posterior$threshold_player[draw, player[seen]]
    context_shift <- if (ncol(features$context)) {
      as.numeric(features$context %*% posterior$beta_context[draw, ])
    } else numeric(nrow(x))
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
    inside <- as.numeric(
      threshold + context_shift + sector_shift >= x$edge_distance_inches
    )
    lower <- posterior$lower_swing[draw, strike]
    upper <- posterior$upper_swing[draw, strike]
    probability[, draw] <- pmin(
      1 - 1e-9, pmax(1e-9, lower + (upper - lower) * inside)
    )
  }
  data.table::data.table(
    game_pk = x$game_pk,
    swing = x$swing,
    swing_probability_mean = rowMeans(probability)
  )
}

continuous_calibration_error <- function(observed, probability, bins = 10L) {
  x <- data.table::data.table(
    observed = as.numeric(observed), probability = as.numeric(probability)
  )
  x <- x[observed %in% 0:1 & is.finite(probability)]
  if (!nrow(x)) return(NA_real_)
  x[, bin := pmin(as.integer(bins), floor(probability * bins) + 1L)]
  grouped <- x[, .(
    n = .N, observed = mean(observed), predicted = mean(probability)
  ), by = bin]
  sum(grouped$n / sum(grouped$n) * abs(grouped$observed - grouped$predicted))
}

continuous_pitch_prior_rosenblatt <- function(prior, location) {
  parsed <- continuous_prior_component_covariances(prior)
  location <- as.numeric(location)
  if (length(location) != 2L || any(!is.finite(location))) {
    stop("Location PIT requires two finite coordinates")
  }
  components <- length(parsed$weights)
  marginal_x <- density_x <- conditional_z <- numeric(components)
  for (component in seq_len(components)) {
    covariance <- parsed$covariance[component, , ]
    sd_x <- sqrt(covariance[1L, 1L])
    sd_z <- sqrt(covariance[2L, 2L])
    rho <- covariance[1L, 2L] / (sd_x * sd_z)
    marginal_x[[component]] <- stats::pnorm(
      location[[1L]], parsed$means[component, 1L], sd_x
    )
    density_x[[component]] <- stats::dnorm(
      location[[1L]], parsed$means[component, 1L], sd_x
    )
    conditional_mean <- parsed$means[component, 2L] +
      rho * sd_z / sd_x *
        (location[[1L]] - parsed$means[component, 1L])
    conditional_sd <- sd_z * sqrt(pmax(1e-12, 1 - rho^2))
    conditional_z[[component]] <- stats::pnorm(
      location[[2L]], conditional_mean, conditional_sd
    )
  }
  conditional_weight <- parsed$weights * density_x
  conditional_weight <- conditional_weight / sum(conditional_weight)
  c(
    u_x = sum(parsed$weights * marginal_x),
    u_z_given_x = sum(conditional_weight * conditional_z)
  )
}

score_continuous_location_calibration_fold <- function(
  fold_fit, location_rows, folds, profile, ndraws = 25L
) {
  heldout <- data.table::as.data.table(location_rows)[
    as.character(game_pk) %in% continuous_heldout_games(folds, fold_fit$fold)
  ]
  posterior <- draws_continuous_pitch_prior(
    fold_fit$prior_fit,
    ndraws = min(as.integer(ndraws), profile$posterior_draws),
    seed = profile$seed + 29000L + fold_fit$fold
  )
  pit <- matrix(NA_real_, nrow(heldout), 2L)
  for (row in seq_len(nrow(heldout))) {
    values <- vapply(seq_along(posterior$draw_id), function(draw) {
      prior <- continuous_pitch_prior_draw_parameters(
        fold_fit$prior_fit, heldout[row], draw, posterior
      )
      continuous_pitch_prior_rosenblatt(
        prior,
        c(12 * heldout$plate_x[[row]],
          12 * (heldout$plate_z[[row]] -
            (heldout$sz_bot[[row]] + heldout$sz_top[[row]]) / 2))
      )
    }, numeric(2L))
    pit[row, ] <- rowMeans(values)
  }
  data.table::data.table(
    fold = fold_fit$fold, game_pk = heldout$game_pk,
    u_x = pit[, 1L], u_z_given_x = pit[, 2L]
  )
}

score_continuous_component_validation_fold <- function(
  fold_fit, location_rows, swing_rows, call_rows, folds, profile
) {
  heldout_games <- continuous_heldout_games(folds, fold_fit$fold)
  swing_heldout <- data.table::as.data.table(swing_rows)[
    as.character(game_pk) %in% heldout_games
  ]
  call_heldout <- data.table::as.data.table(call_rows)[
    as.character(game_pk) %in% heldout_games
  ]
  call_training <- data.table::as.data.table(call_rows)[
    as.character(game_pk) %in% fold_fit$training_games
  ]
  location_heldout <- data.table::as.data.table(location_rows)[
    as.character(game_pk) %in% heldout_games
  ]
  swing_scored <- score_continuous_batter_perception(
    fold_fit$signal_fit, swing_heldout,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 30000L + fold_fit$fold
  )
  swing_zero <- score_continuous_batter_sigma_zero(
    fold_fit$signal_fit, swing_heldout,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 30000L + fold_fit$fold
  )
  swing_loss <- data.table::data.table(
    game_pk = swing_scored$game_pk,
    improvement = bernoulli_log_loss(
      swing_scored$swing, swing_zero$swing_probability_mean
    ) - bernoulli_log_loss(
      swing_scored$swing, swing_scored$swing_probability_mean
    )
  )[, .(swing_improvement = mean(improvement)), by = game_pk]

  call_scored <- score_continuous_call_cue(
    fold_fit$call_fit, call_heldout,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 31000L + fold_fit$fold
  )
  baseline_call <- mean(call_training$called_strike)
  call_loss <- data.table::data.table(
    game_pk = call_scored$game_pk,
    improvement = bernoulli_log_loss(
      call_scored$called_strike, rep(baseline_call, nrow(call_scored))
    ) - bernoulli_log_loss(
      call_scored$called_strike,
      call_scored$called_strike_probability_mean
    )
  )[, .(call_improvement = mean(improvement)), by = game_pk]
  call_calibration <- data.table::data.table(
    fold = fold_fit$fold,
    ece = continuous_calibration_error(
      call_scored$called_strike,
      call_scored$called_strike_probability_mean
    )
  )
  location_with_family <- score_continuous_pitch_prior(
    fold_fit$prior_fit,
    location_heldout,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 32000L + fold_fit$fold
  )
  location_without_family <- score_continuous_pitch_prior(
    fold_fit$prior_no_family_fit,
    location_heldout,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 32000L + fold_fit$fold
  )
  if (nrow(location_with_family) != nrow(location_without_family) ||
      !identical(
        as.character(location_with_family$game_pk),
        as.character(location_without_family$game_pk)
      ) ||
      !identical(
        as.integer(location_with_family$at_bat_number),
        as.integer(location_without_family$at_bat_number)
      ) ||
      !identical(
        as.integer(location_with_family$pitch_number),
        as.integer(location_without_family$pitch_number)
      )) {
    stop("Pitch-family sensitivity scores do not align on held-out pitches")
  }
  pitch_family_sensitivity <- data.table::data.table(
    game_pk = location_with_family$game_pk,
    log_score_difference =
      location_with_family$log_posterior_predictive_density -
      location_without_family$log_posterior_predictive_density
  )[, .(
    with_minus_without_pitch_family = mean(log_score_difference)
  ), by = game_pk]
  list(
    fold = fold_fit$fold,
    swing_by_game = swing_loss,
    call_by_game = call_loss,
    call_calibration = call_calibration,
    pitch_family_by_game = pitch_family_sensitivity,
    location_pit = score_continuous_location_calibration_fold(
      fold_fit, location_rows, folds, profile
    )
  )
}

aggregate_continuous_component_validation <- function(fold_validation) {
  swing <- data.table::rbindlist(
    lapply(fold_validation, `[[`, "swing_by_game"), fill = TRUE
  )
  call <- data.table::rbindlist(
    lapply(fold_validation, `[[`, "call_by_game"), fill = TRUE
  )
  call_calibration <- data.table::rbindlist(
    lapply(fold_validation, `[[`, "call_calibration"), fill = TRUE
  )
  pit <- data.table::rbindlist(
    lapply(fold_validation, `[[`, "location_pit"), fill = TRUE
  )
  pitch_family <- data.table::rbindlist(
    lapply(fold_validation, `[[`, "pitch_family_by_game"), fill = TRUE
  )
  swing_summary <- continuous_game_clustered_mean(
    swing$game_pk, swing$swing_improvement
  )
  call_summary <- continuous_game_clustered_mean(
    call$game_pk, call$call_improvement
  )
  pitch_family_summary <- continuous_game_clustered_mean(
    pitch_family$game_pk, pitch_family$with_minus_without_pitch_family
  )
  ks_x <- suppressWarnings(stats::ks.test(pit$u_x, "punif")$statistic)
  ks_z <- suppressWarnings(stats::ks.test(pit$u_z_given_x, "punif")$statistic)
  list(
    swing_improvement = swing_summary$mean,
    swing_improvement_se = swing_summary$game_se,
    call_improvement = call_summary$mean,
    call_improvement_se = call_summary$game_se,
    call_ece = mean(call_calibration$ece),
    call_performance = is.finite(call_summary$game_se) &&
      call_summary$mean >= call_summary$game_se &&
      mean(call_calibration$ece) <= 0.03,
    pitch_family_log_score_difference = pitch_family_summary$mean,
    pitch_family_log_score_difference_se = pitch_family_summary$game_se,
    location_ks_x = unname(ks_x),
    location_ks_z_given_x = unname(ks_z),
    location_calibrated = max(ks_x, ks_z) <= 0.05,
    by_game = list(
      swing = swing,
      call = call,
      pitch_family_sensitivity = pitch_family
    ),
    location_pit = pit
  )
}

as_continuous_decision_scoring_pitch <- function(row) {
  x <- data.table::copy(data.table::as.data.table(row))
  if (nrow(x) != 1L) stop("A scoring pitch must contain exactly one row")
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "at_bat_number", "pitch_number", "batter_id", "pitcher_id",
      "initial_call", "plate_x", "plate_z", "sz_top", "sz_bot",
      "balls_before", "strikes_before", "pitch_family", "matchup",
      "umpire_id", "catcher_id"
    ),
    "continuous decision scoring pitch"
  )
  x[, `:=`(
    balls = as.integer(balls_before),
    strikes = as.integer(strikes_before),
    swing = 0L,
    description = as.character(initial_call),
    batter_id = as.character(batter_id),
    pitcher_id = as.character(pitcher_id),
    umpire_id = as.character(umpire_id),
    catcher_id = as.character(catcher_id)
  )]
  x[]
}

continuous_component_draw_bundle <- function(
  fold_fit, ndraws, seed = 20260825L
) {
  if (!inherits(fold_fit, "continuous_perception_fold_fit")) {
    stop("fold_fit must be a continuous_perception_fold_fit")
  }
  signal <- draws_continuous_batter_perception(
    fold_fit$signal_fit, ndraws = ndraws, seed = seed + 1L
  )
  prior <- draws_continuous_pitch_prior(
    fold_fit$prior_fit, ndraws = ndraws, seed = seed + 2L
  )
  call <- draws_continuous_call_cue(
    fold_fit$call_fit, ndraws = ndraws, seed = seed + 3L
  )
  draw_map <- make_global_draw_map(
    length(signal$draw_id), length(prior$draw_id), length(call$draw_id),
    ndraws = ndraws, seed = seed + 4L
  )
  list(signal = signal, prior = prior, call = call, draw_map = draw_map)
}

summarize_continuous_signal_q_draw_blocks <- function(
  q_signal, signal_weights, draw_map, nodes_per_draw
) {
  q_signal <- as.matrix(q_signal)
  signal_weights <- normalize_signal_weights(signal_weights)
  validate_global_draw_map(draw_map)
  nodes_per_draw <- as.integer(nodes_per_draw)
  draws <- nrow(data.table::as.data.table(draw_map))
  if (!identical(dim(q_signal), dim(signal_weights)) || nodes_per_draw < 1L ||
      ncol(q_signal) != draws * nodes_per_draw) {
    stop("Signal q values do not align to global-draw quadrature blocks")
  }
  q_by_global_draw <- matrix(
    NA_real_, nrow = nrow(q_signal), ncol = draws,
    dimnames = list(NULL, as.character(draw_map$global_draw_id))
  )
  draw_mass <- matrix(NA_real_, nrow(q_signal), draws)
  for (draw in seq_len(draws)) {
    columns <- seq.int(
      (draw - 1L) * nodes_per_draw + 1L,
      draw * nodes_per_draw
    )
    block_weight <- signal_weights[, columns, drop = FALSE]
    draw_mass[, draw] <- rowSums(block_weight)
    if (any(!is.finite(draw_mass[, draw])) || any(draw_mass[, draw] <= 0)) {
      stop("A global posterior draw has no conditional-S quadrature mass")
    }
    q_by_global_draw[, draw] <- rowSums(
      q_signal[, columns, drop = FALSE] * block_weight
    ) / draw_mass[, draw]
  }
  if (max(abs(draw_mass - 1 / draws)) > 1e-10) {
    stop("Global component draws do not contribute equal posterior mass")
  }
  marginal <- rowSums(q_signal * signal_weights)
  if (!isTRUE(all.equal(
    marginal, rowMeans(q_by_global_draw),
    tolerance = 1e-10, check.attributes = FALSE
  ))) {
    stop("Global-draw and marginal signal summaries disagree")
  }
  list(
    q_by_global_draw = q_by_global_draw,
    mean = marginal,
    sd = if (draws > 1L) {
      apply(q_by_global_draw, 1L, stats::sd)
    } else {
      rep(0, nrow(q_signal))
    },
    lower_95 = apply(
      q_by_global_draw, 1L, stats::quantile, 0.025, names = FALSE
    ),
    median = apply(
      q_by_global_draw, 1L, stats::quantile, 0.5, names = FALSE
    ),
    upper_95 = apply(
      q_by_global_draw, 1L, stats::quantile, 0.975, names = FALSE
    ),
    draw_mass = draw_mass,
    draw_map = data.table::copy(data.table::as.data.table(draw_map))
  )
}

continuous_signal_quadrature_for_rows <- function(
  fold_fit, rows, omega, order = 7L, ndraws = 25L,
  seed = 20260825L, draw_bundle = NULL, perception_sigma_override = NULL
) {
  x <- prepare_continuous_decision_rows(rows)
  if (!nrow(x)) stop("No decision rows are available for signal integration")
  omega <- as.numeric(omega)
  if (length(omega) != 1L || !is.finite(omega) || omega < 0 || omega > 1.5) {
    stop("omega must be one value in [0, 1.5]")
  }
  order <- as.integer(order)
  if (is.null(draw_bundle)) {
    draw_bundle <- continuous_component_draw_bundle(fold_fit, ndraws, seed)
  }
  draw_map <- draw_bundle$draw_map
  validate_global_draw_map(draw_map)
  ndraws <- nrow(draw_map)
  nodes_per_draw <- order^2
  q_signal <- matrix(NA_real_, nrow(x), ndraws * nodes_per_draw)
  signal_weights <- matrix(NA_real_, nrow(x), ndraws * nodes_per_draw)
  fallback <- logical(nrow(x))
  effective_nodes <- numeric(nrow(x))
  for (row_index in seq_len(nrow(x))) {
    pitch <- as_continuous_decision_scoring_pitch(x[row_index])
    true_location <- c(
      12 * pitch$plate_x,
      12 * (pitch$plate_z - (pitch$sz_bot + pitch$sz_top) / 2)
    )
    for (map_index in seq_len(ndraws)) {
      mapped <- draw_map[map_index]
      batter <- continuous_batter_draw_signal_parameters(
        fold_fit$signal_fit, pitch,
        mapped$signal_draw_id, draw_bundle$signal
      )
      if (!is.null(perception_sigma_override)) {
        override <- as.numeric(perception_sigma_override)
        if (!length(override) %in% c(1L, 2L) || any(!is.finite(override)) ||
            any(override < 0)) {
          stop("perception_sigma_override must be one or two nonnegative values")
        }
        batter$perception_sigma <- override
      }
      prior <- continuous_pitch_prior_draw_parameters(
        fold_fit$prior_fit, pitch,
        mapped$prior_draw_id, draw_bundle$prior
      )
      call_likelihood <- if (omega == 0) {
        NULL
      } else {
        continuous_call_cue_draw_likelihood(
          fold_fit$call_fit, pitch,
          mapped$call_draw_id, pitch$initial_call, draw_bundle$call
        )
      }
      integrated <- tryCatch(
        integrate_continuous_signal_draw(
          true_location = true_location,
          perception_sigma = batter$perception_sigma,
          prior = prior,
          initial_call = pitch$initial_call,
          zone_half_height_inches = 6 * (pitch$sz_top - pitch$sz_bot),
          take_likelihood_fn = batter$take_likelihood_fn,
          gain = x$stake_G[[row_index]],
          inventory_loss = x$inventory_loss[[row_index]],
          linear_predictor = 0,
          decision_slope = 0,
          sampling_offset = 0,
          omega = omega,
          call_likelihood_fn = call_likelihood,
          order = order,
          global_draw_id = mapped$global_draw_id,
          return_nodes = TRUE
        ),
        continuous_integration_unavailable = function(error) {
          stop(
            "Signal integration unavailable for game ", x$game_pk[[row_index]],
            ", pitch ", x$pitch_order[[row_index]], ": ",
            conditionMessage(error), call. = FALSE
          )
        }
      )
      columns <- seq.int(
        (map_index - 1L) * nodes_per_draw + 1L,
        map_index * nodes_per_draw
      )
      q_signal[row_index, columns] <- integrated$nodes$private_signal_q
      signal_weights[row_index, columns] <-
        integrated$nodes$conditional_signal_weight / ndraws
      fallback[[row_index]] <- fallback[[row_index]] || batter$batter_fallback
    }
    effective_nodes[[row_index]] <- 1 / sum(signal_weights[row_index, ]^2)
  }
  signal_weights <- normalize_signal_weights(signal_weights)
  q_summary <- summarize_continuous_signal_q_draw_blocks(
    q_signal, signal_weights, draw_map, nodes_per_draw
  )
  list(
    rows = x,
    omega = omega,
    quadrature_order = order,
    q_signal = q_signal,
    signal_weights = signal_weights,
    q_marginal = q_summary$mean,
    q_marginal_sd = q_summary$sd,
    q_marginal_lower_95 = q_summary$lower_95,
    q_marginal_median = q_summary$median,
    q_marginal_upper_95 = q_summary$upper_95,
    q_by_global_draw = q_summary$q_by_global_draw,
    batter_fallback = fallback,
    effective_signal_nodes = effective_nodes,
    draw_map = draw_map
  )
}

fit_continuous_decision_fold <- function(
  fold_fit, decision_rows, folds, profile, omega,
  backend = "cmdstanr", perception_sigma_override = NULL,
  model_variant = "continuous_signal"
) {
  if (!identical(backend, "cmdstanr")) {
    stop("The heavy decision workflow is locked to cmdstanr")
  }
  train_games <- continuous_training_games(folds, fold_fit$fold)
  train <- data.table::as.data.table(decision_rows)[
    as.character(game_pk) %in% train_games
  ]
  train <- continuous_case_control_decision_sample(
    train, zero_fraction = profile$decision_zero_fraction,
    seed = profile$seed + 59000L + fold_fit$fold
  )
  quadrature <- continuous_signal_quadrature_for_rows(
    fold_fit, train, omega = omega, order = profile$gh_order,
    ndraws = profile$decision_fit_draws,
    # All fixed trust variants reuse the same component posterior draw map;
    # only the call update and decision fit differ across omega.
    seed = profile$seed + 60000L + fold_fit$fold,
    perception_sigma_override = perception_sigma_override
  )
  output_dir <- file.path(
    continuous_model_root(), "data", "processed", "stan", "draws",
    paste0("fold-", fold_fit$fold, "-", model_variant, "-omega-", omega)
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  fit <- fit_continuous_human_decision_with_context(
    quadrature$rows, quadrature$q_signal, quadrature$signal_weights,
    chains = profile$chains,
    parallel_chains = profile$parallel_chains,
    iter_warmup = profile$iter_warmup,
    iter_sampling = profile$iter_sampling,
    seed = profile$seed + 70000L + fold_fit$fold + round(100 * omega),
    output_dir = output_dir,
    refresh = 0L
  )
  structure(list(
    fold = fold_fit$fold, omega = omega, fit = fit,
    perception_sigma_override = perception_sigma_override,
    model_variant = model_variant,
    draw_map = quadrature$draw_map
  ), class = "continuous_decision_fold_fit")
}

continuous_signal_trust_grid_for_rows <- function(
  fold_fit, rows, omega_grid = seq(0, 1.5, by = 0.05), order = 7L,
  ndraws = 5L, seed = 20260825L
) {
  omega_grid <- as.numeric(omega_grid)
  if (length(omega_grid) < 5L || any(!is.finite(omega_grid)) ||
      any(diff(omega_grid) <= 0) || abs(omega_grid[[1L]]) > 1e-12 ||
      abs(tail(omega_grid, 1L) - 1.5) > 1e-12) {
    stop("The shared-trust grid must be dense, increasing, and span [0, 1.5]")
  }
  draw_bundle <- continuous_component_draw_bundle(fold_fit, ndraws, seed)
  validate_global_draw_map(draw_bundle$draw_map)
  evaluated <- lapply(seq_along(omega_grid), function(index) {
    continuous_signal_quadrature_for_rows(
      fold_fit, rows, omega = omega_grid[[index]], order = order,
      ndraws = ndraws, seed = seed, draw_bundle = draw_bundle
    )
  })
  reference_weights <- evaluated[[1L]]$signal_weights
  if (any(vapply(evaluated[-1L], function(value) {
    !isTRUE(all.equal(
      value$signal_weights, reference_weights,
      tolerance = 1e-12, check.attributes = FALSE
    ))
  }, logical(1L)))) {
    stop("Call trust changed the private-signal integration weights")
  }
  q_grid <- array(
    NA_real_,
    dim = c(nrow(reference_weights), ncol(reference_weights), length(omega_grid))
  )
  for (index in seq_along(evaluated)) {
    q_grid[, , index] <- evaluated[[index]]$q_signal
  }
  list(
    rows = evaluated[[1L]]$rows,
    q_grid = q_grid,
    omega_grid = omega_grid,
    signal_weights = reference_weights,
    draw_map = draw_bundle$draw_map,
    global_draw_alignment = TRUE,
    batter_fallback = evaluated[[1L]]$batter_fallback,
    effective_signal_nodes = evaluated[[1L]]$effective_signal_nodes
  )
}

score_continuous_decision_fold <- function(
  fold_fit, decision_fit, decision_rows, folds, profile,
  compare_order_11 = FALSE
) {
  heldout_games <- continuous_heldout_games(folds, fold_fit$fold)
  heldout <- data.table::as.data.table(decision_rows)[
    as.character(game_pk) %in% heldout_games
  ]
  assert_continuous_fold_separation(fold_fit$training_games, heldout$game_pk)
  draw_bundle <- continuous_component_draw_bundle(
    fold_fit, profile$scoring_draws,
    profile$seed + 80000L + fold_fit$fold
  )
  quadrature <- continuous_signal_quadrature_for_rows(
    fold_fit, heldout, omega = decision_fit$omega,
    order = profile$gh_order, ndraws = profile$scoring_draws,
    seed = profile$seed + 81000L + fold_fit$fold,
    draw_bundle = draw_bundle,
    perception_sigma_override = decision_fit$perception_sigma_override
  )
  scored <- score_continuous_human_decision(
    decision_fit$fit, quadrature$rows,
    quadrature$q_signal, quadrature$signal_weights,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 82000L + fold_fit$fold,
    return_draws = FALSE
  )
  scored[, `:=`(
    fold = fold_fit$fold,
    trust_variant = paste0("omega_", decision_fit$omega),
    model_variant = decision_fit$model_variant,
    omega = decision_fit$omega,
    q_model_mean = quadrature$q_marginal,
    q_model_sd = quadrature$q_marginal_sd,
    q_model_lower_95 = quadrature$q_marginal_lower_95,
    q_model_median = quadrature$q_marginal_median,
    q_model_upper_95 = quadrature$q_marginal_upper_95,
    signal_global_draws = nrow(quadrature$draw_map),
    quadrature_order = profile$gh_order,
    batter_perception_fallback = quadrature$batter_fallback,
    effective_signal_nodes = quadrature$effective_signal_nodes,
    numerical_status = "available"
  )]
  diagnostics <- NULL
  if (isTRUE(compare_order_11)) {
    higher <- continuous_signal_quadrature_for_rows(
      fold_fit, heldout, omega = decision_fit$omega, order = 11L,
      ndraws = profile$scoring_draws,
      seed = profile$seed + 81000L + fold_fit$fold,
      draw_bundle = draw_bundle,
      perception_sigma_override = decision_fit$perception_sigma_override
    )
    error <- abs(quadrature$q_marginal - higher$q_marginal)
    diagnostics <- data.table::data.table(
      fold = fold_fit$fold, omega = decision_fit$omega,
      mean_absolute_difference = mean(error),
      p99_absolute_difference = stats::quantile(error, 0.99, names = FALSE),
      compared_rows = length(error)
    )
  }
  list(
    scores = scored[],
    quadrature_diagnostics = diagnostics,
    draw_map = quadrature$draw_map,
    q_by_global_draw = quadrature$q_by_global_draw
  )
}

assert_continuous_fixed_trust_draw_alignment <- function(fold_result) {
  if (!inherits(fold_result, "continuous_fixed_trust_fold")) {
    stop("fold_result must be a continuous_fixed_trust_fold")
  }
  identical_maps <- function(maps, label) {
    maps <- lapply(maps, function(value) {
      validate_global_draw_map(value)
      data.table::as.data.table(value)[order(global_draw_id)]
    })
    reference <- maps[[1L]]
    aligned <- all(vapply(maps[-1L], function(value) {
      isTRUE(all.equal(value, reference, check.attributes = FALSE))
    }, logical(1L)))
    if (!aligned) stop("Fixed call-trust ", label, " draw maps are not aligned")
    TRUE
  }
  training <- c(
    lapply(fold_result$decision_fits, `[[`, "draw_map"),
    list(fold_result$sigma_zero_fit$draw_map)
  )
  heldout <- c(
    fold_result$scoring_draw_maps,
    list(fold_result$sigma_zero_scoring_draw_map)
  )
  identical_maps(training, "training")
  identical_maps(heldout, "held-out")
  invisible(TRUE)
}

score_continuous_fixed_trust_variants <- function(
  fold_fit, decision_rows, folds, profile
) {
  variants <- continuous_call_trust_candidates()
  fits <- lapply(variants, function(omega) {
    fit_continuous_decision_fold(
      fold_fit, decision_rows, folds, profile, omega
    )
  })
  sigma_zero_fit <- fit_continuous_decision_fold(
    fold_fit, decision_rows, folds, profile, omega = 0,
    perception_sigma_override = 0, model_variant = "sigma_zero"
  )
  scored <- lapply(seq_along(variants), function(index) {
    score_continuous_decision_fold(
      fold_fit, fits[[index]], decision_rows, folds, profile,
      compare_order_11 = variants[[index]] %in% c(0, 1)
    )
  })
  sigma_zero_scored <- score_continuous_decision_fold(
    fold_fit, sigma_zero_fit, decision_rows, folds, profile,
    compare_order_11 = FALSE
  )
  result <- structure(list(
    fold = fold_fit$fold,
    decision_fits = fits,
    scores = data.table::rbindlist(lapply(scored, `[[`, "scores"), fill = TRUE),
    sigma_zero_fit = sigma_zero_fit,
    sigma_zero_scores = sigma_zero_scored$scores,
    scoring_draw_maps = lapply(scored, `[[`, "draw_map"),
    sigma_zero_scoring_draw_map = sigma_zero_scored$draw_map,
    quadrature_diagnostics = data.table::rbindlist(
      lapply(scored, `[[`, "quadrature_diagnostics"), fill = TRUE
    )
  ), class = "continuous_fixed_trust_fold")
  assert_continuous_fixed_trust_draw_alignment(result)
  result
}

aggregate_continuous_fixed_trust <- function(fold_results) {
  if (inherits(fold_results, "continuous_fixed_trust_fold")) {
    fold_results <- list(fold_results)
  }
  scores <- data.table::rbindlist(
    lapply(fold_results, `[[`, "scores"), fill = TRUE
  )
  quadrature <- data.table::rbindlist(
    lapply(fold_results, `[[`, "quadrature_diagnostics"), fill = TRUE
  )
  sigma_zero_scores <- data.table::rbindlist(
    lapply(fold_results, `[[`, "sigma_zero_scores"), fill = TRUE
  )
  fixed_scores <- scores[, {
    scored <- game_clustered_choice_score(.SD, p_challenge_mean, omega[[1L]])
    scored[, .(log_loss, game_se, games, pitches)]
  }, by = omega]
  sigma_zero_metric <- game_clustered_choice_score(
    sigma_zero_scores, sigma_zero_scores$p_challenge_mean, omega = 0
  )
  baseline_loss <- sigma_zero_scores[, .(
    sigma_zero_loss = mean(bernoulli_log_loss(challenged, p_challenge_mean))
  ), by = game_pk]
  model_loss <- scores[omega == 0, .(
    model_loss = mean(bernoulli_log_loss(challenged, p_challenge_mean))
  ), by = game_pk]
  choice_comparison <- merge(baseline_loss, model_loss, by = "game_pk")
  choice_comparison[, improvement := sigma_zero_loss - model_loss]
  choice_summary <- continuous_game_clustered_mean(
    choice_comparison$game_pk, choice_comparison$improvement
  )
  list(
    scores = scores[],
    quadrature_diagnostics = quadrature[],
    fixed_choice_scores = fixed_scores[],
    sigma_zero_scores = sigma_zero_scores[],
    sigma_zero_metric = sigma_zero_metric[],
    scoring_draw_maps = lapply(fold_results, `[[`, "scoring_draw_maps"),
    sigma_zero_scoring_draw_maps = lapply(
      fold_results, `[[`, "sigma_zero_scoring_draw_map"
    ),
    choice_improvement_over_sigma_zero = choice_summary$mean,
    choice_improvement_se = choice_summary$game_se,
    choice_noninferior = is.finite(choice_summary$mean) &&
      is.finite(choice_summary$game_se) &&
      choice_summary$mean >= -choice_summary$game_se
  )
}

continuous_fixed_trust_pre_gate <- function(
  fixed_scores, minimum_improvement_se = 1
) {
  x <- data.table::copy(data.table::as.data.table(fixed_scores))
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "pitch_order", "omega", "challenged",
      "p_challenge_mean"
    ),
    "fixed call-trust OOF scores"
  )
  expected <- continuous_call_trust_fixed_omegas()
  present <- sort(unique(as.numeric(x$omega)))
  if (!all(vapply(expected, function(value) {
    any(abs(present - value) < 1e-12)
  }, logical(1L)))) {
    stop("The call-trust pre-gate requires OOF scores for omega 0, 0.5, and 1")
  }
  predictions <- x[vapply(omega, function(value) {
    any(abs(value - expected) < 1e-12)
  }, logical(1L)), .(
    model = vapply(omega, continuous_call_trust_stage_id, character(1L)),
    row_id = paste(as.character(game_pk), pitch_order, sep = "::"),
    game_pk,
    challenged = as.integer(challenged),
    p_challenge = as.numeric(p_challenge_mean)
  )]
  metrics <- continuous_call_trust_crossfit_metrics(
    predictions,
    baseline_model = continuous_call_trust_stage_id(0)
  )
  candidates <- data.table::copy(metrics$comparisons)[
    model %in% vapply(c(0.5, 1), continuous_call_trust_stage_id, character(1L))
  ]
  if (nrow(candidates) != 2L) {
    stop("The fixed trust pre-gate could not pair every held-out candidate")
  }
  eligible <- candidates[
    !is.na(improvement_in_se) &
      improvement_in_se >= as.numeric(minimum_improvement_se) &
      improvement_vs_baseline > 0
  ]
  pass <- nrow(eligible) > 0L
  ranked <- if (pass) eligible else candidates
  data.table::setorder(
    ranked, -improvement_vs_baseline, -improvement_in_se, model
  )
  best <- ranked[1L]
  summary <- data.table::data.table(
    pass = pass,
    status = if (pass) "estimated_omega_eligible" else
      "estimated_omega_skipped_fixed_pre_gate",
    baseline_model = continuous_call_trust_stage_id(0),
    best_fixed_model = best$model[[1L]],
    best_fixed_omega = if (best$model[[1L]] ==
      continuous_call_trust_stage_id(0.5)) 0.5 else 1,
    improvement_vs_omega_zero = best$improvement_vs_baseline[[1L]],
    clustered_se = best$clustered_se[[1L]],
    improvement_in_se = best$improvement_in_se[[1L]],
    minimum_improvement_se = as.numeric(minimum_improvement_se),
    reason = if (pass) {
      "a fixed call-cue variant improved paired game-held-out log loss by at least one clustered SE"
    } else {
      "fixed omega 0.5/1 did not improve paired game-held-out log loss by one clustered SE; estimated omega was not fit"
    }
  )
  structure(
    list(summary = summary, metrics = metrics, candidates = candidates),
    class = "continuous_call_trust_pre_gate"
  )
}

continuous_call_trust_pre_gate_pass <- function(pre_gate) {
  is.list(pre_gate) && !is.null(pre_gate$summary) &&
    nrow(data.table::as.data.table(pre_gate$summary)) == 1L &&
    isTRUE(data.table::as.data.table(pre_gate$summary)$pass[[1L]])
}

continuous_call_trust_context_bundle <- function(rows, specification = NULL) {
  x <- prepare_continuous_decision_rows(rows)
  context <- if (is.null(specification)) {
    fit_continuous_decision_context(x)
  } else {
    list(
      matrix = score_continuous_decision_context(x, specification),
      specification = specification
    )
  }
  matrix <- as.matrix(context$matrix)
  if (nrow(matrix) != nrow(x) || any(!is.finite(matrix))) {
    stop("Call-trust context design does not align to decision rows")
  }
  covariates <- if (ncol(matrix)) {
    paste0("call_trust_context__", seq_len(ncol(matrix)))
  } else {
    character()
  }
  if (length(intersect(covariates, names(x)))) {
    stop("Call-trust context column names collide with decision rows")
  }
  for (index in seq_along(covariates)) {
    x[, (covariates[[index]]) := matrix[, index]]
  }
  list(
    rows = x,
    covariates = covariates,
    specification = context$specification,
    matrix_column_names = colnames(matrix)
  )
}

continuous_call_trust_q_marginal_summary <- function(
  q_grid, omega_grid, signal_weights, omega_draws
) {
  grid <- validate_continuous_call_trust_grid(
    q_grid, omega_grid, signal_weights
  )
  omega_draws <- as.numeric(omega_draws)
  if (!length(omega_draws) || any(!is.finite(omega_draws)) ||
      any(omega_draws < 0 | omega_draws > 1.5)) {
    stop("Shared-trust posterior draws must lie in [0, 1.5]")
  }
  q_draws <- vapply(omega_draws, function(omega) {
    q <- continuous_call_trust_interpolate_validated(grid, omega)
    rowSums(grid$signal_weights * q)
  }, numeric(grid$N))
  if (is.null(dim(q_draws))) {
    q_draws <- matrix(q_draws, nrow = grid$N, ncol = 1L)
  }
  data.table::data.table(
    q_model_mean = rowMeans(q_draws),
    q_model_sd = if (ncol(q_draws) > 1L) {
      apply(q_draws, 1L, stats::sd)
    } else {
      rep(0, nrow(q_draws))
    },
    q_model_lower_95 = apply(
      q_draws, 1L, stats::quantile, 0.025, names = FALSE
    ),
    q_model_median = apply(
      q_draws, 1L, stats::quantile, 0.5, names = FALSE
    ),
    q_model_upper_95 = apply(
      q_draws, 1L, stats::quantile, 0.975, names = FALSE
    )
  )
}

continuous_estimated_trust_skip <- function(fold, pre_gate) {
  summary <- if (is.list(pre_gate) && !is.null(pre_gate$summary)) {
    data.table::as.data.table(pre_gate$summary)
  } else {
    data.table::data.table(
      pass = FALSE,
      status = "estimated_omega_skipped_invalid_pre_gate",
      reason = "the fixed-omega pre-gate was unavailable"
    )
  }
  structure(list(
    fold = as.integer(fold),
    status = "skipped_fixed_pre_gate",
    reason = summary$reason[[1L]],
    pre_gate = summary,
    fit = NULL,
    scores = data.table::data.table(),
    posterior_draws = NULL,
    grid_convergence = data.table::data.table(),
    diagnostics = data.table::data.table()
  ), class = c(
    "continuous_estimated_trust_fold_skip",
    "continuous_estimated_trust_fold"
  ))
}

assert_continuous_decision_row_alignment <- function(reference, candidate) {
  left <- data.table::as.data.table(reference)
  right <- data.table::as.data.table(candidate)
  stop_if_missing_columns(
    left, c("game_pk", "pitch_order"), "reference decision rows"
  )
  stop_if_missing_columns(
    right, c("game_pk", "pitch_order"), "candidate decision rows"
  )
  if (nrow(left) != nrow(right) ||
      !identical(as.character(left$game_pk), as.character(right$game_pk)) ||
      !identical(as.integer(left$pitch_order), as.integer(right$pitch_order))) {
    stop("Call-trust q grids, context, and decision rows are misaligned")
  }
  invisible(TRUE)
}

run_continuous_estimated_trust_fold <- function(
  fold_fit, decision_rows, folds, profile, pre_gate,
  backend = "cmdstanr",
  grid_function = continuous_signal_trust_grid_for_rows,
  fit_function = fit_continuous_call_trust,
  score_function = score_continuous_call_trust,
  draw_function = continuous_call_trust_draw_matrix,
  diagnostics_function = continuous_call_trust_diagnostics
) {
  fold <- as.integer(fold_fit$fold)
  if (length(fold) != 1L || !is.finite(fold)) {
    stop("An estimated call-trust fold needs one outer fold identifier")
  }
  if (!continuous_call_trust_pre_gate_pass(pre_gate)) {
    return(continuous_estimated_trust_skip(fold, pre_gate))
  }
  if (!identical(backend, "cmdstanr")) {
    stop("The estimated call-trust workflow is locked to cmdstanr")
  }
  step <- as.numeric(profile$trust_grid_step)
  if (!is.finite(step) || step <= 0 || step > 0.25) {
    stop("trust_grid_step must be in (0, 0.25]")
  }
  omega_grid <- unique(c(seq(0, 1.5, by = step), 1.5))
  train_games <- continuous_training_games(folds, fold)
  heldout_games <- continuous_heldout_games(folds, fold)
  assert_continuous_fold_separation(train_games, heldout_games)

  train <- data.table::as.data.table(decision_rows)[
    as.character(game_pk) %in% train_games
  ]
  train <- continuous_case_control_decision_sample(
    train,
    zero_fraction = profile$decision_zero_fraction,
    seed = profile$seed + 59000L + fold
  )
  training_grid <- grid_function(
    fold_fit,
    train,
    omega_grid = omega_grid,
    order = profile$gh_order,
    ndraws = profile$trust_fit_draws,
    seed = profile$seed + 90000L + fold
  )
  if (!isTRUE(training_grid$global_draw_alignment)) {
    stop("Training q-grid did not preserve the global component-draw map")
  }
  validate_global_draw_map(training_grid$draw_map)
  training_convergence <- continuous_call_trust_grid_convergence(
    training_grid$q_grid,
    training_grid$omega_grid,
    training_grid$signal_weights
  )
  training_context <- continuous_call_trust_context_bundle(training_grid$rows)
  assert_continuous_decision_row_alignment(
    training_grid$rows, training_context$rows
  )
  bundle <- prepare_continuous_call_trust_data(
    training_context$rows,
    training_grid$q_grid,
    training_grid$omega_grid,
    training_grid$signal_weights,
    covariates = training_context$covariates
  )
  fit <- fit_function(
    bundle,
    backend = backend,
    chains = profile$chains,
    parallel_chains = profile$parallel_chains,
    iter_warmup = profile$iter_warmup,
    iter_sampling = profile$iter_sampling,
    seed = profile$seed + 92000L + fold,
    refresh = 0L
  )
  fit$context_specification <- training_context$specification
  fit$context_matrix_column_names <- training_context$matrix_column_names

  heldout <- data.table::as.data.table(decision_rows)[
    as.character(game_pk) %in% heldout_games
  ]
  assert_continuous_fold_separation(fit$training_games, heldout$game_pk)
  heldout_grid <- grid_function(
    fold_fit,
    heldout,
    omega_grid = omega_grid,
    order = profile$gh_order,
    ndraws = profile$trust_fit_draws,
    seed = profile$seed + 93000L + fold
  )
  if (!isTRUE(heldout_grid$global_draw_alignment)) {
    stop("Held-out q-grid did not preserve the global component-draw map")
  }
  validate_global_draw_map(heldout_grid$draw_map)
  heldout_convergence <- continuous_call_trust_grid_convergence(
    heldout_grid$q_grid,
    heldout_grid$omega_grid,
    heldout_grid$signal_weights
  )
  heldout_context <- continuous_call_trust_context_bundle(
    heldout_grid$rows,
    specification = training_context$specification
  )
  assert_continuous_decision_row_alignment(
    heldout_grid$rows, heldout_context$rows
  )
  scored <- score_function(
    fit,
    heldout_context$rows,
    heldout_grid$q_grid,
    heldout_grid$signal_weights,
    omega_grid = heldout_grid$omega_grid,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 94000L + fold
  )
  posterior_draws <- draw_function(
    fit,
    ndraws = profile$posterior_draws,
    seed = profile$seed + 94000L + fold
  )
  q_summary <- continuous_call_trust_q_marginal_summary(
    heldout_grid$q_grid,
    heldout_grid$omega_grid,
    heldout_grid$signal_weights,
    posterior_draws[, "omega_shared"]
  )
  scored <- cbind(scored, q_summary)
  if (length(heldout_context$covariates)) {
    scored[, (heldout_context$covariates) := NULL]
  }
  omega_interval <- stats::quantile(
    posterior_draws[, "omega_shared"], c(0.025, 0.5, 0.975), names = FALSE
  )
  scored[, `:=`(
    fold = fold,
    trust_variant = "omega_estimated",
    model_variant = "continuous_signal_shared_omega",
    omega = omega_interval[[2L]],
    omega_posterior_median = omega_interval[[2L]],
    omega_posterior_lower_95 = omega_interval[[1L]],
    omega_posterior_upper_95 = omega_interval[[3L]],
    p_challenge_mean = p_challenge_call_trust,
    p_challenge_sd = p_challenge_call_trust_sd,
    p_challenge_lower_95 = p_challenge_call_trust_lower,
    p_challenge_median = p_challenge_call_trust,
    p_challenge_upper_95 = p_challenge_call_trust_upper,
    q_chosen_mean = q_chosen_call_trust_mean,
    q_chosen_lower_95 = q_chosen_call_trust_lower,
    q_chosen_median = q_chosen_call_trust_mean,
    q_chosen_upper_95 = q_chosen_call_trust_upper,
    signal_global_draws = nrow(heldout_grid$draw_map),
    quadrature_order = profile$gh_order,
    batter_perception_fallback = heldout_grid$batter_fallback,
    effective_signal_nodes = heldout_grid$effective_signal_nodes,
    numerical_status = "available",
    posterior_median_approximation =
      "posterior mean retained where draw-level action probabilities were not materialized"
  )]
  convergence <- data.table::rbindlist(list(
    data.table::copy(training_convergence$summary)[, `:=`(
      fold = fold, split = "training"
    )],
    data.table::copy(heldout_convergence$summary)[, `:=`(
      fold = fold, split = "heldout"
    )]
  ), fill = TRUE)
  diagnostics <- diagnostics_function(fit)
  diagnostics[, `:=`(
    fold = fold,
    component = "decision_shared_omega",
    omega = NA_real_,
    model_variant = "continuous_signal_shared_omega"
  )]

  output_dir <- file.path(
    continuous_model_root(), "data", "processed", "stan", "draws",
    paste0("fold-", fold, "-continuous-signal-omega-estimated")
  )
  if (inherits(fit$fit, "CmdStanMCMC")) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    fit$fit$save_output_files(
      dir = output_dir,
      basename = paste0("call-trust-fold-", fold),
      timestamp = FALSE,
      random = FALSE
    )
  }
  # The q arrays are deterministic upstream products and dominate serialized
  # fit size. Scoring is complete, so retain their audit metadata, not copies.
  fit$data$q_grid <- NULL
  fit$data$signal_weight <- NULL
  fit$data$X <- NULL
  structure(list(
    fold = fold,
    status = "estimated_omega_fit_and_scored",
    reason = "fixed-omega pre-gate passed",
    pre_gate = pre_gate$summary,
    fit = fit,
    scores = scored[],
    posterior_draws = posterior_draws,
    grid_convergence = convergence[],
    diagnostics = diagnostics[],
    draw_maps = list(
      training = training_grid$draw_map,
      heldout = heldout_grid$draw_map
    ),
    draw_alignment = data.table::data.table(
      fold = fold,
      split = c("training", "heldout"),
      global_draws = c(
        nrow(training_grid$draw_map), nrow(heldout_grid$draw_map)
      ),
      aligned_across_omega = TRUE
    )
  ), class = "continuous_estimated_trust_fold")
}

continuous_fail_closed_trust_gate <- function(
  pre_gate, status, reason,
  improvement_vs_omega_zero = NA_real_, clustered_se = NA_real_,
  improvement_in_se = NA_real_, omega_mean = NA_real_,
  omega_median = NA_real_, omega_lower = NA_real_, omega_upper = NA_real_,
  maximum_correlation = NA_real_, grid_convergence_pass = FALSE,
  fold_coverage_pass = FALSE
) {
  pre_gate_pass <- continuous_call_trust_pre_gate_pass(pre_gate)
  data.table::data.table(
    estimated_model = "omega_estimated",
    baseline_model = continuous_call_trust_stage_id(0),
    pre_gate_pass = pre_gate_pass,
    fold_coverage_pass = isTRUE(fold_coverage_pass),
    improvement_vs_omega_zero = improvement_vs_omega_zero,
    clustered_se = clustered_se,
    improvement_in_se = improvement_in_se,
    minimum_improvement_se = 1,
    improvement_pass = FALSE,
    omega_mean = omega_mean,
    omega_median = omega_median,
    omega_lower = omega_lower,
    omega_upper = omega_upper,
    omega_interval_width = if (all(is.finite(c(omega_lower, omega_upper)))) {
      omega_upper - omega_lower
    } else NA_real_,
    maximum_interval_width = 0.75,
    informative_interval_pass = FALSE,
    grid_convergence_pass = isTRUE(grid_convergence_pass),
    maximum_absolute_decision_correlation = maximum_correlation,
    correlation_tolerance = 0.8,
    identifiability_pass = FALSE,
    pass = FALSE,
    promote_estimated_call_update = FALSE,
    selected_omega = 0,
    primary_omega = 0,
    primary_stage_id = continuous_call_trust_stage_id(0),
    status = as.character(status),
    reason = as.character(reason)
  )
}

aggregate_continuous_estimated_trust <- function(
  fold_results, fixed_scores, pre_gate,
  minimum_improvement_se = 1, interval_probability = 0.95,
  maximum_interval_width = 0.75, boundary_margin = 0.02,
  correlation_tolerance = 0.8
) {
  if (inherits(fold_results, "continuous_estimated_trust_fold")) {
    fold_results <- list(fold_results)
  }
  if (!is.list(fold_results) || !length(fold_results)) {
    stop("Estimated call-trust aggregation requires outer-fold results")
  }
  if (!continuous_call_trust_pre_gate_pass(pre_gate)) {
    reason <- if (is.list(pre_gate) && !is.null(pre_gate$summary)) {
      data.table::as.data.table(pre_gate$summary)$reason[[1L]]
    } else {
      "the fixed-omega pre-gate was unavailable"
    }
    return(list(
      status = "skipped_fixed_pre_gate",
      scores = data.table::data.table(),
      crossfit_metrics = NULL,
      posterior_draws = NULL,
      identifiability = data.table::data.table(),
      grid_convergence = data.table::data.table(),
      diagnostics = data.table::data.table(),
      gate = continuous_fail_closed_trust_gate(
        pre_gate,
        status = "default_omega_zero_pre_gate",
        reason = reason
      )
    ))
  }
  successful <- Filter(function(value) {
    inherits(value, "continuous_estimated_trust_fold") &&
      identical(value$status, "estimated_omega_fit_and_scored")
  }, fold_results)
  fold_coverage_pass <- length(successful) == length(fold_results)
  if (!length(successful)) {
    return(list(
      status = "estimated_omega_no_complete_folds",
      scores = data.table::data.table(),
      crossfit_metrics = NULL,
      posterior_draws = NULL,
      identifiability = data.table::data.table(),
      grid_convergence = data.table::data.table(),
      diagnostics = data.table::data.table(),
      gate = continuous_fail_closed_trust_gate(
        pre_gate,
        status = "default_omega_zero_no_estimated_folds",
        reason = "no estimated-omega outer fold completed",
        fold_coverage_pass = FALSE
      )
    ))
  }
  estimated_scores <- data.table::rbindlist(
    lapply(successful, `[[`, "scores"), fill = TRUE
  )
  fixed <- data.table::copy(data.table::as.data.table(fixed_scores))
  stop_if_missing_columns(
    fixed,
    c("game_pk", "pitch_order", "omega", "challenged", "p_challenge_mean"),
    "fixed OOF scores for estimated trust aggregation"
  )
  stop_if_missing_columns(
    estimated_scores,
    c("game_pk", "pitch_order", "challenged", "p_challenge_mean"),
    "estimated trust OOF scores"
  )
  baseline <- fixed[abs(omega) < 1e-12, .(
    model = continuous_call_trust_stage_id(0),
    row_id = paste(as.character(game_pk), pitch_order, sep = "::"),
    game_pk,
    challenged = as.integer(challenged),
    p_challenge = as.numeric(p_challenge_mean)
  )]
  estimated_predictions <- estimated_scores[, .(
    model = "omega_estimated",
    row_id = paste(as.character(game_pk), pitch_order, sep = "::"),
    game_pk,
    challenged = as.integer(challenged),
    p_challenge = as.numeric(p_challenge_mean)
  )]
  if (anyDuplicated(baseline$row_id) || anyDuplicated(estimated_predictions$row_id)) {
    stop("Estimated call-trust OOF scores contain duplicate pitch keys")
  }
  row_coverage_pass <- setequal(baseline$row_id, estimated_predictions$row_id)
  crossfit_metrics <- if (row_coverage_pass) {
    continuous_call_trust_crossfit_metrics(
      data.table::rbindlist(list(baseline, estimated_predictions)),
      baseline_model = continuous_call_trust_stage_id(0)
    )
  } else {
    NULL
  }
  comparison <- if (is.null(crossfit_metrics)) {
    NULL
  } else {
    crossfit_metrics$comparisons[model == "omega_estimated"]
  }

  posterior_parts <- lapply(successful, function(value) {
    draws <- as.matrix(value$posterior_draws)
    if (!"omega_shared" %in% colnames(draws)) {
      stop("An estimated outer fold is missing omega posterior draws")
    }
    draws
  })
  omega <- unlist(lapply(
    posterior_parts, function(draws) as.numeric(draws[, "omega_shared"])
  ))
  alpha <- (1 - as.numeric(interval_probability)) / 2
  interval <- stats::quantile(omega, c(alpha, 1 - alpha), names = FALSE)
  interval_width <- diff(interval)
  informative <- is.finite(interval_width) &&
    interval_width <= as.numeric(maximum_interval_width) &&
    interval[[1L]] >= as.numeric(boundary_margin) &&
    interval[[2L]] <= 1.5 - as.numeric(boundary_margin)

  identification <- lapply(seq_along(posterior_parts), function(index) {
    value <- continuous_call_trust_identifiability(
      posterior_parts[[index]],
      correlation_tolerance = correlation_tolerance,
      include_varying_effects = TRUE
    )
    table <- data.table::copy(value$table)
    table[, fold := successful[[index]]$fold]
    table
  })
  identification <- data.table::rbindlist(identification, fill = TRUE)
  maximum_correlation <- if (
    nrow(identification) && any(is.finite(identification$absolute_correlation))
  ) {
    max(identification$absolute_correlation, na.rm = TRUE)
  } else {
    NA_real_
  }
  identifiability_pass <- is.finite(maximum_correlation) &&
    maximum_correlation < as.numeric(correlation_tolerance) &&
    all(identification$pass)

  convergence <- data.table::rbindlist(
    lapply(successful, `[[`, "grid_convergence"), fill = TRUE
  )
  grid_pass <- nrow(convergence) == 2L * length(successful) &&
    all(convergence$pass %in% TRUE)
  diagnostics <- data.table::rbindlist(
    lapply(successful, `[[`, "diagnostics"), fill = TRUE
  )
  improvement <- if (is.null(comparison) || nrow(comparison) != 1L) {
    NA_real_
  } else comparison$improvement_vs_baseline[[1L]]
  clustered_se <- if (is.null(comparison) || nrow(comparison) != 1L) {
    NA_real_
  } else comparison$clustered_se[[1L]]
  improvement_in_se <- if (is.null(comparison) || nrow(comparison) != 1L) {
    NA_real_
  } else comparison$improvement_in_se[[1L]]
  improvement_pass <- !is.na(improvement_in_se) &&
    improvement_in_se >= as.numeric(minimum_improvement_se) &&
    is.finite(improvement) && improvement > 0
  coverage_pass <- fold_coverage_pass && row_coverage_pass
  pass <- coverage_pass && improvement_pass && informative &&
    identifiability_pass && grid_pass
  selected_omega <- if (pass) stats::median(omega) else 0
  reason <- if (!coverage_pass) {
    "estimated-omega outer-fold or held-out-row coverage was incomplete"
  } else if (!improvement_pass) {
    "estimated omega did not improve paired game-held-out log loss by one clustered SE"
  } else if (!informative) {
    "the shared-omega posterior interval was not informative and interior"
  } else if (!identifiability_pass) {
    "shared omega had absolute posterior correlation at least 0.8 with a decision parameter"
  } else if (!grid_pass) {
    "the dense omega-grid interpolation convergence check failed"
  } else {
    "estimated shared omega passed prediction, interval, identification, and grid gates"
  }
  gate <- data.table::data.table(
    estimated_model = "omega_estimated",
    baseline_model = continuous_call_trust_stage_id(0),
    pre_gate_pass = TRUE,
    fold_coverage_pass = coverage_pass,
    improvement_vs_omega_zero = improvement,
    clustered_se = clustered_se,
    improvement_in_se = improvement_in_se,
    minimum_improvement_se = as.numeric(minimum_improvement_se),
    improvement_pass = improvement_pass,
    omega_mean = mean(omega),
    omega_median = stats::median(omega),
    omega_lower = interval[[1L]],
    omega_upper = interval[[2L]],
    omega_interval_width = interval_width,
    maximum_interval_width = as.numeric(maximum_interval_width),
    informative_interval_pass = informative,
    grid_convergence_pass = grid_pass,
    maximum_absolute_decision_correlation = maximum_correlation,
    correlation_tolerance = as.numeric(correlation_tolerance),
    identifiability_pass = identifiability_pass,
    pass = pass,
    promote_estimated_call_update = pass,
    selected_omega = selected_omega,
    primary_omega = selected_omega,
    primary_stage_id = if (pass) "omega_estimated" else
      continuous_call_trust_stage_id(0),
    status = if (pass) "estimated_omega_promoted" else "default_omega_zero",
    reason = reason
  )
  list(
    status = gate$status[[1L]],
    scores = estimated_scores[],
    crossfit_metrics = crossfit_metrics,
    posterior_draws = posterior_parts,
    identifiability = identification[],
    grid_convergence = convergence[],
    diagnostics = diagnostics[],
    gate = gate[]
  )
}

continuous_primary_trust_gate <- function(
  fixed_choice_scores, estimated_gate = NULL
) {
  if (!is.null(estimated_gate)) return(data.table::as.data.table(estimated_gate))
  stage_continuous_call_trust(
    fixed_choice_scores,
    correlation = NA_real_, interval = c(NA_real_, NA_real_)
  )
}

format_continuous_human_decision_posteriors <- function(
  fixed_scores, trust_gate, estimated_scores = NULL
) {
  fixed <- data.table::copy(data.table::as.data.table(fixed_scores))
  stop_if_missing_columns(
    fixed,
    c(
      "game_pk", "pitch_order", "fold", "omega", "challenged",
      "q_model_mean", "p_challenge_mean", "q_chosen_mean"
    ),
    "continuous fixed-trust scores"
  )
  q_summary_defaults <- list(
    q_model_sd = 0,
    q_model_lower_95 = fixed$q_model_mean,
    q_model_median = fixed$q_model_mean,
    q_model_upper_95 = fixed$q_model_mean
  )
  for (column in names(q_summary_defaults)) {
    if (!column %in% names(fixed)) {
      fixed[, (column) := q_summary_defaults[[column]]]
    }
  }
  fixed[, trust_variant := vapply(
    omega, continuous_call_trust_stage_id, character(1L)
  )]
  baseline <- fixed[abs(omega) < 1e-12, .(
    game_pk, pitch_order,
    q_private_mean = q_model_mean,
    q_private_sd = q_model_sd,
    q_private_q025 = q_model_lower_95,
    q_private_q50 = q_model_median,
    q_private_q975 = q_model_upper_95
  )]
  if (anyDuplicated(baseline[, .(game_pk, pitch_order)])) {
    stop("Private-signal baseline contains duplicate pitch keys")
  }
  gate <- data.table::as.data.table(trust_gate)
  if (nrow(gate) != 1L) stop("The call-trust formatter needs one promotion gate")
  promoted <- if ("promote_estimated_call_update" %in% names(gate)) {
    isTRUE(gate$promote_estimated_call_update[[1L]])
  } else {
    FALSE
  }
  x <- fixed
  if (promoted) {
    estimated <- data.table::copy(data.table::as.data.table(estimated_scores))
    stop_if_missing_columns(
      estimated,
      c(
        "game_pk", "pitch_order", "fold", "challenged", "q_model_mean",
        "p_challenge_mean", "q_chosen_mean"
      ),
      "promoted estimated-trust OOF scores"
    )
    estimated[, trust_variant := "omega_estimated"]
    estimated_defaults <- list(
      q_model_sd = 0,
      q_model_lower_95 = estimated$q_model_mean,
      q_model_median = estimated$q_model_mean,
      q_model_upper_95 = estimated$q_model_mean
    )
    for (column in names(estimated_defaults)) {
      if (!column %in% names(estimated)) {
        estimated[, (column) := estimated_defaults[[column]]]
      }
    }
    x <- data.table::rbindlist(list(fixed, estimated), fill = TRUE)
  }
  x <- merge(x, baseline, by = c("game_pk", "pitch_order"), all.x = TRUE)
  primary_omega <- if ("primary_omega" %in% names(gate)) {
    as.numeric(gate$primary_omega[[1L]])
  } else 0
  primary_stage <- if ("primary_stage_id" %in% names(gate)) {
    as.character(gate$primary_stage_id[[1L]])
  } else {
    continuous_call_trust_stage_id(primary_omega)
  }
  gate_reason <- if ("reason" %in% names(gate)) {
    as.character(gate$reason[[1L]])
  } else {
    "no estimated shared-trust promotion gate was supplied"
  }
  x[, `:=`(
    q_call_mean = q_model_mean,
    q_call_sd = q_model_sd,
    q_call_q025 = q_model_lower_95,
    q_call_q50 = q_model_median,
    q_call_q975 = q_model_upper_95,
    q_chosen_q025 = q_chosen_lower_95,
    q_chosen_q50 = q_chosen_median,
    q_chosen_q975 = q_chosen_upper_95,
    p_challenge_q025 = p_challenge_lower_95,
    p_challenge_q50 = p_challenge_median,
    p_challenge_q975 = p_challenge_upper_95,
    primary_trust_variant = trust_variant == primary_stage,
    primary_trust_stage = primary_stage,
    primary_omega = primary_omega,
    trust_gate_reason = gate_reason,
    information_regime = paste(
      "tracked X anchors S|X; batter acts on one latent S after a take;",
      "initial call enters only through the declared omega variant"
    )
  )]
  x[, trust_order__ := data.table::fcase(
    trust_variant == continuous_call_trust_stage_id(0), 0,
    trust_variant == continuous_call_trust_stage_id(0.5), 0.5,
    trust_variant == continuous_call_trust_stage_id(1), 1,
    trust_variant == "omega_estimated", 2,
    default = 3
  )]
  data.table::setorder(x, game_pk, pitch_order, trust_order__)
  x[, trust_order__ := NULL]
  x[]
}

attach_continuous_outcome_evaluation <- function(posteriors, labels) {
  x <- data.table::copy(data.table::as.data.table(posteriors))
  y <- data.table::copy(data.table::as.data.table(labels))
  stop_if_missing_columns(
    y, c("game_pk", "pitch_order", "official_success", "geometry_success"),
    "quarantined continuous challenge labels"
  )
  if (anyDuplicated(y[, .(game_pk, pitch_order)])) {
    stop("Quarantined challenge labels contain duplicate pitch keys")
  }
  merge(x, y, by = c("game_pk", "pitch_order"), all.x = TRUE, sort = FALSE)
}

continuous_chosen_calibration <- function(posteriors, primary_only = TRUE) {
  x <- data.table::copy(data.table::as.data.table(posteriors))
  required <- c(
    "challenged", "official_success", "q_chosen_mean", "game_pk",
    "primary_trust_variant"
  )
  stop_if_missing_columns(x, required, "choice-conditioned calibration")
  if (isTRUE(primary_only)) x <- x[primary_trust_variant %in% TRUE]
  x <- x[
    challenged == 1L & !is.na(official_success) & is.finite(q_chosen_mean)
  ]
  if (!nrow(x)) {
    return(data.table::data.table(
      challenges = 0L, brier = NA_real_, calibration_intercept = NA_real_,
      calibration_slope = NA_real_, calibrated = FALSE
    ))
  }
  brier <- mean((as.numeric(x$official_success) - x$q_chosen_mean)^2)
  calibration_fit <- tryCatch(
    stats::glm(
      official_success ~ stats::qlogis(pmin(1 - 1e-6, pmax(1e-6, q_chosen_mean))),
      data = x, family = stats::binomial()
    ),
    error = function(error) NULL
  )
  coefficients <- if (is.null(calibration_fit)) {
    c(NA_real_, NA_real_)
  } else {
    stats::coef(calibration_fit)
  }
  data.table::data.table(
    challenges = nrow(x),
    brier = brier,
    calibration_intercept = unname(coefficients[[1L]]),
    calibration_slope = unname(coefficients[[2L]]),
    calibrated = is.finite(coefficients[[1L]]) &&
      is.finite(coefficients[[2L]]) && abs(coefficients[[1L]]) <= 0.25 &&
      coefficients[[2L]] >= 0.75 && coefficients[[2L]] <= 1.25
  )
}

continuous_sampler_gate <- function(diagnostics) {
  x <- data.table::as.data.table(diagnostics)
  stop_if_missing_columns(
    x, c("max_rhat", "min_bulk_ess", "divergences"),
    "continuous sampler diagnostics"
  )
  all(
    is.finite(x$max_rhat) & x$max_rhat <= 1.01 &
      is.finite(x$min_bulk_ess) & x$min_bulk_ess >= 400 &
      is.finite(x$divergences) & x$divergences == 0
  )
}

continuous_cmdstan_diagnostics <- function(stan_fit) {
  if (inherits(stan_fit, "CmdStanMCMC")) {
    summary <- data.table::as.data.table(stan_fit$summary())
    diagnostic <- tryCatch(stan_fit$diagnostic_summary(), error = function(e) NULL)
    divergences <- if (is.null(diagnostic)) NA_real_ else {
      sum(diagnostic$num_divergent, na.rm = TRUE)
    }
    return(data.table::data.table(
      max_rhat = max(summary$rhat, na.rm = TRUE),
      min_bulk_ess = min(summary$ess_bulk, na.rm = TRUE),
      min_tail_ess = min(summary$ess_tail, na.rm = TRUE),
      divergences = divergences
    ))
  }
  if (inherits(stan_fit, "stanfit")) {
    summary <- rstan::summary(stan_fit)$summary
    return(data.table::data.table(
      max_rhat = max(summary[, "Rhat"], na.rm = TRUE),
      min_bulk_ess = min(summary[, "n_eff"], na.rm = TRUE),
      min_tail_ess = NA_real_,
      divergences = sum(
        vapply(rstan::get_sampler_params(stan_fit, inc_warmup = FALSE),
          function(x) sum(x[, "divergent__"]), numeric(1L)
        )
      )
    ))
  }
  data.table::data.table(
    max_rhat = NA_real_, min_bulk_ess = NA_real_,
    min_tail_ess = NA_real_, divergences = NA_real_
  )
}

continuous_fold_diagnostics <- function(fold_fit) {
  if (!inherits(fold_fit, "continuous_perception_fold_fit")) {
    stop("fold_fit must be a continuous_perception_fold_fit")
  }
  fits <- list(
    signal = fold_fit$signal_fit$stan_fit,
    prior = fold_fit$prior_fit$stan_fit,
    prior_no_family = fold_fit$prior_no_family_fit$stan_fit,
    call = fold_fit$call_fit$stan_fit
  )
  data.table::rbindlist(lapply(names(fits), function(name) {
    cbind(
      data.table::data.table(fold = fold_fit$fold, component = name),
      continuous_cmdstan_diagnostics(fits[[name]])
    )
  }))
}

continuous_fixed_trust_diagnostics <- function(fold_result) {
  if (!inherits(fold_result, "continuous_fixed_trust_fold")) {
    stop("fold_result must be a continuous_fixed_trust_fold")
  }
  fits <- c(fold_result$decision_fits, list(fold_result$sigma_zero_fit))
  data.table::rbindlist(lapply(fits, function(value) {
    cbind(
      data.table::data.table(fold = value$fold, component = "decision",
        omega = value$omega, model_variant = value$model_variant),
      continuous_cmdstan_diagnostics(value$fit$fit)
    )
  }), fill = TRUE)
}

continuous_perception_strategy_correlation <- function(
  fold_fit, ndraws = 400L, seed = 20260825L
) {
  draws <- draws_continuous_batter_perception(
    fold_fit$signal_fit, ndraws = ndraws, seed = seed + fold_fit$fold
  )
  correlations <- vapply(seq_len(ncol(draws$sigma_player)), function(player) {
    stats::cor(
      draws$sigma_player[, player], draws$threshold_player[, player]
    )
  }, numeric(1L))
  anisotropy_correlation <- if (
    stats::sd(draws$anisotropy_ratio) > 1e-12
  ) {
    max(abs(vapply(seq_len(ncol(draws$sigma_player)), function(player) {
      stats::cor(draws$sigma_player[, player], draws$anisotropy_ratio)
    }, numeric(1L))), na.rm = TRUE)
  } else 0
  data.table::data.table(
    fold = fold_fit$fold,
    max_sigma_threshold_correlation = max(abs(correlations), na.rm = TRUE),
    max_sigma_anisotropy_correlation = anisotropy_correlation,
    max_perception_strategy_correlation = max(
      abs(correlations), anisotropy_correlation, na.rm = TRUE
    )
  )
}

continuous_batter_season_exposure <- function(swing_rows) {
  season <- normalize_continuous_batter_input(swing_rows, require_swing = TRUE)
  key <- intersect(
    c("game_pk", "at_bat_number", "pitch_number", "batter_id"),
    names(season)
  )
  if (length(key) < 4L) {
    stop("Season batter exposure requires unique game/at-bat/pitch/player keys")
  }
  season <- unique(season, by = key)
  season[, .(
    training_opportunities = .N,
    training_swings = sum(as.integer(swing))
  ), by = batter_id]
}

summarize_continuous_batter_parameters <- function(
  fold_fits, ndraws = 400L, seed = 20260825L, swing_rows = NULL
) {
  if (inherits(fold_fits, "continuous_perception_fold_fit")) {
    fold_fits <- list(fold_fits)
  }
  pieces <- lapply(seq_along(fold_fits), function(index) {
    fit <- fold_fits[[index]]$signal_fit
    draws <- draws_continuous_batter_perception(
      fit, ndraws = ndraws, seed = seed + index
    )
    sigma <- draws$sigma_player
    shared <- identical(fit$anisotropy, "shared")
    table <- data.table::copy(fit$player_table)
    table[, `:=`(
      fold = fold_fits[[index]]$fold,
      sigma_mean = colMeans(sigma),
      sigma_sd = apply(sigma, 2L, stats::sd),
      sigma_q05 = apply(sigma, 2L, stats::quantile, 0.05, names = FALSE),
      sigma_q50 = apply(sigma, 2L, stats::quantile, 0.50, names = FALSE),
      sigma_q95 = apply(sigma, 2L, stats::quantile, 0.95, names = FALSE),
      anisotropy_ratio_mean = mean(draws$anisotropy_ratio),
      anisotropy_ratio_q05 = stats::quantile(
        draws$anisotropy_ratio, 0.05, names = FALSE
      ),
      anisotropy_ratio_q95 = stats::quantile(
        draws$anisotropy_ratio, 0.95, names = FALSE
      ),
      kernel_form = if (shared) "shared-anisotropic-zero-bias" else
        "isotropic-zero-bias",
      anisotropy_status = if (shared) "promoted" else "not-promoted",
      fallback_level = "hierarchical-player"
    )]
    table[]
  })
  crossfit <- data.table::rbindlist(pieces, fill = TRUE)
  summary <- crossfit[, .(
    mean_fold_training_opportunities = mean(training_opportunities),
    mean_fold_training_swings = mean(training_swings),
    sigma_mean = mean(sigma_mean),
    sigma_sd = sqrt(
      mean(sigma_sd^2) + if (.N > 1L) stats::var(sigma_mean) else 0
    ),
    sigma_q05 = mean(sigma_q05), sigma_q50 = mean(sigma_q50),
    sigma_q95 = mean(sigma_q95),
    anisotropy_ratio_mean = mean(anisotropy_ratio_mean),
    anisotropy_ratio_q05 = mean(anisotropy_ratio_q05),
    anisotropy_ratio_q95 = mean(anisotropy_ratio_q95),
    kernel_form = continuous_mode(kernel_form),
    anisotropy_status = continuous_mode(anisotropy_status),
    fallback_level = continuous_mode(fallback_level),
    contributing_folds = data.table::uniqueN(fold)
  ), by = batter_id]
  if (is.null(swing_rows)) {
    summary[, `:=`(
      training_opportunities = mean_fold_training_opportunities,
      training_swings = mean_fold_training_swings,
      exposure_definition = "mean per-fold training exposure; season rows not supplied"
    )]
    return(summary[])
  }
  exposure <- continuous_batter_season_exposure(swing_rows)
  summary <- merge(summary, exposure, by = "batter_id", all.x = TRUE)
  summary[is.na(training_opportunities), training_opportunities := 0L]
  summary[is.na(training_swings), training_swings := 0L]
  summary[, exposure_definition :=
    "exact unique season eligible swing/take rows; not summed across outer fits"]
  summary[]
}

continuous_gate_row <- function(gate, passed, observed, threshold, detail) {
  data.table::data.table(
    gate = gate,
    passed = isTRUE(passed),
    observed = as.character(observed),
    threshold = as.character(threshold),
    detail = detail
  )
}

build_continuous_validation_gates <- function(metrics) {
  value <- function(name, default = NA) {
    if (!is.null(metrics[[name]]) && length(metrics[[name]])) {
      metrics[[name]][[1L]]
    } else default
  }
  rows <- list(
    continuous_gate_row(
      "game_fold_separation", isTRUE(value("fold_separation", FALSE)),
      value("fold_separation", FALSE), "TRUE",
      "No game may train and score the same component"
    ),
    continuous_gate_row(
      "five_fold_completion", isTRUE(value("full_five_fold", FALSE)),
      value("full_five_fold", FALSE), "all five game folds scored",
      "Pilot profiles remain diagnostic and cannot publish season claims"
    ),
    continuous_gate_row(
      "sampler_health",
      isTRUE(value("sampler_healthy", FALSE)),
      value("sampler_healthy", FALSE), "Rhat<=1.01; ESS>=400; 0 divergences",
      "All perception, prior, call, and decision posteriors"
    ),
    continuous_gate_row(
      "swing_prediction",
      is.finite(value("swing_improvement")) &&
        is.finite(value("swing_improvement_se")) &&
        value("swing_improvement") >= value("swing_improvement_se"),
      value("swing_improvement"), ">= one game-clustered SE",
      "Behavioral localization kernel versus sigma=0"
    ),
    continuous_gate_row(
      "location_density", isTRUE(value("location_calibrated", FALSE)),
      value("location_calibrated", FALSE), "calibrated held-out density",
      "Smallest 1/3/6-component mixture within one SE"
    ),
    continuous_gate_row(
      "initial_call", isTRUE(value("call_performance", FALSE)),
      value("call_performance", FALSE), "beats predeclared call baseline",
      "Taken-pitch initial calls only"
    ),
    continuous_gate_row(
      "perception_strategy_identifiability",
      is.finite(value("max_perception_strategy_correlation")) &&
        abs(value("max_perception_strategy_correlation")) < 0.8,
      value("max_perception_strategy_correlation"), "absolute correlation < 0.8",
      "Blocks the term perception when strategy is not separated"
    ),
    continuous_gate_row(
      "challenge_choice_noninferiority",
      isTRUE(value("choice_noninferior", FALSE)),
      value("choice_noninferior", FALSE), "not worse than sigma=0",
      "Held-out challenge/pass log loss"
    ),
    continuous_gate_row(
      "shared_call_trust_promotion",
      isTRUE(value("shared_call_trust_promoted", FALSE)),
      paste(
        "pre_gate=", value("shared_call_trust_pre_gate", FALSE),
        "; improvement_se=", value("shared_call_trust_improvement_se"),
        "; interval=", value("shared_call_trust_interval", FALSE),
        "; identifiable=", value("shared_call_trust_identifiable", FALSE),
        "; grid=", value("shared_call_trust_grid_converged", FALSE),
        sep = ""
      ),
      "fixed pre-gate and estimated OOF gain >=1 clustered SE; informative interval; |cor|<0.8; converged grid",
      "Failure is recoverable and selects omega=0"
    ),
    continuous_gate_row(
      "quadrature_mean",
      is.finite(value("quadrature_mean_error")) &&
        value("quadrature_mean_error") < 0.001,
      value("quadrature_mean_error"), "<0.001", "Order 7 versus order 11"
    ),
    continuous_gate_row(
      "quadrature_p99",
      is.finite(value("quadrature_p99_error")) &&
        value("quadrature_p99_error") < 0.005,
      value("quadrature_p99_error"), "<0.005", "Order 7 versus order 11"
    ),
    continuous_gate_row(
      "chosen_belief_calibration",
      isTRUE(value("chosen_calibrated", FALSE)),
      value("chosen_calibrated", FALSE), "held-out official overturn calibration",
      "Outcomes are used here for evaluation only"
    )
  )
  data.table::rbindlist(rows, use.names = TRUE)
}

continuous_perception_prior_specifications <- function(profile = NULL) {
  grid_step <- if (is.list(profile) && !is.null(profile$trust_grid_step)) {
    as.numeric(profile$trust_grid_step)
  } else {
    0.05
  }
  list(
    batter_perception = list(
      sigma = list(
        population_log_mean = "normal(log(3 inches), 0.75)",
        player_log_sd = "half-normal(0, 0.5)",
        player_standard_effect = "standard normal"
      ),
      anisotropy_sensitivity = list(
        candidates = c("isotropic", "shared anisotropy"),
        shared_log_ratio = "normal(0, 0.35)",
        promotion = "synthetic recovery plus >=1 game-clustered SE held-out swing gain"
      ),
      threshold = list(
        population_mean_inches = "normal(0, 3)",
        player_sd_inches = "half-normal(0, 2)",
        player_standard_effect = "standard normal",
        context_coefficients = "normal(0, 1.5)",
        angular_sector_coefficients = "normal(0, 1)"
      ),
      lapse_and_ceiling = list(
        strike_count_lower_targets = c(0.08, 0.18, 0.35),
        strike_count_upper_targets = c(0.70, 0.88, 0.97),
        lower_logit_sd = 1,
        conditional_range_logit_sd = 1
      )
    ),
    pitch_location_mixture = list(
      component_candidates = c(1L, 3L, 6L),
      selection = "smallest component count within one game-held-out SE of the best log score",
      sensitivity = "with pitch-family context versus otherwise identical no-family fit",
      weights = list(
        intercept = "normal(0, 1.5)",
        context = "normal(0, 0.75)",
        reference_component = "zero logit"
      ),
      pitcher_effects = list(
        scale = "half-normal(0, 0.5)",
        standardized_effect = "standard normal"
      ),
      component_shapes = list(
        mean_inches = "normal(k-means anchor, 4)",
        marginal_scale_inches = "lognormal(log(cluster anchor scale), 0.4)",
        correlation = "LKJ Cholesky(4)"
      )
    ),
    initial_call = list(
      likelihood = "Bernoulli-logit for the observed taken-pitch call",
      edge_spline = list(
        basis = "natural cubic spline of signed edge distance",
        degrees_of_freedom = 6L,
        coefficients = "normal(0, 1.5)"
      ),
      residual_location = list(
        basis = "12 orthogonalized two-dimensional radial basis functions",
        coefficients = "normal(0, 0.75)"
      ),
      intercept = "normal(0, 2.5)",
      context_coefficients = "normal(0, 0.75)",
      umpire_sd = "half-normal(0, 0.5)",
      catcher_sd = "half-normal(0, 0.5)",
      standardized_umpire_catcher_effects = "standard normal"
    ),
    challenge_decision = list(
      utility = "q * stake_G - (1 - q) * inventory_loss",
      player_population_intercept = "normal(-5, 2)",
      player_sd = "half-normal(0, 1)",
      team_sd = "half-normal(0, 0.5)",
      standardized_player_team_effects = "standard normal",
      positive_utility_slope = "lognormal(0, 0.75)",
      context_coefficients = "normal(0, 1)",
      case_control_offset = "known log inverse zero-sampling fraction"
    ),
    shared_call_trust = list(
      support = c(0, 1.5),
      prior = "normal(0.5, 0.5), truncated to [0, 1.5]",
      interpolation = "continuous piecewise-linear interpolation of signal-specific q",
      omega_grid_step = grid_step,
      fixed_pre_gate = "omega 0, 0.5, 1 OOF; best informed candidate must gain >=1 paired game-clustered SE over omega 0",
      promotion = "estimated OOF gain >=1 paired game-clustered SE; 95% interval width <=0.75 and interior; every |posterior correlation|<0.8; dense-grid convergence",
      failure_default = "omega 0"
    )
  )
}

continuous_global_draw_alignment_manifest <- function(
  fixed_fold_results, estimated_fold_results, gh_order
) {
  if (inherits(fixed_fold_results, "continuous_fixed_trust_fold")) {
    fixed_fold_results <- list(fixed_fold_results)
  }
  if (inherits(estimated_fold_results, "continuous_estimated_trust_fold")) {
    estimated_fold_results <- list(estimated_fold_results)
  }
  fixed <- data.table::rbindlist(lapply(fixed_fold_results, function(value) {
    variants <- vapply(
      value$decision_fits, function(fit) as.numeric(fit$omega), numeric(1L)
    )
    training <- vapply(
      value$decision_fits, function(fit) nrow(fit$draw_map), integer(1L)
    )
    heldout <- vapply(value$scoring_draw_maps, nrow, integer(1L))
    data.table::data.table(
      fold = value$fold,
      stage = vapply(variants, continuous_call_trust_stage_id, character(1L)),
      training_global_draws = training,
      heldout_global_draws = heldout
    )
  }), fill = TRUE)
  estimated <- data.table::rbindlist(lapply(
    estimated_fold_results,
    function(value) {
      if (!is.null(value$draw_alignment) && nrow(value$draw_alignment)) {
        data.table::copy(value$draw_alignment)
      } else {
        data.table::data.table(
          fold = value$fold,
          split = "not_run",
          global_draws = 0L,
          aligned_across_omega = FALSE,
          status = value$status
        )
      }
    }
  ), fill = TRUE)
  list(
    scheme = paste(
      "Each global_draw_id maps one independently thinned batter-signal,",
      "pitch-prior, and initial-call posterior draw. Its GH node block is",
      "kept intact; the identical map and signal-column ordering are reused",
      "at every omega knot."
    ),
    nodes_per_global_draw = as.integer(gh_order)^2,
    fixed_stage_counts = data.table::as.data.frame(fixed),
    estimated_stage_counts = data.table::as.data.frame(estimated)
  )
}

write_continuous_parquet <- function(rows, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(data.table::as.data.frame(rows), path)
  normalizePath(path)
}

write_continuous_csv <- function(rows, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(rows, path)
  normalizePath(path)
}
