# Binned empirical signed-margin priors for fixed-clock challenge policies.
#
# This file intentionally leaves the Gaussian-mixture implementation intact.
# It adds a second prior backend whose role-wide support is the observed 0.01
# inch grid and whose count-specific probabilities are the posterior means
#
#   p_x(b) = {n_x(b) + alpha p_role(b)} / {n_x + alpha}.
#
# Private-signal densities are Gaussian convolutions of these discrete masses.
# Regular bins make those convolutions cheap enough to cache via FFT.  The
# decision rule sees only the posterior q(R, count); true pitch margins remain
# evaluator-only quantities.

empirical_challenge_margin_prior_1d_type <- function() "empirical_binned_count"

.empirical_margin_scalar_number_1d <- function(
  value, name, minimum = -Inf, strictly = FALSE
) {
  value <- as.numeric(value)
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      if (strictly) value <= minimum else value < minimum) {
    comparison <- if (strictly) "greater than" else "at least"
    stop(name, " must be one finite number ", comparison, " ", minimum,
         call. = FALSE)
  }
  value
}

.empirical_margin_bin_index_1d <- function(margin, bin_width_inches) {
  margin <- as.numeric(margin)
  if (anyNA(margin) || any(!is.finite(margin))) {
    stop("Empirical-prior margins must be finite", call. = FALSE)
  }
  # Use half-open bins with zero as an edge.  This preserves the estimand's
  # W = 1{M > 0}: small positive and negative margins can never share a bin.
  # Snap numerical representations of exact bin edges before taking floor.
  scaled <- margin / bin_width_inches
  nearest <- round(scaled)
  scaled[abs(scaled - nearest) < 1e-10] <- nearest[
    abs(scaled - nearest) < 1e-10
  ]
  index <- as.integer(floor(scaled))
  # An exact boundary is nonpositive under W = 1{M > 0}.
  index[margin == 0] <- -1L
  index
}

.empirical_margin_context_1d <- function(value) {
  value <- as.character(value)
  value[is.na(value) | !nzchar(value)] <- "unknown"
  value
}

.empirical_margin_role_alpha_1d <- function(alpha) {
  roles <- revealed_challenge_prior_1d_roles()
  original_names <- names(alpha)
  alpha <- as.numeric(alpha)
  if (length(alpha) == 1L) {
    out <- rep(alpha, length(roles))
    names(out) <- roles
  } else if (!is.null(original_names)) {
    if (anyNA(original_names) || any(!nzchar(original_names)) ||
        anyDuplicated(original_names) || !all(roles %in% original_names)) {
      stop("Named alpha must contain offense and defense", call. = FALSE)
    }
    names(alpha) <- original_names
    out <- alpha[roles]
  } else if (length(alpha) == length(roles)) {
    out <- stats::setNames(alpha, roles)
  } else {
    stop("alpha must be scalar or role-specific", call. = FALSE)
  }
  if (anyNA(out) || any(!is.finite(out)) || any(out < 0)) {
    stop("alpha must be finite and non-negative", call. = FALSE)
  }
  out
}

validate_empirical_challenge_margin_prior_1d <- function(fit) {
  if (!inherits(fit, "empirical_challenge_margin_prior_1d_fit")) {
    stop("fit must be an empirical_challenge_margin_prior_1d_fit",
         call. = FALSE)
  }
  required <- c(
    "prior_type", "components", "weight", "mean", "sd", "bin_index",
    "bin_width_inches", "fold_id", "global_draw_id"
  )
  absent <- required[!vapply(
    required, function(name) !is.null(fit[[name]]), logical(1L)
  )]
  if (length(absent)) {
    stop("Empirical challenge-margin prior is missing: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  k <- as.integer(fit$components)
  if (length(k) != 1L || is.na(k) || k < 1L ||
      length(fit$weight) != k || length(fit$mean) != k ||
      length(fit$sd) != k || length(fit$bin_index) != k ||
      any(!is.finite(fit$weight)) || any(fit$weight <= 0) ||
      abs(sum(fit$weight) - 1) > 1e-10 ||
      any(!is.finite(fit$mean)) || any(fit$sd != 0) ||
      anyDuplicated(fit$bin_index) || any(diff(fit$bin_index) <= 0) ||
      !identical(
        .empirical_margin_bin_index_1d(fit$mean, fit$bin_width_inches),
        as.integer(fit$bin_index)
      )) {
    stop("Empirical challenge-margin prior contains invalid bin masses",
         call. = FALSE)
  }
  if (!is.null(fit$context_weights)) {
    weights <- fit$context_weights
    if (!is.matrix(weights) || ncol(weights) != k ||
        is.null(rownames(weights)) || any(!is.finite(weights)) ||
        any(weights < 0) || any(abs(rowSums(weights) - 1) > 1e-10)) {
      stop("Empirical challenge-margin prior has invalid count weights",
           call. = FALSE)
    }
  }
  invisible(fit)
}

fit_empirical_challenge_margin_prior_1d <- function(
  pitches,
  training_games = NULL,
  fold_id = "full",
  global_draw_id = 1L,
  context_column = "count_state",
  context_prior_strength = 100,
  bin_width_inches = 0.01
) {
  fold_id <- .challenge_margin_scalar_id(fold_id, "fold_id")
  global_draw_id <- .challenge_margin_scalar_id(
    global_draw_id, "global_draw_id"
  )
  alpha <- .empirical_margin_scalar_number_1d(
    context_prior_strength, "context_prior_strength", minimum = 0
  )
  bin_width <- .empirical_margin_scalar_number_1d(
    bin_width_inches, "bin_width_inches", minimum = 0, strictly = TRUE
  )
  x <- normalize_challenge_margin_prior_1d_input(pitches)
  if (!is.null(training_games)) {
    training_games <- unique(as.character(training_games))
    x <- x[game_pk %in% training_games]
    if (!nrow(x)) stop("No requested training games remain", call. = FALSE)
  }
  if (!is.null(context_column) && !identical(context_column, "count_state")) {
    stop("The empirical v1 prior context must be count_state", call. = FALSE)
  }
  x[, bin_index__ := .empirical_margin_bin_index_1d(
    edge_distance_inches, bin_width
  )]
  support <- sort(unique(x$bin_index__))
  support_match <- match(x$bin_index__, support)
  role_count <- tabulate(support_match, nbins = length(support))
  role_weight <- role_count / sum(role_count)
  center <- (support + 0.5) * bin_width

  if (is.null(context_column)) {
    context_model <- list(
      context_column = NULL,
      context_levels = character(),
      context_exposure = integer(),
      context_weights = NULL,
      context_unshrunk_weights = NULL,
      context_count_matrix = NULL,
      context_weight_table = data.table::data.table(),
      context_prior_strength = NA_real_,
      context_weight_method = "role_wide_empirical_only"
    )
  } else {
    context <- .empirical_margin_context_1d(x[[context_column]])
    levels <- sort(unique(context))
    count_matrix <- matrix(
      0, nrow = length(levels), ncol = length(support),
      dimnames = list(levels, as.character(support))
    )
    for (level_index in seq_along(levels)) {
      selected <- context == levels[[level_index]]
      count_matrix[level_index, ] <- tabulate(
        support_match[selected], nbins = length(support)
      )
    }
    exposure <- rowSums(count_matrix)
    raw <- count_matrix / exposure
    shrunk <- sweep(count_matrix, 2L, alpha * role_weight, "+")
    shrunk <- shrunk / (exposure + alpha)
    table <- data.table::rbindlist(lapply(seq_along(levels), function(i) {
      data.table::data.table(
        context_column = context_column,
        context_value = levels[[i]],
        context_exposure = as.integer(exposure[[i]]),
        bin_index = support,
        margin_bin_center_inches = center,
        role_count = role_count,
        context_count = as.integer(count_matrix[i, ]),
        role_weight = role_weight,
        unshrunk_weight = raw[i, ],
        context_weight = shrunk[i, ],
        prior_strength = alpha
      )
    }))
    context_model <- list(
      context_column = context_column,
      context_levels = levels,
      context_exposure = stats::setNames(as.integer(exposure), levels),
      context_weights = shrunk,
      context_unshrunk_weights = raw,
      context_count_matrix = count_matrix,
      context_weight_table = table,
      context_prior_strength = alpha,
      context_weight_method = paste(
        "exact_count_histogram_posterior_mean_toward_role_wide_empirical",
        "p=(n_count_bin+alpha*p_role_bin)/(n_count+alpha)"
      )
    )
  }

  fit <- c(list(
    prior_type = empirical_challenge_margin_prior_1d_type(),
    components = length(support),
    weight = as.numeric(role_weight),
    mean = as.numeric(center),
    # A zero marks a point mass.  Dispatch methods below never interpret this
    # field as a Gaussian component scale; it is retained for artifact/API
    # compatibility with the existing fixed-clock policy object.
    sd = rep(0, length(support)),
    bin_index = as.integer(support),
    bin_width_inches = bin_width,
    bin_count = as.integer(role_count),
    bin_table = data.table::data.table(
      bin_index = support,
      margin_bin_center_inches = center,
      count = as.integer(role_count),
      probability = role_weight,
      wrong_call_bin = center > 0
    )
  ), context_model, list(
    fold_id = fold_id,
    global_draw_id = global_draw_id,
    training_games = sort(unique(x$game_pk)),
    training_rows = nrow(x),
    training_fingerprint = .challenge_margin_prior_fingerprint_1d(x),
    training_fingerprint_algorithm = "sha256",
    conditioning = c("tracked", "structural_abs_eligible", "adverse_call"),
    context_weight_status = if (is.null(context_column)) {
      "role_wide_empirical_001in_v1"
    } else {
      "count_state_empirical_dirichlet_shrinkage_001in_v1"
    },
    information_set = attr(x, "information_set"),
    outcome_action_columns_used = character()
  ))
  class(fit) <- c(
    "empirical_challenge_margin_prior_1d_fit",
    "challenge_margin_prior_1d_fit"
  )
  validate_empirical_challenge_margin_prior_1d(fit)
  fit
}

.empirical_margin_resolve_weights_1d <- function(fit, context, size) {
  validate_empirical_challenge_margin_prior_1d(fit)
  .challenge_margin_resolve_context_weights(fit, context, size)
}

empirical_challenge_margin_prior_ball_rate_1d <- function(
  fit, context = NULL
) {
  size <- if (is.null(context)) 1L else max(1L, length(context))
  resolved <- .empirical_margin_resolve_weights_1d(fit, context, size)
  as.numeric(resolved$weight %*% as.numeric(fit$mean > 0))
}

empirical_challenge_margin_prior_density_1d <- function(
  fit, margin, log = FALSE, context = NULL
) {
  margin <- as.numeric(margin)
  if (anyNA(margin) || any(!is.finite(margin))) {
    stop("margin must contain only finite values", call. = FALSE)
  }
  resolved <- .empirical_margin_resolve_weights_1d(
    fit, context, length(margin)
  )
  index <- .empirical_margin_bin_index_1d(margin, fit$bin_width_inches)
  matched <- match(index, fit$bin_index)
  mass <- numeric(length(margin))
  seen <- !is.na(matched)
  mass[seen] <- resolved$weight[
    cbind(which(seen), matched[seen])
  ]
  # Report a histogram density, so log scores remain comparable across fixed
  # choices of bin width.  Zero predictive mass correctly receives -Inf.
  density <- mass / fit$bin_width_inches
  if (isTRUE(log)) log(density) else density
}

.empirical_margin_log_sum_exp_1d <- function(value) {
  maximum <- max(value)
  if (!is.finite(maximum)) return(maximum)
  maximum + log(sum(exp(value - maximum)))
}

.empirical_margin_subjective_q_scalar_1d <- function(
  fit, signal, sigma, weight
) {
  if (sigma == 0) return(as.numeric(signal > 0))
  prior_rate <- sum(weight[fit$mean > 0])
  if (is.infinite(sigma)) return(prior_rate)
  log_joint <- log(weight) + stats::dnorm(
    signal, mean = fit$mean, sd = sigma, log = TRUE
  )
  denominator <- .empirical_margin_log_sum_exp_1d(log_joint)
  positive <- fit$mean > 0 & weight > 0
  if (!any(positive)) return(0)
  numerator <- .empirical_margin_log_sum_exp_1d(log_joint[positive])
  pmin(1, pmax(0, exp(numerator - denominator)))
}

empirical_challenge_margin_subjective_ball_probability_1d <- function(
  fit, private_margin_signal, perception_sigma, context = NULL,
  block_size = 256L
) {
  signal <- as.numeric(private_margin_signal)
  sigma <- as.numeric(perception_sigma)
  size <- max(length(signal), length(sigma))
  if (length(signal) != 1L && length(signal) != size ||
      length(sigma) != 1L && length(sigma) != size) {
    stop("signal and perception_sigma must be scalar or have a common length",
         call. = FALSE)
  }
  signal <- rep_len(signal, size)
  sigma <- rep_len(sigma, size)
  if (anyNA(signal) || any(!is.finite(signal)) || anyNA(sigma) ||
      any(sigma < 0)) {
    stop("Signals must be finite and sigmas non-missing and non-negative",
         call. = FALSE)
  }
  resolved <- .empirical_margin_resolve_weights_1d(fit, context, size)
  out <- numeric(size)
  for (row in seq_len(size)) {
    out[[row]] <- .empirical_margin_subjective_q_scalar_1d(
      fit, signal[[row]], sigma[[row]], resolved$weight[row, ]
    )
  }
  out
}

score_empirical_challenge_margin_prior_1d <- function(
  fit, pitches, require_game_separation = FALSE
) {
  validate_empirical_challenge_margin_prior_1d(fit)
  x <- normalize_challenge_margin_prior_1d_input(pitches)
  overlap <- intersect(fit$training_games, unique(as.character(x$game_pk)))
  if (isTRUE(require_game_separation) && length(overlap)) {
    stop("Challenge-margin prior training and scoring games overlap",
         call. = FALSE)
  }
  resolved <- .empirical_margin_resolve_weights_1d(
    fit, x$count_state, nrow(x)
  )
  x[, `:=`(
    location_log_density = empirical_challenge_margin_prior_density_1d(
      fit, edge_distance_inches, log = TRUE, context = count_state
    ),
    prior_context = resolved$context_value,
    context_seen_in_training = resolved$context_seen_in_training,
    context_fallback = resolved$context_fallback,
    prior_components = fit$components,
    prior_type = fit$prior_type,
    fold_id = fit$fold_id,
    global_draw_id = fit$global_draw_id,
    prior_training_fingerprint = fit$training_fingerprint,
    prior_scoring_fingerprint = .challenge_margin_prior_fingerprint_1d(x)
  )]
  data.table::setattr(x, "game_separation", list(
    required = isTRUE(require_game_separation),
    overlapping_games = sort(overlap),
    training_fingerprint = fit$training_fingerprint,
    scoring_fingerprint = .challenge_margin_prior_fingerprint_1d(x)
  ))
  x[]
}

# A zero-safe proper quadratic score for selecting alpha on held-out games.
# For an observed bin y and predictive vector p, larger 2 p_y - sum(p^2) is
# better.  Unlike the log score, it remains finite when a training fold assigns
# zero mass to a validation-only tail bin.
score_empirical_challenge_margin_quadratic_1d <- function(fit, pitches) {
  validate_empirical_challenge_margin_prior_1d(fit)
  x <- normalize_challenge_margin_prior_1d_input(pitches)
  resolved <- .empirical_margin_resolve_weights_1d(
    fit, x$count_state, nrow(x)
  )
  observed <- .empirical_margin_bin_index_1d(
    x$edge_distance_inches, fit$bin_width_inches
  )
  matched <- match(observed, fit$bin_index)
  p_observed <- numeric(nrow(x))
  seen <- !is.na(matched)
  p_observed[seen] <- resolved$weight[cbind(which(seen), matched[seen])]
  x[, `:=`(
    predictive_bin_probability = p_observed,
    quadratic_score = 2 * p_observed - rowSums(resolved$weight^2),
    context_fallback = resolved$context_fallback,
    heldout_bin_seen_in_role_training = seen
  )]
  x[]
}

.empirical_margin_crps_vector_1d <- function(center, weight, observation) {
  # Exact CRPS for a finite distribution:
  # E|X-y| - 0.5 E|X-X'|.  Prefix sums make evaluation O(B + n), rather than
  # forming a bins-by-observations matrix.
  cumulative_probability <- cumsum(weight)
  cumulative_first_moment <- cumsum(weight * center)
  total_mean <- tail(cumulative_first_moment, 1L)
  previous_probability <- c(0, head(cumulative_probability, -1L))
  previous_first_moment <- c(0, head(cumulative_first_moment, -1L))
  half_pairwise <- sum(
    weight * (center * previous_probability - previous_first_moment)
  )
  left <- findInterval(observation, center)
  p_left <- ifelse(left > 0L, cumulative_probability[pmax(1L, left)], 0)
  s_left <- ifelse(left > 0L, cumulative_first_moment[pmax(1L, left)], 0)
  absolute_first_moment <- observation * p_left - s_left +
    (total_mean - s_left) - observation * (1 - p_left)
  pmax(0, absolute_first_moment - half_pairwise)
}

score_empirical_challenge_margin_crps_1d <- function(fit, pitches) {
  validate_empirical_challenge_margin_prior_1d(fit)
  x <- normalize_challenge_margin_prior_1d_input(pitches)
  score <- numeric(nrow(x))
  fallback <- logical(nrow(x))
  # Resolve one probability vector per count. Avoid constructing the much
  # larger n_rows-by-n_bins matrix used by the general row-wise API: at 0.01
  # inches that matrix can otherwise consume gigabytes during alpha CV.
  context <- .empirical_margin_context_1d(x$count_state)
  for (level in unique(context)) {
    index <- which(context == level)
    resolved <- .empirical_margin_resolve_weights_1d(fit, level, 1L)
    score[index] <- .empirical_margin_crps_vector_1d(
      fit$mean, resolved$weight[1L, ],
      x$edge_distance_inches[index]
    )
    fallback[index] <- resolved$context_fallback[[1L]]
  }
  x[, `:=`(
    crps_inches = score,
    negative_crps = -score,
    context_fallback = fallback
  )]
  x[]
}

.empirical_margin_fft_convolution_1d <- function(
  signal, center, weight, sigma
) {
  n <- length(signal)
  step <- if (n > 1L) signal[[2L]] - signal[[1L]] else NA_real_
  mapped <- (center - signal[[1L]]) / step
  aligned <- max(abs(mapped - round(mapped))) < 1e-7
  if (!aligned) {
    # This branch is only a safety net.  The lookup builder chooses a step that
    # divides 0.01 exactly, so production calls take the FFT branch.
    out <- numeric(n)
    for (component in seq_along(center)) {
      out <- out + weight[[component]] * stats::dnorm(
        signal, center[[component]], sigma
      )
    }
    return(out)
  }
  fft_n <- 2L^ceiling(log2(2L * n))
  mass <- numeric(fft_n)
  location <- as.integer(round(mapped)) + 1L
  mass[location] <- mass[location] + weight
  kernel <- numeric(fft_n)
  lag <- 0:(n - 1L)
  kernel[seq_len(n)] <- stats::dnorm(lag * step, 0, sigma)
  if (n > 1L) {
    negative_lag_location <- fft_n - seq_len(n - 1L) + 1L
    kernel[negative_lag_location] <- stats::dnorm(
      seq_len(n - 1L) * step, 0, sigma
    )
  }
  value <- Re(stats::fft(
    stats::fft(mass) * stats::fft(kernel), inverse = TRUE
  ) / fft_n)[seq_len(n)]
  value[value < 0 & abs(value) < max(value) * 1e-10] <- 0
  pmax(0, value)
}

.empirical_margin_lookup_step_1d <- function(
  bin_width, requested_step, sigma
) {
  subdivisions <- max(1L, as.integer(ceiling(bin_width / requested_step)))
  if (sigma > 0) {
    subdivisions <- max(
      subdivisions,
      as.integer(ceiling(8 * bin_width / sigma))
    )
  }
  # Centers lie on (integer + 1/2) * bin_width.  An even subdivision count
  # places both those centers and zero exactly on the signal grid.
  if (subdivisions %% 2L == 1L) subdivisions <- subdivisions + 1L
  bin_width / subdivisions
}

.empirical_margin_exact_tail_1d <- function(
  fit, threshold, weight, sigma, successful_only = FALSE
) {
  threshold <- as.numeric(threshold)
  retained <- if (isTRUE(successful_only)) fit$mean > 0 else rep(TRUE, fit$components)
  vapply(threshold, function(cutoff) {
    if (cutoff == -Inf) return(sum(weight[retained]))
    if (cutoff == Inf) return(0)
    if (sigma == 0) {
      return(sum(weight[retained & fit$mean > cutoff]))
    }
    sum(weight[retained] * stats::pnorm(
      cutoff, mean = fit$mean[retained], sd = sigma, lower.tail = FALSE
    ))
  }, numeric(1L))
}

build_empirical_challenge_signal_lookup_1d <- function(
  prior_fit,
  perception_sigma,
  context,
  grid_step = 0.0025,
  tail_standard_deviations = 12,
  minimum_half_width = 40
) {
  validate_empirical_challenge_margin_prior_1d(prior_fit)
  sigma <- .empirical_margin_scalar_number_1d(
    perception_sigma, "perception_sigma", minimum = 0
  )
  requested_step <- .empirical_margin_scalar_number_1d(
    grid_step, "grid_step", minimum = 0, strictly = TRUE
  )
  tail_sd <- .empirical_margin_scalar_number_1d(
    tail_standard_deviations, "tail_standard_deviations", minimum = 8
  )
  half_width <- .empirical_margin_scalar_number_1d(
    minimum_half_width, "minimum_half_width", minimum = 0, strictly = TRUE
  )
  context <- unique(.empirical_margin_context_1d(context))
  if (!length(context)) stop("At least one count context is required",
                             call. = FALSE)
  requested <- unique(c(context, ".league"))

  if (sigma == 0) {
    tables <- lapply(requested, function(key) {
      resolved <- if (identical(key, ".league")) {
        list(weight = prior_fit$weight, context = NA_character_, fallback = TRUE)
      } else {
        value <- .empirical_margin_resolve_weights_1d(prior_fit, key, 1L)
        list(weight = as.numeric(value$weight[1L, ]),
             context = value$context_value[[1L]],
             fallback = value$context_fallback[[1L]])
      }
      rate <- sum(resolved$weight[prior_fit$mean > 0])
      list(
        context = resolved$context,
        context_fallback = resolved$fallback,
        component_weight = resolved$weight,
        signal = c(-half_width, 0, half_width),
        q = c(0, 0, 1),
        inverse_log_odds = c(-Inf, Inf),
        inverse_signal = c(0, 0),
        action_tail = c(1, rate, 0),
        success_tail = c(rate, rate, 0),
        prior_ball_rate = rate,
        prior_mass_closure_error = 0,
        action_mass_closure_error = 0
      )
    })
    names(tables) <- requested
    out <- list(
      prior_fit = prior_fit, perception_sigma = sigma, tables = tables,
      contexts = context, grid_step = requested_step,
      requested_grid_step = requested_step,
      signal_range = c(-half_width, half_width),
      tail_standard_deviations = tail_sd,
      maximum_prior_mass_closure_error = 0,
      maximum_action_mass_closure_error = 0,
      deterministic_geometry_limit = TRUE,
      convolution_method = "analytic_discrete_sigma_zero"
    )
    class(out) <- c(
      "empirical_challenge_signal_lookup_1d", "challenge_signal_lookup_1d"
    )
    return(out)
  }

  step <- .empirical_margin_lookup_step_1d(
    prior_fit$bin_width_inches, requested_step, sigma
  )
  lower_raw <- min(-half_width, min(prior_fit$mean) - tail_sd * sigma)
  upper_raw <- max(half_width, max(prior_fit$mean) + tail_sd * sigma)
  lower <- floor(lower_raw / step) * step
  upper <- ceiling(upper_raw / step) * step
  signal <- lower + seq.int(0L, as.integer(round((upper - lower) / step))) * step

  tables <- vector("list", length(requested)); names(tables) <- requested
  for (key in requested) {
    resolved <- if (identical(key, ".league")) {
      list(weight = prior_fit$weight, context = NA_character_, fallback = TRUE)
    } else {
      value <- .empirical_margin_resolve_weights_1d(prior_fit, key, 1L)
      list(weight = as.numeric(value$weight[1L, ]),
           context = value$context_value[[1L]],
           fallback = value$context_fallback[[1L]])
    }
    density <- .empirical_margin_fft_convolution_1d(
      signal, prior_fit$mean, resolved$weight, sigma
    )
    positive_weight <- resolved$weight * as.numeric(prior_fit$mean > 0)
    success_density <- .empirical_margin_fft_convolution_1d(
      signal, prior_fit$mean, positive_weight, sigma
    )
    numerical_floor <- max(density) * 1e-13
    density[density < numerical_floor] <- 0
    success_density[success_density < numerical_floor] <- 0
    q <- numeric(length(signal))
    informative <- density > 0
    q[informative] <- success_density[informative] / density[informative]
    if (any(informative)) {
      first <- which(informative)[[1L]]
      last <- tail(which(informative), 1L)
      q[seq_len(first - 1L)] <- 0
      if (last < length(q)) q[(last + 1L):length(q)] <- 1
    }
    q <- cummax(pmin(1, pmax(0, q)))
    upper_action <- .empirical_margin_exact_tail_1d(
      prior_fit, tail(signal, 1L), resolved$weight, sigma, FALSE
    )
    upper_success <- .empirical_margin_exact_tail_1d(
      prior_fit, tail(signal, 1L), resolved$weight, sigma, TRUE
    )
    action_tail <- .challenge_signal_right_integral_1d(
      signal, density, upper_action
    )
    success_tail <- .challenge_signal_right_integral_1d(
      signal, success_density, upper_success
    )
    prior_rate <- sum(resolved$weight[prior_fit$mean > 0])
    action_error <- abs(action_tail[[1L]] - 1)
    success_error <- abs(success_tail[[1L]] - prior_rate)
    if (!is.finite(action_error) || !is.finite(success_error) ||
        max(action_error, success_error) > 2e-5) {
      stop(sprintf(
        "Empirical signal lookup failed mass closure (action %.3g; success %.3g)",
        action_error, success_error
      ), call. = FALSE)
    }
    finite_q <- pmin(1 - 1e-15, pmax(1e-15, q))
    log_odds <- stats::qlogis(finite_q)
    keep <- !duplicated(log_odds)
    tables[[key]] <- list(
      context = resolved$context,
      context_fallback = resolved$fallback,
      component_weight = resolved$weight,
      signal = signal,
      q = q,
      inverse_log_odds = log_odds[keep],
      inverse_signal = signal[keep],
      action_tail = pmin(1, pmax(0, action_tail)),
      success_tail = pmin(action_tail, pmax(0, success_tail)),
      prior_ball_rate = prior_rate,
      prior_mass_closure_error = success_error,
      action_mass_closure_error = action_error
    )
  }
  out <- list(
    prior_fit = prior_fit, perception_sigma = sigma, tables = tables,
    contexts = context, grid_step = step,
    requested_grid_step = requested_step,
    signal_range = range(signal), tail_standard_deviations = tail_sd,
    maximum_prior_mass_closure_error = max(vapply(
      tables, `[[`, numeric(1L), "prior_mass_closure_error"
    )),
    maximum_action_mass_closure_error = max(vapply(
      tables, `[[`, numeric(1L), "action_mass_closure_error"
    )),
    convolution_method = "regular_grid_zero_padded_fft"
  )
  class(out) <- c(
    "empirical_challenge_signal_lookup_1d", "challenge_signal_lookup_1d"
  )
  out
}

.empirical_margin_log_odds_scalar_1d <- function(
  fit, signal, sigma, weight
) {
  q <- .empirical_margin_subjective_q_scalar_1d(fit, signal, sigma, weight)
  stats::qlogis(q)
}

.empirical_margin_payoff_root_scalar_1d <- function(
  fit, gain, inventory_loss, perception_sigma, component_weight,
  root_tolerance, root_max_iterations, max_bracket_expansions
) {
  target <- if (inventory_loss == 0) -Inf else if (gain == 0) Inf else
    log(inventory_loss) - log(gain)
  q_target <- stats::plogis(target)
  diagnostic <- function(threshold, status) list(
    threshold = threshold, q_target = q_target, target_log_odds = target,
    inversion_q = NA_real_, inversion_absolute_error = NA_real_,
    log_odds_residual = NA_real_, bracket_lower = NA_real_,
    bracket_upper = NA_real_, bracket_expansions = 0L,
    root_iterations = 0L, estimated_precision = 0, status = status
  )
  if (inventory_loss == 0) return(diagnostic(-Inf, "analytic_zero_failure_cost_always"))
  if (gain == 0) return(diagnostic(Inf, "analytic_zero_gain_never"))
  if (perception_sigma == 0) return(diagnostic(0, "analytic_zero_sigma_generalized_inverse"))
  if (is.infinite(perception_sigma)) {
    rate <- sum(component_weight[fit$mean > 0])
    return(diagnostic(if (rate > q_target) -Inf else Inf,
                      "analytic_diffuse_sigma_prior_comparison"))
  }
  objective <- function(value) {
    .empirical_margin_log_odds_scalar_1d(
      fit, value, perception_sigma, component_weight
    ) - target
  }
  lower <- min(fit$mean) - 8 * perception_sigma
  upper <- max(fit$mean) + 8 * perception_sigma
  lower_value <- objective(lower); upper_value <- objective(upper)
  expansion <- 0L
  width <- upper - lower
  while ((lower_value > 0 || upper_value < 0) &&
         expansion < max_bracket_expansions) {
    width <- width * 2
    lower <- lower - width; upper <- upper + width
    lower_value <- objective(lower); upper_value <- objective(upper)
    expansion <- expansion + 1L
  }
  root <- stats::uniroot(
    objective, c(lower, upper), f.lower = lower_value,
    f.upper = upper_value, tol = root_tolerance,
    maxiter = root_max_iterations
  )
  q <- .empirical_margin_subjective_q_scalar_1d(
    fit, root$root, perception_sigma, component_weight
  )
  list(
    threshold = root$root, q_target = q_target, target_log_odds = target,
    inversion_q = q, inversion_absolute_error = abs(q - q_target),
    log_odds_residual = stats::qlogis(q) - target,
    bracket_lower = lower, bracket_upper = upper,
    bracket_expansions = expansion, root_iterations = root$iter,
    estimated_precision = root$estim.prec, status = "interior_log_odds_root"
  )
}

empirical_challenge_signal_payoff_terms_1d <- function(
  lookup, gain, inventory_loss, context
) {
  size <- max(length(gain), length(inventory_loss), length(context))
  if (!size || any(c(length(gain), length(inventory_loss), length(context)) != 1L &
      c(length(gain), length(inventory_loss), length(context)) != size)) {
    stop("Payoff inputs must be scalar or row-aligned", call. = FALSE)
  }
  gain <- rep_len(as.numeric(gain), size)
  loss <- rep_len(as.numeric(inventory_loss), size)
  context <- rep_len(as.character(context), size)
  if (anyNA(gain) || any(!is.finite(gain)) || anyNA(loss) ||
      any(!is.finite(loss)) || any(loss < 0) || anyNA(context)) {
    stop("Invalid empirical challenge-signal payoff inputs", call. = FALSE)
  }
  threshold <- rep(Inf, size); q_target <- rep(NA_real_, size)
  action <- success <- numeric(size)
  context_fallback <- root_fallback <- logical(size)
  positive <- gain > 0
  always <- positive & loss == 0
  threshold[always] <- -Inf; q_target[always] <- 0
  interior <- positive & loss > 0
  q_target[interior] <- loss[interior] / (gain[interior] + loss[interior])
  if (lookup$perception_sigma == 0) threshold[interior] <- 0

  for (level in unique(context)) {
    index <- which(context == level)
    table <- .challenge_signal_lookup_table_1d(lookup, level)
    context_fallback[index] <- table$context_fallback ||
      !level %in% names(lookup$tables)
    if (lookup$perception_sigma > 0) {
      inside <- index[interior[index]]
      if (length(inside)) {
        target_log_odds <- log(loss[inside]) - log(gain[inside])
        inverse <- stats::approx(
          table$inverse_log_odds, table$inverse_signal,
          xout = target_log_odds, rule = 1, ties = "ordered"
        )$y
        missing <- which(!is.finite(inverse))
        if (length(missing)) for (position in missing) {
          row <- inside[[position]]
          solved <- .empirical_margin_payoff_root_scalar_1d(
            lookup$prior_fit, gain[[row]], loss[[row]],
            lookup$perception_sigma, table$component_weight,
            1e-10, 200L, 40L
          )
          inverse[[position]] <- solved$threshold
          root_fallback[[row]] <- TRUE
        }
        threshold[inside] <- inverse
      }
    }
    always_rows <- index[threshold[index] == -Inf]
    if (length(always_rows)) {
      action[always_rows] <- 1
      success[always_rows] <- table$prior_ball_rate
    }
    finite <- index[is.finite(threshold[index])]
    if (length(finite)) {
      if (lookup$perception_sigma == 0) {
        action[finite] <- .empirical_margin_exact_tail_1d(
          lookup$prior_fit, threshold[finite], table$component_weight, 0, FALSE
        )
        success[finite] <- .empirical_margin_exact_tail_1d(
          lookup$prior_fit, threshold[finite], table$component_weight, 0, TRUE
        )
      } else {
        action[finite] <- stats::approx(
          table$signal, table$action_tail, xout = threshold[finite],
          rule = 2, ties = "ordered"
        )$y
        success[finite] <- stats::approx(
          table$signal, table$success_tail, xout = threshold[finite],
          rule = 2, ties = "ordered"
        )$y
        outside <- finite[
          threshold[finite] < min(table$signal) |
            threshold[finite] > max(table$signal)
        ]
        if (length(outside)) {
          action[outside] <- .empirical_margin_exact_tail_1d(
            lookup$prior_fit, threshold[outside], table$component_weight,
            lookup$perception_sigma, FALSE
          )
          success[outside] <- .empirical_margin_exact_tail_1d(
            lookup$prior_fit, threshold[outside], table$component_weight,
            lookup$perception_sigma, TRUE
          )
        }
      }
    }
  }
  action <- pmin(1, pmax(0, action)); success <- pmin(action, pmax(0, success))
  advantage <- success * gain - (action - success) * loss
  if (any(advantage < -2e-5)) {
    stop("Empirical signal integration produced negative optimal advantage",
         call. = FALSE)
  }
  data.table::data.table(
    gain = gain, inventory_loss = loss, q_target = q_target,
    threshold_inches = threshold,
    prior_challenge_probability = action,
    prior_success_and_challenge_probability = success,
    prior_failure_and_challenge_probability = action - success,
    prior_q_chosen = ifelse(action > 0, success / action, NA_real_),
    prior_expected_advantage_re = pmax(0, advantage),
    prior_context = context, context_fallback = context_fallback,
    exact_root_fallback = root_fallback
  )
}

.empirical_fixed_clock_threshold_masses_1d <- function(
  lookup, threshold, context
) {
  size <- max(length(threshold), length(context))
  threshold <- rep_len(as.numeric(threshold), size)
  context <- rep_len(as.character(context), size)
  action <- success <- numeric(size); fallback <- logical(size)
  for (level in unique(context)) {
    index <- which(context == level)
    table <- .challenge_signal_lookup_table_1d(lookup, level)
    fallback[index] <- table$context_fallback || !level %in% names(lookup$tables)
    always <- index[threshold[index] == -Inf]
    if (length(always)) {
      action[always] <- 1; success[always] <- table$prior_ball_rate
    }
    finite <- index[is.finite(threshold[index])]
    if (length(finite)) {
      if (lookup$perception_sigma == 0) {
        action[finite] <- .empirical_margin_exact_tail_1d(
          lookup$prior_fit, threshold[finite], table$component_weight, 0, FALSE
        )
        success[finite] <- .empirical_margin_exact_tail_1d(
          lookup$prior_fit, threshold[finite], table$component_weight, 0, TRUE
        )
      } else {
        action[finite] <- stats::approx(
          table$signal, table$action_tail, xout = threshold[finite],
          rule = 2, ties = "ordered"
        )$y
        success[finite] <- stats::approx(
          table$signal, table$success_tail, xout = threshold[finite],
          rule = 2, ties = "ordered"
        )$y
      }
    }
  }
  action <- pmin(1, pmax(0, action)); success <- pmin(action, pmax(0, success))
  data.table::data.table(
    action_probability = action,
    success_and_action_probability = success,
    failure_and_action_probability = action - success,
    context_fallback = fallback
  )
}

# Strict role/count input ----------------------------------------------------

build_revealed_empirical_margin_prior_rows_1d <- function(
  pitch_ledger, require_abs_eligible = TRUE
) {
  source <- data.table::as.data.table(pitch_ledger)
  if (isTRUE(require_abs_eligible) && !"abs_eligible" %in% names(source)) {
    stop("Empirical margin priors require the structural abs_eligible flag",
         call. = FALSE)
  }
  rows <- build_revealed_challenge_prior_1d(source)
  if ("abs_eligible" %in% names(rows)) rows <- rows[abs_eligible %in% TRUE]
  rows <- rows[
    count_state_original %in% as.vector(outer(0:3, 0:2, paste, sep = "-"))
  ]
  if (!nrow(rows)) stop("No structural ABS-eligible adverse calls remain",
                        call. = FALSE)
  assert_revealed_challenge_prior_1d_clean(rows, "empirical prior rows")
  rows[]
}

route_revealed_empirical_count_context_1d <- function(opportunities) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  stop_if_missing_columns(
    x, c("count_state", "raw_count_state"),
    "revealed empirical-policy opportunities"
  )
  raw <- .empirical_margin_context_1d(x$raw_count_state)
  valid <- raw %in% as.vector(outer(0:3, 0:2, paste, sep = "-"))
  if (any(!valid)) {
    stop("Empirical policy opportunities contain invalid raw baseball counts",
         call. = FALSE)
  }
  x[, `:=`(
    mixture_context_count_family = as.character(count_state),
    prior_context = raw,
    count_state = raw
  )]
  regime <- attr(opportunities, "information_regime")
  if (is.null(regime)) regime <- list()
  regime$decision_context <- paste(
    "raw baseball count only; role-specific empirical adverse-call margin",
    "distribution shrunk toward role-wide empirical distribution"
  )
  data.table::setattr(x, "information_regime", regime)
  x[]
}

.revealed_empirical_role_rows_1d <- function(rows, role) {
  role_value <- match.arg(role, revealed_challenge_prior_1d_roles())
  x <- data.table::copy(data.table::as.data.table(rows))[role == role_value]
  if ("abs_eligible" %in% names(x)) x <- x[abs_eligible %in% TRUE]
  if (!nrow(x)) stop(role_value, " empirical prior has no eligible rows",
                     call. = FALSE)
  x
}

fit_revealed_empirical_margin_prior_role_1d <- function(
  rows, role, alpha = 100, bin_width_inches = 0.01,
  fold_id = "full", training_games = NULL
) {
  role_value <- match.arg(role, revealed_challenge_prior_1d_roles())
  x <- .revealed_empirical_role_rows_1d(rows, role_value)
  if (!is.null(training_games)) x <- x[game_pk %in% as.character(training_games)]
  fit <- fit_empirical_challenge_margin_prior_1d(
    .revealed_prior_adapter_1d(x, "count_state_original"),
    fold_id = fold_id, context_prior_strength = alpha,
    bin_width_inches = bin_width_inches
  )
  fit$role <- role_value
  fit$context_source_column <- "count_state_original"
  fit$eligible_definition <- paste(
    "tracked structural abs_eligible adverse calls; challenge and pass;",
    "role-oriented margin; valid count"
  )
  fit
}

fit_revealed_empirical_margin_priors_1d <- function(
  rows, alpha = 100, bin_width_inches = 0.01,
  fold_id = "full", training_games = NULL
) {
  roles <- revealed_challenge_prior_1d_roles()
  alpha <- .empirical_margin_role_alpha_1d(alpha)
  out <- lapply(seq_along(roles), function(i) {
    fit_revealed_empirical_margin_prior_role_1d(
      rows, roles[[i]], alpha[[i]], bin_width_inches, fold_id, training_games
    )
  })
  names(out) <- roles
  out
}

crossfit_revealed_empirical_margin_prior_1d <- function(
  rows, fold_assignment = NULL, folds = 5L, seed = 20260826L,
  alpha = 100, bin_width_inches = 0.01, progress = interactive()
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  assert_revealed_challenge_prior_1d_clean(x, "empirical prior cross-fit rows")
  stop_if_missing_columns(
    x, c("game_pk", "role", "count_state_original", "edge_distance_inches"),
    "empirical prior cross-fit rows"
  )
  if ("abs_eligible" %in% names(x)) x <- x[abs_eligible %in% TRUE]
  if (is.null(fold_assignment)) {
    fold_assignment <- continuous_game_folds(x$game_pk, folds, seed)
  }
  fold_assignment <- normalize_joint_common_width_fold_assignment_1d(
    fold_assignment, unique(x$game_pk)
  )
  x[fold_assignment, fold := i.fold, on = "game_pk"]
  fold_ids <- sort(unique(x$fold)); roles <- revealed_challenge_prior_1d_roles()
  role_alpha <- .empirical_margin_role_alpha_1d(alpha)
  fold_fits <- vector("list", length(fold_ids)); names(fold_fits) <- fold_ids
  scores <- list(); diagnostics <- list(); index <- 0L
  for (fold_value in fold_ids) {
    train_games <- unique(x[fold != fold_value, game_pk])
    test_games <- unique(x[fold == fold_value, game_pk])
    fits <- fit_revealed_empirical_margin_priors_1d(
      x[fold != fold_value], alpha = role_alpha,
      bin_width_inches = bin_width_inches, fold_id = fold_value
    )
    fold_fits[[as.character(fold_value)]] <- fits
    for (role_value in roles) {
      if (isTRUE(progress)) message(
        "empirical-prior fold ", fold_value, " role ", role_value
      )
      heldout <- .revealed_empirical_role_rows_1d(
        x[fold == fold_value], role_value
      )
      scored <- score_empirical_challenge_margin_crps_1d(
        fits[[role_value]],
        .revealed_prior_adapter_1d(heldout, "count_state_original")
      )
      index <- index + 1L
      scored[, `:=`(role = role_value, outer_fold = as.integer(fold_value))]
      scores[[index]] <- scored
      diagnostics[[index]] <- data.table::data.table(
        role = role_value, outer_fold = as.integer(fold_value),
        training_games = length(train_games), heldout_games = length(test_games),
        overlap_games = length(intersect(train_games, test_games)),
        training_rows = fits[[role_value]]$training_rows,
        bins = fits[[role_value]]$components,
        alpha = fits[[role_value]]$context_prior_strength,
        bin_width_inches = fits[[role_value]]$bin_width_inches,
        action_outcome_columns_used = ""
      )
    }
  }
  result <- list(
    primary_fits = fold_fits,
    fold_fits = fold_fits,
    oof_scores = data.table::rbindlist(scores, fill = TRUE),
    diagnostics = data.table::rbindlist(diagnostics, fill = TRUE),
    fold_assignment = fold_assignment,
    specification = list(
      prior_type = empirical_challenge_margin_prior_1d_type(),
      roles = roles, context = "count_state_original", alpha = role_alpha,
      bin_width_inches = bin_width_inches,
      score = "held-out CRPS for the count-shrunk empirical margin CDF",
      eligible = "tracked structural abs_eligible adverse calls",
      action_outcome_columns_used = character()
    )
  )
  class(result) <- "revealed_empirical_margin_prior_1d_crossfit"
  result
}

select_revealed_empirical_margin_alpha_1d <- function(
  rows, fold_assignment = NULL, folds = 5L, seed = 20260826L,
  alpha_grid = c(0, 25, 50, 100, 200), bin_width_inches = 0.01
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  if ("abs_eligible" %in% names(x)) x <- x[abs_eligible %in% TRUE]
  if (is.null(fold_assignment)) {
    fold_assignment <- continuous_game_folds(x$game_pk, folds, seed)
  }
  fold_assignment <- normalize_joint_common_width_fold_assignment_1d(
    fold_assignment, unique(x$game_pk)
  )
  x[fold_assignment, fold := i.fold, on = "game_pk"]
  alpha_grid <- sort(unique(as.numeric(alpha_grid)))
  if (!length(alpha_grid) || any(!is.finite(alpha_grid)) || any(alpha_grid < 0)) {
    stop("alpha_grid must contain finite non-negative values", call. = FALSE)
  }
  pieces <- list(); index <- 0L
  for (fold_value in sort(unique(x$fold))) {
    for (role_value in revealed_challenge_prior_1d_roles()) {
      train <- .revealed_empirical_role_rows_1d(x[fold != fold_value], role_value)
      test <- .revealed_empirical_role_rows_1d(x[fold == fold_value], role_value)
      for (alpha in alpha_grid) {
        fit <- fit_revealed_empirical_margin_prior_role_1d(
          train, role_value, alpha, bin_width_inches, fold_value
        )
        score <- score_empirical_challenge_margin_crps_1d(
          fit, .revealed_prior_adapter_1d(test, "count_state_original")
        )[, .(game_pk, negative_crps)]
        score[, `:=`(
          role = role_value, outer_fold = as.integer(fold_value), alpha = alpha
        )]
        index <- index + 1L; pieces[[index]] <- score
      }
    }
  }
  scored <- data.table::rbindlist(pieces, fill = TRUE)
  game_scores <- scored[, .(
    score = mean(negative_crps), rows = .N
  ), by = .(role, alpha, outer_fold, game_pk)]
  metrics <- scored[, {
    row_n <- .N
    estimate <- mean(negative_crps)
    residual <- negative_crps - estimate
    by_game <- data.table::data.table(
      game_pk = game_pk, residual = residual
    )[, .(residual_sum = sum(residual)), by = game_pk]
    games <- nrow(by_game)
    clustered_se <- if (games > 1L) {
      sqrt(
        games / (games - 1L) *
          sum(by_game$residual_sum^2) / row_n^2
      )
    } else {
      0
    }
    list(
      mean_score = estimate,
      game_clustered_se = clustered_se,
      games = games,
      rows = row_n
    )
  }, by = .(role, alpha)]
  metrics[, `:=`(best_mean_score = max(mean_score)), by = role]
  metrics[, one_se_threshold := {
    best <- which.max(mean_score)
    mean_score[[best]] - game_clustered_se[[best]]
  }, by = role]
  metrics[, within_one_se := mean_score >= one_se_threshold]
  # More shrinkage is the lower-variance specification, so the one-SE rule
  # chooses the largest alpha whose held-out score remains adequate.
  metrics[, selected := alpha == max(alpha[within_one_se]), by = role]
  data.table::setorder(metrics, role, alpha)
  list(
    selected_alpha = stats::setNames(
      metrics[selected == TRUE, alpha], metrics[selected == TRUE, role]
    ),
    metrics = metrics[], game_scores = game_scores[], scores = scored[],
    fold_assignment = fold_assignment,
    selection_rule = paste(
      "role-specific one-standard-error rule on game-clustered held-out",
      "CRPS; ties favor more shrinkage"
    )
  )
}

refit_fixed_clock_empirical_priors_1d <- function(
  prior_rows, game_weights = NULL, alpha = 100,
  bin_width_inches = 0.01, fold_id = "bootstrap"
) {
  prior <- data.table::copy(data.table::as.data.table(prior_rows))
  if (!is.null(game_weights)) {
    prior <- expand_fixed_clock_game_bootstrap_1d(prior, game_weights)
  }
  fit_revealed_empirical_margin_priors_1d(
    prior, alpha = alpha, bin_width_inches = bin_width_inches,
    fold_id = fold_id
  )
}

refit_fixed_clock_empirical_development_nuisance_1d <- function(
  prior_rows, selection_rows, game_weights = NULL,
  alpha = 100, bin_width_inches = 0.01,
  local_margin_limit_inches = 3, nthreads = 1L,
  fold_id = "bootstrap"
) {
  selection <- data.table::copy(data.table::as.data.table(selection_rows))
  priors <- refit_fixed_clock_empirical_priors_1d(
    prior_rows, game_weights, alpha, bin_width_inches, fold_id
  )
  if (!is.null(game_weights)) {
    selection <- expand_fixed_clock_game_bootstrap_1d(selection, game_weights)
  }
  widths <- refit_fixed_clock_effective_widths_1d(
    selection, local_margin_limit_inches = local_margin_limit_inches,
    nthreads = nthreads
  )
  list(
    prior_fits = priors,
    width_estimates = widths,
    effective_width = stats::setNames(
      widths$sigma_inches[
        match(revealed_challenge_prior_1d_roles(), widths$role)
      ],
      revealed_challenge_prior_1d_roles()
    ),
    prior_type = empirical_challenge_margin_prior_1d_type(),
    alpha = alpha,
    bin_width_inches = bin_width_inches,
    game_cluster_resampled = !is.null(game_weights)
  )
}

# Runtime dispatch ----------------------------------------------------------
# The project currently uses function interfaces rather than S3 generics.  Keep
# the established Gaussian functions as aliases and dispatch only when the new
# empirical class is present.  This makes the GMM a selectable sensitivity and
# avoids invasive edits to the older modules.

if (!exists(".gmm_challenge_margin_validate_fit_1d", inherits = FALSE)) {
  .gmm_challenge_margin_validate_fit_1d <- .challenge_margin_validate_fit
  .gmm_challenge_margin_prior_density_1d <- challenge_margin_prior_density_1d
  .gmm_score_challenge_margin_prior_1d <- score_challenge_margin_prior_1d
  .gmm_challenge_margin_prior_ball_rate_1d <- challenge_margin_prior_ball_rate_1d
  .gmm_challenge_margin_subjective_ball_probability_1d <-
    challenge_margin_subjective_ball_probability_1d
  .gmm_build_challenge_signal_lookup_1d <- build_challenge_signal_lookup_1d
  .gmm_challenge_signal_payoff_terms_1d <- challenge_signal_payoff_terms_1d
  .gmm_challenge_signal_success_tail_exact_1d <-
    .challenge_signal_success_tail_exact_1d
  .gmm_challenge_margin_payoff_root_scalar_1d <-
    .challenge_margin_payoff_root_scalar_1d
  .gmm_fixed_clock_direct_lookup_threshold_masses_1d <-
    .fixed_clock_direct_lookup_threshold_masses_1d
}

.challenge_margin_validate_fit <- function(fit) {
  if (inherits(fit, "empirical_challenge_margin_prior_1d_fit")) {
    return(validate_empirical_challenge_margin_prior_1d(fit))
  }
  .gmm_challenge_margin_validate_fit_1d(fit)
}

challenge_margin_prior_density_1d <- function(
  fit, margin, log = FALSE, context = NULL
) {
  if (inherits(fit, "empirical_challenge_margin_prior_1d_fit")) {
    return(empirical_challenge_margin_prior_density_1d(
      fit, margin, log, context
    ))
  }
  .gmm_challenge_margin_prior_density_1d(fit, margin, log, context)
}

score_challenge_margin_prior_1d <- function(
  fit, pitches, require_game_separation = FALSE
) {
  if (inherits(fit, "empirical_challenge_margin_prior_1d_fit")) {
    return(score_empirical_challenge_margin_prior_1d(
      fit, pitches, require_game_separation
    ))
  }
  .gmm_score_challenge_margin_prior_1d(
    fit, pitches, require_game_separation
  )
}

challenge_margin_prior_ball_rate_1d <- function(fit, context = NULL) {
  if (inherits(fit, "empirical_challenge_margin_prior_1d_fit")) {
    return(empirical_challenge_margin_prior_ball_rate_1d(fit, context))
  }
  .gmm_challenge_margin_prior_ball_rate_1d(fit, context)
}

challenge_margin_subjective_ball_probability_1d <- function(
  fit, private_margin_signal, perception_sigma, context = NULL
) {
  if (inherits(fit, "empirical_challenge_margin_prior_1d_fit")) {
    return(empirical_challenge_margin_subjective_ball_probability_1d(
      fit, private_margin_signal, perception_sigma, context
    ))
  }
  .gmm_challenge_margin_subjective_ball_probability_1d(
    fit, private_margin_signal, perception_sigma, context
  )
}

build_challenge_signal_lookup_1d <- function(
  prior_fit, perception_sigma, context, grid_step = 0.0025,
  tail_standard_deviations = 12, minimum_half_width = 40
) {
  if (inherits(prior_fit, "empirical_challenge_margin_prior_1d_fit")) {
    return(build_empirical_challenge_signal_lookup_1d(
      prior_fit, perception_sigma, context, grid_step,
      tail_standard_deviations, minimum_half_width
    ))
  }
  .gmm_build_challenge_signal_lookup_1d(
    prior_fit, perception_sigma, context, grid_step,
    tail_standard_deviations, minimum_half_width
  )
}

challenge_signal_payoff_terms_1d <- function(
  lookup, gain, inventory_loss, context
) {
  if (inherits(lookup, "empirical_challenge_signal_lookup_1d")) {
    return(empirical_challenge_signal_payoff_terms_1d(
      lookup, gain, inventory_loss, context
    ))
  }
  .gmm_challenge_signal_payoff_terms_1d(
    lookup, gain, inventory_loss, context
  )
}

.challenge_signal_success_tail_exact_1d <- function(
  lookup, threshold, component_weight
) {
  if (inherits(lookup, "empirical_challenge_signal_lookup_1d")) {
    return(.empirical_margin_exact_tail_1d(
      lookup$prior_fit, threshold, component_weight,
      lookup$perception_sigma, TRUE
    ))
  }
  .gmm_challenge_signal_success_tail_exact_1d(
    lookup, threshold, component_weight
  )
}

.challenge_margin_payoff_root_scalar_1d <- function(
  fit, gain, inventory_loss, perception_sigma, component_weight,
  root_tolerance, root_max_iterations, max_bracket_expansions
) {
  if (inherits(fit, "empirical_challenge_margin_prior_1d_fit")) {
    return(.empirical_margin_payoff_root_scalar_1d(
      fit, gain, inventory_loss, perception_sigma, component_weight,
      root_tolerance, root_max_iterations, max_bracket_expansions
    ))
  }
  .gmm_challenge_margin_payoff_root_scalar_1d(
    fit, gain, inventory_loss, perception_sigma, component_weight,
    root_tolerance, root_max_iterations, max_bracket_expansions
  )
}

.fixed_clock_direct_lookup_threshold_masses_1d <- function(
  lookup, threshold, context
) {
  if (inherits(lookup, "empirical_challenge_signal_lookup_1d")) {
    return(.empirical_fixed_clock_threshold_masses_1d(
      lookup, threshold, context
    ))
  }
  .gmm_fixed_clock_direct_lookup_threshold_masses_1d(
    lookup, threshold, context
  )
}
