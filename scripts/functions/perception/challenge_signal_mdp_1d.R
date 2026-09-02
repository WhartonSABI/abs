# A finite-horizon/semi-Markov inventory model for a batter who observes a
# private one-dimensional location signal.  The signal is integrated at the
# opportunity; only the inventory state persists between opportunities.

.challenge_signal_mdp_required_columns_1d <- function() {
  c(
    "game_pk", "team_id", "pitch_order", "inning", "stage", "role",
    "state_key", "raw_out_index", "edge_distance_inches", "count_state",
    "stake_G"
  )
}

prepare_challenge_signal_mdp_opportunities_1d <- function(
  pitch_ledger, re_model
) {
  x <- data.table::copy(data.table::as.data.table(pitch_ledger))
  required <- c(
    "game_pk", "pitch_order", "inning", "half", "outs_before",
    "balls_before", "strikes_before", "initial_call", "bat_team_id",
    "batter_id", "tracking_available", "edge_distance_inches",
    "challenge_occurred", "challenger_role"
  )
  stop_if_missing_columns(x, required, "challenge-signal MDP pitch ledger")
  optional_int <- c(
    "on_1b", "on_2b", "on_3b", "concurrent_remove_1b",
    "concurrent_remove_2b", "concurrent_remove_3b", "concurrent_add_1b",
    "concurrent_add_2b", "concurrent_add_3b", "concurrent_runs",
    "concurrent_outs"
  )
  for (column in optional_int) {
    if (!column %in% names(x)) {
      x[, (column) := if (startsWith(column, "on_")) NA_integer_ else 0L]
    }
  }
  eligible <- x$initial_call == "called_strike" &
    x$tracking_available %in% TRUE & is.finite(x$edge_distance_inches) &
    !is.na(x$batter_id) & !is.na(x$bat_team_id) &
    x$balls_before %in% 0:3 & x$strikes_before %in% 0:2 &
    x$outs_before %in% 0:2
  if ("abs_eligible" %in% names(x)) {
    eligible <- eligible & x$abs_eligible %in% TRUE
  }
  rows <- continuous_context_fields(x[eligible])
  if (!nrow(rows)) stop("No batter called-strike opportunities are available")

  re_table <- if (is.list(re_model) && !is.null(re_model$table)) {
    re_model$table
  } else {
    re_model
  }
  stop_if_missing_columns(
    re_table, c("outs", "base_state", "balls", "strikes", "re"),
    "run-expectancy model"
  )
  called_strike <- vectorized_call_branch(
    rows, rep.int("called_strike", nrow(rows))
  )
  overturned_ball <- vectorized_call_branch(
    rows, rep.int("ball", nrow(rows))
  )
  gain <- vectorized_branch_re(re_table, overturned_ball) -
    vectorized_branch_re(re_table, called_strike)
  if (any(!is.finite(gain))) {
    stop("Batter challenge gains could not be evaluated")
  }

  rows[, `:=`(
    team_id = as.integer(bat_team_id),
    role = "offense",
    raw_out_index = mdp_out_index(inning, half, outs_before),
    stake_G = as.numeric(gain),
    actual_wrong = edge_distance_inches > 0,
    observed_batter_challenge =
      challenge_occurred %in% TRUE & challenger_role == "batter",
    initial_call = "called_strike",
    tracking_available = TRUE
  )]
  rows[, stage := collapse_mdp_stage(raw_out_index)]
  rows[, state_key := mdp_state_key(stage, role)]
  keep <- unique(c(
    .challenge_signal_mdp_required_columns_1d(),
    "balls_before", "strikes_before", "initial_call", "tracking_available",
    "actual_wrong", "observed_batter_challenge"
  ))
  out <- rows[, ..keep]
  data.table::setorder(out, game_pk, team_id, pitch_order)
  if (anyDuplicated(out[, .(game_pk, pitch_order)])) {
    stop("Challenge-signal MDP opportunities contain duplicate pitch keys")
  }
  out[]
}

.challenge_signal_context_weights_1d <- function(fit, context) {
  resolved <- .challenge_margin_resolve_context_weights(
    fit, as.character(context), 1L
  )
  list(
    weight = as.numeric(resolved$weight[1L, ]),
    context = resolved$context_value[[1L]],
    fallback = resolved$context_fallback[[1L]]
  )
}

.challenge_signal_grid_quantities_1d <- function(
  fit, sigma, component_weight, signal
) {
  components <- length(fit$weight)
  signal <- as.numeric(signal)
  signal_density <- numeric(length(signal))
  ball_joint_density <- numeric(length(signal))
  for (component in seq_len(components)) {
    prior_variance <- fit$sd[[component]]^2
    signal_variance <- sigma^2
    total_variance <- prior_variance + signal_variance
    density <- component_weight[[component]] * stats::dnorm(
      signal,
      mean = fit$mean[[component]],
      sd = sqrt(total_variance)
    )
    posterior_variance <- prior_variance * signal_variance / total_variance
    posterior_mean <- (
      signal_variance * fit$mean[[component]] + prior_variance * signal
    ) / total_variance
    component_ball_probability <- stats::pnorm(
      posterior_mean / sqrt(posterior_variance)
    )
    signal_density <- signal_density + density
    ball_joint_density <- ball_joint_density +
      density * component_ball_probability
  }
  q <- ball_joint_density / signal_density
  list(
    signal_density = signal_density,
    ball_joint_density = ball_joint_density,
    q = pmin(1, pmax(0, q))
  )
}

.challenge_signal_right_integral_1d <- function(x, density, upper_tail = 0) {
  if (length(x) != length(density) || length(x) < 2L ||
      any(!is.finite(x)) || any(diff(x) <= 0) ||
      any(!is.finite(density)) || any(density < 0)) {
    stop("Invalid signal-density grid", call. = FALSE)
  }
  increment <- diff(x) * (density[-length(density)] + density[-1L]) / 2
  c(rev(cumsum(rev(increment))) + upper_tail, upper_tail)
}

build_challenge_signal_lookup_1d <- function(
  prior_fit,
  perception_sigma,
  context,
  grid_step = 0.0025,
  tail_standard_deviations = 12,
  minimum_half_width = 40
) {
  .challenge_margin_validate_fit(prior_fit)
  sigma <- as.numeric(perception_sigma)
  if (length(sigma) != 1L || !is.finite(sigma) || sigma < 0) {
    stop("perception_sigma must be one finite non-negative common width",
      call. = FALSE
    )
  }
  grid_step <- as.numeric(grid_step)
  tail_standard_deviations <- as.numeric(tail_standard_deviations)
  minimum_half_width <- as.numeric(minimum_half_width)
  if (length(grid_step) != 1L || !is.finite(grid_step) || grid_step <= 0 ||
      length(tail_standard_deviations) != 1L ||
      !is.finite(tail_standard_deviations) || tail_standard_deviations < 8 ||
      length(minimum_half_width) != 1L || !is.finite(minimum_half_width) ||
      minimum_half_width <= 0) {
    stop("Invalid challenge-signal lookup controls", call. = FALSE)
  }
  context <- unique(as.character(context))
  if (!length(context) || anyNA(context) || any(!nzchar(context))) {
    stop("At least one explicit prior context is required", call. = FALSE)
  }

  # The kappa profile includes the identified-set endpoint sigma = 0.  At that
  # endpoint R = M exactly and q(R) is the deterministic indicator R > 0.
  # Represent it explicitly rather than approximating it with a small positive
  # width; payoff inversion handles the discontinuity below.
  if (sigma == 0) {
    requested <- unique(c(context, ".league"))
    tables <- lapply(requested, function(key) {
      resolved <- if (identical(key, ".league")) {
        list(
          weight = prior_fit$weight,
          context = NA_character_,
          fallback = TRUE
        )
      } else {
        .challenge_signal_context_weights_1d(prior_fit, key)
      }
      prior_rate <- sum(
        resolved$weight * stats::pnorm(prior_fit$mean / prior_fit$sd)
      )
      list(
        context = resolved$context,
        context_fallback = resolved$fallback,
        component_weight = resolved$weight,
        signal = c(-minimum_half_width, 0, minimum_half_width),
        q = c(0, 0, 1),
        inverse_log_odds = c(-Inf, Inf),
        inverse_signal = c(0, 0),
        success_tail = c(prior_rate, prior_rate, 0),
        prior_ball_rate = prior_rate,
        prior_mass_closure_error = 0
      )
    })
    names(tables) <- requested
    out <- list(
      prior_fit = prior_fit,
      perception_sigma = sigma,
      tables = tables,
      contexts = context,
      grid_step = grid_step,
      signal_range = c(-minimum_half_width, minimum_half_width),
      tail_standard_deviations = tail_standard_deviations,
      maximum_prior_mass_closure_error = 0,
      deterministic_geometry_limit = TRUE
    )
    class(out) <- "challenge_signal_lookup_1d"
    return(out)
  }
  signal_sd <- sqrt(prior_fit$sd^2 + sigma^2)
  lower <- min(
    -minimum_half_width,
    prior_fit$mean - tail_standard_deviations * signal_sd
  )
  upper <- max(
    minimum_half_width,
    prior_fit$mean + tail_standard_deviations * signal_sd
  )
  signal <- seq(lower, upper, by = grid_step)
  if (tail(signal, 1L) < upper) signal <- c(signal, upper)

  requested <- unique(c(context, ".league"))
  tables <- vector("list", length(requested))
  names(tables) <- requested
  for (key in requested) {
    resolved <- if (identical(key, ".league")) {
      list(weight = prior_fit$weight, context = NA_character_, fallback = TRUE)
    } else {
      .challenge_signal_context_weights_1d(prior_fit, key)
    }
    quantities <- .challenge_signal_grid_quantities_1d(
      prior_fit, sigma, resolved$weight, signal
    )
    if (any(diff(quantities$q) < -1e-9)) {
      stop("The contextual private-signal posterior is not monotone")
    }
    q <- cummax(quantities$q)
    signal_component_sd <- sqrt(prior_fit$sd^2 + sigma^2)
    upper_action_tail <- sum(
      resolved$weight * stats::pnorm(
        signal[[length(signal)]], prior_fit$mean, signal_component_sd,
        lower.tail = FALSE
      )
    )
    # At twelve or more signal SDs into the upper tail, P(M > 0 | R) is one
    # to machine precision for the fitted called-strike margin priors.
    upper_success_tail <- upper_action_tail
    success_tail <- .challenge_signal_right_integral_1d(
      signal, quantities$ball_joint_density, upper_success_tail
    )
    prior_rate <- sum(
      resolved$weight * stats::pnorm(prior_fit$mean / prior_fit$sd)
    )
    closure_error <- abs(success_tail[[1L]] - prior_rate)
    if (!is.finite(closure_error) || closure_error > 1e-5) {
      stop(sprintf(
        "Signal lookup failed its prior-mass closure check (%.3g)",
        closure_error
      ))
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
      success_tail = success_tail,
      prior_ball_rate = prior_rate,
      prior_mass_closure_error = closure_error
    )
  }
  out <- list(
    prior_fit = prior_fit,
    perception_sigma = sigma,
    tables = tables,
    contexts = context,
    grid_step = grid_step,
    signal_range = range(signal),
    tail_standard_deviations = tail_standard_deviations,
    maximum_prior_mass_closure_error = max(vapply(
      tables, function(z) z$prior_mass_closure_error, numeric(1)
    ))
  )
  class(out) <- "challenge_signal_lookup_1d"
  out
}

.challenge_signal_lookup_table_1d <- function(lookup, context) {
  key <- as.character(context)
  if (!key %in% names(lookup$tables)) key <- ".league"
  lookup$tables[[key]]
}

.challenge_signal_success_tail_exact_1d <- function(
  lookup, threshold, component_weight
) {
  if (!requireNamespace("mvtnorm", quietly = TRUE)) {
    stop("Out-of-grid signal tails require the mvtnorm package")
  }
  vapply(as.numeric(threshold), function(cutoff) {
    if (cutoff == -Inf) {
      return(sum(
        component_weight * stats::pnorm(
          lookup$prior_fit$mean / lookup$prior_fit$sd
        )
      ))
    }
    if (cutoff == Inf) return(0)
    sum(vapply(seq_along(component_weight), function(component) {
      tau <- lookup$prior_fit$sd[[component]]
      covariance <- matrix(c(
        tau^2, tau^2,
        tau^2, tau^2 + lookup$perception_sigma^2
      ), nrow = 2L)
      component_weight[[component]] * as.numeric(mvtnorm::pmvnorm(
        lower = c(0, cutoff),
        upper = c(Inf, Inf),
        mean = rep(lookup$prior_fit$mean[[component]], 2L),
        sigma = covariance,
        algorithm = mvtnorm::TVPACK()
      ))
    }, numeric(1)))
  }, numeric(1))
}

challenge_signal_payoff_terms_1d <- function(
  lookup, gain, inventory_loss, context
) {
  if (!inherits(lookup, "challenge_signal_lookup_1d")) {
    stop("lookup must be a challenge_signal_lookup_1d", call. = FALSE)
  }
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
    stop("Invalid challenge-signal payoff inputs", call. = FALSE)
  }
  threshold <- rep(Inf, size)
  q_target <- rep(NA_real_, size)
  action_prior <- numeric(size)
  success_prior <- numeric(size)
  context_fallback <- logical(size)
  root_fallback <- logical(size)

  positive <- gain > 0
  zero_loss <- positive & loss == 0
  threshold[zero_loss] <- -Inf
  q_target[zero_loss] <- 0
  interior <- positive & loss > 0
  q_target[interior] <- loss[interior] / (gain[interior] + loss[interior])

  if (lookup$perception_sigma == 0) {
    threshold[interior] <- 0
    for (level in unique(context)) {
      index <- which(context == level)
      table <- .challenge_signal_lookup_table_1d(lookup, level)
      context_fallback[index] <- table$context_fallback ||
        !level %in% names(lookup$tables)
      prior_rate <- table$prior_ball_rate
      always <- index[threshold[index] == -Inf]
      finite <- index[is.finite(threshold[index])]
      if (length(always)) {
        action_prior[always] <- 1
        success_prior[always] <- prior_rate
      }
      if (length(finite)) {
        action_prior[finite] <- prior_rate
        success_prior[finite] <- prior_rate
      }
    }
    expected_advantage <- success_prior * gain -
      (action_prior - success_prior) * loss
    return(data.table::data.table(
      gain = gain,
      inventory_loss = loss,
      q_target = q_target,
      threshold_inches = threshold,
      prior_challenge_probability = action_prior,
      prior_success_and_challenge_probability = success_prior,
      prior_failure_and_challenge_probability = action_prior - success_prior,
      prior_q_chosen = ifelse(
        action_prior > 0, success_prior / action_prior, NA_real_
      ),
      prior_expected_advantage_re = pmax(0, expected_advantage),
      prior_context = context,
      context_fallback = context_fallback,
      exact_root_fallback = root_fallback
    ))
  }

  for (level in unique(context)) {
    index <- which(context == level)
    table <- .challenge_signal_lookup_table_1d(lookup, level)
    context_fallback[index] <- table$context_fallback ||
      !level %in% names(lookup$tables)
    inside <- index[interior[index]]
    if (length(inside)) {
      target_log_odds <- log(loss[inside]) - log(gain[inside])
      inverse <- stats::approx(
        table$inverse_log_odds,
        table$inverse_signal,
        xout = target_log_odds,
        rule = 1,
        ties = "ordered"
      )$y
      missing <- which(!is.finite(inverse))
      if (length(missing)) {
        for (position in missing) {
          row <- inside[[position]]
          solved <- .challenge_margin_payoff_root_scalar_1d(
            fit = lookup$prior_fit,
            gain = gain[[row]],
            inventory_loss = loss[[row]],
            perception_sigma = lookup$perception_sigma,
            component_weight = table$component_weight,
            root_tolerance = 1e-10,
            root_max_iterations = 200L,
            max_bracket_expansions = 40L
          )
          inverse[[position]] <- solved$threshold
          root_fallback[[row]] <- TRUE
        }
      }
      threshold[inside] <- inverse
    }

    finite <- index[is.finite(threshold[index])]
    if (length(finite)) {
      component_sd <- sqrt(
        lookup$prior_fit$sd^2 + lookup$perception_sigma^2
      )
      action_probability <- numeric(length(finite))
      for (component in seq_along(table$component_weight)) {
        action_probability <- action_probability +
          table$component_weight[[component]] * stats::pnorm(
            threshold[finite],
            lookup$prior_fit$mean[[component]],
            component_sd[[component]],
            lower.tail = FALSE
          )
      }
      action_prior[finite] <- action_probability
      success_prior[finite] <- stats::approx(
        table$signal,
        table$success_tail,
        xout = threshold[finite],
        rule = 2,
        ties = "ordered"
      )$y
      outside <- finite[
        threshold[finite] < min(table$signal) |
          threshold[finite] > max(table$signal)
      ]
      if (length(outside)) {
        success_prior[outside] <- .challenge_signal_success_tail_exact_1d(
          lookup,
          threshold = threshold[outside],
          component_weight = table$component_weight
        )
      }
    }
    always <- index[threshold[index] == -Inf]
    if (length(always)) {
      action_prior[always] <- 1
      success_prior[always] <- table$prior_ball_rate
    }
  }
  action_prior <- pmin(1, pmax(0, action_prior))
  success_prior <- pmin(action_prior, pmax(0, success_prior))
  expected_advantage <- success_prior * gain -
    (action_prior - success_prior) * loss
  materially_negative <- expected_advantage < -2e-5
  if (any(materially_negative)) {
    stop("Numerical signal integration produced negative optimal advantage")
  }
  expected_advantage <- pmax(0, expected_advantage)
  data.table::data.table(
    gain = gain,
    inventory_loss = loss,
    q_target = q_target,
    threshold_inches = threshold,
    prior_challenge_probability = action_prior,
    prior_success_and_challenge_probability = success_prior,
    prior_failure_and_challenge_probability = action_prior - success_prior,
    prior_q_chosen = ifelse(action_prior > 0, success_prior / action_prior, NA_real_),
    prior_expected_advantage_re = expected_advantage,
    prior_context = context,
    context_fallback = context_fallback,
    exact_root_fallback = root_fallback
  )
}

.challenge_signal_mdp_fit_rows_1d <- function(opportunities) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  # True margin is needed to fit the separate contextual prior and to score a
  # frozen policy, but not by the Bellman transition/operator itself.
  required <- setdiff(
    .challenge_signal_mdp_required_columns_1d(), "edge_distance_inches"
  )
  stop_if_missing_columns(x, required, "challenge-signal MDP opportunities")
  x <- x[, ..required]
  x[, `:=`(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    inning = as.integer(inning),
    stage = as.integer(stage),
    role = as.character(role),
    state_key = as.character(state_key),
    raw_out_index = as.integer(raw_out_index),
    count_state = as.character(count_state),
    stake_G = as.numeric(stake_G),
    p_hat = 0.5
  )]
  if (!nrow(x) || anyNA(x) || any(!is.finite(x$stake_G)) ||
      anyDuplicated(x[, .(game_pk, pitch_order)])) {
    stop("Challenge-signal MDP opportunities are invalid", call. = FALSE)
  }
  data.table::setorder(x, game_pk, team_id, pitch_order)
  add_mdp_transitions(x)
}

.challenge_signal_mdp_operator_1d <- function(fit_data, arrival_values) {
  states <- fit_data$states
  continuation <- matrix(
    0, nrow = nrow(states), ncol = 3L,
    dimnames = list(states$state_key, as.character(0:2))
  )
  # A reverse-time Gauss-Seidel sweep propagates regulation values through all
  # later stages in one pass.  Repeated sweeps are still required for same-
  # stage opportunities and the collapsed extra-inning cycle.
  updated <- arrival_values
  state_order <- order(states$stage, decreasing = TRUE)
  for (i in state_order) {
    exact <- fit_data$exact_rows[[i]]
    parent <- fit_data$parent_rows[[i]]
    weight <- states$shrink_weight[[i]]
    for (inventory in 0:2) {
      exact_continuation <- mdp_transition_mean(
        exact, states$stage[[i]], inventory, updated,
        fit_data$state_index, relative = FALSE
      )
      parent_continuation <- mdp_transition_mean(
        parent, states$stage[[i]], inventory, updated,
        fit_data$state_index, relative = TRUE
      )
      continuation[i, inventory + 1L] <-
        weight * exact_continuation + (1 - weight) * parent_continuation
    }
    for (inventory in 0:2) {
      wait <- continuation[i, inventory + 1L]
      if (inventory == 0L) {
        updated[i, inventory + 1L] <- wait
        next
      }
      lower <- continuation[i, inventory]
      loss <- wait - lower
      if (loss < -1e-8) {
        stop("Challenge-signal Bellman step produced negative inventory value")
      }
      loss <- pmax(0, loss)
      opportunity_value <- function(rows) {
        if (!nrow(rows)) return(wait)
        terms <- challenge_signal_payoff_terms_1d(
          fit_data$lookup,
          gain = rows$stake_G,
          inventory_loss = loss,
          context = rows$count_state
        )
        wait + mean(terms$prior_expected_advantage_re)
      }
      exact_value <- opportunity_value(exact)
      parent_value <- opportunity_value(parent)
      updated[i, inventory + 1L] <-
        weight * exact_value + (1 - weight) * parent_value
    }
  }
  list(arrival = updated, continuation = continuation)
}

fit_challenge_signal_mdp_1d <- function(
  opportunities,
  prior_fit,
  perception_sigma,
  prior_n = 30,
  tol = 1e-9,
  max_iter = 10000L,
  lookup_grid_step = 0.0025
) {
  x <- .challenge_signal_mdp_fit_rows_1d(opportunities)
  training_games <- sort(unique(x$game_pk))
  prior_games <- sort(as.character(prior_fit$training_games %||% character()))
  if (length(prior_games) && !identical(training_games, prior_games)) {
    stop("The margin prior and Bellman fit must use exactly the same games")
  }
  prior_n <- as.numeric(prior_n)
  tol <- as.numeric(tol)
  max_iter <- as.integer(max_iter)
  if (!is.finite(prior_n) || prior_n < 0 || !is.finite(tol) || tol <= 0 ||
      is.na(max_iter) || max_iter < 1L) {
    stop("Invalid challenge-signal MDP controls", call. = FALSE)
  }
  lookup <- build_challenge_signal_lookup_1d(
    prior_fit,
    perception_sigma,
    context = unique(x$count_state),
    grid_step = lookup_grid_step
  )

  states <- unique(x[, .(stage = as.integer(stage), role = as.character(role))])
  states[, `:=`(
    state_key = mdp_state_key(stage, role),
    parent_key = mdp_parent_key(stage, role)
  )]
  data.table::setorder(states, stage, role)
  state_index <- stats::setNames(seq_len(nrow(states)), states$state_key)
  attr(state_index, "states") <- states
  exact_rows <- lapply(states$state_key, function(key) x[state_key == key])
  parent_rows <- lapply(states$parent_key, function(key) x[parent_key == key])
  states[, `:=`(
    n = vapply(exact_rows, nrow, integer(1)),
    parent_n = vapply(parent_rows, nrow, integer(1))
  )]
  if (any(states$parent_n == 0L)) {
    stop("Every challenge-signal MDP state needs empirical parent rows")
  }
  states[, shrink_weight := if (prior_n == 0) {
    as.numeric(n > 0L)
  } else {
    n / (n + prior_n)
  }]
  fit_data <- list(
    states = states,
    exact_rows = exact_rows,
    parent_rows = parent_rows,
    state_index = state_index,
    lookup = lookup
  )
  arrival <- matrix(
    0, nrow = nrow(states), ncol = 3L,
    dimnames = list(states$state_key, as.character(0:2))
  )
  residual <- Inf
  iteration <- 0L
  while (iteration < max_iter && residual > tol) {
    iteration <- iteration + 1L
    step <- .challenge_signal_mdp_operator_1d(fit_data, arrival)
    residual <- max(abs(step$arrival - arrival))
    arrival <- step$arrival
  }
  if (!is.finite(residual) || residual > tol) {
    stop(sprintf(
      "Challenge-signal MDP did not converge after %s iterations (%.3g)",
      iteration, residual
    ))
  }
  final <- .challenge_signal_mdp_operator_1d(fit_data, arrival)
  bellman_residual <- max(abs(final$arrival - arrival))
  continuation <- final$continuation
  marginal_1 <- continuation[, "1"] - continuation[, "0"]
  marginal_2 <- continuation[, "2"] - continuation[, "1"]
  marginal_1[abs(marginal_1) < 1e-10] <- 0
  marginal_2[abs(marginal_2) < 1e-10] <- 0
  if (any(marginal_1 < -1e-7) || any(marginal_2 < -1e-7)) {
    stop("Challenge-signal MDP produced negative inventory values")
  }
  marginal_1 <- pmax(0, marginal_1)
  marginal_2 <- pmax(0, marginal_2)
  values <- data.table::copy(states)
  values[, `:=`(
    arrival_re_k0 = arrival[, "0"],
    arrival_re_k1 = arrival[, "1"],
    arrival_re_k2 = arrival[, "2"],
    continuation_re_k0 = continuation[, "0"],
    continuation_re_k1 = continuation[, "1"],
    continuation_re_k2 = continuation[, "2"],
    marginal_re_1_to_0 = marginal_1,
    marginal_re_2_to_1 = marginal_2
  )]
  fit <- list(
    state_values = values[],
    arrival = arrival,
    continuation = continuation,
    states = states,
    state_index = state_index,
    lookup = lookup,
    perception_sigma = lookup$perception_sigma,
    residual = bellman_residual,
    iterations = iteration,
    prior_n = prior_n,
    training_games = training_games,
    prior_training_games = prior_games,
    prior_training_fingerprint = prior_fit$training_fingerprint %||% NA_character_,
    information_regime = paste(
      "private margin signal integrated before action;",
      "inventory and empirical game stage persist"
    )
  )
  class(fit) <- "challenge_signal_mdp_1d"
  fit
}

challenge_signal_mdp_action_1d <- function(
  fit, stage, inventory, gain, context, true_margin = NULL, role = "offense"
) {
  if (!inherits(fit, "challenge_signal_mdp_1d")) {
    stop("fit must be a challenge_signal_mdp_1d", call. = FALSE)
  }
  lengths <- c(
    length(stage), length(inventory), length(gain), length(context),
    length(role), if (is.null(true_margin)) 1L else length(true_margin)
  )
  size <- max(lengths)
  if (any(lengths != 1L & lengths != size)) {
    stop("Challenge-signal action inputs must be scalar or aligned")
  }
  stage <- rep_len(collapse_mdp_stage(stage), size)
  inventory <- rep_len(as.integer(inventory), size)
  gain <- rep_len(as.numeric(gain), size)
  context <- rep_len(as.character(context), size)
  role <- rep_len(as.character(role), size)
  if (anyNA(inventory) || any(!inventory %in% 0:2) || anyNA(gain) ||
      any(!is.finite(gain)) || anyNA(context)) {
    stop("Invalid challenge-signal action inputs")
  }
  index <- resolve_mdp_state_index(stage, role, fit$states, fit$state_index)
  wait <- fit$continuation[cbind(index, inventory + 1L)]
  lower <- fit$continuation[cbind(index, pmax(inventory - 1L, 0L) + 1L)]
  loss <- pmax(0, wait - lower)
  terms <- challenge_signal_payoff_terms_1d(
    fit$lookup, gain, loss, context
  )
  unavailable <- inventory == 0L
  terms$threshold_inches[unavailable] <- NA_real_
  terms$q_target[unavailable] <- NA_real_
  terms$prior_challenge_probability[unavailable] <- 0
  terms$prior_success_and_challenge_probability[unavailable] <- 0
  terms$prior_failure_and_challenge_probability[unavailable] <- 0
  terms$prior_q_chosen[unavailable] <- NA_real_
  terms$prior_expected_advantage_re[unavailable] <- 0

  conditional_probability <- rep(NA_real_, size)
  if (!is.null(true_margin)) {
    margin <- rep_len(as.numeric(true_margin), size)
    if (anyNA(margin) || any(!is.finite(margin))) {
      stop("true_margin must be finite when supplied")
    }
    conditional_probability <- if (fit$perception_sigma == 0) {
      as.numeric(margin > terms$threshold_inches)
    } else {
      stats::pnorm(
        (margin - terms$threshold_inches) / fit$perception_sigma
      )
    }
    conditional_probability[terms$threshold_inches == -Inf] <- 1
    conditional_probability[terms$threshold_inches == Inf] <- 0
    conditional_probability[unavailable] <- 0
  }
  data.table::data.table(
    stage = stage,
    role = role,
    inventory = inventory,
    gain = gain,
    continuation_re = wait,
    lower_continuation_re = lower,
    marginal_inventory_re = ifelse(unavailable, NA_real_, loss),
    q_threshold = terms$q_target,
    signal_threshold_inches = terms$threshold_inches,
    prior_challenge_probability = terms$prior_challenge_probability,
    prior_q_chosen = terms$prior_q_chosen,
    prior_expected_advantage_re = terms$prior_expected_advantage_re,
    challenge_probability_given_true_margin = conditional_probability,
    context = context,
    context_fallback = terms$context_fallback
  )
}

replay_challenge_signal_mdp_1d <- function(
  opportunities, fit, initial_inventory = 2L,
  require_game_separation = TRUE
) {
  if (!inherits(fit, "challenge_signal_mdp_1d")) {
    stop("fit must be a challenge_signal_mdp_1d")
  }
  x <- data.table::copy(data.table::as.data.table(opportunities))
  required <- c(
    .challenge_signal_mdp_required_columns_1d(),
    "actual_wrong", "observed_batter_challenge"
  )
  stop_if_missing_columns(x, required, "challenge-signal MDP replay")
  if (isTRUE(require_game_separation) &&
      length(intersect(as.character(unique(x$game_pk)), fit$training_games))) {
    stop("Challenge-signal MDP replay overlaps its training games")
  }
  initial_inventory <- as.integer(initial_inventory)
  if (length(initial_inventory) != 1L || is.na(initial_inventory) ||
      !initial_inventory %in% 0:2) {
    stop("initial_inventory must be 0, 1, or 2")
  }
  data.table::setorder(x, game_pk, team_id, pitch_order)
  decisions <- lapply(0:2, function(inventory) {
    challenge_signal_mdp_action_1d(
      fit,
      stage = x$stage,
      inventory = inventory,
      gain = x$stake_G,
      context = x$count_state,
      true_margin = x$edge_distance_inches,
      role = x$role
    )
  })
  n <- nrow(x)
  inventory_before <- matrix(NA_real_, nrow = n, ncol = 3L)
  policy_challenge_probability <- numeric(n)
  expected_success <- numeric(n)
  expected_failure <- numeric(n)
  expected_captured_re <- numeric(n)
  expected_subjective_advantage_re <- numeric(n)
  prior_policy_challenge_probability <- numeric(n)
  prior_policy_success_probability <- numeric(n)
  threshold_k1 <- decisions[[2L]]$signal_threshold_inches
  threshold_k2 <- decisions[[3L]]$signal_threshold_inches
  loss_k1 <- decisions[[2L]]$marginal_inventory_re
  loss_k2 <- decisions[[3L]]$marginal_inventory_re

  groups <- split(
    seq_len(n), interaction(x$game_pk, x$team_id, drop = TRUE)
  )
  for (indices in groups) {
    mass <- c(`0` = 0, `1` = 0, `2` = 0)
    mass[[as.character(initial_inventory)]] <- 1
    previous_inning <- NA_integer_
    for (row in indices) {
      inning_now <- as.integer(x$inning[[row]])
      extra_inning_entry <- (
        is.na(previous_inning) && inning_now > 9L
      ) || (
        !is.na(previous_inning) && inning_now > previous_inning &&
          inning_now > 9L
      )
      if (extra_inning_entry) {
        mass[["1"]] <- mass[["1"]] + mass[["0"]]
        mass[["0"]] <- 0
      }
      inventory_before[row, ] <- mass
      action <- c(
        `0` = 0,
        `1` = decisions[[2L]]$challenge_probability_given_true_margin[[row]],
        `2` = decisions[[3L]]$challenge_probability_given_true_margin[[row]]
      )
      challenge_probability <- sum(mass * action)
      policy_challenge_probability[[row]] <- challenge_probability
      prior_action <- c(
        `0` = 0,
        `1` = decisions[[2L]]$prior_challenge_probability[[row]],
        `2` = decisions[[3L]]$prior_challenge_probability[[row]]
      )
      prior_selected_q <- c(
        `0` = 0,
        `1` = data.table::fcoalesce(
          decisions[[2L]]$prior_q_chosen[[row]], 0
        ),
        `2` = data.table::fcoalesce(
          decisions[[3L]]$prior_q_chosen[[row]], 0
        )
      )
      prior_policy_challenge_probability[[row]] <- sum(mass * prior_action)
      prior_policy_success_probability[[row]] <- sum(
        mass * prior_action * prior_selected_q
      )
      expected_subjective_advantage_re[[row]] <- sum(mass * c(
        0,
        decisions[[2L]]$prior_expected_advantage_re[[row]],
        decisions[[3L]]$prior_expected_advantage_re[[row]]
      ))
      if (isTRUE(x$actual_wrong[[row]])) {
        expected_success[[row]] <- challenge_probability
        expected_captured_re[[row]] <-
          challenge_probability * x$stake_G[[row]]
      } else {
        expected_failure[[row]] <- challenge_probability
        old <- mass
        mass[["0"]] <- old[["0"]] + old[["1"]] * action[["1"]]
        mass[["1"]] <- old[["1"]] * (1 - action[["1"]]) +
          old[["2"]] * action[["2"]]
        mass[["2"]] <- old[["2"]] * (1 - action[["2"]])
      }
      previous_inning <- inning_now
    }
  }
  x[, `:=`(
    inventory_probability_0 = inventory_before[, 1L],
    inventory_probability_1 = inventory_before[, 2L],
    inventory_probability_2 = inventory_before[, 3L],
    signal_threshold_k1_inches = threshold_k1,
    signal_threshold_k2_inches = threshold_k2,
    marginal_inventory_re_k1 = loss_k1,
    marginal_inventory_re_k2 = loss_k2,
    policy_challenge_probability = policy_challenge_probability,
    expected_policy_success = expected_success,
    expected_policy_failure = expected_failure,
    expected_captured_re = expected_captured_re,
    expected_subjective_advantage_re = expected_subjective_advantage_re,
    prior_policy_challenge_probability =
      prior_policy_challenge_probability,
    prior_policy_success_probability = prior_policy_success_probability,
    prior_policy_selected_success_probability = ifelse(
      prior_policy_challenge_probability > 0,
      prior_policy_success_probability / prior_policy_challenge_probability,
      NA_real_
    )
  )]
  x[]
}

summarize_challenge_signal_mdp_1d <- function(
  replay, bootstrap_reps = 1000L, seed = 20260825L
) {
  x <- data.table::as.data.table(replay)
  required <- c(
    "game_pk", "team_id", "stake_G", "actual_wrong",
    "observed_batter_challenge", "policy_challenge_probability",
    "expected_policy_success", "expected_policy_failure",
    "expected_captured_re", "prior_policy_challenge_probability",
    "prior_policy_success_probability"
  )
  stop_if_missing_columns(x, required, "challenge-signal MDP summary")
  team_game <- x[, .(
    model_re = sum(expected_captured_re),
    model_attempts = sum(policy_challenge_probability),
    model_successes = sum(expected_policy_success),
    model_failures = sum(expected_policy_failure),
    prior_model_attempts = sum(prior_policy_challenge_probability),
    prior_model_successes = sum(prior_policy_success_probability),
    observed_re = sum(stake_G[observed_batter_challenge & actual_wrong]),
    observed_attempts = sum(observed_batter_challenge),
    observed_successes = sum(observed_batter_challenge & actual_wrong),
    oracle_re = sum(stake_G[actual_wrong & stake_G > 0]),
    oracle_attempts = sum(actual_wrong & stake_G > 0)
  ), by = .(game_pk, team_id)]
  season <- team_game[, .(
    model_re = sum(model_re),
    model_attempts = sum(model_attempts),
    model_successes = sum(model_successes),
    model_failures = sum(model_failures),
    prior_model_attempts = sum(prior_model_attempts),
    prior_model_successes = sum(prior_model_successes),
    observed_re = sum(observed_re),
    observed_attempts = sum(observed_attempts),
    observed_successes = sum(observed_successes),
    oracle_re = sum(oracle_re),
    oracle_attempts = sum(oracle_attempts)
  )]
  season[, `:=`(
    additional_re_vs_observed = model_re - observed_re,
    model_success_rate = model_successes / model_attempts,
    prior_model_selected_success_rate =
      prior_model_successes / prior_model_attempts,
    observed_success_rate = observed_successes / observed_attempts,
    model_share_of_oracle = model_re / oracle_re,
    observed_share_of_oracle = observed_re / oracle_re
  )]
  bootstrap_reps <- as.integer(bootstrap_reps)
  bootstrap <- data.table::data.table()
  if (!is.na(bootstrap_reps) && bootstrap_reps > 0L) {
    game <- team_game[, lapply(.SD, sum), by = game_pk, .SDcols = c(
      "model_re", "observed_re", "oracle_re", "model_attempts",
      "model_successes", "observed_attempts", "observed_successes"
    )]
    set.seed(seed)
    bootstrap <- data.table::rbindlist(lapply(
      seq_len(bootstrap_reps),
      function(replicate) {
        sampled <- game[sample.int(nrow(game), nrow(game), replace = TRUE)]
        data.table::data.table(
          replicate = replicate,
          model_re = sum(sampled$model_re),
          observed_re = sum(sampled$observed_re),
          oracle_re = sum(sampled$oracle_re),
          additional_re_vs_observed =
            sum(sampled$model_re - sampled$observed_re)
        )
      }
    ))
    interval <- bootstrap[, .(
      model_re_lower_95 = stats::quantile(model_re, 0.025, names = FALSE),
      model_re_upper_95 = stats::quantile(model_re, 0.975, names = FALSE),
      additional_re_lower_95 = stats::quantile(
        additional_re_vs_observed, 0.025, names = FALSE
      ),
      additional_re_upper_95 = stats::quantile(
        additional_re_vs_observed, 0.975, names = FALSE
      )
    )]
    season <- cbind(season, interval)
  }
  list(season = season[], team_game = team_game[], bootstrap = bootstrap[])
}

crossfit_challenge_signal_mdp_1d <- function(
  opportunities,
  perception_sigma,
  fold_assignment = NULL,
  folds = 5L,
  seed = 20260825L,
  prior_components = 3L,
  prior_n = 30,
  tol = 1e-7,
  max_iter = 10000L,
  lookup_grid_step = 0.005,
  bootstrap_reps = 1000L,
  progress = interactive()
) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  required <- c(
    .challenge_signal_mdp_required_columns_1d(),
    "balls_before", "strikes_before", "initial_call", "tracking_available",
    "actual_wrong", "observed_batter_challenge"
  )
  stop_if_missing_columns(x, required, "challenge-signal MDP cross-fit")
  games <- sort(unique(as.character(x$game_pk)))
  folds <- as.integer(folds)
  if (is.null(fold_assignment)) {
    fold_assignment <- continuous_game_folds(games, folds = folds, seed = seed)
  } else {
    fold_assignment <- data.table::copy(
      data.table::as.data.table(fold_assignment)
    )
    stop_if_missing_columns(
      fold_assignment, c("game_pk", "fold"),
      "challenge-signal MDP folds"
    )
    fold_assignment[, game_pk := as.character(game_pk)]
  }
  if (anyDuplicated(fold_assignment$game_pk) ||
      !setequal(fold_assignment$game_pk, games) ||
      !setequal(unique(fold_assignment$fold), seq_len(folds))) {
    stop("Challenge-signal MDP folds must assign every game exactly once")
  }
  x[, game_pk := as.character(game_pk)]
  x <- merge(x, fold_assignment, by = "game_pk", all.x = TRUE, sort = FALSE)
  data.table::setorder(x, game_pk, team_id, pitch_order)

  fold_fits <- vector("list", folds)
  prior_fits <- vector("list", folds)
  replay_parts <- vector("list", folds)
  fold_diagnostics <- vector("list", folds)
  for (fold_id in seq_len(folds)) {
    training <- x[fold != fold_id]
    heldout <- x[fold == fold_id]
    training_games <- sort(unique(training$game_pk))
    heldout_games <- sort(unique(heldout$game_pk))
    if (isTRUE(progress)) {
      message(sprintf(
        "challenge-signal Bellman fold %s/%s: %s training, %s heldout rows",
        fold_id, folds, nrow(training), nrow(heldout)
      ))
    }
    prior <- fit_challenge_margin_prior_1d(
      training,
      components = prior_components,
      training_games = training_games,
      fold_id = paste0("bellman_fold_", fold_id),
      global_draw_id = 1L
    )
    fit_time <- system.time({
      bellman <- fit_challenge_signal_mdp_1d(
        training,
        prior_fit = prior,
        perception_sigma = perception_sigma,
        prior_n = prior_n,
        tol = tol,
        max_iter = max_iter,
        lookup_grid_step = lookup_grid_step
      )
    })
    if (length(intersect(bellman$training_games, heldout_games))) {
      stop("Heldout games entered a challenge-signal Bellman fit")
    }
    replay <- replay_challenge_signal_mdp_1d(
      heldout,
      bellman,
      require_game_separation = TRUE
    )
    replay[, fold := fold_id]
    prior_fits[[fold_id]] <- prior
    fold_fits[[fold_id]] <- bellman
    replay_parts[[fold_id]] <- replay
    fold_diagnostics[[fold_id]] <- data.table::data.table(
      fold = fold_id,
      training_games = length(training_games),
      heldout_games = length(heldout_games),
      training_rows = nrow(training),
      heldout_rows = nrow(heldout),
      prior_components = prior$components,
      prior_converged = prior$converged,
      bellman_iterations = bellman$iterations,
      bellman_residual = bellman$residual,
      maximum_prior_mass_closure_error =
        bellman$lookup$maximum_prior_mass_closure_error,
      elapsed_seconds = unname(fit_time[["elapsed"]])
    )
    if (isTRUE(progress)) {
      message(sprintf(
        "challenge-signal Bellman fold %s converged in %s iterations (%.1fs)",
        fold_id, bellman$iterations, unname(fit_time[["elapsed"]])
      ))
    }
  }
  replay <- data.table::rbindlist(replay_parts, fill = TRUE)
  summary <- summarize_challenge_signal_mdp_1d(
    replay, bootstrap_reps = bootstrap_reps, seed = seed
  )
  list(
    fold_assignment = fold_assignment[order(game_pk)],
    prior_fits = prior_fits,
    fold_fits = fold_fits,
    fold_diagnostics = data.table::rbindlist(fold_diagnostics),
    replay = replay[],
    season = summary$season,
    team_game = summary$team_game,
    bootstrap = summary$bootstrap,
    perception_sigma = as.numeric(perception_sigma),
    information_regime = paste(
      "five-fold game-heldout prior/Bellman with fixed common private-signal",
      "width; batter-only inventory; contextual called-strike margin prior"
    )
  )
}
