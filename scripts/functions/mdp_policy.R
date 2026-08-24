mdp_out_index <- function(inning, half, outs_before) {
  half <- tolower(as.character(half))
  if (any(!half %in% c("top", "bottom"), na.rm = TRUE)) {
    stop("MDP half must be 'top' or 'bottom'")
  }
  as.integer((as.integer(inning) - 1L) * 6L +
    ifelse(half == "bottom", 3L, 0L) + as.integer(outs_before))
}

collapse_mdp_stage <- function(out_index) {
  out_index <- as.integer(out_index)
  as.integer(ifelse(out_index < 54L, out_index, 54L + ((out_index - 54L) %% 6L)))
}

mdp_state_key <- function(stage, role) paste(as.integer(stage), as.character(role), sep = "|")

mdp_parent_key <- function(stage, role) {
  phase <- ifelse(as.integer(stage) < 54L, "regulation", "extra")
  paste(phase, as.integer(stage) %% 6L, as.character(role), sep = "|")
}

resolve_mdp_state_index <- function(stage, role, states, state_index) {
  key <- mdp_state_key(stage, role)
  index <- unname(state_index[key])
  missing <- which(is.na(index))
  if (length(missing)) {
    requested_parent <- mdp_parent_key(stage[missing], role[missing])
    for (j in seq_along(missing)) {
      candidates <- which(states$parent_key == requested_parent[[j]])
      if (!length(candidates)) stop("No MDP fallback state for ", key[missing[[j]]])
      index[missing[[j]]] <- candidates[
        which.min(abs(states$stage[candidates] - stage[missing[[j]]]))
      ]
    }
  }
  as.integer(index)
}

prepare_mdp_opportunities <- function(
  pitch_ledger, re_model, p_col = "p_hat", min_coverage = 0.99
) {
  x <- data.table::copy(data.table::as.data.table(pitch_ledger))
  required <- c(
    "game_pk", "pitch_order", "inning", "half", "outs_before",
    "balls_before", "strikes_before", "initial_call", "adverse_team_id",
    "adverse_role", "bat_team_id"
  )
  stop_if_missing_columns(x, c(required, p_col), "MDP pitch ledger")

  optional_int <- c(
    "on_1b", "on_2b", "on_3b", "concurrent_remove_1b", "concurrent_remove_2b",
    "concurrent_remove_3b", "concurrent_add_1b", "concurrent_add_2b",
    "concurrent_add_3b", "concurrent_runs", "concurrent_outs"
  )
  for (column in optional_int) {
    if (!column %in% names(x)) {
      x[, (column) := if (startsWith(column, "on_")) NA_integer_ else 0L]
    }
  }
  if (!"challenge_occurred" %in% names(x)) x[, challenge_occurred := FALSE]
  if (!"challenge_outcome" %in% names(x)) x[, challenge_outcome := NA_character_]
  if (!"challenger_team_id" %in% names(x)) x[, challenger_team_id := NA_integer_]

  base_eligible <- x$initial_call %in% c("ball", "called_strike") &
    !is.na(x$adverse_team_id) & x$adverse_role %in% c("offense", "defense") &
    !is.na(x$balls_before) & !is.na(x$strikes_before) & !is.na(x$outs_before)
  if ("abs_eligible" %in% names(x)) base_eligible <- base_eligible & x$abs_eligible %in% TRUE
  if ("edge_distance_inches" %in% names(x)) {
    base_eligible <- base_eligible & is.finite(x$edge_distance_inches) &
      abs(x$edge_distance_inches) <= 12
  }

  ok <- which(base_eligible)
  rows <- x[ok]
  flipped_call <- opposite_call(rows$initial_call)
  stands <- vectorized_call_branch(rows, rows$initial_call)
  flipped <- vectorized_call_branch(rows, flipped_call)
  re_table <- if (is.list(re_model) && !is.null(re_model$table)) re_model$table else re_model
  batting_gain <- vectorized_branch_re(re_table, flipped) -
    vectorized_branch_re(re_table, stands)
  x[, stake_G := NA_real_]
  x$stake_G[ok] <- ifelse(
    rows$adverse_team_id == rows$bat_team_id, batting_gain, -batting_gain
  )
  x[, p_hat := as.numeric(get(p_col))]

  invalid_probability <- base_eligible & !is.na(x$p_hat) &
    (!is.finite(x$p_hat) | x$p_hat < 0 | x$p_hat > 1)
  if (any(invalid_probability)) {
    stop(sum(invalid_probability), " MDP probabilities are outside [0, 1]")
  }
  # RE288 is an empirical table and is not constrained to be monotone in the
  # count. A few geometrically adverse calls can therefore have a negative
  # estimated correction effect. Keep those effects signed: waiting dominates
  # challenging them, whereas flooring them would change the RE objective.
  negative_stake_n <- sum(base_eligible & is.finite(x$stake_G) & x$stake_G < 0)
  if (negative_stake_n) {
    warning(
      negative_stake_n,
      " MDP correction stakes are negative in the empirical RE table; ",
      "they are retained as signed values and the optimal policy will wait",
      call. = FALSE
    )
  }
  scored <- base_eligible & is.finite(x$p_hat) & is.finite(x$stake_G)

  coverage_role <- x[base_eligible, .(
    eligible = .N,
    scored = sum(scored[.I]),
    coverage = mean(scored[.I])
  ), by = .(role = adverse_role)]
  coverage <- data.table::rbindlist(list(
    data.table::data.table(
      role = "league", eligible = sum(base_eligible), scored = sum(scored),
      coverage = if (sum(base_eligible)) mean(scored[base_eligible]) else NA_real_
    ),
    coverage_role
  ))
  if (!is.finite(coverage[role == "league", coverage]) ||
      coverage[role == "league", coverage] < min_coverage) {
    stop(sprintf(
      "MDP scored-pitch coverage %.3f%% is below %.3f%%",
      100 * coverage[role == "league", coverage], 100 * min_coverage
    ))
  }

  out <- x[scored]
  out[, `:=`(
    team_id = as.integer(adverse_team_id),
    role = as.character(adverse_role),
    raw_out_index = mdp_out_index(inning, half, outs_before)
  )]
  out[, stage := collapse_mdp_stage(raw_out_index)]
  out[, state_key := mdp_state_key(stage, role)]
  if ("call_wrong" %in% names(out)) {
    out[, actual_wrong := as.logical(call_wrong)]
  } else if ("abs_call" %in% names(out)) {
    out[, actual_wrong := initial_call != abs_call]
  } else {
    out[, actual_wrong := NA]
  }
  data.table::setorder(out, game_pk, team_id, pitch_order)
  attr(out, "coverage") <- coverage
  out[]
}

add_mdp_transitions <- function(opportunities) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  required <- c(
    "game_pk", "team_id", "pitch_order", "inning", "stage", "role",
    "state_key", "raw_out_index", "p_hat", "stake_G"
  )
  stop_if_missing_columns(x, required, "MDP opportunities")
  data.table::setorder(x, game_pk, team_id, pitch_order)
  x[, `:=`(
    next_state_key = data.table::shift(state_key, type = "lead"),
    next_raw_out_index = data.table::shift(raw_out_index, type = "lead"),
    next_inning = data.table::shift(inning, type = "lead"),
    next_role = data.table::shift(role, type = "lead")
  ), by = .(game_pk, team_id)]
  x[, terminal := is.na(next_state_key)]
  x[, delta_raw_outs := next_raw_out_index - raw_out_index]
  if (any(x[terminal == FALSE, delta_raw_outs < 0], na.rm = TRUE)) {
    stop("MDP decision transitions move backward in game time")
  }
  x[, extra_grant := !terminal & next_inning >= 10L & next_inning > inning]
  x[, parent_key := mdp_parent_key(stage, role)]
  x[]
}

mdp_transition_mean <- function(
  rows, current_stage, inventory, arrival_values, state_index, relative = FALSE
) {
  if (!nrow(rows)) return(0)
  value <- numeric(nrow(rows))
  live <- which(!rows$terminal)
  if (length(live)) {
    if (relative) {
      target_raw <- as.integer(current_stage) + rows$delta_raw_outs[live]
      target_stage <- collapse_mdp_stage(target_raw)
      target_key <- mdp_state_key(target_stage, rows$next_role[live])
      current_inning <- as.integer(current_stage) %/% 6L + 1L
      target_inning <- target_raw %/% 6L + 1L
      grant <- target_inning >= 10L & target_inning > current_inning
    } else {
      target_key <- rows$next_state_key[live]
      grant <- rows$extra_grant[live]
    }
    target_parts <- data.table::tstrsplit(target_key, "|", fixed = TRUE)
    target_index <- resolve_mdp_state_index(
      as.integer(target_parts[[1L]]), target_parts[[2L]],
      attr(state_index, "states"), state_index
    )
    next_inventory <- rep.int(as.integer(inventory), length(live))
    next_inventory[grant & next_inventory == 0L] <- 1L
    value[live] <- arrival_values[cbind(target_index, next_inventory + 1L)]
  }
  mean(value)
}

mdp_bellman_operator <- function(fit_data, arrival_values) {
  states <- fit_data$states
  continuation <- matrix(
    0, nrow = nrow(states), ncol = 3L,
    dimnames = list(states$state_key, as.character(0:2))
  )
  updated <- continuation
  for (i in seq_len(nrow(states))) {
    exact <- fit_data$exact_rows[[i]]
    parent <- fit_data$parent_rows[[i]]
    weight <- states$shrink_weight[[i]]
    for (k in 0:2) {
      exact_c <- mdp_transition_mean(
        exact, states$stage[[i]], k, arrival_values, fit_data$state_index,
        relative = FALSE
      )
      parent_c <- mdp_transition_mean(
        parent, states$stage[[i]], k, arrival_values, fit_data$state_index,
        relative = TRUE
      )
      continuation[i, k + 1L] <- weight * exact_c + (1 - weight) * parent_c
    }

    opportunity_value <- function(rows, k) {
      if (!nrow(rows) || k == 0L) return(continuation[i, k + 1L])
      wait <- continuation[i, k + 1L]
      lower <- continuation[i, k]
      challenge <- rows$p_hat * rows$stake_G + rows$p_hat * wait +
        (1 - rows$p_hat) * lower
      mean(pmax(wait, challenge))
    }
    for (k in 0:2) {
      exact_v <- opportunity_value(exact, k)
      parent_v <- opportunity_value(parent, k)
      updated[i, k + 1L] <- weight * exact_v + (1 - weight) * parent_v
    }
  }
  list(arrival = updated, continuation = continuation)
}

fit_challenge_mdp <- function(
  opportunities, prior_n = 30, tol = 1e-10, max_iter = 10000L
) {
  x <- add_mdp_transitions(opportunities)
  if (!nrow(x)) stop("Cannot fit challenge MDP without opportunities")
  if (any(!is.finite(x$p_hat)) || any(!data.table::between(x$p_hat, 0, 1))) {
    stop("MDP opportunities contain invalid p_hat values")
  }
  if (any(!is.finite(x$stake_G))) {
    stop("MDP opportunities contain non-finite stake_G values")
  }
  prior_n <- as.numeric(prior_n)
  if (!is.finite(prior_n) || prior_n < 0) stop("prior_n must be nonnegative")

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
    n = lengths(exact_rows),
    parent_n = lengths(parent_rows)
  )]
  if (any(states$parent_n == 0L)) {
    missing <- states[parent_n == 0L, unique(parent_key)]
    stop("No empirical MDP parent observations for: ", paste(missing, collapse = ", "))
  }
  states[, shrink_weight := ifelse(
    prior_n == 0 & n > 0L, 1,
    ifelse(prior_n == 0, 0, n / (n + prior_n))
  )]

  fit_data <- list(
    states = states,
    exact_rows = exact_rows,
    parent_rows = parent_rows,
    state_index = state_index
  )
  arrival <- matrix(
    0, nrow = nrow(states), ncol = 3L,
    dimnames = list(states$state_key, as.character(0:2))
  )
  residual <- Inf
  iteration <- 0L
  while (iteration < max_iter && residual > tol) {
    iteration <- iteration + 1L
    step <- mdp_bellman_operator(fit_data, arrival)
    residual <- max(abs(step$arrival - arrival))
    arrival <- step$arrival
  }
  if (!is.finite(residual) || residual > tol) {
    stop(sprintf(
      "Challenge MDP did not converge after %s iterations (residual %.3g)",
      iteration, residual
    ))
  }
  final <- mdp_bellman_operator(fit_data, arrival)
  bellman_residual <- max(abs(final$arrival - arrival))
  continuation <- final$continuation
  marginal_1 <- continuation[, "1"] - continuation[, "0"]
  marginal_2 <- continuation[, "2"] - continuation[, "1"]
  marginal_1[abs(marginal_1) < 1e-12] <- 0
  marginal_2[abs(marginal_2) < 1e-12] <- 0
  if (any(marginal_1 < -1e-8) || any(marginal_2 < -1e-8)) {
    stop("Challenge MDP produced a materially negative inventory value")
  }
  marginal_1 <- pmax(marginal_1, 0)
  marginal_2 <- pmax(marginal_2, 0)

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
    state_values = values,
    arrival = arrival,
    continuation = continuation,
    states = states,
    state_index = state_index,
    residual = bellman_residual,
    iterations = iteration,
    prior_n = prior_n,
    training_games = sort(unique(x$game_pk))
  )
  class(fit) <- "abs_challenge_mdp"
  fit
}

mdp_action <- function(fit, stage, role, inventory, p_hat, stake_G) {
  if (!inherits(fit, "abs_challenge_mdp")) stop("fit must be an abs_challenge_mdp")
  n <- max(length(stage), length(role), length(inventory), length(p_hat), length(stake_G))
  stage <- rep_len(collapse_mdp_stage(stage), n)
  role <- rep_len(as.character(role), n)
  inventory <- rep_len(as.integer(inventory), n)
  p_hat <- rep_len(as.numeric(p_hat), n)
  stake_G <- rep_len(as.numeric(stake_G), n)
  if (any(!inventory %in% 0:2)) stop("inventory must be 0, 1, or 2")
  if (any(!is.finite(p_hat)) || any(!data.table::between(p_hat, 0, 1))) {
    stop("p_hat must be finite and between 0 and 1")
  }
  if (any(!is.finite(stake_G))) stop("stake_G must be finite")
  key <- mdp_state_key(stage, role)
  index <- resolve_mdp_state_index(stage, role, fit$states, fit$state_index)
  continuation <- fit$continuation[cbind(index, inventory + 1L)]
  lower_inventory <- pmax(inventory - 1L, 0L)
  lower <- fit$continuation[cbind(index, lower_inventory + 1L)]
  marginal <- continuation - lower
  marginal[inventory == 0L] <- NA_real_
  advantage <- p_hat * stake_G - (1 - p_hat) * marginal
  advantage[inventory == 0L] <- NA_real_
  threshold <- ifelse(
    stake_G >= 0 & stake_G + marginal > 0,
    marginal / (stake_G + marginal),
    NA_real_
  )
  threshold[!is.finite(threshold) | inventory == 0L] <- NA_real_
  action <- inventory > 0L & !is.na(advantage) & advantage > 0
  data.table::data.table(
    stage = stage,
    role = role,
    inventory = inventory,
    p_hat = p_hat,
    stake_G = stake_G,
    continuation_re = continuation,
    lower_continuation_re = lower,
    marginal_re = marginal,
    p_threshold = threshold,
    challenge_advantage_re = advantage,
    recommended_action = ifelse(action, "challenge", "wait")
  )
}

replay_challenge_policy <- function(
  opportunities, fit = NULL,
  policy = c("mdp", "observed", "static_ev", "positive_ev", "never", "oracle"),
  initial_inventory = 2L, static_threshold = 0.05
) {
  policy <- match.arg(policy)
  x <- data.table::copy(data.table::as.data.table(opportunities))
  required <- c(
    "game_pk", "team_id", "pitch_order", "inning", "stage", "role",
    "p_hat", "stake_G", "actual_wrong"
  )
  stop_if_missing_columns(x, required, "MDP replay opportunities")
  if (policy == "mdp" && is.null(fit)) stop("MDP replay requires a fitted policy")
  if (anyNA(x$actual_wrong)) stop("MDP replay requires known actual_wrong outcomes")
  data.table::setorder(x, game_pk, team_id, pitch_order)
  n_rows <- nrow(x)
  inventory_before_output <- rep.int(NA_integer_, n_rows)
  inventory_after_output <- rep.int(NA_integer_, n_rows)
  challenge_output <- rep.int(FALSE, n_rows)
  success_output <- rep.int(FALSE, n_rows)
  captured_re_output <- numeric(n_rows)
  marginal_re_output <- rep.int(NA_real_, n_rows)
  threshold_output <- rep.int(NA_real_, n_rows)
  advantage_output <- rep.int(NA_real_, n_rows)

  # Calling mdp_action() once per pitch is very expensive because it repeatedly
  # builds state keys and small data tables. Compute all three possible
  # inventory decisions in batches, then select the appropriate precomputed
  # answer while replaying each game sequentially.
  if (policy == "mdp") {
    batched_decisions <- lapply(0:2, function(k) {
      mdp_action(
        fit = fit,
        stage = x$stage,
        role = x$role,
        inventory = k,
        p_hat = x$p_hat,
        stake_G = x$stake_G
      )
    })
    action_by_inventory <- do.call(cbind, lapply(
      batched_decisions,
      function(z) z$recommended_action == "challenge"
    ))
    marginal_by_inventory <- do.call(cbind, lapply(
      batched_decisions,
      function(z) z$marginal_re
    ))
    threshold_by_inventory <- do.call(cbind, lapply(
      batched_decisions,
      function(z) z$p_threshold
    ))
    advantage_by_inventory <- do.call(cbind, lapply(
      batched_decisions,
      function(z) z$challenge_advantage_re
    ))
  }

  groups <- split(seq_len(nrow(x)), interaction(x$game_pk, x$team_id, drop = TRUE))
  for (indices in groups) {
    inventory <- as.integer(initial_inventory)
    previous_inning <- NA_integer_
    for (i in indices) {
      inning_now <- as.integer(x$inning[[i]])
      if (!is.na(previous_inning) && inning_now > previous_inning && inning_now > 9L &&
          inventory == 0L) {
        inventory <- 1L
      }
      inventory_before_output[[i]] <- inventory
      action <- FALSE
      if (policy == "mdp") {
        inventory_column <- inventory + 1L
        action <- action_by_inventory[i, inventory_column]
        marginal_re_output[[i]] <- marginal_by_inventory[i, inventory_column]
        threshold_output[[i]] <- threshold_by_inventory[i, inventory_column]
        advantage_output[[i]] <- advantage_by_inventory[i, inventory_column]
      } else if (policy == "observed") {
        action <- isTRUE(x$challenge_occurred[[i]]) &&
          (is.na(x$challenger_team_id[[i]]) || x$challenger_team_id[[i]] == x$team_id[[i]])
        if (action && inventory == 0L) stop("Observed challenge occurs at zero policy inventory")
      } else if (policy == "static_ev") {
        action <- inventory > 0L && x$p_hat[[i]] * x$stake_G[[i]] > static_threshold
      } else if (policy == "positive_ev") {
        action <- inventory > 0L && x$p_hat[[i]] * x$stake_G[[i]] > 0
      } else if (policy == "oracle") {
        action <- inventory > 0L && isTRUE(x$actual_wrong[[i]]) && x$stake_G[[i]] > 0
      }
      action <- isTRUE(action) && inventory > 0L
      success <- action && isTRUE(x$actual_wrong[[i]])
      if (success) captured_re_output[[i]] <- x$stake_G[[i]]
      if (action && !success) inventory <- inventory - 1L
      if (inventory < 0L) stop("Policy replay produced negative inventory")
      challenge_output[[i]] <- action
      success_output[[i]] <- success
      inventory_after_output[[i]] <- inventory
      previous_inning <- inning_now
    }
  }
  x[, `:=`(
    policy = policy,
    inventory_before_policy = inventory_before_output,
    inventory_after_policy = inventory_after_output,
    policy_challenge = challenge_output,
    policy_success = success_output,
    captured_re = captured_re_output,
    marginal_re = marginal_re_output,
    p_threshold = threshold_output,
    challenge_advantage_re = advantage_output
  )]
  x[]
}

summarize_mdp_evaluation <- function(replays, reps = 500L, seed = 42L) {
  x <- data.table::as.data.table(replays)
  team_game <- x[, .(
    captured_re = sum(captured_re),
    attempts = sum(policy_challenge),
    successes = sum(policy_success),
    opportunities = .N,
    zero_inventory_rate = mean(inventory_before_policy == 0L),
    exhausted = any(inventory_before_policy == 0L)
  ), by = .(policy, game_pk, team_id)]
  summary <- team_game[, .(
    total_re = sum(captured_re),
    mean_re_team_game = mean(captured_re),
    attempts = sum(attempts),
    successes = sum(successes),
    success_rate = if (sum(attempts)) sum(successes) / sum(attempts) else NA_real_,
    zero_inventory_rate = mean(zero_inventory_rate),
    exhaustion_rate = mean(exhausted)
  ), by = policy]
  observed_mean <- summary[policy == "observed", mean_re_team_game]
  if (!length(observed_mean)) observed_mean <- NA_real_
  summary[, difference_vs_observed := mean_re_team_game - observed_mean]

  reps <- as.integer(reps)
  if (reps <= 0L) {
    summary[, `:=`(
      mean_re_lower = NA_real_, mean_re_upper = NA_real_,
      difference_lower = NA_real_, difference_upper = NA_real_
    )]
    return(list(summary = summary[], team_game = team_game[]))
  }
  games <- sort(unique(team_game$game_pk))
  set.seed(seed)
  draws <- vector("list", reps)
  for (b in seq_len(reps)) {
    sampled <- sample(games, length(games), replace = TRUE)
    weights <- data.table::data.table(game_pk = sampled)[, .(weight = .N), by = game_pk]
    draw <- merge(team_game, weights, by = "game_pk")
    draw <- draw[, .(
      mean_re_team_game = stats::weighted.mean(captured_re, weight)
    ), by = policy]
    draw[, replicate := b]
    observed <- draw[policy == "observed", mean_re_team_game]
    draw[, difference_vs_observed := mean_re_team_game - observed]
    draws[[b]] <- draw
  }
  draws <- data.table::rbindlist(draws)
  intervals <- draws[, .(
    mean_re_lower = stats::quantile(mean_re_team_game, 0.025, names = FALSE),
    mean_re_upper = stats::quantile(mean_re_team_game, 0.975, names = FALSE),
    difference_lower = stats::quantile(difference_vs_observed, 0.025, names = FALSE),
    difference_upper = stats::quantile(difference_vs_observed, 0.975, names = FALSE)
  ), by = policy]
  summary <- merge(summary, intervals, by = "policy", all.x = TRUE, sort = FALSE)
  list(summary = summary[], team_game = team_game[], bootstrap = draws[])
}

crossfit_challenge_mdp <- function(
  opportunities, folds = 5L, seed = 42L, prior_n = 30,
  tol = 1e-10, max_iter = 10000L, bootstrap_reps = 500L,
  static_threshold = 0.05
) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  if (anyNA(x$actual_wrong)) stop("Cross-fitted MDP evaluation requires actual outcomes")
  games <- sort(unique(x$game_pk))
  folds <- as.integer(folds)
  if (folds < 2L || length(games) < folds) stop("Cross-fitting requires at least two games per fold setup")
  set.seed(seed)
  shuffled <- sample(games)
  assignment <- data.table::data.table(
    game_pk = shuffled,
    fold = rep(seq_len(folds), length.out = length(shuffled))
  )
  x <- merge(x, assignment, by = "game_pk", all.x = TRUE, sort = FALSE)
  data.table::setorder(x, game_pk, team_id, pitch_order)

  policies <- c("mdp", "observed", "static_ev", "positive_ev", "never", "oracle")
  replay_parts <- vector("list", folds * length(policies))
  fits <- vector("list", folds)
  cursor <- 0L
  for (fold_id in seq_len(folds)) {
    training <- x[fold != fold_id]
    testing <- x[fold == fold_id]
    fit <- fit_challenge_mdp(training, prior_n = prior_n, tol = tol, max_iter = max_iter)
    if (length(intersect(fit$training_games, unique(testing$game_pk)))) {
      stop("Cross-fitted MDP contains held-out games in training")
    }
    fits[[fold_id]] <- fit
    for (policy in policies) {
      cursor <- cursor + 1L
      replay_parts[[cursor]] <- replay_challenge_policy(
        testing, fit = if (policy == "mdp") fit else NULL,
        policy = policy, static_threshold = static_threshold
      )
      replay_parts[[cursor]][, fold := fold_id]
    }
  }
  replays <- data.table::rbindlist(replay_parts, fill = TRUE)
  evaluation <- summarize_mdp_evaluation(replays, reps = bootstrap_reps, seed = seed)
  full_fit <- fit_challenge_mdp(x, prior_n = prior_n, tol = tol, max_iter = max_iter)
  list(
    full_fit = full_fit,
    fold_fits = fits,
    fold_assignment = assignment[order(game_pk)],
    pitch_decisions = replays[policy == "mdp"],
    replays = replays,
    evaluation = evaluation$summary,
    team_game = evaluation$team_game,
    bootstrap = evaluation$bootstrap %||% data.table::data.table(),
    coverage = attr(opportunities, "coverage")
  )
}
