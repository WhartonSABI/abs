continuous_decision_outcome_columns <- function() {
  c(
    "abs_call", "final_call", "is_overturned", "challenge_outcome",
    "official_abs_call", "official_outcome", "actual_wrong"
  )
}

continuous_decision_feature_columns <- function() {
  c(
    "game_pk", "pitch_order", "at_bat_number", "pitch_number",
    "batter_id", "pitcher_id", "bat_team_id", "initial_call", "challenged",
    "stake_G", "inventory_loss", "sampling_offset", "inning", "outs_before",
    "balls_before", "strikes_before", "adverse_challenges_before",
    "score_margin",
    "plate_x", "plate_z", "sz_top", "sz_bot", "edge_distance_inches",
    "pitch_type", "pitch_family", "matchup", "stand", "p_throws",
    "umpire_id", "catcher_id"
  )
}

prepare_continuous_decision_rows <- function(rows) {
  x <- data.table::copy(data.table::as.data.table(rows))
  required <- c(
    "game_pk", "pitch_order", "batter_id", "bat_team_id", "initial_call",
    "challenged", "stake_G", "inventory_loss"
  )
  stop_if_missing_columns(x, required, "continuous human-decision rows")
  x <- x[
    initial_call == "called_strike" & !is.na(batter_id) &
      challenged %in% 0:1 & is.finite(stake_G) & stake_G >= 0 &
      is.finite(inventory_loss) & inventory_loss >= 0
  ]
  if (!nrow(x)) stop("No eligible batter decision rows remain")
  if (!"sampling_offset" %in% names(x)) x[, sampling_offset := 0]
  if (any(!is.finite(x$sampling_offset))) {
    stop("Decision sampling offsets must be finite")
  }
  optional <- setdiff(continuous_decision_feature_columns(), required)
  numeric_optional <- c(
    "sampling_offset", "inning", "outs_before", "adverse_challenges_before",
    "score_margin"
  )
  for (column in setdiff(optional, names(x))) {
    value <- if (column %in% numeric_optional) 0 else NA_character_
    x[, (column) := value]
  }
  retained_columns <- continuous_decision_feature_columns()
  out <- x[, ..retained_columns]
  out[, `:=`(
    batter_id = as.character(batter_id),
    bat_team_id = as.character(bat_team_id),
    challenged = as.integer(challenged)
  )]
  data.table::setorder(out, game_pk, pitch_order)
  if (anyDuplicated(out[, .(game_pk, pitch_order)])) {
    stop("Continuous decision rows contain duplicate pitch keys")
  }
  out[]
}

challenge_expected_utility <- function(q, gain, inventory_loss) {
  q <- as.numeric(q)
  gain <- rep_len(as.numeric(gain), length(q))
  inventory_loss <- rep_len(as.numeric(inventory_loss), length(q))
  if (any(!is.finite(q)) || any(q < 0 | q > 1)) {
    stop("q must contain probabilities")
  }
  if (any(!is.finite(gain)) || any(gain < 0) ||
      any(!is.finite(inventory_loss)) || any(inventory_loss < 0)) {
    stop("Challenge gains and inventory losses must be finite and nonnegative")
  }
  q * gain - (1 - q) * inventory_loss
}

normalize_signal_weights <- function(weights, tolerance = 1e-10) {
  weights <- as.matrix(weights)
  if (any(!is.finite(weights)) || any(weights < 0)) {
    stop("Signal weights must be finite and nonnegative")
  }
  totals <- rowSums(weights)
  if (any(totals <= tolerance)) stop("Every pitch needs positive signal mass")
  weights / totals
}

integrate_human_action <- function(
  q_signal, signal_weights, gain, inventory_loss, linear_predictor = 0,
  decision_slope = 1, sampling_offset = 0
) {
  q_signal <- as.matrix(q_signal)
  signal_weights <- normalize_signal_weights(signal_weights)
  if (!identical(dim(q_signal), dim(signal_weights))) {
    stop("q_signal and signal_weights must have identical dimensions")
  }
  if (any(!is.finite(q_signal)) || any(q_signal < 0 | q_signal > 1)) {
    stop("q_signal must contain probabilities")
  }
  n <- nrow(q_signal)
  gain <- rep_len(as.numeric(gain), n)
  inventory_loss <- rep_len(as.numeric(inventory_loss), n)
  linear_predictor <- rep_len(as.numeric(linear_predictor), n)
  sampling_offset <- rep_len(as.numeric(sampling_offset), n)
  decision_slope <- as.numeric(decision_slope)
  if (length(decision_slope) != 1L || !is.finite(decision_slope) ||
      decision_slope < 0) {
    stop("decision_slope must be one finite nonnegative number")
  }
  utility <- q_signal * gain - (1 - q_signal) * inventory_loss
  eta <- sweep(decision_slope * utility, 1L,
    linear_predictor + sampling_offset, "+")
  action_at_signal <- stats::plogis(eta)
  challenge_probability <- rowSums(signal_weights * action_at_signal)
  numerator <- rowSums(signal_weights * action_at_signal * q_signal)
  choice_conditioned_q <- ifelse(
    challenge_probability > 0,
    numerator / challenge_probability,
    NA_real_
  )
  data.table::data.table(
    challenge_probability = challenge_probability,
    choice_conditioned_q = choice_conditioned_q,
    marginal_signal_q = rowSums(signal_weights * q_signal)
  )
}

condition_signal_weights_on_take <- function(signal_weights, take_probability) {
  signal_weights <- as.matrix(signal_weights)
  take_probability <- as.matrix(take_probability)
  if (!identical(dim(signal_weights), dim(take_probability))) {
    stop("signal_weights and take_probability must have identical dimensions")
  }
  if (any(!is.finite(take_probability)) ||
      any(take_probability < 0 | take_probability > 1)) {
    stop("take_probability must contain probabilities")
  }
  normalize_signal_weights(signal_weights * take_probability)
}

continuous_call_trust_candidates <- function() c(0, 0.5, 1)

bernoulli_log_loss <- function(observed, probability, eps = 1e-9) {
  observed <- as.integer(observed)
  probability <- pmin(1 - eps, pmax(eps, as.numeric(probability)))
  if (length(observed) != length(probability) || any(!observed %in% 0:1)) {
    stop("Observed choices and probabilities are invalid")
  }
  -(observed * log(probability) + (1 - observed) * log1p(-probability))
}

game_clustered_choice_score <- function(rows, probability, omega) {
  x <- data.table::copy(data.table::as.data.table(rows))
  stop_if_missing_columns(x, c("game_pk", "challenged"), "choice scoring rows")
  if (length(probability) != nrow(x)) stop("Probability length does not match rows")
  x[, loss__ := bernoulli_log_loss(challenged, probability)]
  by_game <- x[, .(log_loss = mean(loss__), pitches = .N), by = game_pk]
  data.table::data.table(
    omega = as.numeric(omega),
    log_loss = mean(x$loss__),
    game_se = stats::sd(by_game$log_loss) / sqrt(nrow(by_game)),
    games = nrow(by_game),
    pitches = nrow(x)
  )
}

stage_continuous_call_trust <- function(
  scores, correlation = NA_real_, interval = c(NA_real_, NA_real_),
  minimum_se_improvement = 1, maximum_correlation = 0.8,
  maximum_interval_width = 0.75
) {
  x <- data.table::copy(data.table::as.data.table(scores))
  stop_if_missing_columns(x, c("omega", "log_loss", "game_se"), "call-trust scores")
  baseline <- x[omega == 0]
  informed <- x[omega > 0][order(log_loss)]
  if (nrow(baseline) != 1L || !nrow(informed)) {
    stop("Call-trust staging needs one omega=0 score and at least one informed score")
  }
  best <- informed[1L]
  improvement <- baseline$log_loss - best$log_loss
  comparison_se <- sqrt(baseline$game_se^2 + best$game_se^2)
  fixed_pass <- is.finite(improvement) && comparison_se > 0 &&
    improvement >= minimum_se_improvement * comparison_se
  identifiable <- length(interval) == 2L && all(is.finite(interval)) &&
    diff(range(interval)) <= maximum_interval_width &&
    is.finite(correlation) && abs(correlation) < maximum_correlation
  data.table::data.table(
    baseline_omega = 0,
    best_fixed_omega = best$omega,
    log_loss_improvement = improvement,
    comparison_se = comparison_se,
    fixed_call_update_pass = fixed_pass,
    estimated_trust_identifiable = identifiable,
    promote_estimated_call_update = fixed_pass && identifiable,
    primary_omega = if (fixed_pass && identifiable) best$omega else 0,
    reason = if (!fixed_pass) {
      "call cue did not improve held-out choice prediction by one clustered SE"
    } else if (!identifiable) {
      "shared call trust was not separately identified"
    } else {
      "call cue and shared trust passed"
    }
  )
}

validate_global_draw_map <- function(draw_map) {
  x <- data.table::as.data.table(draw_map)
  stop_if_missing_columns(
    x, c("global_draw_id", "signal_draw_id", "prior_draw_id", "call_draw_id"),
    "continuous posterior draw map"
  )
  if (anyDuplicated(x$global_draw_id) || anyNA(x)) {
    stop("Global draw map must have one complete row per global draw")
  }
  invisible(TRUE)
}

make_global_draw_map <- function(
  signal_draws, prior_draws, call_draws, ndraws = NULL, seed = 20260825L
) {
  available <- min(signal_draws, prior_draws, call_draws)
  if (is.null(ndraws)) ndraws <- available
  ndraws <- as.integer(ndraws)
  if (ndraws < 1L || ndraws > available) stop("Invalid global draw count")
  set.seed(seed)
  out <- data.table::data.table(
    global_draw_id = seq_len(ndraws),
    signal_draw_id = sample.int(signal_draws, ndraws),
    prior_draw_id = sample.int(prior_draws, ndraws),
    call_draw_id = sample.int(call_draws, ndraws)
  )
  validate_global_draw_map(out)
  out[]
}

continuous_decision_stan_path <- function() {
  root <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd()),
    error = function(e) getwd()
  )
  file.path(root, "scripts", "stan", "continuous_human_decision.stan")
}

prepare_continuous_decision_stan_data <- function(
  rows, q_signal, signal_weights, context_matrix = NULL
) {
  x <- prepare_continuous_decision_rows(rows)
  q_signal <- as.matrix(q_signal)
  signal_weights <- normalize_signal_weights(signal_weights)
  if (nrow(q_signal) != nrow(x) || !identical(dim(q_signal), dim(signal_weights))) {
    stop("Decision quadrature does not align to prepared rows")
  }
  players <- sort(unique(x$batter_id))
  teams <- sort(unique(x$bat_team_id))
  if (is.null(context_matrix)) context_matrix <- matrix(0, nrow(x), 0L)
  context_matrix <- as.matrix(context_matrix)
  if (nrow(context_matrix) != nrow(x)) stop("Decision context does not align")
  list(
    data = list(
      N = nrow(x), Q = ncol(q_signal), P = length(players), T = length(teams),
      K = ncol(context_matrix), challenged = x$challenged,
      player = match(x$batter_id, players), team = match(x$bat_team_id, teams),
      gain = x$stake_G, inventory_loss = x$inventory_loss,
      q_signal = q_signal, signal_weight = signal_weights,
      X = context_matrix, sampling_offset = x$sampling_offset
    ),
    rows = x,
    players = players,
    teams = teams
  )
}

fit_continuous_human_decision <- function(
  rows, q_signal, signal_weights, context_matrix = NULL,
  chains = 4L, parallel_chains = chains, iter_warmup = 1000L,
  iter_sampling = 1000L, seed = 20260825L, model = NULL,
  output_dir = NULL, refresh = 0L
) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("fit_continuous_human_decision() requires cmdstanr")
  }
  bundle <- prepare_continuous_decision_stan_data(
    rows, q_signal, signal_weights, context_matrix
  )
  if (is.null(model)) {
    model <- cmdstanr::cmdstan_model(
      continuous_decision_stan_path(), dir = continuous_stan_compile_dir()
    )
  }
  fit <- model$sample(
    data = bundle$data, chains = chains, parallel_chains = parallel_chains,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling, seed = seed,
    output_dir = output_dir, refresh = refresh
  )
  structure(c(bundle, list(fit = fit, seed = seed)),
    class = "continuous_human_decision_fit")
}

continuous_decision_context_rows <- function(rows) {
  x <- prepare_continuous_decision_rows(rows)
  x[, `:=`(
    inning_group = data.table::fcase(
      as.integer(inning) <= 3L, "early",
      as.integer(inning) <= 6L, "middle",
      as.integer(inning) <= 9L, "late",
      default = "extras"
    ),
    outs_state = as.character(as.integer(outs_before)),
    count_state = paste0(as.integer(balls_before), "-", as.integer(strikes_before)),
    inventory_state = as.character(as.integer(adverse_challenges_before)),
    score_margin = as.numeric(score_margin),
    absolute_score_margin = abs(as.numeric(score_margin))
  )]
  x[]
}

fit_continuous_decision_context <- function(rows) {
  x <- continuous_decision_context_rows(rows)
  fit_continuous_design(
    x,
    categorical = c(
      "inning_group", "outs_state", "count_state", "inventory_state"
    ),
    numeric = c("score_margin", "absolute_score_margin")
  )
}

score_continuous_decision_context <- function(rows, specification) {
  score_continuous_design(
    continuous_decision_context_rows(rows), specification
  )
}

fit_continuous_human_decision_with_context <- function(
  rows, q_signal, signal_weights,
  chains = 4L, parallel_chains = chains, iter_warmup = 1000L,
  iter_sampling = 1000L, seed = 20260825L, model = NULL,
  output_dir = NULL, refresh = 0L
) {
  context <- fit_continuous_decision_context(rows)
  fit <- fit_continuous_human_decision(
    rows = rows,
    q_signal = q_signal,
    signal_weights = signal_weights,
    context_matrix = context$matrix,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    seed = seed,
    model = model,
    output_dir = output_dir,
    refresh = refresh
  )
  fit$context_specification <- context$specification
  fit
}

draws_continuous_human_decision <- function(
  fit, ndraws = NULL, seed = 20260825L
) {
  if (!inherits(fit, "continuous_human_decision_fit")) {
    stop("fit must be a continuous_human_decision_fit")
  }
  variables <- c(
    "mu_player", "tau_player", "alpha_player", "tau_team", "alpha_team",
    "decision_slope", "gamma"
  )
  draws <- continuous_thin_draws(
    continuous_draw_matrix(fit$fit, variables), ndraws, seed
  )
  list(
    draw_id = seq_len(nrow(draws)),
    mu_player = continuous_extract_scalar(draws, "mu_player"),
    tau_player = continuous_extract_scalar(draws, "tau_player"),
    alpha_player = continuous_extract_vector(
      draws, "alpha_player", length(fit$players)
    ),
    tau_team = continuous_extract_scalar(draws, "tau_team"),
    alpha_team = continuous_extract_vector(
      draws, "alpha_team", length(fit$teams)
    ),
    decision_slope = continuous_extract_scalar(draws, "decision_slope"),
    gamma = continuous_extract_vector(draws, "gamma", fit$data$K)
  )
}

score_continuous_human_decision <- function(
  fit, rows, q_signal, signal_weights, ndraws = 400L,
  seed = 20260825L, unseen = c("population_mean", "sample"),
  block_size = 250L, return_draws = FALSE
) {
  if (!inherits(fit, "continuous_human_decision_fit")) {
    stop("fit must be a continuous_human_decision_fit")
  }
  unseen <- match.arg(unseen)
  x <- prepare_continuous_decision_rows(rows)
  q_signal <- as.matrix(q_signal)
  signal_weights <- normalize_signal_weights(signal_weights)
  if (nrow(q_signal) != nrow(x) || !identical(dim(q_signal), dim(signal_weights))) {
    stop("Decision quadrature does not align to held-out rows")
  }
  context <- if (!is.null(fit$context_specification)) {
    score_continuous_decision_context(x, fit$context_specification)
  } else {
    matrix(0, nrow(x), fit$data$K)
  }
  if (ncol(context) != fit$data$K) stop("Decision context width changed")
  posterior <- draws_continuous_human_decision(fit, ndraws, seed)
  player <- match(x$batter_id, fit$players)
  team <- match(x$bat_team_id, fit$teams)
  unseen_players <- match(x$batter_id, sort(unique(x$batter_id[is.na(player)])))
  unseen_teams <- match(x$bat_team_id, sort(unique(x$bat_team_id[is.na(team)])))
  probability <- choice_q <- matrix(
    NA_real_, nrow(x), length(posterior$draw_id)
  )
  set.seed(seed + 1L)
  for (draw in seq_along(posterior$draw_id)) {
    player_effect <- rep(posterior$mu_player[[draw]], nrow(x))
    team_effect <- numeric(nrow(x))
    seen_player <- !is.na(player)
    seen_team <- !is.na(team)
    player_effect[seen_player] <- posterior$alpha_player[
      draw, player[seen_player]
    ]
    team_effect[seen_team] <- posterior$alpha_team[draw, team[seen_team]]
    if (unseen == "sample") {
      if (any(!seen_player)) {
        sampled <- stats::rnorm(
          max(unseen_players, na.rm = TRUE),
          posterior$mu_player[[draw]], posterior$tau_player[[draw]]
        )
        player_effect[!seen_player] <- sampled[unseen_players[!seen_player]]
      }
      if (any(!seen_team)) {
        sampled <- stats::rnorm(
          max(unseen_teams, na.rm = TRUE), 0, posterior$tau_team[[draw]]
        )
        team_effect[!seen_team] <- sampled[unseen_teams[!seen_team]]
      }
    }
    base <- player_effect + team_effect + x$sampling_offset
    if (ncol(context)) {
      base <- base + as.numeric(context %*% posterior$gamma[draw, ])
    }
    for (start in seq.int(1L, nrow(x), by = as.integer(block_size))) {
      rows_block <- start:min(nrow(x), start + as.integer(block_size) - 1L)
      utility <- q_signal[rows_block, , drop = FALSE] * x$stake_G[rows_block] -
        (1 - q_signal[rows_block, , drop = FALSE]) *
          x$inventory_loss[rows_block]
      action <- stats::plogis(sweep(
        posterior$decision_slope[[draw]] * utility,
        1L, base[rows_block], "+"
      ))
      p <- rowSums(signal_weights[rows_block, , drop = FALSE] * action)
      probability[rows_block, draw] <- p
      choice_q[rows_block, draw] <- rowSums(
        signal_weights[rows_block, , drop = FALSE] * action *
          q_signal[rows_block, , drop = FALSE]
      ) / pmax(p, 1e-12)
    }
  }
  result <- cbind(
    data.table::copy(x),
    continuous_probability_summary(probability, "p_challenge_"),
    continuous_probability_summary(choice_q, "q_chosen_")
  )
  result[, `:=`(
    q_signal_mean = rowSums(signal_weights * q_signal),
    batter_decision_seen_in_training = !is.na(player),
    team_decision_seen_in_training = !is.na(team)
  )]
  if (isTRUE(return_draws)) {
    return(list(
      summary = result[], draw_id = posterior$draw_id,
      p_challenge = probability, q_chosen = choice_q
    ))
  }
  result[]
}
