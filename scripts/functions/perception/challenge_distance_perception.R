challenge_distance_role_levels <- function() c("batter", "catcher", "pitcher")

prepare_challenge_distance_data <- function(choices) {
  x <- data.table::copy(data.table::as.data.table(choices))
  stop_if_missing_columns(
    x,
    c("game_pk", "role", "player_id", "challenged", "adverse_margin"),
    "challenge-distance choices"
  )
  roles <- challenge_distance_role_levels()
  x <- x[
    challenged %in% TRUE & role %in% roles &
      !is.na(player_id) & is.finite(adverse_margin)
  ]
  if (!nrow(x)) stop("No challenged pitches remain for distance fitting")
  x[, `:=`(
    role = factor(as.character(role), levels = roles),
    player_id = as.character(player_id),
    player_role_id = paste(as.character(role), player_id, sep = ":")
  )]
  players <- x[, .(
    role = as.character(role[[1L]]),
    challenges = .N,
    observed_mean_margin = mean(adverse_margin),
    observed_sd_margin = if (.N > 1L) stats::sd(adverse_margin) else NA_real_
  ), by = player_role_id]
  data.table::setorder(players, role, player_role_id)
  players[, player_index := .I]
  players[, role_index := match(role, roles)]
  x[players, on = "player_role_id", `:=`(
    player_index = i.player_index,
    role_index = i.role_index
  )]
  data.table::setorder(x, game_pk, role, player_id)

  # Deliberately omit actual_wrong and every overturn/result field. This model
  # learns only the locations teams chose to challenge.
  stan_data <- list(
    N = nrow(x),
    P = nrow(players),
    R = length(roles),
    adverse_margin = as.numeric(x$adverse_margin),
    player = as.integer(x$player_index),
    role_of_player = as.integer(players$role_index)
  )
  list(data = stan_data, challenges = x, players = players, roles = roles)
}

default_challenge_distance_stan_file <- function() {
  root <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd()),
    error = function(e) getwd()
  )
  file.path(root, "scripts", "stan", "hierarchical_challenge_distance.stan")
}

fit_hierarchical_challenge_distance <- function(
  choices, chains = 2L, cores = chains, iter = 1200L,
  warmup = floor(iter / 2), seed = 42L, adapt_delta = 0.95,
  max_treedepth = 12L, stan_file = NULL, file = NULL,
  refresh = 100L, force_refit = FALSE
) {
  if (!is.null(file) && file.exists(file) && !isTRUE(force_refit)) {
    cached <- readRDS(file)
    if (inherits(cached, "challenge_distance_perception_fit")) return(cached)
  }
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("fit_hierarchical_challenge_distance() requires rstan")
  }
  if (is.null(stan_file)) stan_file <- default_challenge_distance_stan_file()
  if (!file.exists(stan_file)) stop("Challenge-distance Stan file not found: ", stan_file)
  bundle <- prepare_challenge_distance_data(choices)
  rstan::rstan_options(auto_write = TRUE)
  model <- rstan::stan_model(file = stan_file, auto_write = TRUE)
  stan_fit <- rstan::sampling(
    model,
    data = bundle$data,
    chains = as.integer(chains),
    cores = min(as.integer(cores), as.integer(chains)),
    iter = as.integer(iter),
    warmup = as.integer(warmup),
    seed = as.integer(seed),
    init = 0,
    refresh = as.integer(refresh),
    control = list(
      adapt_delta = as.numeric(adapt_delta),
      max_treedepth = as.integer(max_treedepth)
    )
  )
  fit <- list(
    stan_fit = stan_fit,
    player_table = bundle$players,
    roles = bundle$roles,
    training_games = sort(unique(as.character(choices$game_pk))),
    training_challenges = bundle$data$N,
    controls = list(
      chains = chains, cores = cores, iter = iter, warmup = warmup,
      seed = seed, adapt_delta = adapt_delta, max_treedepth = max_treedepth
    )
  )
  class(fit) <- "challenge_distance_perception_fit"
  if (!is.null(file)) saveRDS(fit, file)
  fit
}

challenge_distance_draws <- function(fit) {
  if (!inherits(fit, "challenge_distance_perception_fit")) {
    stop("fit must come from fit_hierarchical_challenge_distance()")
  }
  rstan::extract(
    fit$stan_fit,
    pars = c(
      "mu_role", "log_sigma_role", "tau_mu_role", "tau_log_sigma_role",
      "mu_player", "sigma_player"
    ),
    permuted = TRUE
  )
}

challenge_distance_interval <- function(x, prefix) {
  probs <- c(0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975)
  values <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  names(values) <- paste0(
    prefix,
    c("_lower_95", "_lower_80", "_lower_50", "_median",
      "_upper_50", "_upper_80", "_upper_95")
  )
  c(stats::setNames(mean(x), paste0(prefix, "_mean")), values)
}

summarize_challenge_distance_fit <- function(fit) {
  draws <- challenge_distance_draws(fit)
  roles <- fit$roles
  population <- data.table::rbindlist(lapply(seq_along(roles), function(r) {
    values <- c(
      challenge_distance_interval(draws$mu_role[, r], "location_mean_inches"),
      challenge_distance_interval(
        exp(draws$log_sigma_role[, r]), "within_player_sigma_inches"
      ),
      challenge_distance_interval(
        draws$tau_mu_role[, r], "between_player_mean_sd_inches"
      ),
      challenge_distance_interval(
        draws$tau_log_sigma_role[, r], "between_player_log_sigma_sd"
      ),
      challenge_distance_interval(
        sqrt(
          draws$tau_mu_role[, r]^2 +
            exp(
              2 * draws$log_sigma_role[, r] +
                2 * draws$tau_log_sigma_role[, r]^2
            )
        ),
        "marginal_sigma_inches"
      )
    )
    row <- data.table::data.table(role = roles[[r]])
    row[, (names(values)) := as.list(values)]
    row
  }), fill = TRUE)
  players <- data.table::rbindlist(lapply(seq_len(nrow(fit$player_table)), function(p) {
    values <- c(
      challenge_distance_interval(draws$mu_player[, p], "location_mean_inches"),
      challenge_distance_interval(draws$sigma_player[, p], "sigma_inches")
    )
    row <- data.table::copy(fit$player_table[p])
    row[, (names(values)) := as.list(values)]
    row
  }), fill = TRUE)
  marginal_sigma_draws <- vapply(seq_along(roles), function(r) {
    sqrt(
      draws$tau_mu_role[, r]^2 +
        exp(
          2 * draws$log_sigma_role[, r] +
            2 * draws$tau_log_sigma_role[, r]^2
        )
    )
  }, numeric(nrow(draws$mu_role)))
  ordering <- data.table::data.table(
    comparison = c("catcher < batter", "pitcher < batter"),
    posterior_probability = c(
      mean(marginal_sigma_draws[, 2L] < marginal_sigma_draws[, 1L]),
      mean(marginal_sigma_draws[, 3L] < marginal_sigma_draws[, 1L])
    )
  )
  list(population = population, players = players, ordering = ordering)
}

challenge_distance_diagnostics <- function(fit) {
  summary_matrix <- rstan::summary(fit$stan_fit)$summary
  sampler <- rstan::get_sampler_params(fit$stan_fit, inc_warmup = FALSE)
  data.table::data.table(
    max_rhat = max(summary_matrix[, "Rhat"], na.rm = TRUE),
    min_bulk_ess = min(summary_matrix[, "n_eff"], na.rm = TRUE),
    divergences = sum(vapply(
      sampler, function(x) sum(x[, "divergent__"]), numeric(1)
    ))
  )[, pass := max_rhat <= 1.01 & min_bulk_ess >= 400 & divergences == 0]
}

challenge_distance_observed_summary <- function(challenges, scope) {
  x <- data.table::as.data.table(challenges)
  x[, .(
    scope = scope,
    challenges = .N,
    mean_margin = mean(adverse_margin),
    sd_margin = if (.N > 1L) stats::sd(adverse_margin) else NA_real_,
    q10 = stats::quantile(adverse_margin, 0.10),
    q25 = stats::quantile(adverse_margin, 0.25),
    median = stats::median(adverse_margin),
    q75 = stats::quantile(adverse_margin, 0.75),
    q90 = stats::quantile(adverse_margin, 0.90),
    success_rate = if ("actual_wrong" %in% names(x)) mean(actual_wrong) else NA_real_
  ), by = role]
}

validate_challenge_distance_fit <- function(fit, heldout_choices) {
  heldout <- data.table::copy(data.table::as.data.table(heldout_choices))
  heldout <- heldout[
    challenged %in% TRUE & role %in% fit$roles & is.finite(adverse_margin)
  ]
  if (!nrow(heldout)) stop("No held-out challenges are available")
  if (length(intersect(
    fit$training_games, unique(as.character(heldout$game_pk))
  ))) stop("Challenge-distance training and held-out games overlap")

  observed <- challenge_distance_observed_summary(heldout, "heldout")
  population <- summarize_challenge_distance_fit(fit)$population
  comparison <- merge(
    observed,
    population[, .(
      role,
      fitted_mean = location_mean_inches_median,
      fitted_within_player_sigma = within_player_sigma_inches_median,
      fitted_between_player_mean_sd = between_player_mean_sd_inches_median,
      fitted_marginal_sd = marginal_sigma_inches_median
    )],
    by = "role", all.x = TRUE, sort = FALSE
  )
  comparison[, `:=`(
    mean_error_inches = fitted_mean - mean_margin,
    sd_error_inches = fitted_marginal_sd - sd_margin
  )]
  comparison[, mean_and_sd_within_half_inch :=
    abs(mean_error_inches) <= 0.5 & abs(sd_error_inches) <= 0.5]
  comparison[role == "pitcher", mean_and_sd_within_half_inch := NA]

  bins <- seq(-8, 8, by = 0.5)
  heldout[, distance_bin := cut(
    adverse_margin, breaks = c(-Inf, bins, Inf), include.lowest = TRUE
  )]
  histogram <- heldout[, .(observed_challenges = .N), by = .(role, distance_bin)]
  histogram[, observed_share := observed_challenges / sum(observed_challenges), by = role]
  list(comparison = comparison, histogram = histogram, heldout = heldout)
}

plot_challenge_distance_fit <- function(fit, heldout_choices) {
  validation <- validate_challenge_distance_fit(fit, heldout_choices)
  population <- summarize_challenge_distance_fit(fit)$population
  plot_data <- data.table::copy(validation$heldout)
  curve_data <- population[, {
    x <- seq(-7, 7, length.out = 300L)
    .(
      adverse_margin = x,
      density = stats::dnorm(
        x, mean = location_mean_inches_median, sd = marginal_sigma_inches_median
      )
    )
  }, by = role]
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = adverse_margin, y = ggplot2::after_stat(density), fill = role)
  ) +
    ggplot2::geom_histogram(binwidth = 0.5, alpha = 0.45, color = "white") +
    ggplot2::geom_line(
      data = curve_data,
      ggplot2::aes(x = adverse_margin, y = density, color = role),
      linewidth = 1.1,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_wrap(~role, scales = "free_y") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey35") +
    ggplot2::scale_fill_manual(values = c(
      batter = "#20AAB3", catcher = "#F45B5B", pitcher = "#7C6BC4"
    )) +
    ggplot2::scale_color_manual(values = c(
      batter = "#087D85", catcher = "#B52E2E", pitcher = "#51449A"
    )) +
    ggplot2::coord_cartesian(xlim = c(-7, 7)) +
    ggplot2::labs(
      title = "Where Teams Challenged, by Role",
      subtitle = "Held-out challenges; curves are hierarchical Gaussian fits from training games",
      x = "Distance relative to the ABS boundary (inches)",
      y = "Density",
      caption = "Positive means the challenge should succeed; negative means the umpire's call was correct."
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "none")
}

challenge_distance_sigma_table <- function(fit) {
  population <- summarize_challenge_distance_fit(fit)$population
  out <- population[
    role %in% c("batter", "catcher"),
    .(
      observer_role = role,
      sigma_inches = marginal_sigma_inches_median,
      sigma_lower_95 = marginal_sigma_inches_lower_95,
      sigma_upper_95 = marginal_sigma_inches_upper_95
    )
  ]
  if (!setequal(out$observer_role, c("batter", "catcher"))) {
    stop("Perception fit must contain batter and catcher population spreads")
  }
  if (any(!is.finite(out$sigma_inches)) || any(out$sigma_inches < 0)) {
    stop("Perception spreads must be finite and nonnegative")
  }
  out[]
}

score_challenge_distance_perception <- function(
  opportunities, sigma_table, spatial_scale, p_col = "p_hat"
) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  stop_if_missing_columns(
    x, c("game_pk", "pitch_order", "role", "initial_call", p_col),
    "challenge-distance MDP opportunities"
  )
  if (anyDuplicated(x[, .(game_pk, pitch_order)])) {
    stop("Challenge-distance MDP opportunities duplicate pitches")
  }
  if (any(
    (x$role == "offense" & x$initial_call != "called_strike") |
      (x$role == "defense" & x$initial_call != "ball")
  )) {
    stop("MDP role and adverse call direction do not agree")
  }
  sigma <- data.table::copy(data.table::as.data.table(sigma_table))
  scale <- data.table::copy(data.table::as.data.table(spatial_scale))
  stop_if_missing_columns(
    sigma, c("observer_role", "sigma_inches"), "challenge-distance sigma table"
  )
  stop_if_missing_columns(
    scale, c("initial_call", "spatial_scale"), "perception spatial-scale table"
  )
  if (anyDuplicated(sigma$observer_role)) stop("Sigma table duplicates observer roles")
  if (anyDuplicated(scale$initial_call)) stop("Spatial-scale table duplicates call types")
  if (any(!is.finite(sigma$sigma_inches)) || any(sigma$sigma_inches < 0)) {
    stop("All perception sigmas must be finite and nonnegative")
  }
  if (any(!is.finite(scale$spatial_scale)) || any(scale$spatial_scale <= 0)) {
    stop("All perception spatial scales must be finite and positive")
  }

  x[, row_id__ := .I]
  x[, observer_role := data.table::fcase(
    role == "offense", "batter",
    role == "defense", "catcher",
    default = NA_character_
  )]
  x <- merge(x, sigma, by = "observer_role", all.x = TRUE, sort = FALSE)
  x <- merge(x, scale, by = "initial_call", all.x = TRUE, sort = FALSE)
  data.table::setorder(x, row_id__)
  if (anyNA(x$observer_role) || anyNA(x$sigma_inches) || anyNA(x$spatial_scale)) {
    stop("Every MDP opportunity must map to one observer role and spatial scale")
  }
  x[, p_tracking := as.numeric(get(p_col))]
  if (any(!is.finite(x$p_tracking)) ||
      any(!data.table::between(x$p_tracking, 0, 1))) {
    stop("Tracking probabilities must be finite and in [0, 1]")
  }
  x[, p_human := perception_blur(
    p_tracking, sigma = sigma_inches, spatial_scale = spatial_scale
  )]
  if (any(!is.finite(x$p_human)) || any(!data.table::between(x$p_human, 0, 1))) {
    stop("Human-perception probabilities must be finite and in [0, 1]")
  }
  if (any(abs(x$p_human - 0.5) > abs(x$p_tracking - 0.5) + 1e-12)) {
    stop("Perception scoring moved a probability away from 50 percent")
  }
  x[, row_id__ := NULL]
  x[]
}

as_human_mdp_opportunities <- function(scored_opportunities) {
  x <- data.table::copy(data.table::as.data.table(scored_opportunities))
  stop_if_missing_columns(
    x, c("p_tracking", "p_human"), "scored human-perception opportunities"
  )
  x[, p_hat := p_human]
  x[]
}

summarize_human_mdp_comparison <- function(
  replays, bootstrap_reps = 500L, seed = 42L
) {
  x <- data.table::copy(data.table::as.data.table(replays))
  stop_if_missing_columns(
    x,
    c(
      "policy", "game_pk", "team_id", "role", "policy_challenge",
      "policy_success", "captured_re", "inventory_before_policy"
    ),
    "human-perception MDP replays"
  )
  required_policies <- c("tracking_mdp", "human_eyes_mdp", "observed", "never")
  if (!all(required_policies %in% unique(x$policy))) {
    stop("MDP comparison is missing one or more required policies")
  }
  evaluation <- summarize_mdp_evaluation(
    x, reps = as.integer(bootstrap_reps), seed = as.integer(seed)
  )
  role_evaluation <- x[, .(
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

  summary <- evaluation$summary
  total_lookup <- stats::setNames(summary$total_re, summary$policy)
  mean_lookup <- stats::setNames(summary$mean_re_team_game, summary$policy)
  decomposition <- data.table::data.table(
    component = c("perception_cost", "strategy_cost"),
    total_re = c(
      total_lookup[["tracking_mdp"]] - total_lookup[["human_eyes_mdp"]],
      total_lookup[["human_eyes_mdp"]] - total_lookup[["observed"]]
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
    if (all(required_policies[1:3] %in% names(wide))) {
      team_games <- data.table::uniqueN(
        evaluation$team_game[, .(game_pk, team_id)]
      )
      draws <- data.table::rbindlist(list(
        data.table::data.table(
          component = "perception_cost",
          value = (wide$tracking_mdp - wide$human_eyes_mdp) * team_games
        ),
        data.table::data.table(
          component = "strategy_cost",
          value = (wide$human_eyes_mdp - wide$observed) * team_games
        )
      ))
      intervals <- draws[, .(
        lower_95 = stats::quantile(value, 0.025, names = FALSE),
        upper_95 = stats::quantile(value, 0.975, names = FALSE)
      ), by = component]
      decomposition <- merge(
        decomposition, intervals, by = "component", all.x = TRUE, sort = FALSE
      )
    }
  }
  list(
    evaluation = evaluation$summary,
    role_evaluation = role_evaluation,
    decomposition = decomposition,
    team_game = evaluation$team_game,
    bootstrap = evaluation$bootstrap %||% data.table::data.table()
  )
}

plot_human_mdp_ladder <- function(evaluation, subtitle = NULL) {
  x <- data.table::copy(data.table::as.data.table(evaluation))
  stop_if_missing_columns(x, c("policy", "total_re"), "human MDP evaluation")
  labels <- c(
    never = "Never challenge",
    observed = "Actual teams",
    human_eyes_mdp = "MDP with human perception",
    tracking_mdp = "MDP with tracking"
  )
  x <- x[policy %in% names(labels)]
  x[, policy_label := factor(
    labels[policy], levels = unname(labels[names(labels)])
  )]
  ggplot2::ggplot(x, ggplot2::aes(x = policy_label, y = total_re, fill = policy)) +
    ggplot2::geom_col(width = 0.68, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(total_re, accuracy = 0.1)),
      vjust = -0.4, fontface = "bold", size = 4
    ) +
    ggplot2::scale_fill_manual(values = c(
      never = "#B9BEC5", observed = "#F45B5B",
      human_eyes_mdp = "#20AAB3", tracking_mdp = "#087D85"
    )) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.10))
    ) +
    ggplot2::labs(
      title = "Run Expectancy Captured by Challenge Policy",
      subtitle = subtitle,
      x = NULL,
      y = "Total run expectancy captured"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 15, hjust = 1)
    )
}

fit_and_replay_human_mdp <- function(
  train_tracking, test_tracking, sigma_table, spatial_scale,
  prior_n = 30, tol = 1e-10, max_iter = 10000L,
  require_game_separation = TRUE
) {
  train <- data.table::copy(data.table::as.data.table(train_tracking))
  test <- data.table::copy(data.table::as.data.table(test_tracking))
  train_games <- unique(as.character(train$game_pk))
  test_games <- unique(as.character(test$game_pk))
  if (isTRUE(require_game_separation) && length(intersect(train_games, test_games))) {
    stop("Human-perception MDP training and testing games overlap")
  }
  train_scored <- score_challenge_distance_perception(
    train, sigma_table, spatial_scale
  )
  test_scored <- score_challenge_distance_perception(
    test, sigma_table, spatial_scale
  )
  train_human <- as_human_mdp_opportunities(train_scored)
  test_human <- as_human_mdp_opportunities(test_scored)

  tracking_fit <- fit_challenge_mdp(
    train_scored, prior_n = prior_n, tol = tol, max_iter = max_iter
  )
  human_fit <- fit_challenge_mdp(
    train_human, prior_n = prior_n, tol = tol, max_iter = max_iter
  )
  if (isTRUE(require_game_separation)) {
    if (length(intersect(as.character(tracking_fit$training_games), test_games)) ||
        length(intersect(as.character(human_fit$training_games), test_games))) {
      stop("An MDP fit contains held-out games")
    }
  }
  replays <- list(
    tracking_mdp = replay_challenge_policy(test_scored, tracking_fit, "mdp"),
    human_eyes_mdp = replay_challenge_policy(test_human, human_fit, "mdp"),
    observed = replay_challenge_policy(test_scored, policy = "observed"),
    never = replay_challenge_policy(test_scored, policy = "never")
  )
  replays <- data.table::rbindlist(lapply(names(replays), function(policy_name) {
    value <- replays[[policy_name]]
    value[, policy := policy_name]
    value
  }), fill = TRUE)
  if (any(replays$inventory_before_policy < 0L) ||
      any(replays$inventory_after_policy < 0L)) {
    stop("Human-perception MDP replay produced negative inventory")
  }
  list(
    tracking_fit = tracking_fit,
    human_fit = human_fit,
    replays = replays,
    train_scored = train_scored,
    test_scored = test_scored
  )
}
