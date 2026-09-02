swing_take_descriptions <- function() {
  list(
    swing = c(
      "swinging_strike", "swinging_strike_blocked", "foul", "foul_tip",
      "hit_into_play"
    ),
    take = c("ball", "called_strike", "blocked_ball")
  )
}

classify_swing_take <- function(description) {
  groups <- swing_take_descriptions()
  description <- as.character(description)
  data.table::fcase(
    description %in% groups$swing, 1L,
    description %in% groups$take, 0L,
    default = NA_integer_
  )
}

swing_take_pitch_family <- function(pitch_type) {
  pitch_type <- as.character(pitch_type)
  data.table::fcase(
    pitch_type %in% c("FF", "FA", "SI", "FC"), "fastball",
    pitch_type %in% c("SL", "ST", "SV", "CU", "KC", "CS"), "breaking",
    pitch_type %in% c("CH", "FS", "FO", "SC"), "offspeed",
    default = "other"
  )
}

read_swing_take_statcast <- function(paths) {
  paths <- sort(unique(as.character(paths)))
  if (!length(paths) || any(!file.exists(paths))) {
    stop("Swing/take Statcast paths are missing")
  }
  wanted <- c(
    "game_pk", "game_date", "game_type", "at_bat_number", "pitch_number",
    "batter", "pitcher", "description", "pitch_type", "release_speed",
    "balls", "strikes", "outs_when_up", "inning", "inning_topbot",
    "plate_x", "plate_z", "sz_top", "sz_bot", "stand", "p_throws"
  )
  pieces <- lapply(paths, function(path) {
    available <- names(data.table::fread(path, nrows = 0L, showProgress = FALSE))
    data.table::fread(
      path,
      select = intersect(wanted, available),
      na.strings = c("", "NA", "null"),
      showProgress = FALSE
    )
  })
  x <- data.table::rbindlist(pieces, fill = TRUE, use.names = TRUE)
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "at_bat_number", "pitch_number", "batter", "description",
      "balls", "strikes", "plate_x", "plate_z", "sz_top", "sz_bot"
    ),
    "swing/take Statcast input"
  )
  if ("game_date" %in% names(x)) {
    x[, game_date := data.table::as.IDate(game_date)]
  }
  x[]
}

prepare_swing_take_choices <- function(
  statcast, boundary_inches = 6, regular_season_only = TRUE
) {
  x <- data.table::copy(data.table::as.data.table(statcast))
  boundary_inches <- as.numeric(boundary_inches)
  if (!is.finite(boundary_inches) || boundary_inches <= 0) {
    stop("boundary_inches must be positive")
  }
  if (regular_season_only && "game_type" %in% names(x)) {
    x <- x[is.na(game_type) | game_type == "R"]
  }
  x[, swing := classify_swing_take(description)]
  x <- x[!is.na(swing)]
  x <- x[
    is.finite(plate_x) & is.finite(plate_z) &
      is.finite(sz_top) & is.finite(sz_bot)
  ]
  x[, edge_distance_inches := abs_edge_distance_inches(
    plate_x, plate_z, sz_top, sz_bot
  )]
  x <- x[
    is.finite(edge_distance_inches) &
      abs(edge_distance_inches) <= boundary_inches &
      balls %in% 0:3 & strikes %in% 0:2 & !is.na(batter)
  ]
  x[, `:=`(
    batter_id = as.character(batter),
    pitcher_id = as.character(pitcher),
    pitch_family = swing_take_pitch_family(pitch_type),
    count_state = paste0(balls, "-", strikes),
    matchup = paste0(
      data.table::fcoalesce(as.character(stand), "U"), "-",
      data.table::fcoalesce(as.character(p_throws), "U")
    ),
    game_key__ = as.character(game_pk)
  )]
  key <- intersect(c("game_pk", "at_bat_number", "pitch_number"), names(x))
  if (length(key) == 3L && anyDuplicated(x[, ..key])) {
    stop("Swing/take choices contain duplicate pitch keys")
  }
  data.table::setorder(x, game_pk, at_bat_number, pitch_number)
  x[]
}

deterministic_swing_take_split <- function(
  choices, train_fraction = 0.8, seed = 42L
) {
  x <- data.table::as.data.table(choices)
  games <- sort(unique(as.character(x$game_pk)))
  if (length(games) < 2L) stop("Swing/take split requires at least two games")
  train_fraction <- as.numeric(train_fraction)
  if (!is.finite(train_fraction) || train_fraction <= 0 || train_fraction >= 1) {
    stop("train_fraction must be between zero and one")
  }
  set.seed(seed)
  train_n <- max(1L, min(length(games) - 1L, floor(length(games) * train_fraction)))
  train_games <- sample(games, train_n)
  data.table::data.table(
    game_pk = games,
    split = ifelse(games %in% train_games, "train", "validation")
  )
}

fit_swing_take_feasibility <- function(train_choices) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("fit_swing_take_feasibility() requires mgcv")
  }
  x <- data.table::copy(data.table::as.data.table(train_choices))
  required <- c(
    "swing", "edge_distance_inches", "count_state", "pitch_family", "matchup"
  )
  stop_if_missing_columns(x, required, "swing/take training choices")
  x[, `:=`(
    count_state = factor(count_state),
    pitch_family = factor(pitch_family),
    matchup = factor(matchup)
  )]
  context_formula <- stats::as.formula(
    "swing ~ count_state + pitch_family + matchup"
  )
  edge_formula <- stats::as.formula(
    paste0(
      "swing ~ count_state + pitch_family + matchup + ",
      "s(edge_distance_inches, by = factor(strikes), k = 12, bs = 'cr')"
    )
  )
  context_fit <- mgcv::bam(
    context_formula,
    data = x,
    family = stats::binomial(),
    method = "fREML"
  )
  edge_fit <- mgcv::bam(
    edge_formula,
    data = x,
    family = stats::binomial(),
    method = "fREML",
    discrete = TRUE
  )
  list(context = context_fit, edge = edge_fit)
}

score_swing_take_feasibility <- function(fits, validation_choices) {
  x <- data.table::copy(data.table::as.data.table(validation_choices))
  for (column in c("count_state", "pitch_family", "matchup")) {
    training_levels <- levels(fits$edge$model[[column]])
    fallback <- names(sort(table(fits$edge$model[[column]]), decreasing = TRUE))[[1L]]
    value <- as.character(x[[column]])
    value[!value %in% training_levels] <- fallback
    x[, (column) := factor(value, levels = training_levels)]
  }
  x[, `:=`(
    prediction_context = as.numeric(stats::predict(
      fits$context, newdata = x, type = "response"
    )),
    prediction_edge = as.numeric(stats::predict(
      fits$edge, newdata = x, type = "response"
    ))
  )]
  for (column in c("prediction_context", "prediction_edge")) {
    x[, (column) := pmin(1 - 1e-9, pmax(1e-9, get(column)))]
  }
  x[]
}

swing_take_log_loss <- function(y, probability) {
  probability <- pmin(1 - 1e-9, pmax(1e-9, as.numeric(probability)))
  -mean(y * log(probability) + (1 - y) * log1p(-probability))
}

validate_swing_take_signal <- function(scored_choices, bin_width = 0.5) {
  x <- data.table::copy(data.table::as.data.table(scored_choices))
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "swing", "strikes", "edge_distance_inches",
      "prediction_context", "prediction_edge"
    ),
    "scored swing/take choices"
  )
  x[, `:=`(
    loss_context = -(
      swing * log(prediction_context) +
        (1 - swing) * log1p(-prediction_context)
    ),
    loss_edge = -(
      swing * log(prediction_edge) +
        (1 - swing) * log1p(-prediction_edge)
    )
  )]
  by_game <- x[, .(
    pitches = .N,
    context_log_loss = mean(loss_context),
    edge_log_loss = mean(loss_edge),
    difference = mean(loss_edge - loss_context)
  ), by = game_pk]
  difference_se <- stats::sd(by_game$difference) / sqrt(nrow(by_game))
  bins <- seq(-6, 6, by = bin_width)
  x[, edge_bin := cut(
    edge_distance_inches,
    breaks = bins,
    include.lowest = TRUE,
    right = FALSE
  )]
  midpoint_table <- data.table::data.table(
    edge_bin = levels(x$edge_bin),
    midpoint = head(bins, -1L) + bin_width / 2
  )
  curve <- x[!is.na(edge_bin), .(
    pitches = .N,
    swings = sum(swing),
    swing_rate = mean(swing),
    predicted_swing_rate = mean(prediction_edge)
  ), by = .(strikes, edge_bin)]
  curve <- merge(curve, midpoint_table, by = "edge_bin", all.x = TRUE)
  curve[, standard_error := sqrt(swing_rate * (1 - swing_rate) / pitches)]
  curve[, `:=`(
    lower_95 = pmax(0, swing_rate - 1.96 * standard_error),
    upper_95 = pmin(1, swing_rate + 1.96 * standard_error)
  )]
  data.table::setorder(curve, strikes, midpoint)
  central <- curve[midpoint >= -3 & midpoint <= 3 & pitches >= 100]
  monotonicity <- central[, .(
    populated_bins = .N,
    spearman_distance_swing = stats::cor(
      midpoint, swing_rate, method = "spearman"
    )
  ), by = strikes]
  metrics <- data.table::data.table(
    validation_pitches = nrow(x),
    validation_games = data.table::uniqueN(x$game_pk),
    context_log_loss = mean(x$loss_context),
    edge_log_loss = mean(x$loss_edge),
    paired_log_loss_difference = mean(x$loss_edge - x$loss_context),
    paired_log_loss_difference_se = difference_se,
    all_count_curves_decrease = all(
      monotonicity$spearman_distance_swing <= -0.8
    ),
    edge_improves_log_loss = mean(x$loss_edge - x$loss_context) < -difference_se,
    pass = all(monotonicity$spearman_distance_swing <= -0.8) &&
      mean(x$loss_edge - x$loss_context) < -difference_se
  )
  calibration <- x[, .(
    pitches = .N,
    predicted_swing_rate = mean(prediction_edge),
    actual_swing_rate = mean(swing)
  ), by = .(probability_bin = cut(
    prediction_edge,
    breaks = stats::quantile(
      prediction_edge, seq(0, 1, 0.1), na.rm = TRUE
    ) |> unique(),
    include.lowest = TRUE
  ))]
  list(
    metrics = metrics,
    by_game = by_game,
    curve = curve,
    monotonicity = monotonicity,
    calibration = calibration
  )
}

plot_swing_take_signal <- function(validation) {
  curve <- data.table::copy(validation$curve)
  curve[, strikes_label := factor(
    paste(strikes, ifelse(strikes == 1L, "strike", "strikes")),
    levels = c("0 strikes", "1 strike", "2 strikes")
  )]
  ggplot2::ggplot(
    curve,
    ggplot2::aes(x = midpoint, y = swing_rate)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower_95, ymax = upper_95),
      fill = "#18AEB7", alpha = 0.16
    ) +
    ggplot2::geom_line(color = "#18AEB7", linewidth = 1) +
    ggplot2::geom_point(
      ggplot2::aes(size = pitches), color = "#18AEB7", alpha = 0.85
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = predicted_swing_rate),
      color = "#F45B57", linewidth = 0.9, linetype = "dashed"
    ) +
    ggplot2::geom_vline(xintercept = 0, color = "grey45", linetype = "dotted") +
    ggplot2::facet_wrap(~strikes_label, ncol = 1) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::scale_x_continuous(breaks = seq(-6, 6, by = 1)) +
    ggplot2::labs(
      title = "Does Batter Swing/Take Behavior Contain a Location Signal?",
      subtitle = paste0(
        "Held-out games: solid teal is observed; dashed red is the fitted model\n",
        "Negative distance is inside the ABS zone"
      ),
      x = "Signed ball-edge distance from the ABS boundary (inches)",
      y = "Swing rate",
      size = "Pitches"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
}

plot_swing_take_calibration <- function(validation) {
  ggplot2::ggplot(
    validation$calibration,
    ggplot2::aes(x = predicted_swing_rate, y = actual_swing_rate)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_line(color = "#18AEB7", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(size = pitches), color = "#18AEB7") +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(
      title = "Held-Out Swing/Take Calibration",
      x = "Predicted swing rate",
      y = "Actual swing rate",
      size = "Pitches"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
}

subsample_swing_take_pilot <- function(
  choices, fraction = 0.2, min_per_batter = 100L, seed = 42L
) {
  x <- data.table::copy(data.table::as.data.table(choices))
  stop_if_missing_columns(x, "batter_id", "swing/take pilot choices")
  fraction <- as.numeric(fraction)
  min_per_batter <- as.integer(min_per_batter)
  if (!is.finite(fraction) || fraction <= 0 || fraction > 1) {
    stop("fraction must be in (0, 1]")
  }
  if (min_per_batter < 1L) stop("min_per_batter must be positive")
  x[, pilot_row__ := .I]
  set.seed(seed)
  keep <- x[, {
    keep_n <- min(.N, max(min_per_batter, ceiling(.N * fraction)))
    .(pilot_row__ = sample(pilot_row__, keep_n, replace = FALSE))
  }, by = batter_id]
  x <- x[keep$pilot_row__]
  x[, pilot_row__ := NULL]
  data.table::setorder(x, game_pk, at_bat_number, pitch_number)
  x[]
}

swing_take_context_matrix <- function(choices, specification = NULL) {
  x <- data.table::copy(data.table::as.data.table(choices))
  required <- c("balls", "strikes", "pitch_family", "matchup")
  stop_if_missing_columns(x, required, "swing/take context data")
  if (is.null(specification)) {
    specification <- list(
      balls_levels = as.character(0:3),
      strikes_levels = as.character(0:2),
      pitch_family_levels = c("fastball", "breaking", "offspeed", "other"),
      matchup_levels = sort(unique(as.character(x$matchup)))
    )
    if (!length(specification$matchup_levels)) {
      specification$matchup_levels <- "U-U"
    }
  }
  coerce_known_factor <- function(value, levels) {
    value <- as.character(value)
    value[is.na(value) | !value %in% levels] <- levels[[1L]]
    factor(value, levels = levels)
  }
  treatment_columns <- function(value, levels, prefix) {
    value <- coerce_known_factor(value, levels)
    comparison_levels <- levels[-1L]
    if (!length(comparison_levels)) {
      return(matrix(0, nrow = length(value), ncol = 0L))
    }
    result <- vapply(
      comparison_levels,
      function(level) as.numeric(value == level),
      numeric(length(value))
    )
    if (is.null(dim(result))) result <- matrix(result, ncol = 1L)
    colnames(result) <- paste0(prefix, comparison_levels)
    result
  }
  matrix <- cbind(
    treatment_columns(x$balls, specification$balls_levels, "balls_"),
    treatment_columns(x$strikes, specification$strikes_levels, "strikes_"),
    treatment_columns(
      x$pitch_family, specification$pitch_family_levels, "pitch_family_"
    ),
    treatment_columns(x$matchup, specification$matchup_levels, "matchup_")
  )
  if (is.null(specification$column_names)) {
    specification$column_names <- colnames(matrix)
  } else {
    missing <- setdiff(specification$column_names, colnames(matrix))
    if (length(missing)) {
      padding <- matrix(0, nrow = nrow(matrix), ncol = length(missing))
      colnames(padding) <- missing
      matrix <- cbind(matrix, padding)
    }
    matrix <- matrix[, specification$column_names, drop = FALSE]
  }
  storage.mode(matrix) <- "double"
  list(matrix = matrix, specification = specification)
}

prepare_swing_take_stan_data <- function(
  choices, distance_bin_width = 0.25,
  model_variant = c("simple", "strategy_limits")
) {
  model_variant <- match.arg(model_variant)
  x <- data.table::copy(data.table::as.data.table(choices))
  required <- c(
    "game_pk", "batter_id", "swing", "edge_distance_inches", "balls",
    "strikes", "pitch_family", "matchup"
  )
  stop_if_missing_columns(x, required, "hierarchical swing/take choices")
  x <- x[
    swing %in% 0:1 & is.finite(edge_distance_inches) &
      balls %in% 0:3 & strikes %in% 0:2 & !is.na(batter_id)
  ]
  if (!nrow(x)) stop("No eligible swing/take rows remain")
  distance_bin_width <- as.numeric(distance_bin_width)
  if (!is.finite(distance_bin_width) || distance_bin_width < 0) {
    stop("distance_bin_width must be nonnegative")
  }
  player_table <- x[, .(
    training_opportunities = .N,
    training_swings = sum(swing),
    training_swing_rate = mean(swing)
  ), by = batter_id]
  data.table::setorder(player_table, batter_id)
  player_table[, player_index := .I]

  x[, `:=`(
    batter_id = as.character(batter_id),
    strike_group = as.integer(strikes) + 1L
  )]
  if (distance_bin_width > 0) {
    x[, edge_distance_model :=
      round(edge_distance_inches / distance_bin_width) * distance_bin_width]
    x <- x[, .(
      swings = sum(swing),
      trials = .N
    ), by = .(
      batter_id, strike_group, edge_distance_model, balls, strikes,
      pitch_family, matchup
    )]
  } else {
    x[, `:=`(
      edge_distance_model = edge_distance_inches,
      swings = as.integer(swing),
      trials = 1L
    )]
  }
  x[player_table, on = "batter_id", player_index := i.player_index]
  if (anyNA(x$player_index)) stop("Some training batters lack a player index")
  context <- swing_take_context_matrix(x)
  stan_data <- list(
    N = nrow(x),
    P = nrow(player_table),
    K = ncol(context$matrix),
    swings = as.integer(x$swings),
    trials = as.integer(x$trials),
    player = as.integer(x$player_index),
    edge_distance = as.numeric(x$edge_distance_model),
    X = context$matrix
  )
  if (model_variant == "strategy_limits") {
    lower_target <- c(0.08, 0.18, 0.35)
    upper_target <- c(0.70, 0.88, 0.97)
    conditional_range <- (upper_target - lower_target) / (1 - lower_target)
    stan_data$strike_group <- as.integer(x$strike_group)
    stan_data$lower_prior_mean <- stats::qlogis(lower_target)
    stan_data$range_prior_mean <- stats::qlogis(conditional_range)
  }
  list(
    data = stan_data,
    aggregated_choices = x,
    player_table = player_table,
    context_specification = context$specification,
    distance_bin_width = distance_bin_width,
    model_variant = model_variant
  )
}

default_swing_take_perception_stan_file <- function(
  model_variant = c("simple", "strategy_limits")
) {
  model_variant <- match.arg(model_variant)
  root <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd()),
    error = function(e) getwd()
  )
  filename <- if (model_variant == "simple") {
    "hierarchical_swing_take_perception.stan"
  } else {
    "hierarchical_swing_take_perception_strategy.stan"
  }
  file.path(root, "scripts", "stan", filename)
}

fit_hierarchical_swing_take_perception <- function(
  choices, chains = 2L, cores = chains, iter = 2000L,
  warmup = floor(iter / 2), seed = 42L, distance_bin_width = 0.25,
  adapt_delta = 0.95, max_treedepth = 12L, stan_file = NULL,
  file = NULL, refresh = 100L, force_refit = FALSE,
  model_variant = c("simple", "strategy_limits")
) {
  model_variant <- match.arg(model_variant)
  if (!is.null(file) && file.exists(file) && !isTRUE(force_refit)) {
    cached <- readRDS(file)
    cached_variant <- if (is.null(cached$model_variant)) {
      "simple"
    } else {
      cached$model_variant
    }
    if (inherits(cached, "swing_take_perception_fit") &&
        identical(cached_variant, model_variant)) return(cached)
  }
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("fit_hierarchical_swing_take_perception() requires rstan")
  }
  if (is.null(stan_file)) {
    stan_file <- default_swing_take_perception_stan_file(model_variant)
  }
  if (!file.exists(stan_file)) stop("Swing/take Stan file not found: ", stan_file)
  bundle <- prepare_swing_take_stan_data(
    choices, distance_bin_width, model_variant
  )
  rstan::rstan_options(auto_write = TRUE)
  model <- rstan::stan_model(file = stan_file, auto_write = TRUE)
  stan_fit <- rstan::sampling(
    model,
    data = bundle$data,
    chains = as.integer(chains),
    cores = min(as.integer(cores), as.integer(chains)),
    iter = as.integer(iter),
    warmup = as.integer(warmup),
    init = 0,
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
    context_specification = bundle$context_specification,
    distance_bin_width = bundle$distance_bin_width,
    model_variant = model_variant,
    training_games = sort(unique(as.character(choices$game_pk))),
    training_pitches = nrow(choices),
    aggregated_training_rows = bundle$data$N,
    controls = list(
      chains = chains, cores = cores, iter = iter, warmup = warmup,
      seed = seed, adapt_delta = adapt_delta, max_treedepth = max_treedepth
    )
  )
  class(fit) <- "swing_take_perception_fit"
  if (!is.null(file)) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(fit, file)
  }
  fit
}

swing_take_extract_indexed <- function(draw_matrix, parameter, expected = NULL) {
  columns <- grep(paste0("^", parameter, "\\["), colnames(draw_matrix))
  if (!length(columns)) stop("Posterior is missing parameter ", parameter)
  indices <- as.integer(sub(
    ".*\\[([0-9]+)\\].*", "\\1", colnames(draw_matrix)[columns]
  ))
  columns <- columns[order(indices)]
  result <- draw_matrix[, columns, drop = FALSE]
  if (!is.null(expected) && ncol(result) != expected) {
    stop(parameter, " posterior has the wrong number of entries")
  }
  result
}

swing_take_parameter_draws <- function(fit, ndraws = NULL, seed = 42L) {
  if (!inherits(fit, "swing_take_perception_fit")) {
    stop("fit must be a swing_take_perception_fit")
  }
  parameters <- c(
    "mu_log_sigma", "tau_log_sigma", "sigma_player", "mu_threshold",
    "tau_threshold", "threshold_player", "beta"
  )
  model_variant <- if (is.null(fit$model_variant)) "simple" else fit$model_variant
  if (model_variant == "strategy_limits") {
    parameters <- c(parameters, "lower_swing", "upper_swing")
  }
  draws <- as.matrix(fit$stan_fit, pars = parameters)
  if (!is.null(ndraws) && nrow(draws) > as.integer(ndraws)) {
    set.seed(seed)
    draws <- draws[sample.int(nrow(draws), as.integer(ndraws)), , drop = FALSE]
  }
  result <- list(
    mu_log_sigma = as.numeric(draws[, "mu_log_sigma"]),
    tau_log_sigma = as.numeric(draws[, "tau_log_sigma"]),
    sigma_player = swing_take_extract_indexed(
      draws, "sigma_player", nrow(fit$player_table)
    ),
    mu_threshold = as.numeric(draws[, "mu_threshold"]),
    tau_threshold = as.numeric(draws[, "tau_threshold"]),
    threshold_player = swing_take_extract_indexed(
      draws, "threshold_player", nrow(fit$player_table)
    ),
    beta = swing_take_extract_indexed(
      draws, "beta", length(fit$context_specification$column_names)
    )
  )
  if (model_variant == "strategy_limits") {
    result$lower_swing <- swing_take_extract_indexed(draws, "lower_swing", 3L)
    result$upper_swing <- swing_take_extract_indexed(draws, "upper_swing", 3L)
  }
  result
}

score_hierarchical_swing_take <- function(
  fit, choices, ndraws = 500L, seed = 42L
) {
  if (!inherits(fit, "swing_take_perception_fit")) {
    stop("fit must be a swing_take_perception_fit")
  }
  x <- data.table::copy(data.table::as.data.table(choices))
  context <- swing_take_context_matrix(x, fit$context_specification)$matrix
  player_index <- match(as.character(x$batter_id), fit$player_table$batter_id)
  new_players <- sort(unique(as.character(x$batter_id[is.na(player_index)])))
  new_index <- match(as.character(x$batter_id), new_players)
  draws <- swing_take_parameter_draws(fit, ndraws = ndraws, seed = seed)
  model_variant <- if (is.null(fit$model_variant)) "simple" else fit$model_variant
  draw_count <- length(draws$mu_log_sigma)
  probability_sum <- sigma_sum <- threshold_sum <- numeric(nrow(x))
  set.seed(seed + 1L)
  for (draw in seq_len(draw_count)) {
    sigma <- threshold <- numeric(nrow(x))
    seen <- !is.na(player_index)
    if (any(seen)) {
      sigma[seen] <- draws$sigma_player[draw, player_index[seen]]
      threshold[seen] <- draws$threshold_player[draw, player_index[seen]]
    }
    if (length(new_players)) {
      sigma_new <- exp(
        draws$mu_log_sigma[[draw]] + draws$tau_log_sigma[[draw]] *
          stats::rnorm(length(new_players))
      )
      threshold_new <- draws$mu_threshold[[draw]] +
        draws$tau_threshold[[draw]] * stats::rnorm(length(new_players))
      sigma[!seen] <- sigma_new[new_index[!seen]]
      threshold[!seen] <- threshold_new[new_index[!seen]]
    }
    center <- threshold + as.numeric(context %*% draws$beta[draw, ])
    perceived_inside <- stats::pnorm(
      (center - x$edge_distance_inches) / sigma
    )
    if (model_variant == "strategy_limits") {
      strike_group <- as.integer(x$strikes) + 1L
      lower <- draws$lower_swing[draw, strike_group]
      upper <- draws$upper_swing[draw, strike_group]
      probability_sum <- probability_sum +
        lower + (upper - lower) * perceived_inside
    } else {
      probability_sum <- probability_sum + perceived_inside
    }
    sigma_sum <- sigma_sum + sigma
    threshold_sum <- threshold_sum + threshold
  }
  x[, `:=`(
    prediction_hierarchical = pmin(
      1 - 1e-9, pmax(1e-9, probability_sum / draw_count)
    ),
    sigma_posterior_mean = sigma_sum / draw_count,
    threshold_posterior_mean = threshold_sum / draw_count,
    batter_seen_in_training = !is.na(player_index)
  )]
  x[]
}

swing_take_fit_diagnostics <- function(fit) {
  if (!requireNamespace("posterior", quietly = TRUE)) {
    stop("swing_take_fit_diagnostics() requires posterior")
  }
  summary <- posterior::summarise_draws(
    posterior::as_draws_array(fit$stan_fit), "rhat", "ess_bulk", "ess_tail"
  )
  sampler <- rstan::get_sampler_params(fit$stan_fit, inc_warmup = FALSE)
  divergences <- sum(vapply(
    sampler, function(x) sum(x[, "divergent__"] > 0), numeric(1L)
  ))
  data.table::data.table(
    max_rhat = max(summary$rhat, na.rm = TRUE),
    min_bulk_ess = min(summary$ess_bulk, na.rm = TRUE),
    min_tail_ess = min(summary$ess_tail, na.rm = TRUE),
    divergences = divergences,
    pass = max(summary$rhat, na.rm = TRUE) <= 1.01 &
      min(summary$ess_bulk, na.rm = TRUE) >= 400 &
      min(summary$ess_tail, na.rm = TRUE) >= 400 & divergences == 0
  )
}

summarize_swing_take_draws <- function(values, prefix = "") {
  quantiles <- stats::quantile(
    values, c(0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975),
    names = FALSE, na.rm = TRUE
  )
  result <- c(
    mean = mean(values), lower_95 = quantiles[[1L]],
    lower_80 = quantiles[[2L]], lower_50 = quantiles[[3L]],
    median = quantiles[[4L]], upper_50 = quantiles[[5L]],
    upper_80 = quantiles[[6L]], upper_95 = quantiles[[7L]]
  )
  names(result) <- paste0(prefix, names(result))
  as.list(result)
}

swing_take_fit_summaries <- function(fit, ndraws = NULL, seed = 42L) {
  draws <- swing_take_parameter_draws(fit, ndraws = ndraws, seed = seed)
  population_median <- exp(draws$mu_log_sigma)
  population_mean <- exp(
    draws$mu_log_sigma + 0.5 * draws$tau_log_sigma^2
  )
  population_sd <- sqrt(
    (exp(draws$tau_log_sigma^2) - 1) *
      exp(2 * draws$mu_log_sigma + draws$tau_log_sigma^2)
  )
  population <- data.table::as.data.table(c(
    summarize_swing_take_draws(population_mean, "population_sigma_mean_"),
    summarize_swing_take_draws(population_median, "population_sigma_median_"),
    summarize_swing_take_draws(population_sd, "between_batter_sigma_sd_"),
    summarize_swing_take_draws(draws$tau_log_sigma, "tau_log_sigma_")
  ))
  players <- data.table::rbindlist(lapply(
    seq_len(nrow(fit$player_table)), function(index) {
      data.table::as.data.table(c(
        as.list(fit$player_table[index]),
        summarize_swing_take_draws(draws$sigma_player[, index], "sigma_"),
        summarize_swing_take_draws(
          draws$threshold_player[, index], "threshold_"
        )
      ))
    }
  ), fill = TRUE)
  correlation_targets <- cbind(
    mu_threshold = draws$mu_threshold,
    tau_threshold = draws$tau_threshold,
    draws$beta
  )
  strategy_limits <- data.table::data.table()
  if (!is.null(draws$lower_swing)) {
    strategy_limits <- data.table::rbindlist(lapply(1:3, function(index) {
      data.table::as.data.table(c(
        list(strikes = index - 1L),
        summarize_swing_take_draws(
          draws$lower_swing[, index], "lower_swing_"
        ),
        summarize_swing_take_draws(
          draws$upper_swing[, index], "upper_swing_"
        )
      ))
    }), fill = TRUE)
    correlation_targets <- cbind(
      correlation_targets, draws$lower_swing, draws$upper_swing
    )
  }
  correlations <- vapply(
    seq_len(ncol(correlation_targets)),
    function(index) stats::cor(population_median, correlation_targets[, index]),
    numeric(1L)
  )
  identifiability <- data.table::data.table(
    paired_parameter = colnames(correlation_targets),
    sigma_posterior_correlation = correlations,
    tolerance = 0.8,
    pass = abs(correlations) < 0.8
  )
  list(
    population = population,
    players = players,
    strategy_limits = strategy_limits,
    identifiability = identifiability
  )
}

validate_hierarchical_swing_take <- function(
  scored_choices, minimum_bin_pitches = 100L, bin_tolerance = 0.05
) {
  x <- data.table::copy(data.table::as.data.table(scored_choices))
  required <- c(
    "game_pk", "swing", "strikes", "edge_distance_inches",
    "prediction_context", "prediction_edge", "prediction_hierarchical"
  )
  stop_if_missing_columns(x, required, "hierarchical swing/take validation")
  for (column in c(
    "prediction_context", "prediction_edge", "prediction_hierarchical"
  )) {
    probability <- pmin(1 - 1e-9, pmax(1e-9, x[[column]]))
    x[, paste0("loss_", sub("prediction_", "", column)) := -(
      swing * log(probability) + (1 - swing) * log1p(-probability)
    )]
  }
  by_game <- x[, .(
    pitches = .N,
    context_log_loss = mean(loss_context),
    flexible_location_log_loss = mean(loss_edge),
    hierarchical_log_loss = mean(loss_hierarchical),
    hierarchical_minus_context = mean(loss_hierarchical - loss_context),
    hierarchical_minus_flexible = mean(loss_hierarchical - loss_edge)
  ), by = game_pk]
  context_se <- stats::sd(by_game$hierarchical_minus_context) /
    sqrt(nrow(by_game))
  flexible_se <- stats::sd(by_game$hierarchical_minus_flexible) /
    sqrt(nrow(by_game))
  breaks <- seq(-6, 6, by = 0.5)
  x[, edge_bin__ := cut(
    edge_distance_inches, breaks = breaks, include.lowest = TRUE, right = FALSE
  )]
  midpoint <- data.table::data.table(
    edge_bin__ = levels(x$edge_bin__),
    midpoint = head(breaks, -1L) + 0.25
  )
  curve <- x[!is.na(edge_bin__), .(
    pitches = .N,
    observed_swing_rate = mean(swing),
    predicted_swing_rate = mean(prediction_hierarchical)
  ), by = .(strikes, edge_bin__)]
  curve <- merge(curve, midpoint, by = "edge_bin__", all.x = TRUE)
  curve[, absolute_error := abs(predicted_swing_rate - observed_swing_rate)]
  included <- curve[pitches >= as.integer(minimum_bin_pitches)]
  maximum_error <- if (nrow(included)) {
    max(included$absolute_error, na.rm = TRUE)
  } else {
    NA_real_
  }
  metrics <- data.table::data.table(
    validation_pitches = nrow(x),
    validation_games = data.table::uniqueN(x$game_pk),
    context_log_loss = mean(x$loss_context),
    flexible_location_log_loss = mean(x$loss_edge),
    hierarchical_log_loss = mean(x$loss_hierarchical),
    hierarchical_minus_context = mean(x$loss_hierarchical - x$loss_context),
    hierarchical_minus_context_se = context_se,
    hierarchical_minus_flexible = mean(x$loss_hierarchical - x$loss_edge),
    hierarchical_minus_flexible_se = flexible_se,
    maximum_populated_bin_error = maximum_error,
    bin_tolerance = bin_tolerance,
    improves_context = mean(x$loss_hierarchical - x$loss_context) < -context_se,
    matches_flexible_model = mean(x$loss_hierarchical - x$loss_edge) <= flexible_se,
    bin_pass = is.finite(maximum_error) && maximum_error <= bin_tolerance,
    predictive_pass =
      mean(x$loss_hierarchical - x$loss_context) < -context_se &&
      is.finite(maximum_error) && maximum_error <= bin_tolerance
  )
  data.table::setorder(curve, strikes, midpoint)
  list(metrics = metrics, by_game = by_game, curve = curve)
}

plot_hierarchical_swing_take <- function(validation) {
  curve <- data.table::copy(validation$curve)
  curve[, strikes_label := factor(
    paste(strikes, ifelse(strikes == 1L, "strike", "strikes")),
    levels = c("0 strikes", "1 strike", "2 strikes")
  )]
  long <- data.table::rbindlist(list(
    curve[, .(
      strikes_label, midpoint, pitches,
      swing_rate = observed_swing_rate, source = "Observed"
    )],
    curve[, .(
      strikes_label, midpoint, pitches,
      swing_rate = predicted_swing_rate, source = "Gaussian perception model"
    )]
  ))
  ggplot2::ggplot(long, ggplot2::aes(
    midpoint, swing_rate, color = source, linetype = source
  )) +
    ggplot2::geom_vline(xintercept = 0, color = "grey55", linetype = "dotted") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(
      data = long[source == "Observed"],
      ggplot2::aes(size = pitches), alpha = 0.75
    ) +
    ggplot2::facet_wrap(~strikes_label, ncol = 1) +
    ggplot2::scale_y_continuous(labels = function(value) {
      paste0(round(100 * value), "%")
    }) +
    ggplot2::scale_x_continuous(breaks = seq(-6, 6, 1)) +
    ggplot2::scale_color_manual(values = c(
      "Observed" = "#18AEB7", "Gaussian perception model" = "#F45B57"
    )) +
    ggplot2::scale_linetype_manual(values = c(
      "Observed" = "solid", "Gaussian perception model" = "dashed"
    )) +
    ggplot2::labs(
      title = "Can a Gaussian Perception Curve Explain Unseen Swing Decisions?",
      subtitle = paste0(
        "Solid teal is observed; dashed red is the posterior prediction\n",
        "Negative distance is inside the ABS zone"
      ),
      x = "Signed ball-edge distance from the ABS boundary (inches)",
      y = "Swing rate", color = NULL, linetype = NULL, size = "Pitches"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
}
