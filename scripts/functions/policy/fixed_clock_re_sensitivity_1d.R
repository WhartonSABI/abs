# Run-expectancy scoring utilities for the fixed-clock confirmation analysis.
#
# A policy is allowed to see G_decision, calculated from the frozen 2023--2025
# RE288 table and the two possible called-pitch branch states.  Geometry is not
# needed to construct either branch.  G_evaluation may be replaced by a
# bootstrap RE288 draw, or by the context-adjusted sensitivity below, without
# changing the frozen decision rule.

.fixed_clock_re_table_1d <- function(re_model, label = "run-expectancy model") {
  table <- if (is.list(re_model) && !is.null(re_model$table)) {
    re_model$table
  } else {
    re_model
  }
  table <- data.table::copy(data.table::as.data.table(table))
  stop_if_missing_columns(
    table, c("outs", "base_state", "balls", "strikes", "re"), label
  )
  table[, `:=`(
    outs = as.integer(outs),
    base_state = as.character(base_state),
    balls = as.integer(balls),
    strikes = as.integer(strikes),
    re = as.numeric(re)
  )]
  if (nrow(table) != 288L || anyNA(table[, .(
    outs, base_state, balls, strikes, re
  )]) || any(!is.finite(table$re)) ||
      anyDuplicated(table[, .(outs, base_state, balls, strikes)])) {
    stop(label, " must contain one finite value for every RE288 state",
      call. = FALSE
    )
  }
  data.table::setorder(table, outs, base_state, balls, strikes)
  table[]
}

fixed_clock_re288_hash_1d <- function(re_model) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("digest is required to hash the RE288 table", call. = FALSE)
  }
  table <- .fixed_clock_re_table_1d(re_model)
  digest::digest(data.table::setDF(table), algo = "sha256", serialize = TRUE)
}

fixed_clock_re288_weighted_table_1d <- function(
  observations, game_weights = NULL, shrinkage = 20
) {
  x <- data.table::copy(data.table::as.data.table(observations))
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "outs", "base_state", "balls", "strikes",
      "runs_to_end"
    ),
    "weighted RE288 observations"
  )
  shrinkage <- as.numeric(shrinkage)
  if (length(shrinkage) != 1L || !is.finite(shrinkage) || shrinkage < 0) {
    stop("shrinkage must be one finite non-negative number", call. = FALSE)
  }
  x[, game_pk := as.character(game_pk)]
  if (is.null(game_weights)) {
    x[, re_weight__ := 1]
  } else {
    weights <- data.table::copy(data.table::as.data.table(game_weights))
    weight_column <- intersect(
      c("bootstrap_weight", "weight", "game_weight"), names(weights)
    )
    if (!"game_pk" %in% names(weights) || length(weight_column) != 1L) {
      stop(
        "game_weights needs game_pk and one bootstrap_weight/weight/game_weight column",
        call. = FALSE
      )
    }
    weights <- weights[, .(
      game_pk = as.character(game_pk),
      re_weight__ = as.numeric(get(weight_column))
    )]
    if (anyNA(weights) || any(!nzchar(weights$game_pk)) ||
        any(!is.finite(weights$re_weight__)) ||
        any(weights$re_weight__ < 0) || anyDuplicated(weights$game_pk)) {
      stop("game_weights are invalid", call. = FALSE)
    }
    x <- weights[x, on = "game_pk", nomatch = 0L]
  }
  if (!nrow(x) || sum(x$re_weight__) <= 0) {
    stop("The weighted RE288 sample is empty", call. = FALSE)
  }
  re24 <- x[, .(
    re24 = stats::weighted.mean(runs_to_end, re_weight__),
    n_re24 = sum(re_weight__)
  ), by = .(outs, base_state)]
  re288 <- x[, .(
    sum_runs = sum(runs_to_end * re_weight__),
    n = sum(re_weight__)
  ), by = .(outs, base_state, balls, strikes)]
  grid <- data.table::CJ(
    outs = 0:2,
    base_state = c("000", "001", "010", "011", "100", "101", "110", "111"),
    balls = 0:3,
    strikes = 0:2,
    sorted = TRUE
  )
  table <- merge(grid, re24, by = c("outs", "base_state"), all.x = TRUE)
  table <- merge(
    table, re288,
    by = c("outs", "base_state", "balls", "strikes"), all.x = TRUE
  )
  overall <- stats::weighted.mean(x$runs_to_end, x$re_weight__)
  table[is.na(re24), `:=`(re24 = overall, n_re24 = 0)]
  table[is.na(n), `:=`(n = 0, sum_runs = 0)]
  table[, re := data.table::fifelse(
    n + shrinkage > 0,
    (sum_runs + shrinkage * re24) / (n + shrinkage),
    re24
  )]
  data.table::setkey(table, outs, base_state, balls, strikes)
  .fixed_clock_re_table_1d(table)
}

refit_fixed_clock_re288_bootstrap_1d <- function(
  history_statcast, game_weights, shrinkage = 20
) {
  observations <- prepare_re_observations(history_statcast)
  table <- fixed_clock_re288_weighted_table_1d(
    observations, game_weights = game_weights, shrinkage = shrinkage
  )
  structure(
    list(
      table = table,
      shrinkage = as.numeric(shrinkage),
      table_sha256 = fixed_clock_re288_hash_1d(table),
      resampling_unit = "complete historical game"
    ),
    class = "fixed_clock_re288_bootstrap_1d"
  )
}

fixed_clock_pitch_gains_1d <- function(
  pitch_ledger, re_model, output_column = "G_evaluation"
) {
  x <- data.table::copy(data.table::as.data.table(pitch_ledger))
  required <- c(
    "initial_call", "adverse_team_id", "bat_team_id", "balls_before",
    "strikes_before", "outs_before", "inning", "half", "on_1b", "on_2b",
    "on_3b"
  )
  stop_if_missing_columns(x, required, "fixed-clock pitch ledger")
  for (column in c(
    "concurrent_remove_1b", "concurrent_remove_2b", "concurrent_remove_3b",
    "concurrent_add_1b", "concurrent_add_2b", "concurrent_add_3b",
    "concurrent_runs", "concurrent_outs"
  )) {
    if (!column %in% names(x)) x[, (column) := 0L]
  }
  eligible <- x$initial_call %in% c("ball", "called_strike") &
    x$balls_before %in% 0:3 & x$strikes_before %in% 0:2 &
    x$outs_before %in% 0:2 & !is.na(x$adverse_team_id) &
    !is.na(x$bat_team_id)
  gain <- rep(NA_real_, nrow(x))
  if (any(eligible)) {
    rows <- x[eligible]
    standing <- vectorized_call_branch(rows, rows$initial_call)
    corrected <- vectorized_call_branch(rows, opposite_call(rows$initial_call))
    batting_gain <-
      vectorized_branch_re(.fixed_clock_re_table_1d(re_model), corrected) -
      vectorized_branch_re(.fixed_clock_re_table_1d(re_model), standing)
    gain[eligible] <- ifelse(
      rows$adverse_team_id == rows$bat_team_id,
      batting_gain,
      -batting_gain
    )
  }
  if (length(output_column) != 1L || is.na(output_column) ||
      !nzchar(output_column)) {
    stop("output_column must be one non-empty name", call. = FALSE)
  }
  x[, (output_column) := as.numeric(gain)]
  data.table::setattr(x, "gain_information_regime", paste(
    "standing and opposite-call branch states only; pitch location, ABS truth,",
    "official outcome, and future context excluded"
  ))
  x[]
}

prepare_fixed_clock_context_re_observations_1d <- function(statcast) {
  x <- data.table::copy(data.table::as.data.table(statcast))
  required <- c(
    "game_pk", "game_date", "inning", "inning_topbot", "balls", "strikes",
    "outs_when_up", "on_1b", "on_2b", "on_3b", "bat_score",
    "post_bat_score", "batter", "pitcher", "home_team"
  )
  stop_if_missing_columns(x, required, "context-adjusted RE Statcast")
  x[, `:=`(
    game_pk = as.character(game_pk),
    game_date = as.Date(game_date),
    base_state = base_state_code(on_1b, on_2b, on_3b)
  )]
  x[, half_final_bat_score := max(c(bat_score, post_bat_score), na.rm = TRUE),
    by = .(game_pk, inning, inning_topbot)
  ]
  x[, runs_to_end := half_final_bat_score - bat_score]
  x <- x[
    data.table::between(balls, 0L, 3L) &
      data.table::between(strikes, 0L, 2L) &
      data.table::between(outs_when_up, 0L, 2L) &
      is.finite(runs_to_end) & runs_to_end >= 0 &
      !is.na(batter) & !is.na(pitcher) & !is.na(home_team)
  ]
  state_levels <- as.vector(outer(
    as.vector(outer(0:2, c(
      "000", "001", "010", "011", "100", "101", "110", "111"
    ), paste, sep = "|")),
    as.vector(outer(0:3, 0:2, paste, sep = "|")),
    paste, sep = "|"
  ))
  # The outer construction above orders every outs/base pair against every
  # count; explicit paste below is the canonical key used for prediction.
  x[, `:=`(
    state_288 = factor(
      paste(outs_when_up, base_state, balls, strikes, sep = "|"),
      levels = state_levels
    ),
    season_centered = as.numeric(format(game_date, "%Y")) - 2024,
    park_id = factor(as.character(home_team)),
    batter_id = factor(as.character(batter)),
    pitcher_id = factor(as.character(pitcher)),
    runs_to_end = as.integer(round(runs_to_end))
  )]
  if (anyNA(x$state_288)) {
    stop("Context-adjusted RE observations contain an unknown RE288 state",
      call. = FALSE
    )
  }
  x[, .(
    game_pk, game_date, state_288, season_centered, park_id, batter_id,
    pitcher_id, runs_to_end
  )]
}

fit_fixed_clock_context_re_1d <- function(
  history_statcast, game_weights = NULL, nthreads = 1L
) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv is required for context-adjusted RE sensitivity", call. = FALSE)
  }
  x <- prepare_fixed_clock_context_re_observations_1d(history_statcast)
  x[, fit_weight__ := 1]
  if (!is.null(game_weights)) {
    weights <- data.table::copy(data.table::as.data.table(game_weights))
    weight_column <- intersect(
      c("bootstrap_weight", "weight", "game_weight"), names(weights)
    )
    if (!"game_pk" %in% names(weights) || length(weight_column) != 1L) {
      stop("Invalid context-adjusted RE game weights", call. = FALSE)
    }
    weights <- weights[, .(
      game_pk = as.character(game_pk),
      fit_weight__ = as.numeric(get(weight_column))
    )]
    x[, fit_weight__ := NULL]
    x <- weights[x, on = "game_pk", nomatch = 0L]
  }
  formula <- runs_to_end ~ state_288 + season_centered +
    s(park_id, bs = "re") + s(batter_id, bs = "re") +
    s(pitcher_id, bs = "re")
  model <- mgcv::bam(
    formula,
    data = x,
    weights = fit_weight__,
    family = mgcv::nb(link = "log"),
    method = "fREML",
    discrete = TRUE,
    nthreads = as.integer(nthreads),
    gc.level = 1L
  )
  result <- list(
    model = model,
    formula = formula,
    training_games = sort(unique(x$game_pk)),
    factor_levels = list(
      state_288 = levels(x$state_288),
      park_id = levels(x$park_id),
      batter_id = levels(x$batter_id),
      pitcher_id = levels(x$pitcher_id)
    ),
    reference_levels = list(
      park_id = levels(x$park_id)[[1L]],
      batter_id = levels(x$batter_id)[[1L]],
      pitcher_id = levels(x$pitcher_id)[[1L]]
    ),
    rows = nrow(x),
    games = data.table::uniqueN(x$game_pk),
    information_regime = paste(
      "negative-binomial RE sensitivity using RE288 state, linear season,",
      "and partially pooled park, batter, and pitcher effects"
    )
  )
  class(result) <- "fixed_clock_context_re_1d"
  result
}

.predict_fixed_clock_context_re_1d <- function(fit, newdata) {
  if (!inherits(fit, "fixed_clock_context_re_1d")) {
    stop("fit must be a fixed_clock_context_re_1d", call. = FALSE)
  }
  x <- data.table::copy(data.table::as.data.table(newdata))
  required <- c(
    "state_288", "season_centered", "park_id", "batter_id", "pitcher_id"
  )
  stop_if_missing_columns(x, required, "context-adjusted RE prediction data")
  random_fields <- c("park_id", "batter_id", "pitcher_id")
  missing <- lapply(random_fields, function(field) {
    !as.character(x[[field]]) %in% fit$factor_levels[[field]]
  })
  names(missing) <- random_fields
  for (field in random_fields) {
    value <- as.character(x[[field]])
    value[missing[[field]]] <- fit$reference_levels[[field]]
    x[, (field) := factor(value, levels = fit$factor_levels[[field]])]
  }
  x[, state_288 := factor(
    as.character(state_288), levels = fit$factor_levels$state_288
  )]
  if (anyNA(x$state_288) || any(!is.finite(x$season_centered))) {
    stop("Context-adjusted RE prediction state is invalid", call. = FALSE)
  }
  pattern <-
    as.integer(missing$park_id) + 2L * as.integer(missing$batter_id) +
    4L * as.integer(missing$pitcher_id)
  prediction <- rep(NA_real_, nrow(x))
  for (mask in sort(unique(pattern))) {
    rows <- which(pattern == mask)
    exclude <- character()
    if (bitwAnd(mask, 1L)) exclude <- c(exclude, "s(park_id)")
    if (bitwAnd(mask, 2L)) exclude <- c(exclude, "s(batter_id)")
    if (bitwAnd(mask, 4L)) exclude <- c(exclude, "s(pitcher_id)")
    prediction[rows] <- as.numeric(stats::predict(
      fit$model,
      newdata = x[rows],
      type = "response",
      exclude = exclude
    ))
  }
  prediction
}

fixed_clock_context_pitch_gains_1d <- function(
  pitch_ledger, fit, output_column = "G_context_evaluation"
) {
  x <- data.table::copy(data.table::as.data.table(pitch_ledger))
  required <- c(
    "initial_call", "adverse_team_id", "bat_team_id", "balls_before",
    "strikes_before", "outs_before", "inning", "half", "on_1b", "on_2b",
    "on_3b", "game_date", "home_team", "batter_id", "pitcher_id"
  )
  stop_if_missing_columns(x, required, "context-adjusted fixed clock")
  for (column in c(
    "concurrent_remove_1b", "concurrent_remove_2b", "concurrent_remove_3b",
    "concurrent_add_1b", "concurrent_add_2b", "concurrent_add_3b",
    "concurrent_runs", "concurrent_outs"
  )) {
    if (!column %in% names(x)) x[, (column) := 0L]
  }
  eligible <- x$initial_call %in% c("ball", "called_strike") &
    x$balls_before %in% 0:3 & x$strikes_before %in% 0:2 &
    x$outs_before %in% 0:2
  gain <- rep(NA_real_, nrow(x))
  if (any(eligible)) {
    rows <- x[eligible]
    standing <- vectorized_call_branch(rows, rows$initial_call)
    corrected <- vectorized_call_branch(rows, opposite_call(rows$initial_call))
    make_newdata <- function(state) {
      data.table::data.table(
        state_288 = paste(
          pmin(state$outs, 2L),
          base_state_code(state$on_1b, state$on_2b, state$on_3b),
          state$balls,
          state$strikes,
          sep = "|"
        ),
        season_centered = as.numeric(format(as.Date(rows$game_date), "%Y")) -
          2024,
        park_id = as.character(rows$home_team),
        batter_id = as.character(rows$batter_id),
        pitcher_id = as.character(rows$pitcher_id)
      )
    }
    standing_re <- .predict_fixed_clock_context_re_1d(fit, make_newdata(standing))
    corrected_re <- .predict_fixed_clock_context_re_1d(
      fit, make_newdata(corrected)
    )
    standing_re[standing$outs >= 3L] <- 0
    corrected_re[corrected$outs >= 3L] <- 0
    batting_gain <- corrected$runs + corrected_re - standing$runs - standing_re
    gain[eligible] <- ifelse(
      rows$adverse_team_id == rows$bat_team_id,
      batting_gain,
      -batting_gain
    )
  }
  x[, (output_column) := as.numeric(gain)]
  x[]
}
