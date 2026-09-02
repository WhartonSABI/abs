# Outcome-free one-dimensional discrimination model for a fielding team's
# challenge/pass decision after a taken pitch is initially called a ball.
#
# The repository geometry uses d > 0 outside the ABS zone.  A called ball is
# wrong when d < 0, so this adapter defines the decision margin M = -d.  Thus
# M > 0 always means that a defensive challenge would succeed.  The existing
# Stan program is unit-agnostic: its "player" index is a defensive team here,
# its common-sigma mode estimates one league-wide defensive effective width,
# and its crossed "team" index is the opponent batting team.

defense_challenge_discrimination_1d_outcome_columns <- function() {
  unique(c(
    challenge_discrimination_1d_outcome_columns(),
    "call_wrong", "correctable_opportunity", "potential_challenger_re",
    "potential_challenger_wpa"
  ))
}

defense_challenge_discrimination_1d_default_context <- function() {
  challenge_discrimination_1d_default_context()
}

defense_challenge_discrimination_1d_stake_epsilon <- function() {
  challenge_discrimination_1d_stake_epsilon()
}

.defense_challenge_scalar_flag_1d <- function(value, name) {
  if (length(value) != 1L || is.na(value)) {
    stop(name, " must be one non-missing logical value", call. = FALSE)
  }
  isTRUE(value)
}

.defense_challenge_exact_edge_1d <- function(x, tolerance = 1e-8) {
  geometry <- c("plate_x", "plate_z", "sz_top", "sz_bot")
  has_geometry <- all(geometry %in% names(x))
  supplied_name <- if ("physical_edge_distance_inches" %in% names(x)) {
    "physical_edge_distance_inches"
  } else if ("edge_distance_inches" %in% names(x)) {
    "edge_distance_inches"
  } else {
    NULL
  }
  if (!has_geometry && is.null(supplied_name)) {
    stop(
      "Defense discrimination rows need exact edge distance or tracked geometry",
      call. = FALSE
    )
  }
  supplied <- if (is.null(supplied_name)) {
    rep(NA_real_, nrow(x))
  } else {
    as.numeric(x[[supplied_name]])
  }
  exact <- rep(NA_real_, nrow(x))
  if (has_geometry) {
    exact <- abs_edge_distance_inches(
      as.numeric(x$plate_x), as.numeric(x$plate_z),
      as.numeric(x$sz_top), as.numeric(x$sz_bot)
    )
    disagreement <- is.finite(exact) & is.finite(supplied) &
      abs(exact - supplied) > tolerance
    if (any(disagreement)) {
      stop(
        "Defense edge distances disagree with exact rounded ABS geometry",
        call. = FALSE
      )
    }
  }
  use_exact <- is.finite(exact)
  physical <- supplied
  physical[use_exact] <- exact[use_exact]
  list(
    value = physical,
    source = ifelse(
      use_exact, "exact_rounded_geometry", "precomputed_exact_edge"
    )
  )
}

.defense_challenge_context_fields_1d <- function(x) {
  if (!"balls_before" %in% names(x)) x[, balls_before := NA_integer_]
  if (!"strikes_before" %in% names(x)) x[, strikes_before := NA_integer_]
  balls <- suppressWarnings(as.integer(x$balls_before))
  strikes <- suppressWarnings(as.integer(x$strikes_before))
  valid_count <- balls %in% 0:3 & strikes %in% 0:2
  x[, count_state := data.table::fifelse(
    valid_count, paste0(balls, "-", strikes), "unknown"
  )]
  if (!"pitch_family" %in% names(x)) {
    if ("pitch_type" %in% names(x)) {
      x[, pitch_family := as.character(pitch_type)]
    } else {
      x[, pitch_family := "unknown"]
    }
  }
  x[is.na(pitch_family) | !nzchar(pitch_family), pitch_family := "unknown"]
  if (!"matchup" %in% names(x)) {
    if (all(c("stand", "p_throws") %in% names(x))) {
      x[, matchup := paste0(
        data.table::fcoalesce(as.character(stand), "U"), "-",
        data.table::fcoalesce(as.character(p_throws), "U")
      )]
    } else {
      x[, matchup := "U-U"]
    }
  }
  x[is.na(matchup) | !nzchar(matchup), matchup := "U-U"]
  for (column in c("inning", "score_margin")) {
    if (!column %in% names(x)) x[, (column) := 0]
  }
  x[]
}

normalize_defense_challenge_discrimination_1d_rows <- function(
  rows, require_action = TRUE, geometry_tolerance = 1e-8,
  stake_log_odds_epsilon = defense_challenge_discrimination_1d_stake_epsilon()
) {
  raw <- data.table::copy(data.table::as.data.table(rows))
  source_rows <- nrow(raw)
  if (!source_rows) stop("Defense challenge-discrimination input is empty")
  require_action <- .defense_challenge_scalar_flag_1d(
    require_action, "require_action"
  )
  epsilon <- as.numeric(stake_log_odds_epsilon)
  if (length(epsilon) != 1L || !is.finite(epsilon) ||
      epsilon <= 0 || epsilon >= 0.5) {
    stop("stake_log_odds_epsilon must be strictly between 0 and 0.5")
  }
  if (!require_action && !"challenged" %in% names(raw)) {
    raw[, challenged := NA_integer_]
  }
  required <- c(
    "game_pk", "pitch_order", "defensive_team_id", "opponent_team_id",
    "initial_call", "stake_G", "inventory_loss"
  )
  if (require_action) required <- c(required, "challenged")
  stop_if_missing_columns(raw, required, "defense challenge-discrimination input")

  # This is an explicit allowlist. Official calls, ABS labels, and challenge
  # outcomes may be present upstream but cannot survive into this table.
  allowed <- c(
    required, "challenged", "physical_edge_distance_inches",
    "edge_distance_inches", "plate_x", "plate_z", "sz_top", "sz_bot",
    "defense_inventory_before", "adverse_challenges_before",
    "inning", "outs_before", "balls_before", "strikes_before",
    "score_margin", "pitch_type", "pitch_family", "matchup", "stand",
    "p_throws", "umpire_id", "catcher_id", "pitcher_id",
    "at_bat_number", "pitch_number", "sampling_offset", "abs_eligible",
    "tracking_available"
  )
  x <- raw[, intersect(allowed, names(raw)), with = FALSE]
  for (column in c("umpire_id", "catcher_id")) {
    if (!column %in% names(x)) x[, (column) := "__UNKNOWN__"]
  }
  if (!"defense_inventory_before" %in% names(x)) {
    if ("adverse_challenges_before" %in% names(x)) {
      x[, defense_inventory_before := adverse_challenges_before]
    } else {
      x[, defense_inventory_before := NA_integer_]
    }
  }
  if (!"sampling_offset" %in% names(x)) x[, sampling_offset := 0]
  offset <- suppressWarnings(as.numeric(x$sampling_offset))
  if (any(!is.finite(offset)) || any(abs(offset) > 1e-12)) {
    stop(
      "The defense probit requires every eligible pass; sampling offsets are unsupported",
      call. = FALSE
    )
  }
  x[, sampling_offset := NULL]

  edge <- .defense_challenge_exact_edge_1d(x, geometry_tolerance)
  x[, `:=`(
    physical_edge_distance_inches = edge$value,
    defense_margin_inches = -edge$value,
    margin_inches = -edge$value,
    margin_source = edge$source
  )]
  x <- .defense_challenge_context_fields_1d(x)
  x[, `:=`(
    game_pk = as.character(game_pk),
    defensive_team_id = as.character(defensive_team_id),
    opponent_team_id = as.character(opponent_team_id),
    initial_call = as.character(initial_call),
    challenged = as.integer(challenged),
    defense_inventory_before = as.integer(defense_inventory_before),
    umpire_id = as.character(umpire_id),
    catcher_id = as.character(catcher_id),
    pitch_family = as.character(pitch_family),
    matchup = as.character(matchup),
    stake_G = as.numeric(stake_G),
    inventory_loss = as.numeric(inventory_loss)
  )]
  for (column in c(
    "defensive_team_id", "opponent_team_id", "umpire_id", "catcher_id"
  )) {
    value <- x[[column]]
    value[is.na(value) | !nzchar(value)] <- "__UNKNOWN__"
    x[, (column) := value]
  }
  total_stake <- x$stake_G + x$inventory_loss
  q_star <- x$inventory_loss / total_stake
  clipped <- pmin(1 - epsilon, pmax(epsilon, q_star))
  x[, `:=`(
    total_stake = total_stake,
    q_star = q_star,
    q_star_log_odds_value = clipped,
    q_star_log_odds_clipped = is.finite(q_star) & abs(clipped - q_star) > 0,
    q_star_log_odds_epsilon = epsilon,
    challenge_cost_log_odds = stats::qlogis(clipped)
  )]

  action_ok <- if (require_action) {
    !is.na(x$challenged) & x$challenged %in% 0:1
  } else {
    rep(TRUE, nrow(x))
  }
  inventory_ok <- !is.na(x$defense_inventory_before) &
    x$defense_inventory_before >= 1L
  tracking_ok <- if ("tracking_available" %in% names(x)) {
    x$tracking_available %in% TRUE
  } else {
    rep(TRUE, nrow(x))
  }
  abs_eligible_ok <- if ("abs_eligible" %in% names(x)) {
    x$abs_eligible %in% TRUE
  } else {
    rep(TRUE, nrow(x))
  }
  eligible <- x$initial_call == "ball" &
    !is.na(x$game_pk) & nzchar(x$game_pk) &
    x$defensive_team_id != "__UNKNOWN__" &
    is.finite(x$defense_margin_inches) &
    is.finite(x$stake_G) & x$stake_G >= 0 &
    is.finite(x$inventory_loss) & x$inventory_loss >= 0 &
    is.finite(x$total_stake) & x$total_stake > 0 &
    is.finite(x$challenge_cost_log_odds) &
    inventory_ok & tracking_ok & abs_eligible_ok & action_ok
  x <- x[eligible]
  if (!nrow(x)) stop("No eligible tracked taken initial called balls remain")
  data.table::setorder(x, game_pk, pitch_order)
  if (anyDuplicated(x[, .(game_pk, pitch_order)])) {
    stop("Defense discrimination rows contain duplicate pitch keys")
  }
  x[, `:=`(
    taken_initial_called_ball = TRUE,
    decision_unit = "fielding_team",
    margin_orientation = "positive_means_called_ball_was_strike"
  )]
  forbidden <- intersect(
    defense_challenge_discrimination_1d_outcome_columns(), names(x)
  )
  if (length(forbidden)) {
    stop("Outcome columns survived the defense discrimination allowlist")
  }
  data.table::setattr(x, "eligibility", data.table::data.table(
    source_rows = source_rows,
    eligible_rows = nrow(x),
    excluded_rows = source_rows - nrow(x)
  ))
  x[]
}

build_defense_challenge_discrimination_1d_rows <- function(
  pitch_ledger, remaining_opportunities, re_model,
  stake_log_odds_epsilon = defense_challenge_discrimination_1d_stake_epsilon()
) {
  ledger <- data.table::copy(data.table::as.data.table(pitch_ledger))
  remaining <- data.table::copy(data.table::as.data.table(remaining_opportunities))
  required <- c(
    "game_pk", "pitch_order", "initial_call", "bat_team_id", "fld_team_id",
    "challenge_occurred", "challenger_team_id", "challenger_role",
    "fld_team_challenges_before", "inning", "half", "outs_before",
    "balls_before", "strikes_before", "bat_score", "fld_score",
    "edge_distance_inches", "umpire_id", "fielder_2"
  )
  stop_if_missing_columns(ledger, required, "defense decision pitch ledger")
  stop_if_missing_columns(
    remaining,
    c(
      "game_pk", "pitch_order", "team_id", "expected_correctable_remaining",
      "expected_re_remaining"
    ),
    "defense remaining-opportunity source"
  )
  ledger[, game_pk := as.character(game_pk)]
  remaining[, game_pk := as.character(game_pk)]
  x <- ledger[
    initial_call == "ball" & !is.na(fld_team_id) &
      fld_team_challenges_before > 0L
  ]
  defensive_action <- x$challenge_occurred %in% TRUE &
    as.character(x$challenger_team_id) == as.character(x$fld_team_id)
  invalid_action <- x$challenge_occurred %in% TRUE &
    (!defensive_action | !x$challenger_role %in% c("catcher", "pitcher"))
  if (any(invalid_action)) {
    stop(
      "An initial called-ball challenge is not attributable to catcher/pitcher defense",
      call. = FALSE
    )
  }
  x[, `:=`(
    challenged = as.integer(defensive_action),
    defensive_team_id = as.character(fld_team_id),
    opponent_team_id = as.character(bat_team_id),
    defense_inventory_before = as.integer(fld_team_challenges_before),
    score_margin = as.numeric(fld_score) - as.numeric(bat_score),
    catcher_id = as.character(fielder_2),
    umpire_id = as.character(umpire_id)
  )]
  x <- .defense_challenge_context_fields_1d(x)
  marginal <- remaining[, .(
    game_pk, pitch_order,
    defensive_team_id = as.character(team_id),
    expected_correctable_remaining,
    expected_re_remaining
  )]
  x <- merge(
    x, marginal,
    by = c("game_pk", "pitch_order", "defensive_team_id"),
    all.x = TRUE, sort = FALSE
  )
  missing_inventory_value <- !is.finite(x$expected_correctable_remaining) |
    !is.finite(x$expected_re_remaining)
  if (any(missing_inventory_value)) {
    stop(
      sum(missing_inventory_value),
      " eligible defensive rows lack remaining-opportunity values",
      call. = FALSE
    )
  }
  x[, inventory_loss := continuous_expected_inventory_loss(
    expected_correctable_remaining,
    expected_re_remaining,
    defense_inventory_before
  )]
  # For a called ball, the defensive gain from an overturn is the batting run
  # expectancy of ball minus called strike. This is the same unsigned branch
  # contrast computed by the existing helper and never consults ABS truth.
  x[, stake_G := continuous_hypothetical_batter_gain(.SD, re_model)]
  normalize_defense_challenge_discrimination_1d_rows(
    x, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
}

.defense_challenge_core_alias_rows_1d <- function(rows, require_action = TRUE) {
  x <- normalize_defense_challenge_discrimination_1d_rows(
    rows, require_action = require_action
  )
  # Aliases exist only at the internal Stan boundary. Physical geometry and the
  # true initial call remain intact in every public defense table.
  x[, `:=`(
    batter_id = defensive_team_id,
    bat_team_id = opponent_team_id,
    initial_call = "called_strike",
    edge_distance_inches = defense_margin_inches,
    adverse_challenges_before = defense_inventory_before,
    sampling_offset = 0
  )]
  # The core normalizer treats its edge distance as the physical signed ABS
  # distance whenever tracked geometry is also present.  Here that internal
  # field is deliberately the oppositely oriented defense margin, so do not
  # pass the original geometry across this private adapter boundary.  Exact
  # rounded geometry remains in every public defense table above.
  geometry_columns <- intersect(
    c("plate_x", "plate_z", "sz_top", "sz_bot"), names(x)
  )
  if (length(geometry_columns)) x[, (geometry_columns) := NULL]
  retained <- intersect(challenge_discrimination_1d_allowed_columns(), names(x))
  out <- x[, ..retained]
  if (!isTRUE(require_action) && "challenged" %in% names(out)) {
    out[, challenged := NULL]
  }
  out[]
}

defense_challenge_discrimination_1d_tail_diagnostics <- function(
  rows, margin_limit_inches = 3
) {
  limit <- validate_challenge_discrimination_1d_margin_limit(
    margin_limit_inches
  )
  x <- data.table::as.data.table(rows)
  stop_if_missing_columns(
    x, c("defense_margin_inches", "challenged"),
    "defense discrimination tail diagnostics"
  )
  tail <- x[abs(defense_margin_inches) > limit]
  if (!nrow(tail)) {
    return(data.table::data.table(
      margin_limit_inches = limit,
      tail_side = character(), tail_opportunities = integer(),
      tail_challenges = integer(), tail_challenge_rate = numeric(),
      mean_absolute_margin_inches = numeric(),
      maximum_absolute_margin_inches = numeric()
    ))
  }
  tail[, tail_side := ifelse(
    defense_margin_inches > limit,
    "far_wrong_called_ball", "far_correct_called_ball"
  )]
  tail[, .(
    margin_limit_inches = limit,
    tail_opportunities = .N,
    tail_challenges = sum(challenged, na.rm = TRUE),
    tail_challenge_rate = mean(challenged, na.rm = TRUE),
    mean_absolute_margin_inches = mean(abs(defense_margin_inches)),
    maximum_absolute_margin_inches = max(abs(defense_margin_inches))
  ), by = tail_side][]
}

prepare_defense_challenge_discrimination_1d <- function(
  rows,
  categorical_context = defense_challenge_discrimination_1d_default_context()$categorical,
  numeric_context = defense_challenge_discrimination_1d_default_context()$numeric,
  margin_limit_inches = 3,
  stake_log_odds_epsilon = defense_challenge_discrimination_1d_stake_epsilon()
) {
  normalized <- normalize_defense_challenge_discrimination_1d_rows(
    rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  eligibility <- data.table::copy(attr(normalized, "eligibility"))
  alias <- .defense_challenge_core_alias_rows_1d(normalized, TRUE)
  core <- prepare_challenge_discrimination_1d(
    alias, sigma_model = "common",
    categorical_context = categorical_context,
    numeric_context = numeric_context,
    margin_limit_inches = margin_limit_inches,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  limit <- core$margin_limit_inches
  local <- normalized[abs(defense_margin_inches) <= limit]
  tail <- normalized[abs(defense_margin_inches) > limit]
  eligibility[, `:=`(
    model_rows = nrow(local),
    tail_diagnostic_rows = nrow(tail),
    margin_limit_inches = limit
  )]
  defense_teams <- data.table::copy(core$player_table)
  data.table::setnames(defense_teams, "batter_id", "defensive_team_id")
  if ("outside_zone_opportunities" %in% names(defense_teams)) {
    data.table::setnames(
      defense_teams, "outside_zone_opportunities", "wrong_call_opportunities"
    )
  }
  opponent_teams <- data.table::copy(core$team_table)
  data.table::setnames(opponent_teams, "team_id", "opponent_team_id")
  list(
    data = core$data,
    rows = local[], tail_rows = tail[], normalized_rows = normalized[],
    alias_rows = alias[], core_bundle = core,
    defense_team_table = defense_teams[],
    opponent_team_table = opponent_teams[],
    umpire_table = core$umpire_table,
    catcher_table = core$catcher_table,
    context_specification = core$context_specification,
    context_columns = core$context_columns,
    context_design_columns = core$context_design_columns,
    sigma_model = "common",
    margin_limit_inches = limit,
    stake_log_odds_epsilon = stake_log_odds_epsilon,
    eligibility = eligibility[],
    tail_diagnostics = defense_challenge_discrimination_1d_tail_diagnostics(
      normalized, limit
    ),
    information_set = c(
      "taken_initial_called_ball", "fielding_team_challenge_or_pass",
      "exact_oriented_margin_positive_if_wrong", "count", "pitch_family",
      "matchup", "decision_stakes", "fielding_team", "opponent_team",
      "umpire", "catcher"
    ),
    excluded_information = defense_challenge_discrimination_1d_outcome_columns()
  )
}

default_defense_challenge_discrimination_1d_source_file <- function() {
  root <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd()),
    error = function(error) getwd()
  )
  file.path(
    root, "scripts", "functions", "perception",
    "defense_challenge_discrimination_1d.R"
  )
}

defense_challenge_discrimination_1d_cache_identity <- function(
  bundle, backend, seed, stan_file,
  chains, iter_warmup, iter_sampling, adapt_delta, max_treedepth
) {
  if (is.null(bundle$core_bundle)) {
    stop("bundle must come from prepare_defense_challenge_discrimination_1d()")
  }
  core <- challenge_discrimination_1d_cache_identity(
    bundle$core_bundle, backend = backend, seed = seed, stan_file = stan_file,
    sampling_controls = challenge_discrimination_1d_sampling_identity(
      chains, iter_warmup, iter_sampling, adapt_delta, max_treedepth
    )
  )
  source_file <- default_defense_challenge_discrimination_1d_source_file()
  list(
    identity_schema = 1L,
    adapter = "called_ball_fielding_team_M_equals_negative_d_common_sigma_v1",
    adapter_source_sha256 = digest::digest(
      file = source_file, algo = "sha256", serialize = FALSE
    ),
    core_identity = core
  )
}

assert_defense_challenge_discrimination_1d_cache_identity <- function(
  cached, expected, file
) {
  if (!inherits(cached, "defense_challenge_discrimination_1d_fit")) {
    stop("Existing defense cache has the wrong model class: ", file)
  }
  if (is.null(cached$defense_cache_identity) ||
      !identical(cached$defense_cache_identity, expected)) {
    stop(
      "Defense cache identity mismatch; data/games, seed, backend, controls, ",
      "Stan, or adapter source changed. Set force_refit=TRUE.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

fit_defense_challenge_discrimination_1d <- function(
  rows,
  categorical_context = defense_challenge_discrimination_1d_default_context()$categorical,
  numeric_context = defense_challenge_discrimination_1d_default_context()$numeric,
  margin_limit_inches = 3,
  stake_log_odds_epsilon = defense_challenge_discrimination_1d_stake_epsilon(),
  backend = c("cmdstanr", "rstan"), chains = 4L,
  parallel_chains = chains, iter_warmup = 1000L, iter_sampling = 1000L,
  seed = 20260825L, adapt_delta = 0.97, max_treedepth = 12L,
  refresh = 100L, stan_file = NULL, file = NULL, force_refit = FALSE
) {
  backend <- match.arg(backend)
  bundle <- prepare_defense_challenge_discrimination_1d(
    rows, categorical_context, numeric_context, margin_limit_inches,
    stake_log_odds_epsilon
  )
  if (is.null(stan_file)) stan_file <- default_challenge_discrimination_1d_stan_file()
  identity <- defense_challenge_discrimination_1d_cache_identity(
    bundle, backend, seed, stan_file, chains, iter_warmup, iter_sampling,
    adapt_delta, max_treedepth
  )
  if (!is.null(file) && file.exists(file) && !isTRUE(force_refit)) {
    cached <- readRDS(file)
    assert_defense_challenge_discrimination_1d_cache_identity(
      cached, identity, file
    )
    return(cached)
  }
  core <- fit_challenge_discrimination_1d(
    bundle$alias_rows, sigma_model = "common",
    categorical_context = categorical_context,
    numeric_context = numeric_context,
    margin_limit_inches = margin_limit_inches,
    stake_log_odds_epsilon = stake_log_odds_epsilon,
    backend = backend, chains = chains, parallel_chains = parallel_chains,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling, seed = seed,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth,
    refresh = refresh, stan_file = stan_file, file = NULL
  )
  core$defense_team_table <- bundle$defense_team_table
  core$opponent_team_table <- bundle$opponent_team_table
  core$defense_eligibility <- bundle$eligibility
  core$defense_tail_diagnostics <- bundle$tail_diagnostics
  core$defense_cache_identity <- identity
  core$decision_unit <- "fielding_team"
  core$decision_call <- "ball"
  core$margin_definition <- "M = -physical_edge_distance_inches"
  core$information_set <- bundle$information_set
  core$excluded_information <- bundle$excluded_information
  core$sigma_interpretation <- paste(
    "one league-wide effective defensive-team discrimination width in inches;",
    "catcher and pitcher challenges are one fielding-team action"
  )
  class(core) <- unique(c(
    "defense_challenge_discrimination_1d_fit", class(core)
  ))
  if (!is.null(file)) saveRDS(core, file)
  core
}

draws_defense_challenge_discrimination_1d <- function(
  fit, ndraws = NULL, seed = 20260825L
) {
  if (!inherits(fit, "defense_challenge_discrimination_1d_fit")) {
    stop("fit must be a defense_challenge_discrimination_1d_fit")
  }
  draws <- draws_challenge_discrimination_1d(fit, ndraws, seed)
  draws$threshold_defense_team <- draws$threshold_player
  draws$common_sigma_inches <- exp(draws$mu_log_sigma)
  draws
}

summarize_defense_challenge_discrimination_1d <- function(
  fit, ndraws = NULL, seed = 20260825L
) {
  if (!inherits(fit, "defense_challenge_discrimination_1d_fit")) {
    stop("fit must be a defense_challenge_discrimination_1d_fit")
  }
  result <- summarize_challenge_discrimination_1d(fit, ndraws, seed)
  defense <- data.table::copy(result$players)
  data.table::setnames(defense, "batter_id", "defensive_team_id")
  opponent <- data.table::copy(result$teams)
  data.table::setnames(opponent, "team_id", "opponent_team_id")
  result$population[, `:=`(
    decision_unit = "fielding_team",
    decision_call = "ball",
    margin_definition = "M=-d; positive means called ball was a strike",
    defense_teams = players,
    players = NULL,
    opponent_teams = teams,
    teams = NULL
  )]
  list(
    population = result$population[],
    defense_teams = defense[],
    opponent_teams = opponent[],
    umpires = result$umpires,
    catchers = result$catchers,
    tail_diagnostics = data.table::copy(fit$defense_tail_diagnostics)
  )
}

score_defense_challenge_discrimination_1d <- function(
  fit, rows, ndraws = 400L, seed = 20260825L,
  new_defense_team = c("population", "sample"), return_draws = FALSE,
  require_game_separation = TRUE, allow_in_sample = FALSE
) {
  if (!inherits(fit, "defense_challenge_discrimination_1d_fit")) {
    stop("fit must be a defense_challenge_discrimination_1d_fit")
  }
  new_defense_team <- match.arg(new_defense_team)
  require_action <- "challenged" %in% names(rows)
  normalized <- normalize_defense_challenge_discrimination_1d_rows(
    rows, require_action = require_action,
    stake_log_odds_epsilon = fit$stake_log_odds_epsilon
  )
  local <- normalized[abs(defense_margin_inches) <= fit$margin_limit_inches]
  alias <- .defense_challenge_core_alias_rows_1d(normalized, require_action)
  scored <- score_challenge_discrimination_1d(
    fit, alias, ndraws = ndraws, seed = seed,
    new_player = new_defense_team, return_draws = return_draws,
    require_game_separation = require_game_separation,
    allow_in_sample = allow_in_sample
  )
  core_summary <- if (isTRUE(return_draws)) scored$summary else scored
  key <- paste(core_summary$game_pk, core_summary$pitch_order, sep = "|")
  local_key <- paste(local$game_pk, local$pitch_order, sep = "|")
  order_index <- match(key, local_key)
  if (anyNA(order_index) || anyDuplicated(order_index)) {
    stop("Defense scoring rows lost alignment at the internal model boundary")
  }
  result <- data.table::copy(local[order_index])
  prediction_columns <- grep(
    paste0(
      "^(challenge_probability_|player_threshold_inches_|sigma_inches_|",
      "total_threshold_inches_|batter_seen_in_training$|team_seen_in_training$|",
      "umpire_seen_in_training$|catcher_seen_in_training$|",
      "player_reliability_tier$|player_low_reliability_flag$|fallback_|",
      "sigma_model$|margin_limit_inches$|local_discrimination_domain$|",
      "scoring_regime$)"
    ),
    names(core_summary), value = TRUE
  )
  result <- cbind(result, core_summary[, ..prediction_columns])
  rename <- c(
    player_threshold_inches_mean = "defense_team_threshold_inches_mean",
    player_threshold_inches_lower_95 = "defense_team_threshold_inches_lower_95",
    player_threshold_inches_median = "defense_team_threshold_inches_median",
    player_threshold_inches_upper_95 = "defense_team_threshold_inches_upper_95",
    batter_seen_in_training = "defense_team_seen_in_training",
    team_seen_in_training = "opponent_team_seen_in_training",
    player_reliability_tier = "defense_team_reliability_tier",
    player_low_reliability_flag = "defense_team_low_reliability_flag"
  )
  present <- intersect(names(rename), names(result))
  data.table::setnames(result, present, unname(rename[present]))
  result[, decision_unit := "fielding_team"]
  tail <- defense_challenge_discrimination_1d_tail_diagnostics(
    normalized, fit$margin_limit_inches
  )
  if (isTRUE(return_draws)) {
    return(list(
      summary = result[], draw_id = scored$draw_id,
      challenge_probability = scored$challenge_probability,
      defense_team_threshold_inches = scored$player_threshold_inches,
      sigma_inches = scored$sigma_inches,
      total_threshold_inches = scored$total_threshold_inches,
      context_shift_inches = scored$context_shift_inches,
      opponent_team_shift_inches = scored$team_shift_inches,
      umpire_shift_inches = scored$umpire_shift_inches,
      catcher_shift_inches = scored$catcher_shift_inches,
      tail_diagnostics = tail
    ))
  }
  data.table::setattr(result, "tail_diagnostics", tail)
  result[]
}

make_defense_challenge_discrimination_1d_game_folds <- function(
  rows, folds = 5L, seed = 20260825L, margin_limit_inches = 3,
  stake_log_odds_epsilon = defense_challenge_discrimination_1d_stake_epsilon()
) {
  x <- normalize_defense_challenge_discrimination_1d_rows(
    rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  limit <- validate_challenge_discrimination_1d_margin_limit(
    margin_limit_inches
  )
  games <- sort(unique(x$game_pk))
  folds <- as.integer(folds)
  if (length(folds) != 1L || is.na(folds) || folds < 2L ||
      length(games) < folds) {
    stop("Defense game folds need at least two folds and enough games")
  }
  set.seed(seed)
  out <- data.table::data.table(
    game_pk = sample(games),
    fold = rep(seq_len(folds), length.out = length(games)),
    margin_limit_inches = limit,
    stake_log_odds_epsilon = stake_log_odds_epsilon,
    decision_side = "defense_called_ball"
  )
  counts <- x[, .(
    eligible_margin_rows = .N,
    local_model_rows = sum(abs(defense_margin_inches) <= limit),
    tail_diagnostic_rows = sum(abs(defense_margin_inches) > limit)
  ), by = game_pk]
  out <- merge(out, counts, by = "game_pk", all.x = TRUE, sort = FALSE)
  data.table::setorder(out, game_pk)
  validate_defense_challenge_discrimination_1d_game_folds(out, folds)
  out[]
}

validate_defense_challenge_discrimination_1d_game_folds <- function(
  fold_table, expected_folds = 5L, expected_games = NULL
) {
  x <- data.table::copy(data.table::as.data.table(fold_table))
  validate_challenge_discrimination_1d_game_folds(x, expected_folds)
  if ("decision_side" %in% names(x) &&
      !identical(unique(as.character(x$decision_side)), "defense_called_ball")) {
    stop("Fold table is not a defense called-ball fold table")
  }
  if (!is.null(expected_games) &&
      !setequal(as.character(x$game_pk), as.character(expected_games))) {
    stop("Defense fold table must cover exactly the eligible games")
  }
  invisible(TRUE)
}

split_defense_challenge_discrimination_1d_fold <- function(
  rows, fold_table, heldout_fold, margin_limit_inches = 3,
  stake_log_odds_epsilon = defense_challenge_discrimination_1d_stake_epsilon()
) {
  x <- normalize_defense_challenge_discrimination_1d_rows(
    rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  folds <- data.table::copy(data.table::as.data.table(fold_table))
  stop_if_missing_columns(folds, c("game_pk", "fold"), "defense game folds")
  if (!nrow(folds) || anyNA(folds$fold) || any(as.integer(folds$fold) < 1L)) {
    stop("Defense game folds contain invalid fold identifiers")
  }
  fold_count <- max(as.integer(folds$fold))
  validate_defense_challenge_discrimination_1d_game_folds(
    folds, fold_count, unique(x$game_pk)
  )
  limit <- validate_challenge_discrimination_1d_margin_limit(
    margin_limit_inches
  )
  if ("margin_limit_inches" %in% names(folds) &&
      !identical(unique(as.numeric(folds$margin_limit_inches)), limit)) {
    stop("Defense split must use the margin limit recorded by its folds")
  }
  if ("stake_log_odds_epsilon" %in% names(folds) &&
      !identical(
        unique(as.numeric(folds$stake_log_odds_epsilon)),
        as.numeric(stake_log_odds_epsilon)
      )) {
    stop("Defense split must use the stake epsilon recorded by its folds")
  }
  heldout_fold <- as.integer(heldout_fold)
  if (length(heldout_fold) != 1L || !heldout_fold %in% folds$fold) {
    stop("heldout_fold is absent from the defense fold table")
  }
  index <- folds$fold[match(x$game_pk, as.character(folds$game_pk))]
  if (anyNA(index)) stop("Defense fold table does not cover every eligible row")
  train <- x[index != heldout_fold]
  heldout <- x[index == heldout_fold]
  if (length(intersect(unique(train$game_pk), unique(heldout$game_pk)))) {
    stop("Defense training and held-out games overlap")
  }
  summarize_split <- function(z, label) data.table::data.table(
    split = label,
    eligible_margin_rows = nrow(z),
    local_model_rows = sum(abs(z$defense_margin_inches) <= limit),
    tail_diagnostic_rows = sum(abs(z$defense_margin_inches) > limit)
  )
  list(
    train = train[], heldout = heldout[], heldout_fold = heldout_fold,
    margin_limit_inches = limit,
    split_summary = data.table::rbindlist(list(
      summarize_split(train, "training"),
      summarize_split(heldout, "heldout")
    )),
    training_tail_diagnostics =
      defense_challenge_discrimination_1d_tail_diagnostics(train, limit),
    heldout_tail_diagnostics =
      defense_challenge_discrimination_1d_tail_diagnostics(heldout, limit)
  )
}

defense_challenge_discrimination_1d_game_log_loss <- function(
  scored, probability_column = "challenge_probability_mean"
) {
  x <- data.table::copy(data.table::as.data.table(scored))
  stop_if_missing_columns(
    x, c("game_pk", "challenged", probability_column),
    "defense held-out scores"
  )
  x[, log_loss := challenge_discrimination_1d_log_loss(
    challenged, get(probability_column)
  )]
  games <- data.table::uniqueN(x$game_pk)
  if (games < 2L) stop("Defense clustered scoring needs at least two games")
  estimate <- mean(x$log_loss)
  cluster <- x[, .(residual_sum = sum(log_loss - estimate)), by = game_pk]
  standard_error <- sqrt(
    games / (games - 1) * sum(cluster$residual_sum^2) / nrow(x)^2
  )
  data.table::data.table(
    heldout_rows = nrow(x), heldout_games = games,
    mean_log_loss = estimate, game_clustered_se = standard_error
  )
}
