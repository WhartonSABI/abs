challenge_discrimination_1d_allowed_columns <- function() {
  c(
    "game_pk", "pitch_order", "at_bat_number", "pitch_number",
    "batter_id", "bat_team_id", "initial_call", "challenged",
    "edge_distance_inches", "plate_x", "plate_z", "sz_top", "sz_bot",
    "stake_G", "inventory_loss", "sampling_offset",
    "inning", "outs_before", "balls_before", "strikes_before",
    "adverse_challenges_before", "score_margin", "pitch_type",
    "pitch_family", "matchup", "stand", "p_throws", "umpire_id",
    "catcher_id"
  )
}

challenge_discrimination_1d_outcome_columns <- function() {
  c(
    "abs_call", "final_call", "description", "is_overturned",
    "challenge_outcome", "official_abs_call", "official_outcome",
    "official_success", "geometry_success", "actual_wrong", "overturned",
    "challenge_success", "outcome"
  )
}

challenge_discrimination_1d_default_context <- function() {
  list(
    categorical = c("count_state", "pitch_family", "matchup"),
    numeric = c(
      "challenge_cost_log_odds", "total_stake", "inning", "score_margin"
    )
  )
}

challenge_discrimination_1d_stake_epsilon <- function() 1e-6

challenge_discrimination_1d_reliability <- function(
  opportunities, challenges, shrinkage_exposure = 100L
) {
  opportunities <- as.integer(opportunities)
  challenges <- as.integer(challenges)
  if (
    length(opportunities) != length(challenges) || anyNA(opportunities) ||
      anyNA(challenges) || any(opportunities < 1L) || any(challenges < 0L) ||
      any(challenges > opportunities)
  ) {
    stop("Player reliability requires valid opportunity and challenge counts")
  }
  tier <- data.table::fcase(
    opportunities >= 250L & challenges >= 10L, "high",
    opportunities >= 100L & challenges >= 3L, "moderate",
    default = "limited"
  )
  fallback <- opportunities < 50L | challenges == 0L
  reason <- data.table::fcase(
    challenges == 0L, "no_observed_challenges",
    opportunities < 50L, "fewer_than_50_opportunities",
    default = "hierarchical_partial_pooling"
  )
  data.table::data.table(
    reliability_tier = tier,
    exposure_reliability_weight = opportunities /
      (opportunities + as.numeric(shrinkage_exposure)),
    fallback_flag = fallback,
    fallback_reason = reason
  )
}

normalize_challenge_discrimination_1d_rows <- function(
  rows, require_action = TRUE, geometry_tolerance = 1e-8,
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon()
) {
  raw <- data.table::copy(data.table::as.data.table(rows))
  source_rows <- nrow(raw)
  if (!source_rows) stop("Challenge-discrimination input is empty")
  stake_log_odds_epsilon <- as.numeric(stake_log_odds_epsilon)
  if (
    length(stake_log_odds_epsilon) != 1L ||
      !is.finite(stake_log_odds_epsilon) ||
      stake_log_odds_epsilon <= 0 || stake_log_odds_epsilon >= 0.5
  ) {
    stop("stake_log_odds_epsilon must be one number strictly between 0 and 0.5")
  }
  if (!isTRUE(require_action) && !"challenged" %in% names(raw)) {
    raw[, challenged := NA_integer_]
  }
  required <- c(
    "game_pk", "pitch_order", "batter_id", "bat_team_id", "initial_call",
    "stake_G", "inventory_loss"
  )
  if (isTRUE(require_action)) required <- c(required, "challenged")
  stop_if_missing_columns(raw, required, "1D challenge-discrimination input")
  has_precomputed_margin <- "edge_distance_inches" %in% names(raw)
  geometry_columns <- c("plate_x", "plate_z", "sz_top", "sz_bot")
  has_geometry <- all(geometry_columns %in% names(raw))
  if (!has_precomputed_margin && !has_geometry) {
    stop(
      "1D challenge-discrimination input needs exact edge_distance_inches ",
      "or the four tracked geometry columns"
    )
  }

  retained <- intersect(challenge_discrimination_1d_allowed_columns(), names(raw))
  x <- raw[, ..retained]
  for (column in c("umpire_id", "catcher_id")) {
    if (!column %in% names(x)) x[, (column) := "__UNKNOWN__"]
  }
  if (!"sampling_offset" %in% names(x)) x[, sampling_offset := 0]
  if (any(!is.finite(as.numeric(x$sampling_offset))) ||
      any(abs(as.numeric(x$sampling_offset)) > 1e-12)) {
    stop(
      "The 1D probit model requires all eligible pitches; nonzero ",
      "case-control sampling offsets are not supported"
    )
  }
  x[, sampling_offset := NULL]

  if (!"edge_distance_inches" %in% names(x)) {
    x[, edge_distance_inches := NA_real_]
  }
  supplied_margin <- as.numeric(x$edge_distance_inches)
  exact_margin <- rep(NA_real_, nrow(x))
  if (has_geometry) {
    exact_margin <- abs_edge_distance_inches(
      as.numeric(x$plate_x), as.numeric(x$plate_z),
      as.numeric(x$sz_top), as.numeric(x$sz_bot)
    )
    disagreement <- is.finite(exact_margin) & is.finite(supplied_margin) &
      abs(exact_margin - supplied_margin) > geometry_tolerance
    if (any(disagreement)) {
      stop(
        "Precomputed edge distances disagree with exact rounded ABS geometry"
      )
    }
  }
  use_geometry <- is.finite(exact_margin)
  margin <- supplied_margin
  margin[use_geometry] <- exact_margin[use_geometry]
  x[, `:=`(
    edge_distance_inches = margin,
    margin_inches = margin,
    margin_source = ifelse(
      use_geometry, "exact_rounded_geometry", "precomputed_exact_edge"
    )
  )]

  if (!"balls_before" %in% names(x)) x[, balls_before := NA_integer_]
  if (!"strikes_before" %in% names(x)) x[, strikes_before := NA_integer_]
  x[, count_state := ifelse(
    is.finite(as.numeric(balls_before)) &
      is.finite(as.numeric(strikes_before)),
    paste0(as.integer(balls_before), "-", as.integer(strikes_before)),
    "unknown"
  )]
  if (!"pitch_family" %in% names(x)) {
    if ("pitch_type" %in% names(x)) {
      x[, pitch_family := as.character(pitch_type)]
    } else {
      x[, pitch_family := "unknown"]
    }
  }
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
  for (column in c("inning", "score_margin")) {
    if (!column %in% names(x)) x[, (column) := 0]
  }

  x[, `:=`(
    game_pk = as.character(game_pk),
    batter_id = as.character(batter_id),
    bat_team_id = as.character(bat_team_id),
    umpire_id = as.character(umpire_id),
    catcher_id = as.character(catcher_id),
    initial_call = as.character(initial_call),
    challenged = as.integer(challenged),
    pitch_family = as.character(pitch_family),
    matchup = as.character(matchup),
    stake_G = as.numeric(stake_G),
    inventory_loss = as.numeric(inventory_loss)
  )]
  total_stake <- x$stake_G + x$inventory_loss
  q_star <- x$inventory_loss / total_stake
  q_star_clipped <- pmin(
    1 - stake_log_odds_epsilon,
    pmax(stake_log_odds_epsilon, q_star)
  )
  x[, `:=`(
    total_stake = total_stake,
    q_star = q_star,
    q_star_log_odds_value = q_star_clipped,
    q_star_log_odds_clipped = is.finite(q_star) &
      abs(q_star_clipped - q_star) > 0,
    q_star_log_odds_epsilon = stake_log_odds_epsilon,
    challenge_cost_log_odds = stats::qlogis(q_star_clipped)
  )]
  for (column in c("bat_team_id", "umpire_id", "catcher_id")) {
    value <- x[[column]]
    value[is.na(value) | !nzchar(value)] <- "__UNKNOWN__"
    x[, (column) := value]
  }
  action_ok <- if (isTRUE(require_action)) {
    !is.na(x$challenged) & x$challenged %in% 0:1
  } else {
    rep(TRUE, nrow(x))
  }
  inventory_ok <- rep(TRUE, nrow(x))
  if ("adverse_challenges_before" %in% names(x)) {
    inventory_ok <- !is.na(x$adverse_challenges_before) &
      as.numeric(x$adverse_challenges_before) >= 1
  }
  eligible <-
    x$initial_call == "called_strike" &
    !is.na(x$game_pk) & nzchar(x$game_pk) &
    !is.na(x$batter_id) & nzchar(x$batter_id) &
    is.finite(x$margin_inches) &
    is.finite(x$stake_G) & x$stake_G >= 0 &
    is.finite(x$inventory_loss) & x$inventory_loss >= 0 &
    is.finite(x$total_stake) & x$total_stake > 0 &
    is.finite(x$challenge_cost_log_odds) &
    inventory_ok & action_ok
  x <- x[eligible]
  if (!nrow(x)) stop("No eligible taken called strikes remain")
  data.table::setorder(x, game_pk, pitch_order)
  if (anyDuplicated(x[, .(game_pk, pitch_order)])) {
    stop("1D challenge-discrimination rows contain duplicate pitch keys")
  }
  x[, taken_called_strike := TRUE]
  forbidden <- intersect(
    challenge_discrimination_1d_outcome_columns(), names(x)
  )
  if (length(forbidden)) {
    stop("Outcome columns survived the challenge-discrimination allowlist")
  }
  data.table::setattr(x, "eligibility", data.table::data.table(
    source_rows = source_rows,
    eligible_rows = nrow(x),
    excluded_rows = source_rows - nrow(x),
    excluded_for_missing_location_or_other_eligibility = source_rows - nrow(x)
  ))
  x[]
}

validate_challenge_discrimination_1d_context <- function(
  rows, categorical_context, numeric_context
) {
  categorical_context <- unique(as.character(categorical_context))
  numeric_context <- unique(as.character(numeric_context))
  selected <- c(categorical_context, numeric_context)
  if (anyDuplicated(selected)) {
    stop("A context column cannot be both categorical and numeric")
  }
  forbidden <- c(
    challenge_discrimination_1d_outcome_columns(), "challenged",
    "edge_distance_inches", "margin_inches", "initial_call", "batter_id",
    "bat_team_id", "umpire_id", "catcher_id", "game_pk", "pitch_order"
  )
  leaked <- intersect(selected, forbidden)
  if (length(leaked)) {
    stop(
      "Invalid or outcome-bearing 1D context columns: ",
      paste(leaked, collapse = ", ")
    )
  }
  stop_if_missing_columns(rows, selected, "1D challenge-discrimination context")
  list(categorical = categorical_context, numeric = numeric_context)
}

challenge_discrimination_1d_group_table <- function(rows, column) {
  id_name <- sub("_id$", "", column)
  out <- rows[, .(
    training_opportunities = .N,
    training_challenges = sum(challenged),
    training_challenge_rate = mean(challenged)
  ), by = column]
  data.table::setnames(out, column, paste0(id_name, "_id"))
  data.table::setorderv(out, paste0(id_name, "_id"))
  out[, group_index := .I]
  out[]
}

validate_challenge_discrimination_1d_margin_limit <- function(
  margin_limit_inches
) {
  margin_limit_inches <- as.numeric(margin_limit_inches)
  if (
    length(margin_limit_inches) != 1L || is.na(margin_limit_inches) ||
      margin_limit_inches <= 0
  ) {
    stop("margin_limit_inches must be one positive number or Inf")
  }
  margin_limit_inches
}

challenge_discrimination_1d_tail_diagnostics <- function(
  rows, margin_limit_inches = 3
) {
  margin_limit_inches <- validate_challenge_discrimination_1d_margin_limit(
    margin_limit_inches
  )
  x <- data.table::copy(data.table::as.data.table(rows))
  stop_if_missing_columns(
    x, c("margin_inches", "challenged"), "1D tail diagnostics"
  )
  tail <- x[abs(margin_inches) > margin_limit_inches]
  if (!nrow(tail)) {
    return(data.table::data.table(
      margin_limit_inches = margin_limit_inches,
      tail_side = character(),
      tail_opportunities = integer(),
      tail_challenges = integer(),
      tail_challenge_rate = numeric(),
      mean_absolute_margin_inches = numeric(),
      maximum_absolute_margin_inches = numeric()
    ))
  }
  tail[, tail_side := ifelse(
    margin_inches < -margin_limit_inches,
    "far_inside_called_strike", "far_outside_called_strike"
  )]
  tail[, .(
    margin_limit_inches = margin_limit_inches,
    tail_opportunities = .N,
    tail_challenges = sum(challenged),
    tail_challenge_rate = mean(challenged),
    mean_absolute_margin_inches = mean(abs(margin_inches)),
    maximum_absolute_margin_inches = max(abs(margin_inches))
  ), by = tail_side][]
}

partition_challenge_discrimination_1d_domain <- function(
  rows, margin_limit_inches = 3, label = "1D challenge-discrimination"
) {
  margin_limit_inches <- validate_challenge_discrimination_1d_margin_limit(
    margin_limit_inches
  )
  x <- data.table::copy(data.table::as.data.table(rows))
  stop_if_missing_columns(x, "margin_inches", label)
  local <- x[abs(margin_inches) <= margin_limit_inches]
  tail <- x[abs(margin_inches) > margin_limit_inches]
  if (!nrow(local)) {
    stop(label, " has no rows within +/-", margin_limit_inches, " inches")
  }
  list(
    rows = local[],
    tail_rows = tail[],
    tail_diagnostics = challenge_discrimination_1d_tail_diagnostics(
      x, margin_limit_inches
    ),
    margin_limit_inches = margin_limit_inches
  )
}

prepare_challenge_discrimination_1d <- function(
  rows, sigma_model = c("hierarchical", "common"),
  categorical_context = challenge_discrimination_1d_default_context()$categorical,
  numeric_context = challenge_discrimination_1d_default_context()$numeric,
  margin_limit_inches = 3,
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon()
) {
  sigma_model <- match.arg(sigma_model)
  eligible_rows <- normalize_challenge_discrimination_1d_rows(
    rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  eligibility <- data.table::copy(attr(eligible_rows, "eligibility"))
  domain <- partition_challenge_discrimination_1d_domain(
    eligible_rows, margin_limit_inches,
    label = "1D challenge-discrimination training data"
  )
  x <- domain$rows
  eligibility[, `:=`(
    model_rows = nrow(domain$rows),
    tail_diagnostic_rows = nrow(domain$tail_rows),
    margin_limit_inches = domain$margin_limit_inches
  )]
  context_columns <- validate_challenge_discrimination_1d_context(
    x, categorical_context, numeric_context
  )
  context <- fit_continuous_design(
    x, categorical = context_columns$categorical,
    numeric = context_columns$numeric
  )

  player_table <- x[, .(
    training_opportunities = .N,
    training_challenges = sum(challenged),
    training_challenge_rate = mean(challenged),
    mean_margin_inches = mean(margin_inches),
    sd_margin_inches = if (.N > 1L) stats::sd(margin_inches) else NA_real_,
    outside_zone_opportunities = sum(margin_inches > 0)
  ), by = batter_id]
  data.table::setorder(player_table, batter_id)
  player_table[, player_index := .I]
  player_table <- cbind(
    player_table,
    challenge_discrimination_1d_reliability(
      player_table$training_opportunities,
      player_table$training_challenges
    )
  )
  team_table <- challenge_discrimination_1d_group_table(x, "bat_team_id")
  data.table::setnames(team_table, "bat_team_id", "team_id")
  umpire_table <- challenge_discrimination_1d_group_table(x, "umpire_id")
  catcher_table <- challenge_discrimination_1d_group_table(x, "catcher_id")

  x[, `:=`(
    player_index = match(batter_id, player_table$batter_id),
    team_index = match(bat_team_id, team_table$team_id),
    umpire_index = match(umpire_id, umpire_table$umpire_id),
    catcher_index = match(catcher_id, catcher_table$catcher_id)
  )]
  if (anyNA(x[, .(player_index, team_index, umpire_index, catcher_index)])) {
    stop("Internal group indexing failed for the 1D challenge model")
  }

  stan_data <- list(
    N = nrow(x),
    P = nrow(player_table),
    T = nrow(team_table),
    U = nrow(umpire_table),
    C = nrow(catcher_table),
    K = ncol(context$matrix),
    challenged = as.integer(x$challenged),
    player = as.integer(x$player_index),
    team = as.integer(x$team_index),
    umpire = as.integer(x$umpire_index),
    catcher = as.integer(x$catcher_index),
    margin = as.numeric(x$margin_inches),
    X = context$matrix,
    use_player_sigma = as.integer(sigma_model == "hierarchical")
  )
  list(
    data = stan_data,
    rows = x,
    player_table = player_table,
    team_table = team_table,
    umpire_table = umpire_table,
    catcher_table = catcher_table,
    context_specification = context$specification,
    context_columns = context_columns,
    context_design_columns = colnames(context$matrix),
    sigma_model = sigma_model,
    margin_limit_inches = domain$margin_limit_inches,
    stake_log_odds_epsilon = stake_log_odds_epsilon,
    eligibility = eligibility,
    tail_rows = domain$tail_rows,
    tail_diagnostics = domain$tail_diagnostics,
    information_set = c(
      "taken_initial_called_strike", "batter_action", "exact_signed_margin",
      "count", "pitch_family", "matchup", "decision_stakes",
      "team", "umpire", "catcher"
    ),
    excluded_information = challenge_discrimination_1d_outcome_columns()
  )
}

default_challenge_discrimination_1d_stan_file <- function() {
  root <- tryCatch(
    rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd()),
    error = function(error) getwd()
  )
  file.path(
    root, "scripts", "stan", "hierarchical_challenge_discrimination_1d.stan"
  )
}

challenge_discrimination_1d_prior_specification <- function() {
  list(
    population_threshold_inches = "normal(4, 2)",
    population_log_sigma = "normal(log(3), 0.4)",
    between_player_threshold_sd = "normal+(0, 1.25)",
    between_player_log_sigma_sd = "normal+(0, 0.30)",
    player_correlation = "LKJ(4)",
    context_shift_inches = "normal(0, 1)",
    team_shift_sd_inches = "normal+(0, 0.35)",
    umpire_shift_sd_inches = "normal+(0, 0.50)",
    catcher_shift_sd_inches = "normal+(0, 0.35)"
  )
}

challenge_discrimination_1d_canonical_columns <- function(rows, columns) {
  x <- data.table::as.data.table(rows)
  stop_if_missing_columns(x, columns, "1D cache fingerprint rows")
  result <- lapply(columns, function(column) {
    value <- x[[column]]
    if (is.factor(value)) value <- as.character(value)
    unname(value)
  })
  names(result) <- columns
  result
}

challenge_discrimination_1d_training_fingerprint <- function(bundle) {
  required <- c(
    "game_pk", "pitch_order", "batter_id", "bat_team_id", "umpire_id",
    "catcher_id", "challenged", "margin_inches", "stake_G",
    "inventory_loss", "q_star", "challenge_cost_log_odds", "total_stake"
  )
  tail_required <- intersect(required, names(bundle$tail_rows))
  payload <- list(
    fingerprint_schema = 1L,
    sigma_model = bundle$sigma_model,
    margin_limit_inches = bundle$margin_limit_inches,
    stake_log_odds_epsilon = bundle$stake_log_odds_epsilon,
    context_design_columns = unname(bundle$context_design_columns),
    model_rows = challenge_discrimination_1d_canonical_columns(
      bundle$rows, required
    ),
    tail_rows = challenge_discrimination_1d_canonical_columns(
      bundle$tail_rows, tail_required
    ),
    context_matrix = unname(as.matrix(bundle$data$X))
  )
  digest::digest(payload, algo = "sha256", serialize = TRUE)
}

challenge_discrimination_1d_game_fingerprint <- function(bundle) {
  games <- sort(unique(as.character(c(
    bundle$rows$game_pk, bundle$tail_rows$game_pk
  ))))
  digest::digest(
    list(fingerprint_schema = 1L, training_games = games),
    algo = "sha256", serialize = TRUE
  )
}

challenge_discrimination_1d_stan_fingerprint <- function(stan_file) {
  if (length(stan_file) != 1L || !file.exists(stan_file)) {
    stop("Cannot fingerprint missing 1D Stan source: ", stan_file)
  }
  digest::digest(file = stan_file, algo = "sha256", serialize = FALSE)
}

challenge_discrimination_1d_cache_identity <- function(
  bundle, backend, seed, stan_file, sampling_controls = list()
) {
  list(
    identity_schema = 1L,
    training_data_sha256 =
      challenge_discrimination_1d_training_fingerprint(bundle),
    training_games_sha256 =
      challenge_discrimination_1d_game_fingerprint(bundle),
    backend = as.character(backend),
    seed = as.integer(seed),
    sampling_controls = sampling_controls,
    stan_source_sha256 =
      challenge_discrimination_1d_stan_fingerprint(stan_file)
  )
}

challenge_discrimination_1d_sampling_identity <- function(
  chains, iter_warmup, iter_sampling, adapt_delta, max_treedepth
) {
  list(
    chains = as.integer(chains),
    iter_warmup = as.integer(iter_warmup),
    iter_sampling = as.integer(iter_sampling),
    adapt_delta = as.numeric(adapt_delta),
    max_treedepth = as.integer(max_treedepth)
  )
}

assert_challenge_discrimination_1d_cache_identity <- function(
  cached, expected, file
) {
  if (!inherits(cached, "challenge_discrimination_1d_fit")) {
    stop(
      "Existing 1D cache is not a challenge-discrimination fit: ", file,
      ". Set force_refit=TRUE to replace it."
    )
  }
  if (is.null(cached$cache_identity) ||
      !identical(cached$cache_identity, expected)) {
    stop(
      "1D cache identity mismatch for ", file,
      "; training data/games, backend, seed, sampling controls, or Stan ",
      "source changed. ",
      "Set force_refit=TRUE to refit explicitly."
    )
  }
  invisible(TRUE)
}

fit_challenge_discrimination_1d <- function(
  rows, sigma_model = c("hierarchical", "common"),
  categorical_context = challenge_discrimination_1d_default_context()$categorical,
  numeric_context = challenge_discrimination_1d_default_context()$numeric,
  margin_limit_inches = 3,
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon(),
  backend = c("cmdstanr", "rstan"), chains = 4L,
  parallel_chains = chains, iter_warmup = 1000L, iter_sampling = 1000L,
  seed = 20260825L, adapt_delta = 0.97, max_treedepth = 12L,
  refresh = 100L, stan_file = NULL, file = NULL, force_refit = FALSE
) {
  sigma_model <- match.arg(sigma_model)
  backend <- match.arg(backend)
  bundle <- prepare_challenge_discrimination_1d(
    rows, sigma_model = sigma_model,
    categorical_context = categorical_context,
    numeric_context = numeric_context,
    margin_limit_inches = margin_limit_inches,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  if (is.null(stan_file)) {
    stan_file <- default_challenge_discrimination_1d_stan_file()
  }
  cache_identity <- challenge_discrimination_1d_cache_identity(
    bundle, backend = backend, seed = seed, stan_file = stan_file,
    sampling_controls = challenge_discrimination_1d_sampling_identity(
      chains, iter_warmup, iter_sampling, adapt_delta, max_treedepth
    )
  )
  if (!is.null(file) && file.exists(file) && !isTRUE(force_refit)) {
    cached <- readRDS(file)
    assert_challenge_discrimination_1d_cache_identity(
      cached, cache_identity, file
    )
    return(cached)
  }
  stan_fit <- continuous_stan_fit(
    stan_file = stan_file, data = bundle$data, backend = backend,
    chains = chains, parallel_chains = parallel_chains,
    iter_warmup = iter_warmup, iter_sampling = iter_sampling, seed = seed,
    adapt_delta = adapt_delta, max_treedepth = max_treedepth,
    refresh = refresh, init = 0
  )
  fit <- list(
    stan_fit = stan_fit,
    sigma_model = sigma_model,
    margin_limit_inches = bundle$margin_limit_inches,
    stake_log_odds_epsilon = bundle$stake_log_odds_epsilon,
    player_table = bundle$player_table,
    team_table = bundle$team_table,
    umpire_table = bundle$umpire_table,
    catcher_table = bundle$catcher_table,
    context_specification = bundle$context_specification,
    context_columns = bundle$context_columns,
    context_design_columns = bundle$context_design_columns,
    training_games = sort(unique(as.character(c(
      bundle$rows$game_pk, bundle$tail_rows$game_pk
    )))),
    training_rows = nrow(bundle$rows),
    eligibility = bundle$eligibility,
    tail_diagnostics = bundle$tail_diagnostics,
    cache_identity = cache_identity,
    training_data_fingerprint = cache_identity$training_data_sha256,
    training_game_fingerprint = cache_identity$training_games_sha256,
    stan_source_fingerprint = cache_identity$stan_source_sha256,
    information_set = bundle$information_set,
    excluded_information = bundle$excluded_information,
    priors = challenge_discrimination_1d_prior_specification(),
    backend = backend,
    controls = list(
      chains = chains, parallel_chains = parallel_chains,
      iter_warmup = iter_warmup, iter_sampling = iter_sampling, seed = seed,
      adapt_delta = adapt_delta, max_treedepth = max_treedepth
    )
  )
  class(fit) <- "challenge_discrimination_1d_fit"
  if (!is.null(file)) saveRDS(fit, file)
  fit
}

challenge_discrimination_1d_source_draw_ids <- function(stan_fit, draw_count) {
  for (attribute in c("source_draw_id", "draw_id", "global_draw_id")) {
    value <- attr(stan_fit, attribute, exact = TRUE)
    if (
      length(value) == draw_count && !anyNA(value) &&
        !anyDuplicated(value)
    ) {
      return(value)
    }
  }
  if (inherits(stan_fit, "CmdStanMCMC")) {
    metadata <- tryCatch(
      stan_fit$draws(variables = "mu_threshold", format = "draws_df"),
      error = function(error) NULL
    )
    if (!is.null(metadata) && ".draw" %in% names(metadata) &&
        nrow(metadata) == draw_count && !anyDuplicated(metadata$.draw)) {
      return(as.integer(metadata$.draw))
    }
  }
  seq_len(draw_count)
}

challenge_discrimination_1d_thin_draw_matrix <- function(
  draws, source_draw_id, ndraws = NULL, seed = 20260825L
) {
  draws <- as.matrix(draws)
  if (
    length(source_draw_id) != nrow(draws) || anyNA(source_draw_id) ||
      anyDuplicated(source_draw_id)
  ) {
    stop("Source posterior draw IDs must align uniquely to the draw matrix")
  }
  selected <- seq_len(nrow(draws))
  if (!is.null(ndraws)) {
    ndraws <- as.integer(ndraws)
    if (length(ndraws) != 1L || is.na(ndraws) || ndraws < 1L) {
      stop("ndraws must be one positive integer")
    }
    if (ndraws < nrow(draws)) {
      set.seed(seed)
      selected <- sort(sample.int(nrow(draws), ndraws))
    }
  }
  result <- draws[selected, , drop = FALSE]
  data.table::setattr(
    result, "source_draw_id", source_draw_id[selected]
  )
  result
}

draws_challenge_discrimination_1d <- function(
  fit, ndraws = NULL, seed = 20260825L
) {
  if (!inherits(fit, "challenge_discrimination_1d_fit")) {
    stop("fit must be a challenge_discrimination_1d_fit")
  }
  variables <- c(
    "mu_threshold", "mu_log_sigma", "tau_player",
    "rho_threshold_log_sigma", "threshold_player", "sigma_player",
    "team_shift", "umpire_shift", "catcher_shift"
  )
  if (length(fit$context_design_columns)) {
    variables <- c(variables, "beta_context")
  }
  full_draws <- continuous_draw_matrix(fit$stan_fit, variables)
  source_draw_id <- challenge_discrimination_1d_source_draw_ids(
    fit$stan_fit, nrow(full_draws)
  )
  matrix_draws <- challenge_discrimination_1d_thin_draw_matrix(
    full_draws, source_draw_id, ndraws, seed
  )
  source_draw_id <- attr(matrix_draws, "source_draw_id", exact = TRUE)
  tau <- continuous_extract_vector(matrix_draws, "tau_player", 2L)
  list(
    draw_id = source_draw_id,
    mu_threshold = continuous_extract_scalar(matrix_draws, "mu_threshold"),
    mu_log_sigma = continuous_extract_scalar(matrix_draws, "mu_log_sigma"),
    tau_threshold = tau[, 1L],
    tau_log_sigma = tau[, 2L],
    rho_threshold_log_sigma = continuous_extract_scalar(
      matrix_draws, "rho_threshold_log_sigma"
    ),
    threshold_player = continuous_extract_vector(
      matrix_draws, "threshold_player", nrow(fit$player_table)
    ),
    sigma_player = continuous_extract_vector(
      matrix_draws, "sigma_player", nrow(fit$player_table)
    ),
    beta_context = continuous_extract_vector(
      matrix_draws, "beta_context", length(fit$context_design_columns)
    ),
    team_shift = continuous_extract_vector(
      matrix_draws, "team_shift", nrow(fit$team_table)
    ),
    umpire_shift = continuous_extract_vector(
      matrix_draws, "umpire_shift", nrow(fit$umpire_table)
    ),
    catcher_shift = continuous_extract_vector(
      matrix_draws, "catcher_shift", nrow(fit$catcher_table)
    )
  )
}

challenge_discrimination_1d_summary_block <- function(values, prefix) {
  values <- as.numeric(values)
  interval <- stats::quantile(
    values, c(0.025, 0.5, 0.975), names = FALSE, na.rm = TRUE
  )
  stats::setNames(
    c(mean(values, na.rm = TRUE), interval),
    paste0(prefix, c("mean", "lower_95", "median", "upper_95"))
  )
}

challenge_discrimination_1d_group_summary <- function(
  table, draws, id_column
) {
  probability <- continuous_probability_summary(
    t(draws), "threshold_shift_inches_"
  )
  cbind(data.table::copy(table), probability)[, group_index := NULL][]
}

summarize_challenge_discrimination_1d <- function(
  fit, ndraws = NULL, seed = 20260825L
) {
  posterior <- draws_challenge_discrimination_1d(fit, ndraws, seed)
  population_values <- c(
    challenge_discrimination_1d_summary_block(
      posterior$mu_threshold, "population_threshold_inches_"
    ),
    challenge_discrimination_1d_summary_block(
      exp(posterior$mu_log_sigma), "population_sigma_inches_"
    ),
    challenge_discrimination_1d_summary_block(
      posterior$tau_threshold, "between_player_threshold_sd_inches_"
    ),
    challenge_discrimination_1d_summary_block(
      posterior$tau_log_sigma, "between_player_log_sigma_sd_"
    ),
    challenge_discrimination_1d_summary_block(
      posterior$rho_threshold_log_sigma,
      "threshold_log_sigma_correlation_"
    )
  )
  population <- data.table::data.table(
    sigma_model = fit$sigma_model,
    margin_limit_inches = fit$margin_limit_inches,
    stake_log_odds_epsilon = fit$stake_log_odds_epsilon,
    training_rows = fit$training_rows,
    tail_diagnostic_rows = if (!is.null(fit$eligibility$tail_diagnostic_rows)) {
      fit$eligibility$tail_diagnostic_rows[[1L]]
    } else {
      NA_integer_
    },
    training_games = length(fit$training_games),
    players = nrow(fit$player_table),
    teams = nrow(fit$team_table),
    umpires = nrow(fit$umpire_table),
    catchers = nrow(fit$catcher_table)
  )
  population[, (names(population_values)) := as.list(population_values)]

  player_threshold <- continuous_probability_summary(
    t(posterior$threshold_player), "threshold_inches_"
  )
  player_sigma <- continuous_probability_summary(
    t(posterior$sigma_player), "sigma_inches_"
  )
  players <- cbind(
    data.table::copy(fit$player_table), player_threshold, player_sigma
  )
  players[, estimate_source := data.table::fcase(
    rep.int(fit$sigma_model == "common", .N), "league_common_sigma",
    fallback_flag, "hierarchical_shrunk_low_exposure",
    default = "hierarchical_player"
  )]
  list(
    population = population[],
    players = players[],
    teams = challenge_discrimination_1d_group_summary(
      fit$team_table, posterior$team_shift, "team_id"
    ),
    umpires = challenge_discrimination_1d_group_summary(
      fit$umpire_table, posterior$umpire_shift, "umpire_id"
    ),
    catchers = challenge_discrimination_1d_group_summary(
      fit$catcher_table, posterior$catcher_shift, "catcher_id"
    ),
    tail_diagnostics = data.table::copy(fit$tail_diagnostics)
  )
}

challenge_discrimination_1d_scoring_features <- function(rows, fit) {
  require_action <- "challenged" %in% names(rows)
  eligible_rows <- normalize_challenge_discrimination_1d_rows(
    rows, require_action = require_action,
    stake_log_odds_epsilon = fit$stake_log_odds_epsilon
  )
  domain <- partition_challenge_discrimination_1d_domain(
    eligible_rows, fit$margin_limit_inches,
    label = "1D challenge-discrimination scoring data"
  )
  list(
    rows = domain$rows,
    context = score_continuous_design(
      domain$rows, fit$context_specification
    ),
    tail_rows = domain$tail_rows,
    tail_diagnostics = domain$tail_diagnostics
  )
}

score_challenge_discrimination_1d <- function(
  fit, rows, ndraws = 400L, seed = 20260825L,
  new_player = c("population", "sample"), return_draws = FALSE,
  require_game_separation = TRUE, allow_in_sample = FALSE
) {
  if (!inherits(fit, "challenge_discrimination_1d_fit")) {
    stop("fit must be a challenge_discrimination_1d_fit")
  }
  new_player <- match.arg(new_player)
  if (
    length(require_game_separation) != 1L ||
      is.na(require_game_separation) ||
      length(allow_in_sample) != 1L || is.na(allow_in_sample)
  ) {
    stop("Scoring separation controls must be single logical values")
  }
  require_game_separation <- isTRUE(require_game_separation)
  allow_in_sample <- isTRUE(allow_in_sample)
  features <- challenge_discrimination_1d_scoring_features(rows, fit)
  x <- features$rows
  if (require_game_separation && !allow_in_sample &&
      (!length(fit$training_games) || anyNA(fit$training_games))) {
    stop("Cannot verify held-out scoring because training games are unavailable")
  }
  overlap <- intersect(
    as.character(fit$training_games), unique(as.character(x$game_pk))
  )
  if (require_game_separation && !allow_in_sample && length(overlap)) {
    stop("Challenge-discrimination training and held-out games overlap")
  }
  posterior <- draws_challenge_discrimination_1d(fit, ndraws, seed)
  draws <- length(posterior$draw_id)
  n <- nrow(x)
  player <- match(x$batter_id, fit$player_table$batter_id)
  team <- match(x$bat_team_id, fit$team_table$team_id)
  umpire <- match(x$umpire_id, fit$umpire_table$umpire_id)
  catcher <- match(x$catcher_id, fit$catcher_table$catcher_id)
  seen <- !is.na(player)

  threshold <- sigma <- matrix(NA_real_, n, draws)
  threshold[seen, ] <- t(posterior$threshold_player[, player[seen], drop = FALSE])
  sigma[seen, ] <- t(posterior$sigma_player[, player[seen], drop = FALSE])
  if (any(!seen)) {
    unseen_names <- sort(unique(x$batter_id[!seen]))
    unseen_index <- match(x$batter_id, unseen_names)
    set.seed(seed + 1L)
    for (draw in seq_len(draws)) {
      if (new_player == "sample") {
        z_threshold <- stats::rnorm(length(unseen_names))
        z_independent <- stats::rnorm(length(unseen_names))
        rho <- posterior$rho_threshold_log_sigma[[draw]]
        z_sigma <- rho * z_threshold +
          sqrt(pmax(0, 1 - rho^2)) * z_independent
        threshold_new <- posterior$mu_threshold[[draw]] +
          posterior$tau_threshold[[draw]] * z_threshold
        log_sigma_new <- posterior$mu_log_sigma[[draw]] +
          as.integer(fit$sigma_model == "hierarchical") *
            posterior$tau_log_sigma[[draw]] * z_sigma
      } else {
        threshold_new <- rep(
          posterior$mu_threshold[[draw]], length(unseen_names)
        )
        log_sigma_new <- rep(
          posterior$mu_log_sigma[[draw]], length(unseen_names)
        )
      }
      threshold[!seen, draw] <- threshold_new[unseen_index[!seen]]
      sigma[!seen, draw] <- exp(log_sigma_new[unseen_index[!seen]])
    }
  }

  context_shift <- if (ncol(features$context)) {
    features$context %*% t(posterior$beta_context)
  } else {
    matrix(0, n, draws)
  }
  group_draws <- function(index, values) {
    result <- matrix(0, n, draws)
    present <- !is.na(index)
    if (any(present)) {
      result[present, ] <- t(values[, index[present], drop = FALSE])
    }
    result
  }
  team_shift <- group_draws(team, posterior$team_shift)
  umpire_shift <- group_draws(umpire, posterior$umpire_shift)
  catcher_shift <- group_draws(catcher, posterior$catcher_shift)
  total_threshold <- threshold + context_shift + team_shift +
    umpire_shift + catcher_shift
  eta <- (matrix(x$margin_inches, n, draws) - total_threshold) / sigma
  probability <- matrix(
    stats::pnorm(as.numeric(eta)), nrow = n, ncol = draws
  )
  probability[] <- pmin(1 - 1e-12, pmax(1e-12, probability))

  result <- cbind(
    data.table::copy(x),
    continuous_probability_summary(probability, "challenge_probability_"),
    continuous_probability_summary(threshold, "player_threshold_inches_"),
    continuous_probability_summary(sigma, "sigma_inches_"),
    continuous_probability_summary(
      total_threshold, "total_threshold_inches_"
    )
  )
  reliability <- fit$player_table$reliability_tier[player]
  low_reliability <- fit$player_table$fallback_flag[player]
  result[, `:=`(
    batter_seen_in_training = seen,
    team_seen_in_training = !is.na(team),
    umpire_seen_in_training = !is.na(umpire),
    catcher_seen_in_training = !is.na(catcher),
    player_reliability_tier = ifelse(seen, reliability, "unseen"),
    player_low_reliability_flag = ifelse(seen, low_reliability, TRUE),
    fallback_flag = !seen,
    fallback_source = ifelse(
      seen, "hierarchical_partial_pooling",
      if (new_player == "population") "population_mean" else "population_sample"
    ),
    sigma_model = fit$sigma_model,
    margin_limit_inches = fit$margin_limit_inches,
    local_discrimination_domain = TRUE,
    stake_log_odds_epsilon = fit$stake_log_odds_epsilon,
    scoring_regime = if (length(overlap)) {
      "in_sample_explicit_opt_out"
    } else {
      "game_heldout"
    }
  )]
  if (isTRUE(return_draws)) {
    return(list(
      summary = result[],
      draw_id = posterior$draw_id,
      challenge_probability = probability,
      player_threshold_inches = threshold,
      sigma_inches = sigma,
      total_threshold_inches = total_threshold,
      context_shift_inches = context_shift,
      team_shift_inches = team_shift,
      umpire_shift_inches = umpire_shift,
      catcher_shift_inches = catcher_shift,
      tail_diagnostics = features$tail_diagnostics
    ))
  }
  data.table::setattr(result, "tail_diagnostics", features$tail_diagnostics)
  result[]
}

challenge_discrimination_1d_resolve_level <- function(
  value, levels, label
) {
  original_names <- names(value)
  value <- as.numeric(value)
  if (length(value) == 1L) return(rep.int(value, length(levels)))
  if (!is.null(original_names)) {
    matched <- match(levels, original_names)
    if (!anyNA(matched)) return(value[matched])
  }
  if (length(value) == length(levels)) return(value)
  stop(label, " must be scalar, level-aligned, or named by level")
}

challenge_discrimination_1d_resolve_beta <- function(
  value, design_columns
) {
  if (!length(design_columns)) return(numeric())
  original_names <- names(value)
  value <- as.numeric(value)
  if (length(value) == 1L) return(rep.int(value, length(design_columns)))
  if (!is.null(original_names)) {
    matched <- match(design_columns, original_names)
    if (!anyNA(matched)) return(value[matched])
  }
  if (length(value) == length(design_columns)) return(value)
  stop("context_beta must be scalar, design-aligned, or named by design column")
}

simulate_challenge_discrimination_1d <- function(
  rows, threshold_player = NULL, sigma_player = NULL,
  mu_threshold = 4, mu_log_sigma = log(3), tau_threshold = 1,
  tau_log_sigma = 0.25, rho_threshold_log_sigma = 0.25,
  context_beta = 0, team_shift = NULL, umpire_shift = NULL,
  catcher_shift = NULL, team_sd = 0, umpire_sd = 0, catcher_sd = 0,
  categorical_context = challenge_discrimination_1d_default_context()$categorical,
  numeric_context = challenge_discrimination_1d_default_context()$numeric,
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon(),
  seed = 20260825L
) {
  x <- normalize_challenge_discrimination_1d_rows(
    rows, require_action = FALSE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  context_columns <- validate_challenge_discrimination_1d_context(
    x, categorical_context, numeric_context
  )
  context <- fit_continuous_design(
    x, categorical = context_columns$categorical,
    numeric = context_columns$numeric
  )
  hyper <- c(
    mu_threshold, mu_log_sigma, tau_threshold, tau_log_sigma,
    rho_threshold_log_sigma, team_sd, umpire_sd, catcher_sd
  )
  if (
    any(!is.finite(hyper)) || tau_threshold < 0 || tau_log_sigma < 0 ||
      abs(rho_threshold_log_sigma) >= 1 || team_sd < 0 || umpire_sd < 0 ||
      catcher_sd < 0
  ) {
    stop("Simulation hyperparameters are invalid")
  }
  set.seed(seed)
  players <- sort(unique(x$batter_id))
  z_threshold <- stats::rnorm(length(players))
  z_independent <- stats::rnorm(length(players))
  z_sigma <- rho_threshold_log_sigma * z_threshold +
    sqrt(1 - rho_threshold_log_sigma^2) * z_independent
  threshold_by_player <- if (is.null(threshold_player)) {
    mu_threshold + tau_threshold * z_threshold
  } else {
    challenge_discrimination_1d_resolve_level(
      threshold_player, players, "threshold_player"
    )
  }
  sigma_by_player <- if (is.null(sigma_player)) {
    exp(mu_log_sigma + tau_log_sigma * z_sigma)
  } else {
    challenge_discrimination_1d_resolve_level(
      sigma_player, players, "sigma_player"
    )
  }
  if (any(!is.finite(sigma_by_player)) || any(sigma_by_player <= 0)) {
    stop("Simulation player sigmas must be positive and finite")
  }
  names(threshold_by_player) <- names(sigma_by_player) <- players

  simulate_group <- function(ids, supplied, sd) {
    levels <- sort(unique(ids))
    values <- if (is.null(supplied)) {
      stats::rnorm(length(levels), 0, sd)
    } else {
      challenge_discrimination_1d_resolve_level(supplied, levels, "group shift")
    }
    names(values) <- levels
    list(levels = levels, values = values, row = values[match(ids, levels)])
  }
  team <- simulate_group(x$bat_team_id, team_shift, team_sd)
  umpire <- simulate_group(x$umpire_id, umpire_shift, umpire_sd)
  catcher <- simulate_group(x$catcher_id, catcher_shift, catcher_sd)
  beta <- challenge_discrimination_1d_resolve_beta(
    context_beta, colnames(context$matrix)
  )
  context_shift_row <- if (length(beta)) {
    as.numeric(context$matrix %*% beta)
  } else {
    numeric(nrow(x))
  }
  player_index <- match(x$batter_id, players)
  threshold_row <- threshold_by_player[player_index]
  sigma_row <- sigma_by_player[player_index]
  total_threshold <- threshold_row + context_shift_row + team$row +
    umpire$row + catcher$row
  probability <- stats::pnorm(
    (x$margin_inches - total_threshold) / sigma_row
  )
  x[, `:=`(
    challenge_probability = probability,
    challenged = stats::rbinom(.N, 1L, probability),
    simulation_player_threshold_inches = threshold_row,
    simulation_sigma_inches = sigma_row,
    simulation_context_shift_inches = context_shift_row,
    simulation_team_shift_inches = team$row,
    simulation_umpire_shift_inches = umpire$row,
    simulation_catcher_shift_inches = catcher$row,
    simulation_total_threshold_inches = total_threshold
  )]
  truth <- list(
    players = data.table::data.table(
      batter_id = players,
      threshold_inches = as.numeric(threshold_by_player),
      sigma_inches = as.numeric(sigma_by_player)
    ),
    teams = data.table::data.table(
      team_id = team$levels, shift_inches = as.numeric(team$values)
    ),
    umpires = data.table::data.table(
      umpire_id = umpire$levels, shift_inches = as.numeric(umpire$values)
    ),
    catchers = data.table::data.table(
      catcher_id = catcher$levels, shift_inches = as.numeric(catcher$values)
    ),
    context = data.table::data.table(
      design_column = colnames(context$matrix), beta_inches = beta
    ),
    hyperparameters = list(
      mu_threshold = mu_threshold, mu_log_sigma = mu_log_sigma,
      tau_threshold = tau_threshold, tau_log_sigma = tau_log_sigma,
      rho_threshold_log_sigma = rho_threshold_log_sigma,
      stake_log_odds_epsilon = stake_log_odds_epsilon
    )
  )
  data.table::setattr(x, "challenge_discrimination_1d_truth", truth)
  x[]
}

challenge_discrimination_1d_recovery_truth <- function(truth) {
  if (data.table::is.data.table(truth) || is.data.frame(truth)) {
    attached <- attr(truth, "challenge_discrimination_1d_truth")
    if (is.null(attached)) {
      stop("Simulated rows do not contain 1D recovery truth")
    }
    return(attached)
  }
  if (!is.list(truth) || is.null(truth$players)) {
    stop("Recovery truth must be a simulation truth list or simulated rows")
  }
  truth
}

validate_challenge_discrimination_1d_recovery <- function(
  fit, truth, ndraws = NULL, seed = 20260825L,
  minimum_player_correlation = 0.60,
  maximum_threshold_rmse_inches = 1.0,
  maximum_log_sigma_rmse = 0.35,
  maximum_rho_error = 0.30
) {
  if (!inherits(fit, "challenge_discrimination_1d_fit")) {
    stop("fit must be a challenge_discrimination_1d_fit")
  }
  truth <- challenge_discrimination_1d_recovery_truth(truth)
  truth_players <- data.table::copy(data.table::as.data.table(truth$players))
  stop_if_missing_columns(
    truth_players, c("batter_id", "threshold_inches", "sigma_inches"),
    "1D recovery player truth"
  )
  posterior <- draws_challenge_discrimination_1d(fit, ndraws, seed)
  fitted <- data.table::data.table(
    batter_id = fit$player_table$batter_id,
    threshold_median = apply(posterior$threshold_player, 2L, stats::median),
    threshold_lower_95 = apply(
      posterior$threshold_player, 2L, stats::quantile,
      probs = 0.025, names = FALSE
    ),
    threshold_upper_95 = apply(
      posterior$threshold_player, 2L, stats::quantile,
      probs = 0.975, names = FALSE
    ),
    sigma_median = apply(posterior$sigma_player, 2L, stats::median),
    sigma_lower_95 = apply(
      posterior$sigma_player, 2L, stats::quantile,
      probs = 0.025, names = FALSE
    ),
    sigma_upper_95 = apply(
      posterior$sigma_player, 2L, stats::quantile,
      probs = 0.975, names = FALSE
    )
  )
  players <- merge(
    truth_players, fitted, by = "batter_id", all = FALSE, sort = FALSE
  )
  if (nrow(players) != nrow(fit$player_table)) {
    stop("Recovery truth does not cover every fitted batter")
  }
  safe_correlation <- function(estimate, target) {
    if (length(target) < 2L || stats::sd(target) < 1e-10 ||
        stats::sd(estimate) < 1e-10) return(NA_real_)
    stats::cor(estimate, target)
  }
  threshold_correlation <- safe_correlation(
    players$threshold_median, players$threshold_inches
  )
  sigma_correlation <- safe_correlation(
    log(players$sigma_median), log(players$sigma_inches)
  )
  threshold_rmse <- sqrt(mean(
    (players$threshold_median - players$threshold_inches)^2
  ))
  log_sigma_rmse <- sqrt(mean(
    (log(players$sigma_median) - log(players$sigma_inches))^2
  ))
  correlation_pass <- function(value, target) {
    target_sd <- stats::sd(target)
    if (length(target) < 2L || !is.finite(target_sd) || target_sd < 1e-10) {
      return(TRUE)
    }
    is.finite(value) && value >= minimum_player_correlation
  }
  threshold_pass <-
    correlation_pass(threshold_correlation, players$threshold_inches) &&
    threshold_rmse <= maximum_threshold_rmse_inches
  sigma_pass <-
    correlation_pass(sigma_correlation, log(players$sigma_inches)) &&
    log_sigma_rmse <= maximum_log_sigma_rmse

  true_rho <- if (!is.null(truth$hyperparameters)) {
    as.numeric(truth$hyperparameters$rho_threshold_log_sigma)
  } else {
    NA_real_
  }
  if (length(true_rho) != 1L || !is.finite(true_rho)) true_rho <- NA_real_
  rho_interval <- stats::quantile(
    posterior$rho_threshold_log_sigma, c(0.025, 0.975), names = FALSE
  )
  rho_median <- stats::median(posterior$rho_threshold_log_sigma)
  rho_applicable <- fit$sigma_model == "hierarchical" &&
    length(true_rho) == 1L && is.finite(true_rho)
  rho_pass <- if (rho_applicable) {
    rho_interval[[1L]] <= true_rho && rho_interval[[2L]] >= true_rho &&
      abs(rho_median - true_rho) <= maximum_rho_error
  } else {
    TRUE
  }
  players[, `:=`(
    threshold_interval_covers_truth =
      threshold_lower_95 <= threshold_inches &
      threshold_upper_95 >= threshold_inches,
    sigma_interval_covers_truth =
      sigma_lower_95 <= sigma_inches & sigma_upper_95 >= sigma_inches
  )]
  metrics <- data.table::data.table(
    sigma_model = fit$sigma_model,
    players = nrow(players),
    threshold_correlation = threshold_correlation,
    threshold_rmse_inches = threshold_rmse,
    log_sigma_correlation = sigma_correlation,
    log_sigma_rmse = log_sigma_rmse,
    threshold_interval_coverage = mean(
      players$threshold_interval_covers_truth
    ),
    sigma_interval_coverage = mean(players$sigma_interval_covers_truth),
    true_rho = true_rho,
    rho_posterior_median = rho_median,
    rho_lower_95 = rho_interval[[1L]],
    rho_upper_95 = rho_interval[[2L]],
    threshold_recovery_pass = threshold_pass,
    sigma_recovery_pass = sigma_pass,
    correlation_recovery_pass = rho_pass,
    recovery_pass = threshold_pass && sigma_pass && rho_pass
  )
  list(metrics = metrics[], players = players[])
}

make_challenge_discrimination_1d_game_folds <- function(
  rows, folds = 5L, seed = 20260825L, margin_limit_inches = 3,
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon()
) {
  eligible_rows <- normalize_challenge_discrimination_1d_rows(
    rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  margin_limit_inches <- validate_challenge_discrimination_1d_margin_limit(
    margin_limit_inches
  )
  x <- eligible_rows
  games <- sort(unique(as.character(x$game_pk)))
  folds <- as.integer(folds)
  if (length(folds) != 1L || folds < 2L || length(games) < folds) {
    stop("Game folds require at least two folds and enough eligible games")
  }
  set.seed(seed)
  out <- data.table::data.table(
    game_pk = sample(games),
    fold = rep(seq_len(folds), length.out = length(games)),
    margin_limit_inches = margin_limit_inches,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  counts <- x[, .(
    eligible_margin_rows = .N,
    local_model_rows = sum(abs(margin_inches) <= margin_limit_inches),
    tail_diagnostic_rows = sum(abs(margin_inches) > margin_limit_inches)
  ), by = game_pk]
  out <- merge(out, counts, by = "game_pk", all.x = TRUE, sort = FALSE)
  data.table::setorder(out, game_pk)
  validate_challenge_discrimination_1d_game_folds(out, expected_folds = folds)
  out[]
}

validate_challenge_discrimination_1d_game_folds <- function(
  fold_table, expected_folds = 5L
) {
  x <- data.table::copy(data.table::as.data.table(fold_table))
  stop_if_missing_columns(x, c("game_pk", "fold"), "1D game folds")
  expected_folds <- as.integer(expected_folds)
  if (
    anyNA(x[, .(game_pk, fold)]) || anyDuplicated(as.character(x$game_pk)) ||
      !setequal(unique(as.integer(x$fold)), seq_len(expected_folds))
  ) {
    stop("Each game must appear in exactly one complete 1D model fold")
  }
  if ("margin_limit_inches" %in% names(x)) {
    limits <- unique(as.numeric(x$margin_limit_inches))
    if (length(limits) != 1L) {
      stop("A 1D fold table must record one common margin limit")
    }
    validate_challenge_discrimination_1d_margin_limit(limits)
  }
  if ("stake_log_odds_epsilon" %in% names(x)) {
    epsilon <- unique(as.numeric(x$stake_log_odds_epsilon))
    if (
      length(epsilon) != 1L || !is.finite(epsilon) ||
        epsilon <= 0 || epsilon >= 0.5
    ) {
      stop("A 1D fold table must record one valid stake epsilon")
    }
  }
  count_columns <- c(
    "eligible_margin_rows", "local_model_rows", "tail_diagnostic_rows"
  )
  if (all(count_columns %in% names(x))) {
    if (
      anyNA(x[, ..count_columns]) ||
        any(as.matrix(x[, ..count_columns]) < 0) ||
        any(x$eligible_margin_rows !=
          x$local_model_rows + x$tail_diagnostic_rows)
    ) {
      stop("Recorded fold row counts are incomplete or inconsistent")
    }
  }
  invisible(TRUE)
}

split_challenge_discrimination_1d_fold <- function(
  rows, fold_table, heldout_fold, margin_limit_inches = 3,
  stake_log_odds_epsilon = challenge_discrimination_1d_stake_epsilon()
) {
  eligible_rows <- normalize_challenge_discrimination_1d_rows(
    rows, require_action = TRUE,
    stake_log_odds_epsilon = stake_log_odds_epsilon
  )
  margin_limit_inches <- validate_challenge_discrimination_1d_margin_limit(
    margin_limit_inches
  )
  x <- eligible_rows
  folds <- data.table::copy(data.table::as.data.table(fold_table))
  validate_challenge_discrimination_1d_game_folds(
    folds, expected_folds = max(as.integer(folds$fold))
  )
  if (
    "margin_limit_inches" %in% names(folds) &&
      !identical(
        unique(as.numeric(folds$margin_limit_inches)),
        as.numeric(margin_limit_inches)
      )
  ) {
    stop("Fold splitting must use the margin limit recorded by the fold table")
  }
  if (
    "stake_log_odds_epsilon" %in% names(folds) &&
      !identical(
        unique(as.numeric(folds$stake_log_odds_epsilon)),
        as.numeric(stake_log_odds_epsilon)
      )
  ) {
    stop("Fold splitting must use the stake epsilon recorded by the fold table")
  }
  heldout_fold <- as.integer(heldout_fold)
  if (length(heldout_fold) != 1L || !heldout_fold %in% folds$fold) {
    stop("heldout_fold is not present in the game-fold table")
  }
  fold_index <- folds$fold[match(as.character(x$game_pk), as.character(folds$game_pk))]
  if (anyNA(fold_index)) stop("The fold table does not cover every eligible game")
  train <- x[fold_index != heldout_fold]
  heldout <- x[fold_index == heldout_fold]
  if (length(intersect(unique(train$game_pk), unique(heldout$game_pk)))) {
    stop("Training and held-out 1D model games overlap")
  }
  split_summary <- data.table::rbindlist(list(
    data.table::data.table(
      split = "training",
      eligible_margin_rows = nrow(train),
      local_model_rows = sum(
        abs(train$margin_inches) <= margin_limit_inches
      ),
      tail_diagnostic_rows = sum(
        abs(train$margin_inches) > margin_limit_inches
      )
    ),
    data.table::data.table(
      split = "heldout",
      eligible_margin_rows = nrow(heldout),
      local_model_rows = sum(
        abs(heldout$margin_inches) <= margin_limit_inches
      ),
      tail_diagnostic_rows = sum(
        abs(heldout$margin_inches) > margin_limit_inches
      )
    )
  ))
  list(
    train = train[],
    heldout = heldout[],
    heldout_fold = heldout_fold,
    margin_limit_inches = margin_limit_inches,
    split_summary = split_summary[],
    training_tail_diagnostics =
      challenge_discrimination_1d_tail_diagnostics(
        train, margin_limit_inches
      ),
    heldout_tail_diagnostics =
      challenge_discrimination_1d_tail_diagnostics(
        heldout, margin_limit_inches
      )
  )
}

challenge_discrimination_1d_log_loss <- function(
  observed, probability, epsilon = 1e-9
) {
  observed <- as.integer(observed)
  probability <- pmin(1 - epsilon, pmax(epsilon, as.numeric(probability)))
  if (
    length(observed) != length(probability) || anyNA(observed) ||
      any(!observed %in% 0:1) || any(!is.finite(probability))
  ) {
    stop("Invalid choices or probabilities for 1D log loss")
  }
  -(observed * log(probability) + (1 - observed) * log1p(-probability))
}

compare_challenge_discrimination_1d_heldout <- function(
  common_scored, hierarchical_scored,
  probability_column = "challenge_probability_mean"
) {
  common <- data.table::copy(data.table::as.data.table(common_scored))
  hierarchical <- data.table::copy(data.table::as.data.table(hierarchical_scored))
  required <- c("game_pk", "pitch_order", "challenged", probability_column)
  stop_if_missing_columns(common, required, "common-sigma held-out scores")
  stop_if_missing_columns(
    hierarchical, required, "hierarchical-sigma held-out scores"
  )
  keys <- c("game_pk", "pitch_order")
  if (
    nrow(common) != nrow(hierarchical) ||
      !identical(common[, ..keys], hierarchical[, ..keys]) ||
      !identical(as.integer(common$challenged),
        as.integer(hierarchical$challenged))
  ) {
    stop("Common and hierarchical held-out scoring rows are not aligned")
  }
  if (all(c("margin_limit_inches", "margin_inches") %in% names(common)) &&
      all(c("margin_limit_inches", "margin_inches") %in% names(hierarchical))) {
    common_limit <- unique(as.numeric(common$margin_limit_inches))
    hierarchical_limit <- unique(as.numeric(hierarchical$margin_limit_inches))
    if (
      length(common_limit) != 1L || length(hierarchical_limit) != 1L ||
        !identical(common_limit, hierarchical_limit) ||
        any(abs(common$margin_inches) > common_limit) ||
        any(abs(hierarchical$margin_inches) > hierarchical_limit)
    ) {
      stop("Held-out scores do not share one enforced local margin domain")
    }
  }
  observed <- as.integer(common$challenged)
  probability_common <- as.numeric(common[[probability_column]])
  probability_hierarchical <- as.numeric(hierarchical[[probability_column]])
  loss_common <- challenge_discrimination_1d_log_loss(
    observed, probability_common
  )
  loss_hierarchical <- challenge_discrimination_1d_log_loss(
    observed, probability_hierarchical
  )
  comparison <- data.table::data.table(
    game_pk = as.character(common$game_pk),
    improvement = loss_common - loss_hierarchical
  )
  by_game <- comparison[, .(
    pitches = .N,
    mean_log_loss_improvement = mean(improvement)
  ), by = game_pk]
  improvement <- mean(by_game$mean_log_loss_improvement)
  clustered_se <- if (nrow(by_game) >= 2L) {
    stats::sd(by_game$mean_log_loss_improvement) / sqrt(nrow(by_game))
  } else {
    NA_real_
  }
  list(
    metrics = data.table::data.table(
      heldout_pitches = nrow(common),
      heldout_games = nrow(by_game),
      common_log_loss = mean(loss_common),
      hierarchical_log_loss = mean(loss_hierarchical),
      common_brier_score = mean((observed - probability_common)^2),
      hierarchical_brier_score = mean(
        (observed - probability_hierarchical)^2
      ),
      game_weighted_log_loss_improvement = improvement,
      game_clustered_se = clustered_se,
      one_game_clustered_se_pass = is.finite(clustered_se) &&
        improvement >= clustered_se
    ),
    by_game = by_game[]
  )
}

gate_challenge_discrimination_1d <- function(
  heldout_validation, recovery_validation,
  minimum_game_clustered_standard_errors = 1
) {
  heldout <- if (is.list(heldout_validation) &&
      !is.data.frame(heldout_validation)) {
    data.table::as.data.table(heldout_validation$metrics)
  } else {
    data.table::as.data.table(heldout_validation)
  }
  recovery <- if (is.list(recovery_validation) &&
      !is.data.frame(recovery_validation)) {
    data.table::as.data.table(recovery_validation$metrics)
  } else {
    data.table::as.data.table(recovery_validation)
  }
  stop_if_missing_columns(
    heldout,
    c("game_weighted_log_loss_improvement", "game_clustered_se"),
    "1D held-out comparison"
  )
  stop_if_missing_columns(recovery, "recovery_pass", "1D recovery validation")
  if (nrow(heldout) != 1L || nrow(recovery) != 1L) {
    stop("The 1D promotion gate requires one held-out and one recovery row")
  }
  minimum_game_clustered_standard_errors <- as.numeric(
    minimum_game_clustered_standard_errors
  )
  if (
    length(minimum_game_clustered_standard_errors) != 1L ||
      !is.finite(minimum_game_clustered_standard_errors) ||
      minimum_game_clustered_standard_errors < 0
  ) {
    stop("minimum_game_clustered_standard_errors must be nonnegative")
  }
  improvement <- heldout$game_weighted_log_loss_improvement[[1L]]
  standard_error <- heldout$game_clustered_se[[1L]]
  predictive_pass <- is.finite(standard_error) && is.finite(improvement) &&
    improvement >= minimum_game_clustered_standard_errors * standard_error
  recovery_pass <- isTRUE(recovery$recovery_pass[[1L]])
  promote <- predictive_pass && recovery_pass
  data.table::data.table(
    candidate = "hierarchical_player_sigma",
    baseline = "common_sigma",
    game_weighted_log_loss_improvement = improvement,
    game_clustered_se = standard_error,
    required_standard_errors = minimum_game_clustered_standard_errors,
    predictive_pass = predictive_pass,
    recovery_pass = recovery_pass,
    promote_hierarchical_sigma = promote,
    selected_sigma_model = if (promote) "hierarchical" else "common",
    reason = data.table::fifelse(
      !predictive_pass,
      "hierarchical sigma did not improve by one game-clustered SE",
      data.table::fifelse(
        !recovery_pass, "hierarchical sigma failed simulation recovery",
        "hierarchical sigma passed prediction and recovery gates"
      )
    )
  )
}
