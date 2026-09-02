continuous_perception_profile <- function(profile = c("pilot", "full")) {
  profile <- match.arg(profile)
  if (profile == "pilot") {
    return(list(
      name = profile, folds = 5L, active_folds = 1L,
      chains = 2L, parallel_chains = 2L,
      iter_warmup = 500L, iter_sampling = 500L, posterior_draws = 100L,
      decision_fit_draws = 10L, scoring_draws = 25L,
      trust_fit_draws = 3L, decision_zero_fraction = 0.20,
      trust_grid_step = 0.05,
      gh_order = 7L, seed = 20260825L
    ))
  }
  list(
    name = profile, folds = 5L, active_folds = 5L,
    chains = 4L, parallel_chains = 4L,
    iter_warmup = 1000L, iter_sampling = 1000L, posterior_draws = 400L,
    decision_fit_draws = 25L, scoring_draws = 100L,
    trust_fit_draws = 5L, decision_zero_fraction = 0.25,
    trust_grid_step = 0.05,
    gh_order = 7L, seed = 20260825L
  )
}

continuous_physical_coordinates <- function(rows) {
  x <- data.table::copy(data.table::as.data.table(rows))
  stop_if_missing_columns(
    x, c("plate_x", "plate_z", "sz_bot", "sz_top"),
    "continuous perception coordinates"
  )
  x[, `:=`(
    x_in = 12 * as.numeric(plate_x),
    zone_mid_ft = (as.numeric(sz_bot) + as.numeric(sz_top)) / 2,
    zone_half_height_in = 6 * (as.numeric(sz_top) - as.numeric(sz_bot))
  )]
  x[, z_rel_in := 12 * (as.numeric(plate_z) - zone_mid_ft)]
  if (any(!is.finite(x$x_in)) || any(!is.finite(x$z_rel_in)) ||
      any(!is.finite(x$zone_half_height_in)) || any(x$zone_half_height_in <= 0)) {
    stop("Continuous physical coordinates are invalid")
  }
  x[]
}

continuous_context_fields <- function(rows) {
  x <- data.table::copy(data.table::as.data.table(rows))
  if (!"pitch_family" %in% names(x)) {
    if ("pitch_type" %in% names(x)) {
      x[, pitch_family := swing_take_pitch_family(pitch_type)]
    } else {
      x[, pitch_family := "other"]
    }
  }
  balls_column <- if ("balls_before" %in% names(x)) "balls_before" else "balls"
  strikes_column <- if ("strikes_before" %in% names(x)) "strikes_before" else "strikes"
  stop_if_missing_columns(x, c(balls_column, strikes_column), "continuous context")
  stand <- if ("stand" %in% names(x)) as.character(x$stand) else rep("U", nrow(x))
  throws <- if ("p_throws" %in% names(x)) as.character(x$p_throws) else rep("U", nrow(x))
  x[, `:=`(
    balls_context = as.integer(get(balls_column)),
    strikes_context = as.integer(get(strikes_column)),
    count_state = paste0(get(balls_column), "-", get(strikes_column)),
    matchup = paste0(data.table::fcoalesce(stand, "U"), "-",
      data.table::fcoalesce(throws, "U")),
    pitch_family = data.table::fcoalesce(as.character(pitch_family), "other")
  )]
  x[]
}

prepare_continuous_location_features <- function(statcast) {
  x <- data.table::copy(data.table::as.data.table(statcast))
  required <- c(
    "game_pk", "at_bat_number", "pitch_number", "batter", "pitcher",
    "plate_x", "plate_z", "sz_bot", "sz_top", "balls", "strikes"
  )
  stop_if_missing_columns(x, required, "continuous location-prior source")
  x <- x[
    is.finite(plate_x) & is.finite(plate_z) & is.finite(sz_bot) &
      is.finite(sz_top) & sz_top > sz_bot & balls %in% 0:3 & strikes %in% 0:2
  ]
  x <- continuous_context_fields(continuous_physical_coordinates(x))
  for (column in c("pitch_type", "stand", "p_throws")) {
    if (!column %in% names(x)) x[, (column) := NA_character_]
  }
  out <- x[, .(
    game_pk, at_bat_number, pitch_number,
    pitcher_id = as.character(pitcher),
    plate_x, plate_z, sz_top, sz_bot,
    balls, strikes, pitch_type, stand, p_throws,
    x_in, z_rel_in,
    zone_half_height_in, balls_context, strikes_context, count_state,
    pitch_family, matchup
  )]
  data.table::setorder(out, game_pk, at_bat_number, pitch_number)
  if (anyDuplicated(out[, .(game_pk, at_bat_number, pitch_number)])) {
    stop("Continuous location features contain duplicate pitch keys")
  }
  out[]
}

prepare_continuous_swing_features <- function(statcast, boundary_inches = 6) {
  choices <- prepare_swing_take_choices(statcast, boundary_inches = boundary_inches)
  choices <- continuous_context_fields(continuous_physical_coordinates(choices))
  keep <- c(
    "game_pk", "at_bat_number", "pitch_number", "batter_id", "pitcher_id",
    "swing", "description", "plate_x", "plate_z", "sz_top", "sz_bot",
    "edge_distance_inches", "x_in", "z_rel_in", "zone_half_height_in",
    "balls", "strikes", "pitch_type", "stand", "p_throws", "count_state",
    "pitch_family", "matchup"
  )
  choices[, intersect(keep, names(choices)), with = FALSE]
}

prepare_continuous_call_features <- function(pitch_ledger) {
  x <- data.table::copy(data.table::as.data.table(pitch_ledger))
  required <- c(
    "game_pk", "pitch_order", "initial_call", "plate_x", "plate_z",
    "sz_bot", "sz_top", "edge_distance_inches", "umpire_id", "fielder_2"
  )
  stop_if_missing_columns(x, required, "continuous initial-call source")
  x <- x[
    initial_call %in% c("ball", "called_strike") &
      is.finite(plate_x) & is.finite(plate_z) & is.finite(sz_bot) &
      is.finite(sz_top) & !is.na(umpire_id) & !is.na(fielder_2)
  ]
  x <- continuous_context_fields(continuous_physical_coordinates(x))
  out <- x[, .(
    game_pk, pitch_order, at_bat_number, pitch_number,
    initial_call, called_strike = as.integer(
      initial_call == "called_strike"
    ), plate_x, plate_z, sz_top, sz_bot,
    balls = balls_context, strikes = strikes_context,
    pitch_type = if ("pitch_type" %in% names(x)) as.character(pitch_type) else NA_character_,
    stand = if ("stand" %in% names(x)) as.character(stand) else "U",
    x_in, z_rel_in, zone_half_height_in, edge_distance_inches,
    umpire_id = as.character(umpire_id), catcher_id = as.character(fielder_2),
    p_throws = if ("p_throws" %in% names(x)) as.character(p_throws) else "U",
    balls_context, strikes_context, count_state, pitch_family, matchup
  )]
  data.table::setorder(out, game_pk, pitch_order)
  if (anyDuplicated(out[, .(game_pk, pitch_order)])) {
    stop("Continuous initial-call features contain duplicate pitch keys")
  }
  out[]
}

continuous_game_folds <- function(game_pk, folds = 5L, seed = 20260825L) {
  games <- sort(unique(as.character(game_pk)))
  folds <- as.integer(folds)
  if (folds < 2L || length(games) < folds) {
    stop("Game folds require at least two folds and enough games")
  }
  set.seed(seed)
  shuffled <- sample(games)
  out <- data.table::data.table(
    game_pk = shuffled,
    fold = rep(seq_len(folds), length.out = length(shuffled))
  )
  data.table::setorder(out, game_pk)
  out[]
}

validate_continuous_game_folds <- function(fold_table, expected_folds = 5L) {
  x <- data.table::as.data.table(fold_table)
  stop_if_missing_columns(x, c("game_pk", "fold"), "continuous game folds")
  if (anyDuplicated(x$game_pk) || anyNA(x) ||
      !setequal(unique(x$fold), seq_len(expected_folds))) {
    stop("Each game must appear in exactly one complete perception fold")
  }
  invisible(TRUE)
}

continuous_hypothetical_batter_gain <- function(rows, re_model) {
  x <- data.table::copy(data.table::as.data.table(rows))
  required <- c(
    "balls_before", "strikes_before", "outs_before", "on_1b", "on_2b",
    "on_3b", "inning", "half", "concurrent_outs", "concurrent_runs",
    "concurrent_remove_1b", "concurrent_remove_2b", "concurrent_remove_3b",
    "concurrent_add_1b", "concurrent_add_2b", "concurrent_add_3b"
  )
  stop_if_missing_columns(x, required, "hypothetical batter-overturn gain")
  re_table <- if (is.list(re_model) && !is.null(re_model$table)) {
    re_model$table
  } else {
    re_model
  }
  stop_if_missing_columns(
    re_table, c("outs", "base_state", "balls", "strikes", "re"),
    "run-expectancy model"
  )
  called_strike_state <- vectorized_call_branch(
    x, rep.int("called_strike", nrow(x))
  )
  overturned_ball_state <- vectorized_call_branch(
    x, rep.int("ball", nrow(x))
  )
  gain <- vectorized_branch_re(re_table, overturned_ball_state) -
    vectorized_branch_re(re_table, called_strike_state)
  if (any(!is.finite(gain))) {
    stop("Hypothetical batter-overturn gain could not be evaluated")
  }
  pmax(0, as.numeric(gain))
}

continuous_expected_inventory_loss <- function(
  expected_correctable_remaining, expected_re_remaining, inventory_before
) {
  lengths <- c(
    length(expected_correctable_remaining), length(expected_re_remaining),
    length(inventory_before)
  )
  size <- max(lengths)
  if (any(lengths != 1L & lengths != size)) {
    stop("Expected inventory-loss inputs are not row-aligned")
  }
  lambda <- pmax(
    0, rep_len(as.numeric(expected_correctable_remaining), size)
  )
  expected_re <- pmax(0, rep_len(as.numeric(expected_re_remaining), size))
  inventory <- rep_len(as.integer(inventory_before), size)
  if (any(!is.finite(lambda)) || any(!is.finite(expected_re)) ||
      anyNA(inventory) || any(inventory < 1L)) {
    stop("Expected inventory-loss inputs are invalid")
  }
  average_gain <- ifelse(lambda > 1e-10, expected_re / lambda, 0)
  first_unit <- (1 - exp(-lambda)) * average_gain
  second_unit <- (1 - exp(-lambda) * (1 + lambda)) * average_gain
  ifelse(inventory <= 1L, first_unit, second_unit)
}

build_batter_decision_features <- function(
  pitch_ledger, remaining_opportunities, re_model
) {
  ledger <- data.table::copy(data.table::as.data.table(pitch_ledger))
  remaining <- data.table::copy(data.table::as.data.table(remaining_opportunities))
  required <- c(
    "game_pk", "pitch_order", "initial_call", "batter_id", "bat_team_id",
    "challenge_occurred", "challenger_role", "bat_team_challenges_before",
    "inning", "outs_before", "balls_before",
    "strikes_before", "at_bat_number", "pitch_number", "pitcher_id",
    "bat_score", "fld_score",
    "plate_x", "plate_z", "sz_top", "sz_bot", "edge_distance_inches",
    "umpire_id", "fielder_2"
  )
  stop_if_missing_columns(ledger, required, "batter decision source")
  stop_if_missing_columns(
    remaining,
    c(
      "game_pk", "pitch_order", "team_id",
      "expected_correctable_remaining", "expected_re_remaining"
    ),
    "remaining-opportunity inventory source"
  )
  x <- ledger[
    initial_call == "called_strike" & !is.na(batter_id) &
      bat_team_challenges_before > 0L
  ]
  x[, challenged := as.integer(
    challenge_occurred & challenger_role == "batter"
  )]
  x[, score_margin := as.numeric(bat_score) - as.numeric(fld_score)]
  x <- continuous_context_fields(x)
  x[, `:=`(
    catcher_id = as.character(fielder_2),
    pitcher_id = as.character(pitcher_id),
    umpire_id = as.character(umpire_id)
  )]
  marginal <- remaining[, .(
    game_pk, pitch_order, bat_team_id = team_id,
    expected_correctable_remaining, expected_re_remaining
  )]
  x <- merge(
    x, marginal, by = c("game_pk", "pitch_order", "bat_team_id"),
    all.x = TRUE, sort = FALSE
  )
  x[, inventory_loss := continuous_expected_inventory_loss(
    expected_correctable_remaining,
    expected_re_remaining,
    bat_team_challenges_before
  )]
  # G is the run-expectancy gain of the hypothetical strike-to-ball branch on
  # every eligible called strike. It never consults ABS truth or whether an
  # observed challenge succeeded, so correct calls remain valid pass rows.
  x[, stake_G := continuous_hypothetical_batter_gain(.SD, re_model)]
  x <- x[is.finite(inventory_loss) & inventory_loss >= 0]
  prepare_continuous_decision_rows(x)
}

build_batter_challenge_labels <- function(pitch_ledger) {
  x <- data.table::copy(data.table::as.data.table(pitch_ledger))
  required <- c(
    "game_pk", "pitch_order", "challenge_occurred", "challenger_role",
    "challenge_outcome", "abs_call", "initial_call"
  )
  stop_if_missing_columns(x, required, "batter challenge labels")
  x[
    challenge_occurred & challenger_role == "batter",
    .(
      game_pk, pitch_order,
      official_success = challenge_outcome == "overturned",
      geometry_success = abs_call != initial_call
    )
  ]
}

assert_perception_fit_is_label_free <- function(fit_columns) {
  forbidden <- intersect(
    as.character(fit_columns),
    c(continuous_decision_outcome_columns(), "challenged", "challenge_occurred")
  )
  if (length(forbidden)) {
    stop("Perception fit leaked forbidden labels: ", paste(forbidden, collapse = ", "))
  }
  invisible(TRUE)
}

write_continuous_perception_manifest <- function(
  path, profile, folds, draw_map = NULL, gates = list(), extra = list()
) {
  validate_continuous_game_folds(folds, max(folds$fold))
  if (!is.null(draw_map)) validate_global_draw_map(draw_map)
  manifest <- c(list(
    model = "continuous-human-perception-v1",
    interpretation = paste(
      "Cross-fitted model-implied human belief under an unobserved private",
      "location signal; not physical tracking uncertainty or observed eyesight"
    ),
    profile = profile,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    git_sha = tryCatch(
      system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
      error = function(e) NA_character_
    ),
    folds = data.table::as.data.frame(folds),
    draw_map = if (is.null(draw_map)) NULL else data.table::as.data.frame(draw_map),
    gates = gates,
    session = utils::capture.output(utils::sessionInfo())
  ), extra)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  normalizePath(path)
}
