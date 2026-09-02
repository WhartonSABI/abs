# Outcome-free opportunity-selection models for the role-oriented 1D margin.
#
# These models describe whether an inventory-available adverse call is
# challenged. They never use whether the challenge succeeded, the final call,
# ABS truth, or realized run/win value. The full-support benchmark allows a
# nonlinear margin relationship and explicitly represents release speed; the
# local baseline retains the existing linear-probit interpretation on +/- 3 in.

revealed_challenge_selection_1d_roles <- function() c("offense", "defense")

revealed_challenge_selection_1d_outcome_columns <- function() {
  unique(c(
    "abs_call", "call_wrong", "actual_wrong", "geometry_wrong",
    "geometry_success", "official_success", "official_abs_call",
    "official_outcome", "final_call", "description", "sc_description",
    "challenge_outcome", "is_overturned", "overturned",
    "challenge_success", "observed_success", "truth_source",
    "official_geometry_mismatch", "delta_run_exp", "delta_home_win_exp",
    "home_win_exp", "potential_challenger_re", "potential_challenger_wpa",
    "actual_re_gain", "actual_wpa_gain", "outcome"
  ))
}

revealed_challenge_selection_1d_source_allowlist <- function() {
  c(
    "game_pk", "pitch_order", "at_bat_number", "pitch_number",
    "initial_call", "challenge_occurred", "challenger_role",
    "challenger_team_id", "batter_id", "pitcher_id", "fielder_2",
    "bat_team_id", "fld_team_id", "bat_team_challenges_before",
    "fld_team_challenges_before", "adverse_challenges_before",
    "tracking_available", "abs_eligible", "edge_distance_inches",
    "plate_x", "plate_z", "sz_top", "sz_bot", "balls_before",
    "strikes_before", "inning", "outs_before", "score_margin",
    "bat_score", "fld_score", "release_speed", "pitch_type", "p_throws",
    "stand", "umpire_id", "half", "on_1b", "on_2b", "on_3b",
    "concurrent_remove_1b", "concurrent_remove_2b",
    "concurrent_remove_3b", "concurrent_add_1b", "concurrent_add_2b",
    "concurrent_add_3b", "concurrent_runs", "concurrent_outs"
  )
}

assert_revealed_challenge_selection_1d_outcome_free <- function(rows, label) {
  leaked <- intersect(
    names(data.table::as.data.table(rows)),
    revealed_challenge_selection_1d_outcome_columns()
  )
  if (length(leaked)) {
    stop(
      label, " contains forbidden outcome/evaluation columns: ",
      paste(sort(leaked), collapse = ", "), call. = FALSE
    )
  }
  invisible(TRUE)
}

revealed_challenge_selection_pitch_family_1d <- function(pitch_type) {
  value <- as.character(pitch_type)
  data.table::fcase(
    value %in% c("FF", "FA", "SI", "FC"), "fastball",
    value %in% c("SL", "ST", "SV", "CU", "KC", "CS"), "breaking",
    value %in% c("CH", "FS", "FO", "SC"), "offspeed",
    default = "other"
  )
}

.revealed_challenge_selection_exact_edge_1d <- function(
  rows, geometry_tolerance = 1e-8
) {
  x <- data.table::as.data.table(rows)
  supplied <- if ("edge_distance_inches" %in% names(x)) {
    as.numeric(x$edge_distance_inches)
  } else {
    rep(NA_real_, nrow(x))
  }
  geometry <- c("plate_x", "plate_z", "sz_top", "sz_bot")
  exact <- rep(NA_real_, nrow(x))
  if (all(geometry %in% names(x))) {
    exact <- abs_edge_distance_inches(
      as.numeric(x$plate_x), as.numeric(x$plate_z),
      as.numeric(x$sz_top), as.numeric(x$sz_bot)
    )
    disagree <- is.finite(exact) & is.finite(supplied) &
      abs(exact - supplied) > geometry_tolerance
    if (any(disagree)) {
      stop("Supplied margins disagree with exact rounded ABS geometry",
        call. = FALSE
      )
    }
  }
  use_exact <- is.finite(exact)
  value <- supplied
  value[use_exact] <- exact[use_exact]
  list(
    value = value,
    source = ifelse(use_exact, "exact_rounded_geometry", "precomputed_exact_edge")
  )
}

.revealed_challenge_selection_id_1d <- function(value) {
  value <- as.character(value)
  value[is.na(value) | !nzchar(value)] <- "__UNKNOWN__"
  value
}

.revealed_challenge_selection_context_1d <- function(
  rows, slow_cutoff_mph = 75
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  cutoff <- as.numeric(slow_cutoff_mph)
  if (length(cutoff) != 1L || !is.finite(cutoff) || cutoff <= 0) {
    stop("slow_cutoff_mph must be one positive finite number", call. = FALSE)
  }
  if (!"pitch_type" %in% names(x)) x[, pitch_type := NA_character_]
  if (!"release_speed" %in% names(x)) x[, release_speed := NA_real_]
  if (!"balls_before" %in% names(x)) x[, balls_before := NA_integer_]
  if (!"strikes_before" %in% names(x)) x[, strikes_before := NA_integer_]
  if (!"stand" %in% names(x)) x[, stand := "U"]
  if (!"p_throws" %in% names(x)) x[, p_throws := "U"]
  if (!"outs_before" %in% names(x)) x[, outs_before := NA_integer_]
  count_ok <- x$balls_before %in% 0:3 & x$strikes_before %in% 0:2
  speed <- as.numeric(x$release_speed)
  eephus <- !is.na(x$pitch_type) & as.character(x$pitch_type) == "EP"
  slow <- is.finite(speed) & speed < cutoff & !eephus
  x[, `:=`(
    release_speed_mph = speed,
    pitch_family_coarse = revealed_challenge_selection_pitch_family_1d(pitch_type),
    count_state = ifelse(
      count_ok, paste0(as.integer(balls_before), "-", as.integer(strikes_before)),
      "unknown"
    ),
    matchup = paste0(
      data.table::fcoalesce(as.character(stand), "U"), "-",
      data.table::fcoalesce(as.character(p_throws), "U")
    ),
    outs_state = ifelse(outs_before %in% 0:2, as.character(outs_before), "unknown"),
    inventory_state = as.character(inventory_before),
    is_eephus = eephus,
    is_slow = slow,
    speed_stratum = data.table::fcase(
      eephus, "eephus",
      slow, "slow_non_eephus",
      !is.finite(speed), "speed_missing",
      default = "regular_speed"
    ),
    margin_stratum = ifelse(
      abs(role_margin_inches) <= 3, "local_abs_margin_le_3",
      "tail_abs_margin_gt_3"
    )
  )]
  x[, selection_stratum := paste(speed_stratum, margin_stratum, sep = "__")]
  data.table::setattr(x, "slow_cutoff_mph", cutoff)
  x[]
}

.revealed_challenge_selection_gain_1d <- function(rows, re_model, role) {
  if (is.null(re_model)) {
    return(list(
      value = rep(0, nrow(rows)),
      source = rep("not_supplied", nrow(rows))
    ))
  }
  re_table <- if (is.list(re_model) && !is.null(re_model$table)) {
    re_model$table
  } else {
    re_model
  }
  stop_if_missing_columns(
    re_table, c("outs", "base_state", "balls", "strikes", "re"),
    "revealed-selection run-expectancy model"
  )
  stands <- vectorized_call_branch(rows, rows$initial_call)
  flipped <- vectorized_call_branch(rows, opposite_call(rows$initial_call))
  batting_gain <- vectorized_branch_re(re_table, flipped) -
    vectorized_branch_re(re_table, stands)
  value <- if (identical(role, "offense")) batting_gain else -batting_gain
  if (any(!is.finite(value))) {
    stop("Revealed-selection immediate correction gain is unavailable",
      call. = FALSE
    )
  }
  list(
    value = as.numeric(value),
    source = rep("run_expectancy_call_branch_difference", nrow(rows))
  )
}

.revealed_challenge_selection_build_role_1d <- function(
  pitch_ledger, role, re_model = NULL, slow_cutoff_mph = 75,
  geometry_tolerance = 1e-8, require_positive_inventory = TRUE
) {
  role <- match.arg(role, revealed_challenge_selection_1d_roles())
  role_value <- role
  source <- data.table::as.data.table(pitch_ledger)
  required <- c(
    "game_pk", "pitch_order", "initial_call", "challenge_occurred",
    "challenger_role", "challenger_team_id", "batter_id", "pitcher_id",
    "fielder_2", "bat_team_id", "fld_team_id", "edge_distance_inches"
  )
  inventory_column <- if (role == "offense") {
    "bat_team_challenges_before"
  } else {
    "fld_team_challenges_before"
  }
  stop_if_missing_columns(
    source, c(required, inventory_column),
    paste(role, "revealed-selection source")
  )
  retained <- intersect(
    revealed_challenge_selection_1d_source_allowlist(), names(source)
  )
  x <- data.table::copy(source[, ..retained])
  assert_revealed_challenge_selection_1d_outcome_free(
    x, paste(role, "revealed-selection allowlist")
  )
  edge <- .revealed_challenge_selection_exact_edge_1d(
    x, geometry_tolerance = geometry_tolerance
  )
  physical_margin <- edge$value
  inventory <- as.integer(x[[inventory_column]])
  expected_call <- if (role == "offense") "called_strike" else "ball"
  acting_team <- if (role == "offense") x$bat_team_id else x$fld_team_id
  expected_role <- if (role == "offense") "batter" else c("catcher", "pitcher")
  team_challenge <- x$challenge_occurred %in% TRUE &
    as.character(x$challenger_team_id) == as.character(acting_team)
  role_challenge <- x$challenger_role %in% expected_role
  invalid_challenge <- x$initial_call == expected_call &
    x$challenge_occurred %in% TRUE & (!team_challenge | !role_challenge)
  if (any(invalid_challenge)) {
    stop(
      "An observed ", role,
      " opportunity has a challenge attributed to the wrong team or role",
      call. = FALSE
    )
  }
  tracking_ok <- if ("tracking_available" %in% names(x)) {
    x$tracking_available %in% TRUE
  } else {
    is.finite(physical_margin)
  }
  abs_ok <- if ("abs_eligible" %in% names(x)) {
    x$abs_eligible %in% TRUE
  } else {
    rep(TRUE, nrow(x))
  }
  inventory_ok <- !is.na(inventory) & if (isTRUE(require_positive_inventory)) {
    inventory >= 1L
  } else {
    inventory >= 0L
  }
  keep <- x$initial_call == expected_call & inventory_ok &
    is.finite(physical_margin) & tracking_ok & abs_ok &
    !is.na(x$game_pk) & !is.na(x$pitch_order)
  x <- x[keep]
  physical_margin <- physical_margin[keep]
  inventory <- inventory[keep]
  team_challenge <- team_challenge[keep]
  batter <- .revealed_challenge_selection_id_1d(x$batter_id)
  pitcher <- .revealed_challenge_selection_id_1d(x$pitcher_id)
  catcher <- .revealed_challenge_selection_id_1d(x$fielder_2)
  dyad <- paste(pitcher, catcher, sep = "|")
  team <- .revealed_challenge_selection_id_1d(
    if (role == "offense") x$bat_team_id else x$fld_team_id
  )
  gain <- .revealed_challenge_selection_gain_1d(x, re_model, role)
  batting_score <- if ("bat_score" %in% names(x)) {
    as.numeric(x$bat_score)
  } else {
    rep(0, nrow(x))
  }
  fielding_score <- if ("fld_score" %in% names(x)) {
    as.numeric(x$fld_score)
  } else {
    rep(0, nrow(x))
  }
  batting_score_margin <- batting_score - fielding_score
  batting_score_margin[!is.finite(batting_score_margin)] <- 0
  x[, `:=`(
    game_pk = as.character(game_pk),
    pitch_order = as.integer(pitch_order),
    role = role,
    challenged = as.integer(team_challenge),
    inventory_before = inventory,
    physical_edge_distance_inches = physical_margin,
    role_margin_inches = if (role == "offense") physical_margin else -physical_margin,
    margin_source = edge$source[keep],
    batter_id = batter,
    pitcher_id = pitcher,
    catcher_id = catcher,
    pitcher_catcher_dyad_id = dyad,
    team_id = team,
    decision_unit_id = if (role == "offense") batter else dyad
  )]
  x[, `:=`(
    stake_G = gain$value,
    stake_G_source = gain$source,
    role_score_margin = if (role_value == "offense") {
      batting_score_margin
    } else {
      -batting_score_margin
    }
  )]
  x <- .revealed_challenge_selection_context_1d(
    x, slow_cutoff_mph = slow_cutoff_mph
  )
  keep_columns <- c(
    "game_pk", "pitch_order", "at_bat_number", "pitch_number", "role",
    "challenged", "inventory_before", "initial_call",
    "physical_edge_distance_inches", "role_margin_inches", "margin_source",
    "batter_id", "pitcher_id", "catcher_id", "pitcher_catcher_dyad_id",
    "team_id", "decision_unit_id", "release_speed_mph", "pitch_type",
    "pitch_family_coarse", "count_state", "is_eephus", "is_slow",
    "speed_stratum", "margin_stratum", "selection_stratum", "matchup",
    "inning", "outs_before", "outs_state", "inventory_state",
    "role_score_margin", "stake_G", "stake_G_source", "umpire_id"
  )
  for (column in setdiff(keep_columns, names(x))) x[, (column) := NA]
  out <- x[, ..keep_columns]
  assert_revealed_challenge_selection_1d_outcome_free(
    out, paste(role, "revealed-selection rows")
  )
  data.table::setorder(out, game_pk, pitch_order)
  if (!nrow(out) || anyDuplicated(out[, .(game_pk, pitch_order)])) {
    stop(role, " revealed-selection rows are empty or duplicated", call. = FALSE)
  }
  data.table::setattr(out, "information_set", c(
    if (isTRUE(require_positive_inventory)) {
      "inventory_available"
    } else {
      "all_observed_inventory_states_for_policy_scoring"
    },
    "initial_adverse_call", "challenge_or_pass",
    "exact_role_oriented_margin", "release_speed", "coarse_pitch_family"
  ))
  data.table::setattr(out, "slow_cutoff_mph", as.numeric(slow_cutoff_mph))
  out[]
}

build_revealed_challenge_selection_offense_1d <- function(
  pitch_ledger, re_model = NULL, slow_cutoff_mph = 75,
  geometry_tolerance = 1e-8, require_positive_inventory = TRUE
) {
  .revealed_challenge_selection_build_role_1d(
    pitch_ledger, "offense", re_model, slow_cutoff_mph, geometry_tolerance,
    require_positive_inventory
  )
}

build_revealed_challenge_selection_defense_1d <- function(
  pitch_ledger, re_model = NULL, slow_cutoff_mph = 75,
  geometry_tolerance = 1e-8, require_positive_inventory = TRUE
) {
  .revealed_challenge_selection_build_role_1d(
    pitch_ledger, "defense", re_model, slow_cutoff_mph, geometry_tolerance,
    require_positive_inventory
  )
}

build_revealed_challenge_selection_1d <- function(
  pitch_ledger, re_model = NULL, slow_cutoff_mph = 75,
  geometry_tolerance = 1e-8, require_positive_inventory = TRUE
) {
  data.table::rbindlist(list(
    build_revealed_challenge_selection_offense_1d(
      pitch_ledger, re_model, slow_cutoff_mph, geometry_tolerance,
      require_positive_inventory
    ),
    build_revealed_challenge_selection_defense_1d(
      pitch_ledger, re_model, slow_cutoff_mph, geometry_tolerance,
      require_positive_inventory
    )
  ), use.names = TRUE, fill = TRUE)[order(game_pk, pitch_order)]
}

.revealed_selection_factor_columns_1d <- function(full_support) {
  c(
    "count_state", "pitch_family_coarse", "matchup", "outs_state",
    "inventory_state"
  )
}

.revealed_selection_random_columns_1d <- function(role) {
  role <- match.arg(role, revealed_challenge_selection_1d_roles())
  if (role == "offense") {
    c("batter_id", "team_id")
  } else {
    c("pitcher_catcher_dyad_id", "team_id")
  }
}

.revealed_selection_training_spec_1d <- function(rows, full_support) {
  x <- data.table::copy(data.table::as.data.table(rows))
  role <- unique(as.character(x$role))
  if (length(role) != 1L || !role %in% revealed_challenge_selection_1d_roles()) {
    stop("A selection-model specification must contain exactly one role")
  }
  factor_columns <- .revealed_selection_factor_columns_1d(full_support)
  random_columns <- .revealed_selection_random_columns_1d(role)
  global_speed <- stats::median(x$release_speed_mph, na.rm = TRUE)
  if (!is.finite(global_speed)) global_speed <- 90
  family_speed <- x[is.finite(release_speed_mph), .(
    family_speed = stats::median(release_speed_mph)
  ), by = pitch_family_coarse]
  family_map <- stats::setNames(family_speed$family_speed, family_speed$pitch_family_coarse)
  missing_speed <- !is.finite(x$release_speed_mph)
  imputed <- unname(family_map[x$pitch_family_coarse])
  imputed[!is.finite(imputed)] <- global_speed
  x[, `:=`(
    release_speed_missing = factor(
      ifelse(missing_speed, "missing", "observed"),
      levels = c("observed", "missing")
    ),
    release_speed_model = ifelse(missing_speed, imputed, release_speed_mph)
  )]
  for (column in c("stake_G", "inning", "role_score_margin")) {
    if (!column %in% names(x)) x[, (column) := 0]
    value <- as.numeric(x[[column]])
    value[!is.finite(value)] <- if (column == "inning") 5 else 0
    x[, (column) := value]
  }
  factor_levels <- lapply(factor_columns, function(column) {
    value <- as.character(x[[column]])
    value[is.na(value) | !nzchar(value)] <- "unknown"
    sort(unique(value))
  })
  names(factor_levels) <- factor_columns
  random_levels <- lapply(random_columns, function(column) {
    value <- .revealed_challenge_selection_id_1d(x[[column]])
    sort(unique(value))
  })
  names(random_levels) <- random_columns
  for (column in factor_columns) {
    value <- as.character(x[[column]])
    value[is.na(value) | !nzchar(value)] <- "unknown"
    x[, (column) := factor(value, levels = factor_levels[[column]])]
  }
  for (column in random_columns) {
    x[, (column) := factor(
      .revealed_challenge_selection_id_1d(get(column)),
      levels = random_levels[[column]]
    )]
  }
  list(
    data = x,
    full_support = isTRUE(full_support),
    factor_levels = factor_levels,
    random_levels = random_levels,
    family_speed = family_map,
    global_speed = global_speed
  )
}

.revealed_selection_score_data_1d <- function(rows, specification) {
  x <- data.table::copy(data.table::as.data.table(rows))
  missing_speed <- !is.finite(x$release_speed_mph)
  imputed <- unname(specification$family_speed[x$pitch_family_coarse])
  imputed[!is.finite(imputed)] <- specification$global_speed
  x[, `:=`(
    release_speed_missing = factor(
      ifelse(missing_speed, "missing", "observed"),
      levels = c("observed", "missing")
    ),
    release_speed_model = ifelse(missing_speed, imputed, release_speed_mph)
  )]
  for (column in c("stake_G", "inning", "role_score_margin")) {
    if (!column %in% names(x)) x[, (column) := 0]
    value <- as.numeric(x[[column]])
    value[!is.finite(value)] <- if (column == "inning") 5 else 0
    x[, (column) := value]
  }
  unseen <- list()
  for (column in names(specification$factor_levels)) {
    value <- as.character(x[[column]])
    value[is.na(value) | !nzchar(value)] <- "unknown"
    levels <- specification$factor_levels[[column]]
    is_unseen <- !value %in% levels
    value[is_unseen] <- levels[[1L]]
    x[, (column) := factor(value, levels = levels)]
    unseen[[column]] <- is_unseen
  }
  for (column in names(specification$random_levels)) {
    value <- .revealed_challenge_selection_id_1d(x[[column]])
    levels <- specification$random_levels[[column]]
    is_unseen <- !value %in% levels
    value[is_unseen] <- levels[[1L]]
    x[, (column) := factor(value, levels = levels)]
    unseen[[column]] <- is_unseen
  }
  list(data = x, unseen = unseen)
}

.revealed_selection_smooth_term_1d <- function(value, name, maximum_k) {
  unique_n <- data.table::uniqueN(value)
  if (unique_n < 4L || stats::sd(as.numeric(value)) < 1e-10) return(name)
  k <- min(as.integer(maximum_k), unique_n - 1L)
  sprintf("s(%s, k = %s, bs = 'cr')", name, k)
}

.revealed_selection_formula_1d <- function(specification) {
  x <- specification$data
  full <- specification$full_support
  margin <- if (full) {
    .revealed_selection_smooth_term_1d(
      x$role_margin_inches, "role_margin_inches", 12L
    )
  } else {
    "role_margin_inches"
  }
  speed <- if (full) {
    c(
      .revealed_selection_smooth_term_1d(
        x$release_speed_model, "release_speed_model", 8L
      ),
      if (data.table::uniqueN(x$release_speed_missing) > 1L) {
        "release_speed_missing"
      }
    )
  }
  margin_speed <- if (full &&
      data.table::uniqueN(x$role_margin_inches) >= 8L &&
      data.table::uniqueN(x$release_speed_model) >= 8L) {
    "ti(role_margin_inches, release_speed_model, k = c(8, 6), bs = c('cr', 'cr'))"
  }
  game_state <- c(
    .revealed_selection_smooth_term_1d(x$stake_G, "stake_G", 7L),
    .revealed_selection_smooth_term_1d(x$inning, "inning", 7L),
    .revealed_selection_smooth_term_1d(
      x$role_score_margin, "role_score_margin", 9L
    )
  )
  fixed <- names(specification$factor_levels)[vapply(
    specification$factor_levels, length, integer(1)
  ) > 1L]
  random <- names(specification$random_levels)[vapply(
    specification$random_levels, length, integer(1)
  ) > 1L]
  rhs <- c(
    margin, speed, margin_speed, game_state, fixed,
    sprintf("s(%s, bs = 're')", random)
  )
  list(
    formula = stats::as.formula(
      paste("challenged ~", paste(rhs, collapse = " + "))
    ),
    random_columns = random,
    formula_text = paste(rhs, collapse = " + ")
  )
}

.revealed_selection_predict_1d <- function(fit, rows, specification, formula) {
  scored <- .revealed_selection_score_data_1d(rows, specification)
  terms <- stats::predict(fit, newdata = scored$data, type = "terms")
  if (is.null(dim(terms))) terms <- matrix(terms, nrow = nrow(scored$data))
  eta <- rep(unname(stats::coef(fit)[["(Intercept)"]]), nrow(scored$data))
  if (ncol(terms)) {
    for (column in formula$random_columns) {
      label <- paste0("s(", column, ")")
      if (label %in% colnames(terms)) terms[scored$unseen[[column]], label] <- 0
    }
    eta <- eta + rowSums(terms)
  }
  list(
    probability = stats::pnorm(eta),
    linear_predictor = eta,
    unseen = scored$unseen
  )
}

fit_revealed_challenge_selection_role_fold_1d <- function(
  training_rows, heldout_rows, role, local_margin_limit_inches = 3,
  nthreads = 1L, keep_models = FALSE
) {
  role <- match.arg(role, revealed_challenge_selection_1d_roles())
  role_value <- role
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("The revealed-selection benchmark requires mgcv", call. = FALSE)
  }
  train <- data.table::copy(data.table::as.data.table(training_rows))
  test <- data.table::copy(data.table::as.data.table(heldout_rows))
  assert_revealed_challenge_selection_1d_outcome_free(train, "training rows")
  assert_revealed_challenge_selection_1d_outcome_free(test, "held-out rows")
  required <- c(
    "game_pk", "pitch_order", "role", "challenged", "role_margin_inches",
    "release_speed_mph", "pitch_family_coarse", "count_state",
    "speed_stratum", "margin_stratum", "batter_id",
    "pitcher_catcher_dyad_id", "team_id", "matchup", "outs_state",
    "inventory_state", "stake_G", "inning", "role_score_margin"
  )
  stop_if_missing_columns(train, required, "revealed-selection training rows")
  stop_if_missing_columns(test, required, "revealed-selection held-out rows")
  train <- train[role == role_value]
  test <- test[role == role_value]
  if (length(intersect(unique(train$game_pk), unique(test$game_pk)))) {
    stop("Revealed-selection training and held-out games overlap", call. = FALSE)
  }
  if (nrow(train) < 100L || sum(train$challenged) < 5L ||
      sum(1L - train$challenged) < 5L) {
    stop(role_value, " training rows lack challenge/pass support", call. = FALSE)
  }
  local <- train[abs(role_margin_inches) <= local_margin_limit_inches]
  if (nrow(local) < 100L || sum(local$challenged) < 5L ||
      sum(1L - local$challenged) < 5L) {
    stop(role_value, " local baseline lacks challenge/pass support", call. = FALSE)
  }
  fit_one <- function(rows, full_support) {
    specification <- .revealed_selection_training_spec_1d(rows, full_support)
    formula <- .revealed_selection_formula_1d(specification)
    fit_warnings <- character()
    fit <- withCallingHandlers(
      mgcv::bam(
        formula$formula, data = specification$data,
        family = stats::binomial(link = "probit"), method = "fREML",
        discrete = TRUE, nthreads = as.integer(nthreads), gc.level = 1L
      ),
      warning = function(condition) {
        fit_warnings <<- c(fit_warnings, conditionMessage(condition))
      }
    )
    list(
      fit = fit, specification = specification, formula = formula,
      warnings = unique(fit_warnings)
    )
  }
  full_fit <- fit_one(train, TRUE)
  local_fit <- fit_one(local, FALSE)
  full_prediction <- .revealed_selection_predict_1d(
    full_fit$fit, test, full_fit$specification, full_fit$formula
  )
  baseline_prediction <- .revealed_selection_predict_1d(
    local_fit$fit, test, local_fit$specification, local_fit$formula
  )
  clip <- function(value) pmin(1 - 1e-12, pmax(1e-12, value))
  full_probability <- clip(full_prediction$probability)
  baseline_probability <- clip(baseline_prediction$probability)
  oof <- test[, .(
    game_pk, pitch_order, role, challenged, role_margin_inches,
    release_speed_mph, pitch_family_coarse, count_state, is_eephus, is_slow,
    speed_stratum, margin_stratum, selection_stratum, matchup, inning,
    outs_state, inventory_before, inventory_state, stake_G,
    role_score_margin, decision_unit_id, batter_id,
    pitcher_catcher_dyad_id, team_id
  )]
  actor_column <- if (role_value == "offense") {
    "batter_id"
  } else {
    "pitcher_catcher_dyad_id"
  }
  oof[, `:=`(
    full_support_probability = full_probability,
    local_baseline_probability = baseline_probability,
    full_support_log_loss = -(
      challenged * log(full_probability) +
        (1 - challenged) * log1p(-full_probability)
    ),
    local_baseline_log_loss = -(
      challenged * log(baseline_probability) +
        (1 - challenged) * log1p(-baseline_probability)
    ),
    baseline_extrapolated = abs(role_margin_inches) > local_margin_limit_inches,
    full_unit_fallback = full_prediction$unseen[[actor_column]],
    full_team_fallback = full_prediction$unseen[["team_id"]],
    local_unit_fallback = baseline_prediction$unseen[[actor_column]],
    local_team_fallback = baseline_prediction$unseen[["team_id"]]
  )]
  diagnostic <- data.table::data.table(
    role = role_value,
    training_rows_full = nrow(train),
    training_rows_local = nrow(local),
    training_challenges_full = sum(train$challenged),
    training_challenges_local = sum(local$challenged),
    heldout_rows = nrow(test),
    heldout_local_rows = sum(abs(test$role_margin_inches) <= local_margin_limit_inches),
    heldout_tail_rows = sum(abs(test$role_margin_inches) > local_margin_limit_inches),
    full_formula = full_fit$formula$formula_text,
    local_formula = local_fit$formula$formula_text,
    local_margin_limit_inches = local_margin_limit_inches,
    local_margin_probit_slope = unname(
      stats::coef(local_fit$fit)[["role_margin_inches"]]
    ),
    effective_width_inches = {
      slope <- unname(stats::coef(local_fit$fit)[["role_margin_inches"]])
      if (is.finite(slope) && slope > 0) 1 / slope else NA_real_
    },
    effective_width_status = {
      slope <- unname(stats::coef(local_fit$fit)[["role_margin_inches"]])
      if (is.finite(slope) && slope > 0) {
        "identified_positive_common_probit_slope"
      } else {
        "invalid_nonpositive_common_probit_slope"
      }
    },
    full_fit_converged = isTRUE(full_fit$fit$converged),
    local_fit_converged = isTRUE(local_fit$fit$converged),
    full_fit_warning_count = length(full_fit$warnings),
    local_fit_warning_count = length(local_fit$warnings),
    full_fit_warnings = paste(full_fit$warnings, collapse = " | "),
    local_fit_warnings = paste(local_fit$warnings, collapse = " | ")
  )
  list(
    oof_predictions = oof[], diagnostics = diagnostic[],
    full_model = if (isTRUE(keep_models)) full_fit else NULL,
    local_model = if (isTRUE(keep_models)) local_fit else NULL
  )
}

.revealed_selection_clustered_difference_1d <- function(rows, scope) {
  x <- data.table::as.data.table(rows)
  difference <- x$local_baseline_log_loss - x$full_support_log_loss
  estimate <- mean(difference)
  by_game <- data.table::data.table(
    game_pk = x$game_pk, residual = difference - estimate
  )[, .(residual_sum = sum(residual)), by = game_pk]
  games <- nrow(by_game)
  se <- if (games >= 2L) {
    sqrt(games / (games - 1) * sum(by_game$residual_sum^2) / nrow(x)^2)
  } else {
    NA_real_
  }
  has_calibration <- all(c(
    "challenged", "full_support_probability", "local_baseline_probability"
  ) %in% names(x))
  full_calibration <- local_calibration <- calibration_difference <-
    calibration_se <- full_calibration_se <- local_calibration_se <-
      observed_rate <- full_rate <- local_rate <- NA_real_
  if (has_calibration) {
    observed_rate <- mean(x$challenged)
    full_rate <- mean(x$full_support_probability)
    local_rate <- mean(x$local_baseline_probability)
    full_calibration <- full_rate - observed_rate
    local_calibration <- local_rate - observed_rate
    calibration_difference <-
      abs(local_calibration) - abs(full_calibration)
    calibration_contribution <-
      x$local_baseline_probability - x$full_support_probability
    calibration_game <- data.table::data.table(
      game_pk = x$game_pk,
      residual = calibration_contribution - mean(calibration_contribution)
    )[, .(residual_sum = sum(residual)), by = game_pk]
    calibration_se <- if (games >= 2L) {
      sqrt(
        games / (games - 1) *
          sum(calibration_game$residual_sum^2) / nrow(x)^2
      )
    } else {
      NA_real_
    }
    clustered_calibration_se <- function(probability) {
      residual <- probability - x$challenged
      estimate <- mean(residual)
      by_game <- data.table::data.table(
        game_pk = x$game_pk,
        centered = residual - estimate
      )[, .(centered_sum = sum(centered)), by = game_pk]
      if (nrow(by_game) < 2L) return(NA_real_)
      sqrt(
        nrow(by_game) / (nrow(by_game) - 1L) *
          sum(by_game$centered_sum^2) / nrow(x)^2
      )
    }
    full_calibration_se <- clustered_calibration_se(
      x$full_support_probability
    )
    local_calibration_se <- clustered_calibration_se(
      x$local_baseline_probability
    )
  }
  data.table::data.table(
    scope = scope, rows = nrow(x), games = games,
    full_support_log_loss = mean(x$full_support_log_loss),
    local_baseline_log_loss = mean(x$local_baseline_log_loss),
    full_minus_local_log_loss = -estimate,
    improvement_full_over_local = estimate,
    game_clustered_se = se,
    one_se_improvement = is.finite(se) && estimate > se,
    one_se_noninferior = is.finite(se) && estimate + se >= 0,
    observed_challenge_rate = observed_rate,
    full_support_predicted_rate = full_rate,
    local_baseline_predicted_rate = local_rate,
    full_support_calibration_error = full_calibration,
    local_baseline_calibration_error = local_calibration,
    full_support_calibration_game_clustered_se = full_calibration_se,
    local_baseline_calibration_game_clustered_se = local_calibration_se,
    full_support_calibrated_within_one_se = is.finite(full_calibration_se) &&
      abs(full_calibration) <= full_calibration_se,
    local_baseline_calibrated_within_one_se = is.finite(local_calibration_se) &&
      abs(local_calibration) <= local_calibration_se,
    absolute_calibration_improvement = calibration_difference,
    calibration_difference_game_clustered_se = calibration_se,
    one_se_calibration_improvement = is.finite(calibration_se) &&
      calibration_difference > calibration_se
  )
}

compare_revealed_challenge_selection_1d <- function(oof_predictions) {
  x <- data.table::as.data.table(oof_predictions)
  required <- c(
    "game_pk", "role", "margin_stratum", "speed_stratum",
    "full_support_log_loss", "local_baseline_log_loss"
  )
  stop_if_missing_columns(x, required, "revealed-selection OOF predictions")
  scopes <- list(
    full_support = rep(TRUE, nrow(x)),
    local_abs_margin_le_3 = x$margin_stratum == "local_abs_margin_le_3",
    tail_abs_margin_gt_3 = x$margin_stratum == "tail_abs_margin_gt_3",
    slow_or_eephus = x$speed_stratum %in% c("slow_non_eephus", "eephus"),
    ordinary_speed_tail = x$margin_stratum == "tail_abs_margin_gt_3" &
      x$speed_stratum == "regular_speed",
    slow_or_eephus_tail = x$margin_stratum == "tail_abs_margin_gt_3" &
      x$speed_stratum %in% c("slow_non_eephus", "eephus")
  )
  out <- data.table::rbindlist(lapply(unique(x$role), function(role_value) {
    role_rows <- x[role == role_value]
    data.table::rbindlist(lapply(names(scopes), function(scope) {
      selected <- switch(scope,
        full_support = rep(TRUE, nrow(role_rows)),
        local_abs_margin_le_3 =
          role_rows$margin_stratum == "local_abs_margin_le_3",
        tail_abs_margin_gt_3 =
          role_rows$margin_stratum == "tail_abs_margin_gt_3",
        slow_or_eephus =
          role_rows$speed_stratum %in% c("slow_non_eephus", "eephus"),
        ordinary_speed_tail =
          role_rows$margin_stratum == "tail_abs_margin_gt_3" &
            role_rows$speed_stratum == "regular_speed",
        slow_or_eephus_tail =
          role_rows$margin_stratum == "tail_abs_margin_gt_3" &
            role_rows$speed_stratum %in% c("slow_non_eephus", "eephus")
      )
      if (!any(selected)) return(NULL)
      value <- .revealed_selection_clustered_difference_1d(
        role_rows[selected], scope
      )
      value[, role := role_value]
      value
    }), fill = TRUE)
  }), fill = TRUE)
  data.table::setcolorder(out, c("role", setdiff(names(out), "role")))
  out[]
}

gate_revealed_challenge_selection_1d <- function(comparison) {
  x <- data.table::as.data.table(comparison)
  stop_if_missing_columns(
    x, c("role", "scope", "one_se_improvement", "one_se_noninferior"),
    "revealed-selection comparison"
  )
  data.table::rbindlist(lapply(unique(x$role), function(role_value) {
    full <- x[role == role_value & scope == "full_support"]
    local <- x[role == role_value & scope == "local_abs_margin_le_3"]
    tails <- x[
      role == role_value &
        scope %in% c("ordinary_speed_tail", "slow_or_eephus_tail")
    ]
    if (nrow(full) != 1L || nrow(local) != 1L) {
      stop("Each role needs full-support and local comparison rows")
    }
    tail_fix <- any(
      tails$one_se_calibration_improvement & tails$one_se_noninferior,
      na.rm = TRUE
    )
    promoted <- full$one_se_improvement ||
      (full$one_se_noninferior && tail_fix)
    tail_pass <- function(scope_value) {
      value <- tails[get("scope") == scope_value]
      if (nrow(value) != 1L) return(FALSE)
      if (promoted) {
        isTRUE(value$full_support_calibrated_within_one_se[[1L]])
      } else {
        isTRUE(value$local_baseline_calibrated_within_one_se[[1L]])
      }
    }
    ordinary_tail_pass <- tail_pass("ordinary_speed_tail")
    slow_tail_pass <- tail_pass("slow_or_eephus_tail")
    data.table::data.table(
      role = role_value,
      full_support_one_se_improvement = full$one_se_improvement,
      full_support_one_se_noninferior = full$one_se_noninferior,
      local_one_se_noninferior = local$one_se_noninferior,
      predeclared_tail_calibration_fix = tail_fix,
      promote_speed_aware_full_support = promoted,
      ordinary_speed_tail_calibrated = ordinary_tail_pass,
      slow_or_eephus_tail_calibrated = slow_tail_pass,
      pooled_tail_interpretation_allowed = ordinary_tail_pass && slow_tail_pass,
      tail_interpretation = if (ordinary_tail_pass && slow_tail_pass) {
        "pooled tail interpretation allowed"
      } else {
        paste(
          "pooled tail interpretation blocked; retain separate ordinary",
          "and slow/eephus diagnostics"
        )
      },
      promotion_reason = data.table::fcase(
        full$one_se_improvement,
        "overall held-out log loss improves by at least one clustered SE",
        full$one_se_noninferior && tail_fix,
        paste(
          "predeclared tail calibration improves by one clustered SE",
          "without overall one-SE degradation"
        ),
        default = "retain local common-probit baseline"
      )
    )
  }))
}

crossfit_revealed_challenge_selection_1d <- function(
  rows, fold_assignment = NULL, folds = 5L, seed = 20260826L,
  local_margin_limit_inches = 3, nthreads = 1L,
  keep_models = FALSE, progress = interactive()
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  assert_revealed_challenge_selection_1d_outcome_free(x, "cross-fit rows")
  stop_if_missing_columns(
    x, c("game_pk", "pitch_order", "role", "challenged"),
    "revealed-selection cross-fit rows"
  )
  if ("inventory_before" %in% names(x) &&
      any(as.integer(x$inventory_before) < 1L, na.rm = TRUE)) {
    stop("Revealed-selection likelihood requires positive observed inventory")
  }
  games <- sort(unique(as.character(x$game_pk)))
  if (is.null(fold_assignment)) {
    fold_assignment <- continuous_game_folds(games, folds = folds, seed = seed)
  }
  assignment <- normalize_joint_common_width_fold_assignment_1d(
    fold_assignment, games
  )
  folds <- max(assignment$fold)
  x[, game_pk := as.character(game_pk)]
  x[, fold := assignment$fold[match(game_pk, assignment$game_pk)]]
  parts <- diagnostics <- list()
  models <- if (isTRUE(keep_models)) vector("list", folds) else NULL
  index <- 0L
  for (fold_id in seq_len(folds)) {
    if (isTRUE(keep_models)) models[[fold_id]] <- list()
    for (role_value in revealed_challenge_selection_1d_roles()) {
      index <- index + 1L
      if (isTRUE(progress)) {
        message("revealed-selection fold ", fold_id, "/", folds, " ", role_value)
      }
      fitted <- fit_revealed_challenge_selection_role_fold_1d(
        x[fold != fold_id], x[fold == fold_id], role = role_value,
        local_margin_limit_inches = local_margin_limit_inches,
        nthreads = nthreads, keep_models = keep_models
      )
      fitted$oof_predictions[, fold := fold_id]
      fitted$diagnostics[, fold := fold_id]
      parts[[index]] <- fitted$oof_predictions
      diagnostics[[index]] <- fitted$diagnostics
      if (isTRUE(keep_models)) {
        models[[fold_id]][[role_value]] <- list(
          full = fitted$full_model, local = fitted$local_model
        )
      }
    }
  }
  oof <- data.table::rbindlist(parts, fill = TRUE)
  comparison <- compare_revealed_challenge_selection_1d(oof)
  gate <- gate_revealed_challenge_selection_1d(comparison)
  oof <- merge(
    oof,
    gate[, .(role, promote_speed_aware_full_support)],
    by = "role", all.x = TRUE, sort = FALSE
  )
  oof[, `:=`(
    selected_model = ifelse(
      promote_speed_aware_full_support,
      "full_support_speed_aware_gam", "local_common_probit"
    ),
    challenge_probability = ifelse(
      promote_speed_aware_full_support,
      full_support_probability, local_baseline_probability
    ),
    unit_fallback = ifelse(
      promote_speed_aware_full_support,
      full_unit_fallback, local_unit_fallback
    ),
    team_fallback = ifelse(
      promote_speed_aware_full_support,
      full_team_fallback, local_team_fallback
    )
  )]
  diagnostic_table <- data.table::rbindlist(diagnostics, fill = TRUE)
  width_estimates <- diagnostic_table[, .(
    fold,
    role,
    sigma_inches = effective_width_inches,
    margin_probit_slope = local_margin_probit_slope,
    margin_limit_inches = local_margin_limit_inches,
    width_status = effective_width_status,
    training_rows = training_rows_local,
    training_challenges = training_challenges_local
  )]
  if (any(!is.finite(width_estimates$sigma_inches) |
      width_estimates$sigma_inches <= 0)) {
    stop("At least one fold/role effective width is not identified")
  }
  list(
    fold_assignment = assignment[],
    oof_predictions = oof[],
    diagnostics = diagnostic_table[],
    width_estimates = width_estimates[],
    comparison = comparison[], gate = gate[], models = models,
    information_set = c(
      "challenge_or_pass_only", "inventory_available", "initial_adverse_call",
      "exact_oriented_margin", "release_speed", "coarse_pitch_family",
      "game_cross_fitted"
    )
  )
}

score_revealed_challenge_selection_policy_clock_1d <- function(
  rows, crossfit_result
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  assert_revealed_challenge_selection_1d_outcome_free(
    x, "revealed-selection policy clock"
  )
  stop_if_missing_columns(
    x,
    c(
      "game_pk", "pitch_order", "role", "role_margin_inches",
      "inventory_before"
    ),
    "revealed-selection policy clock"
  )
  if (!is.list(crossfit_result) || is.null(crossfit_result$models) ||
      is.null(crossfit_result$fold_assignment) ||
      is.null(crossfit_result$gate)) {
    stop("Policy-clock scoring requires retained cross-fit model bundles")
  }
  assignment <- data.table::as.data.table(crossfit_result$fold_assignment)
  x[, game_pk := as.character(game_pk)]
  x[, fold := assignment$fold[match(game_pk, as.character(assignment$game_pk))]]
  if (anyNA(x$fold)) {
    stop("Policy-clock rows include games absent from the cross-fit assignment")
  }
  pieces <- list()
  index <- 0L
  for (fold_id in sort(unique(x$fold))) {
    for (role_value in revealed_challenge_selection_1d_roles()) {
      model <- crossfit_result$models[[fold_id]][[role_value]]
      if (is.null(model$full) || is.null(model$local)) {
        stop("A retained fold/role selection model bundle is missing")
      }
      block <- x[fold == fold_id & role == role_value]
      if (!nrow(block)) next
      predictions <- lapply(1:2, function(inventory) {
        scored <- data.table::copy(block)
        scored[, `:=`(
          inventory_before = inventory,
          inventory_state = as.character(inventory)
        )]
        full <- .revealed_selection_predict_1d(
          model$full$fit,
          scored,
          model$full$specification,
          model$full$formula
        )
        local <- .revealed_selection_predict_1d(
          model$local$fit,
          scored,
          model$local$specification,
          model$local$formula
        )
        actor_column <- if (role_value == "offense") {
          "batter_id"
        } else {
          "pitcher_catcher_dyad_id"
        }
        list(
          full = pmin(1 - 1e-12, pmax(1e-12, full$probability)),
          local = pmin(1 - 1e-12, pmax(1e-12, local$probability)),
          full_unit_fallback = full$unseen[[actor_column]],
          local_unit_fallback = local$unseen[[actor_column]],
          full_team_fallback = full$unseen[["team_id"]],
          local_team_fallback = local$unseen[["team_id"]]
        )
      })
      promote <- crossfit_result$gate[
        role == role_value, promote_speed_aware_full_support
      ]
      if (length(promote) != 1L || is.na(promote)) {
        stop("Every role needs one frozen revealed-selection promotion result")
      }
      index <- index + 1L
      pieces[[index]] <- block[, .(
        game_pk, pitch_order, role, fold, challenged, role_margin_inches,
        count_state, pitch_family_coarse, release_speed_mph, speed_stratum,
        margin_stratum, selection_stratum, decision_unit_id, team_id,
        inning, stake_G, role_score_margin
      )]
      pieces[[index]][, `:=`(
        selected_model = if (promote) {
          "full_support_speed_aware_gam"
        } else {
          "local_common_probit"
        },
        probability_k1 = if (promote) {
          predictions[[1L]]$full
        } else {
          predictions[[1L]]$local
        },
        probability_k2 = if (promote) {
          predictions[[2L]]$full
        } else {
          predictions[[2L]]$local
        },
        unit_fallback_k1 = if (promote) {
          predictions[[1L]]$full_unit_fallback
        } else {
          predictions[[1L]]$local_unit_fallback
        },
        unit_fallback_k2 = if (promote) {
          predictions[[2L]]$full_unit_fallback
        } else {
          predictions[[2L]]$local_unit_fallback
        },
        team_fallback_k1 = if (promote) {
          predictions[[1L]]$full_team_fallback
        } else {
          predictions[[1L]]$local_team_fallback
        },
        team_fallback_k2 = if (promote) {
          predictions[[2L]]$full_team_fallback
        } else {
          predictions[[2L]]$local_team_fallback
        }
      )]
    }
  }
  out <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  data.table::setorder(out, game_pk, pitch_order, role)
  if (nrow(out) != nrow(x) ||
      anyDuplicated(out[, .(game_pk, pitch_order, role)]) ||
      any(!is.finite(out$probability_k1)) ||
      any(!is.finite(out$probability_k2)) ||
      any(out$probability_k1 < 0 | out$probability_k1 > 1) ||
      any(out$probability_k2 < 0 | out$probability_k2 > 1)) {
    stop("Revealed-selection policy-clock OOF predictions are incomplete")
  }
  out[]
}
