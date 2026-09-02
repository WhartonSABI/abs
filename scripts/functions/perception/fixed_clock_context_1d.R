swing_take_pitch_family <- function(pitch_type) {
  pitch_type <- as.character(pitch_type)
  data.table::fcase(
    pitch_type %in% c("FF", "FA", "SI", "FC"), "fastball",
    pitch_type %in% c("SL", "ST", "SV", "CU", "KC", "CS"), "breaking",
    pitch_type %in% c("CH", "FS", "FO", "SC"), "offspeed",
    default = "other"
  )
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
  strikes_column <- if ("strikes_before" %in% names(x)) {
    "strikes_before"
  } else {
    "strikes"
  }
  stop_if_missing_columns(x, c(balls_column, strikes_column), "fixed-clock context")
  stand <- if ("stand" %in% names(x)) as.character(x$stand) else rep("U", nrow(x))
  throws <- if ("p_throws" %in% names(x)) {
    as.character(x$p_throws)
  } else {
    rep("U", nrow(x))
  }
  x[, `:=`(
    balls_context = as.integer(get(balls_column)),
    strikes_context = as.integer(get(strikes_column)),
    count_state = paste0(get(balls_column), "-", get(strikes_column)),
    matchup = paste0(
      data.table::fcoalesce(stand, "U"), "-",
      data.table::fcoalesce(throws, "U")
    ),
    pitch_family = data.table::fcoalesce(as.character(pitch_family), "other")
  )]
  x[]
}

continuous_hypothetical_batter_gain <- function(rows, re_model) {
  x <- data.table::copy(data.table::as.data.table(rows))
  required <- c(
    "balls_before", "strikes_before", "outs_before", "on_1b", "on_2b",
    "on_3b", "inning", "half", "concurrent_outs", "concurrent_runs",
    "concurrent_remove_1b", "concurrent_remove_2b", "concurrent_remove_3b",
    "concurrent_add_1b", "concurrent_add_2b", "concurrent_add_3b"
  )
  stop_if_missing_columns(x, required, "hypothetical challenge gain")
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
    stop("Hypothetical challenge gain could not be evaluated")
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
  stop_if_missing_columns(x, c("game_pk", "fold"), "fixed-clock game folds")
  if (anyDuplicated(x$game_pk) || anyNA(x) ||
      !setequal(unique(x$fold), seq_len(expected_folds))) {
    stop("Each game must appear in exactly one complete fixed-clock fold")
  }
  invisible(TRUE)
}
