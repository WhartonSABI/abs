apply_concurrent_movement <- function(state, movement) {
  bases <- c(on_1b = state$on_1b, on_2b = state$on_2b, on_3b = state$on_3b)
  for (base in c("1b", "2b", "3b")) {
    if ((movement[[paste0("remove_", base)]] %||% 0L) > 0L) bases[[paste0("on_", base)]] <- 0L
    if ((movement[[paste0("add_", base)]] %||% 0L) > 0L) bases[[paste0("on_", base)]] <- 1L
  }
  state$on_1b <- bases[["on_1b"]]
  state$on_2b <- bases[["on_2b"]]
  state$on_3b <- bases[["on_3b"]]
  state$outs <- state$outs + (movement$outs %||% 0L)
  state$runs <- state$runs + (movement$runs %||% 0L)
  state
}

apply_walk <- function(state) {
  if (state$on_1b == 1L) {
    if (state$on_2b == 1L) {
      if (state$on_3b == 1L) state$runs <- state$runs + 1L
      state$on_3b <- 1L
    }
    state$on_2b <- 1L
  }
  state$on_1b <- 1L
  state
}

call_branch_state <- function(
  call, balls, strikes, outs, on_1b, on_2b, on_3b,
  movement = list(), inning = NA_integer_, half = NA_character_
) {
  state <- list(
    balls = as.integer(balls), strikes = as.integer(strikes), outs = as.integer(outs),
    on_1b = as.integer(!is.na(on_1b) && on_1b != 0),
    on_2b = as.integer(!is.na(on_2b) && on_2b != 0),
    on_3b = as.integer(!is.na(on_3b) && on_3b != 0),
    runs = 0L, inning = as.integer(inning), half = half
  )
  state <- apply_concurrent_movement(state, movement)
  if (call == "ball") {
    if (state$balls < 3L) {
      state$balls <- state$balls + 1L
    } else {
      state <- apply_walk(state)
      state$balls <- 0L
      state$strikes <- 0L
    }
  } else if (call == "called_strike") {
    if (state$strikes < 2L) {
      state$strikes <- state$strikes + 1L
    } else {
      state$outs <- state$outs + 1L
      state$balls <- 0L
      state$strikes <- 0L
    }
  } else {
    stop("Unsupported called-pitch branch: ", call)
  }
  state
}

lookup_re <- function(re_table, state) {
  if (state$outs >= 3L) return(0)
  row <- re_table[
    outs == state$outs &
      base_state == base_state_code(state$on_1b, state$on_2b, state$on_3b) &
      balls == state$balls & strikes == state$strikes
  ]
  if (!nrow(row)) return(NA_real_)
  row$re[[1L]]
}

branch_re <- function(re_table, state) state$runs + lookup_re(re_table, state)

state_for_wpa <- function(state, row) {
  inning <- state$inning
  half <- state$half
  if (state$outs >= 3L) {
    if (half == "top") {
      half <- "bottom"
    } else {
      half <- "top"
      inning <- inning + 1L
    }
    state$outs <- 0L
    state$on_1b <- state$on_2b <- state$on_3b <- 0L
    state$balls <- state$strikes <- 0L
  }
  home_score <- if (row$half == "bottom") row$bat_score else row$fld_score
  away_score <- if (row$half == "bottom") row$fld_score else row$bat_score
  if (row$half == "bottom") home_score <- home_score + state$runs else away_score <- away_score + state$runs
  data.frame(
    inning = pmin(as.integer(inning), 12L),
    is_bottom = as.integer(half == "bottom"),
    score_diff = pmax(-10, pmin(10, home_score - away_score)),
    outs = as.integer(state$outs),
    base_state = factor(base_state_code(state$on_1b, state$on_2b, state$on_3b),
      levels = c("000", "001", "010", "011", "100", "101", "110", "111")),
    count_state = factor(paste0(state$balls, "-", state$strikes),
      levels = as.vector(outer(0:3, 0:2, paste, sep = "-")))
  )
}

branch_home_wp <- function(wpa_model, state, row) {
  if (is.null(wpa_model)) return(NA_real_)
  as.numeric(stats::predict(wpa_model, newdata = state_for_wpa(state, row), type = "response"))
}

pitch_branch_values <- function(row, re_table, wpa_model = NULL) {
  movement <- list(
    remove_1b = row$concurrent_remove_1b, remove_2b = row$concurrent_remove_2b,
    remove_3b = row$concurrent_remove_3b, add_1b = row$concurrent_add_1b,
    add_2b = row$concurrent_add_2b, add_3b = row$concurrent_add_3b,
    runs = row$concurrent_runs, outs = row$concurrent_outs
  )
  make_state <- function(call) call_branch_state(
    call, row$balls_before, row$strikes_before, row$outs_before,
    row$on_1b, row$on_2b, row$on_3b, movement, row$inning, row$half
  )
  initial_state <- make_state(row$initial_call)
  corrected_state <- make_state(row$abs_call)
  batting_re <- branch_re(re_table, corrected_state) - branch_re(re_table, initial_state)
  home_wp <- branch_home_wp(wpa_model, corrected_state, row) -
    branch_home_wp(wpa_model, initial_state, row)
  challenger_re <- if (row$adverse_team_id == row$bat_team_id) batting_re else -batting_re
  challenger_wpa <- if (row$adverse_team_id == row$home_team_id) home_wp else -home_wp
  c(re = challenger_re, wpa = challenger_wpa)
}

vectorized_call_branch <- function(rows, calls) {
  n <- nrow(rows)
  coalesce_int <- function(name) {
    value <- rows[[name]]
    value[is.na(value)] <- 0
    as.integer(value)
  }
  state <- data.table::data.table(
    balls = as.integer(rows$balls_before),
    strikes = as.integer(rows$strikes_before),
    outs = as.integer(rows$outs_before) + coalesce_int("concurrent_outs"),
    on_1b = as.integer(!is.na(rows$on_1b) & rows$on_1b != 0),
    on_2b = as.integer(!is.na(rows$on_2b) & rows$on_2b != 0),
    on_3b = as.integer(!is.na(rows$on_3b) & rows$on_3b != 0),
    runs = coalesce_int("concurrent_runs"),
    inning = as.integer(rows$inning),
    half = rows$half
  )
  for (base in c("1b", "2b", "3b")) {
    column <- paste0("on_", base)
    removed <- coalesce_int(paste0("concurrent_remove_", base)) > 0L
    added <- coalesce_int(paste0("concurrent_add_", base)) > 0L
    state[[column]][removed] <- 0L
    state[[column]][added] <- 1L
  }

  ball <- calls == "ball"
  walk <- ball & state$balls >= 3L
  ordinary_ball <- ball & !walk
  state$balls[ordinary_ball] <- state$balls[ordinary_ball] + 1L
  if (any(walk)) {
    forced_run <- walk & state$on_1b == 1L & state$on_2b == 1L & state$on_3b == 1L
    force_third <- walk & state$on_1b == 1L & state$on_2b == 1L
    force_second <- walk & state$on_1b == 1L
    state$runs[forced_run] <- state$runs[forced_run] + 1L
    state$on_3b[force_third] <- 1L
    state$on_2b[force_second] <- 1L
    state$on_1b[walk] <- 1L
    state$balls[walk] <- 0L
    state$strikes[walk] <- 0L
  }

  called_strike <- calls == "called_strike"
  strikeout <- called_strike & state$strikes >= 2L
  ordinary_strike <- called_strike & !strikeout
  state$strikes[ordinary_strike] <- state$strikes[ordinary_strike] + 1L
  state$outs[strikeout] <- state$outs[strikeout] + 1L
  state$balls[strikeout] <- 0L
  state$strikes[strikeout] <- 0L
  if (any(!ball & !called_strike)) stop("Unsupported called-pitch branch")
  if (nrow(state) != n) stop("Vectorized branch changed row count")
  state[]
}

vectorized_branch_re <- function(re_table, state) {
  lookup <- stats::setNames(
    re_table$re,
    paste(re_table$outs, re_table$base_state, re_table$balls, re_table$strikes)
  )
  key <- paste(
    state$outs, base_state_code(state$on_1b, state$on_2b, state$on_3b),
    state$balls, state$strikes
  )
  remaining <- unname(lookup[key])
  remaining[state$outs >= 3L] <- 0
  state$runs + remaining
}

vectorized_state_for_wpa <- function(state, rows) {
  inning <- state$inning
  half <- state$half
  outs <- state$outs
  on_1b <- state$on_1b
  on_2b <- state$on_2b
  on_3b <- state$on_3b
  balls <- state$balls
  strikes <- state$strikes
  completed <- outs >= 3L
  completed_top <- completed & half == "top"
  completed_bottom <- completed & half == "bottom"
  half[completed_top] <- "bottom"
  half[completed_bottom] <- "top"
  inning[completed_bottom] <- inning[completed_bottom] + 1L
  outs[completed] <- 0L
  on_1b[completed] <- on_2b[completed] <- on_3b[completed] <- 0L
  balls[completed] <- strikes[completed] <- 0L

  home_score <- ifelse(rows$half == "bottom", rows$bat_score, rows$fld_score)
  away_score <- ifelse(rows$half == "bottom", rows$fld_score, rows$bat_score)
  home_score <- home_score + ifelse(rows$half == "bottom", state$runs, 0L)
  away_score <- away_score + ifelse(rows$half == "top", state$runs, 0L)
  data.frame(
    inning = pmin(as.integer(inning), 12L),
    is_bottom = as.integer(half == "bottom"),
    score_diff = pmax(-10, pmin(10, home_score - away_score)),
    outs = as.integer(outs),
    base_state = factor(base_state_code(on_1b, on_2b, on_3b),
      levels = c("000", "001", "010", "011", "100", "101", "110", "111")),
    count_state = factor(paste0(balls, "-", strikes),
      levels = as.vector(outer(0:3, 0:2, paste, sep = "-")))
  )
}

add_pitch_values <- function(pitch_ledger, re_model, wpa_model = NULL) {
  x <- data.table::copy(data.table::as.data.table(pitch_ledger))
  x[, `:=`(
    potential_challenger_re = NA_real_,
    potential_challenger_wpa = NA_real_
  )]
  eligible <- which(x$correctable_opportunity & !is.na(x$balls_before) &
    !is.na(x$strikes_before) & !is.na(x$outs_before))
  wpa_object <- if (is.null(wpa_model)) {
    NULL
  } else if (!is.null(wpa_model$model) && inherits(wpa_model$model, "gam")) {
    wpa_model$model
  } else {
    wpa_model
  }
  rows <- x[eligible]
  initial_state <- vectorized_call_branch(rows, rows$initial_call)
  corrected_state <- vectorized_call_branch(rows, rows$abs_call)
  batting_re <- vectorized_branch_re(re_model$table, corrected_state) -
    vectorized_branch_re(re_model$table, initial_state)
  x$potential_challenger_re[eligible] <- ifelse(
    rows$adverse_team_id == rows$bat_team_id, batting_re, -batting_re
  )
  if (!is.null(wpa_object) && length(eligible)) {
    initial_wp <- as.numeric(stats::predict(
      wpa_object, newdata = vectorized_state_for_wpa(initial_state, rows),
      type = "response"
    ))
    corrected_wp <- as.numeric(stats::predict(
      wpa_object, newdata = vectorized_state_for_wpa(corrected_state, rows),
      type = "response"
    ))
    home_delta <- corrected_wp - initial_wp
    challenger_delta <- ifelse(
      x$adverse_team_id[eligible] == x$home_team_id[eligible],
      home_delta, -home_delta
    )
    x$potential_challenger_wpa[eligible] <- challenger_delta
  }
  x[, actual_re_gain := data.table::fifelse(
    challenge_outcome == "overturned", potential_challenger_re, 0
  )]
  x[, actual_wpa_gain := data.table::fifelse(
    challenge_outcome == "overturned", potential_challenger_wpa, 0
  )]
  x[]
}

attach_savant_values <- function(challenges, savant) {
  official_cols <- intersect(c(
    "play_id", "sz_challenge_runs", "sz_challenge_overturned_runs",
    "sz_challenge_lost_runs", "sz_challenge_prob", "sz_challenge_prob_gain",
    "sz_challenge_prob_lost", "edge_dist_calc", "savant_outcome",
    "savant_initial_call"
  ), names(savant))
  out <- merge(challenges, savant[, ..official_cols], by = "play_id", all.x = TRUE,
    sort = FALSE)
  out[, savant_available := !is.na(savant_outcome)]
  out[, publication_eligible := savant_available]
  out[]
}
