normalize_feed_call <- function(code, description = NA_character_) {
  if (!is.na(code) && code == "C") return("called_strike")
  if (!is.na(code) && code == "B") return("ball")
  if (!is.na(description) && tolower(description) == "called strike") return("called_strike")
  if (!is.na(description) && tolower(description) == "ball") return("ball")
  NA_character_
}

review_outcome <- function(review) {
  value <- review$isOverturned
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) return("unresolved")
  if (isTRUE(as.logical(value[[1L]]))) "overturned" else "upheld"
}

extract_mj_reviews <- function(review) {
  if (is.null(review)) return(list())
  candidates <- c(list(review), review$additionalReviews %||% list())
  Filter(function(item) identical(scalar_chr(item$reviewType), "MJ"), candidates)
}

review_signature <- function(review) {
  paste(
    scalar_int(review$challengeTeamId), scalar_int(review$player$id),
    review_outcome(review), sep = "|"
  )
}

extract_home_plate_umpire <- function(game) {
  officials <- game$liveData$boxscore$officials %||% list()
  home_plate <- Filter(function(item) {
    identical(tolower(scalar_chr(item$officialType)), "home plate")
  }, officials)
  if (!length(home_plate)) {
    return(list(id = NA_integer_, name = NA_character_))
  }
  official <- home_plate[[1L]]$official %||% list()
  list(
    id = scalar_int(official$id),
    name = scalar_chr(official$fullName)
  )
}

called_pitch_events <- function(play) {
  events <- play$playEvents %||% list()
  Filter(function(event) {
    isTRUE(event$isPitch) &&
      normalize_feed_call(
        scalar_chr(event$details$call$code),
        scalar_chr(event$details$call$description, scalar_chr(event$details$description))
      ) %in% c("ball", "called_strike")
  }, events)
}

concurrent_runner_movements <- function(play, event) {
  runners <- play$runners %||% list()
  linked <- Filter(function(runner) {
    identical(scalar_int(runner$details$playIndex), scalar_int(event$index)) &&
      scalar_int(runner$details$runner$id) != scalar_int(play$matchup$batter$id)
  }, runners)
  bases <- c("1B", "2B", "3B")
  removed <- setNames(integer(3L), bases)
  added <- setNames(integer(3L), bases)
  runs <- 0L
  outs <- 0L
  for (runner in linked) {
    start <- scalar_chr(runner$movement$start)
    end <- scalar_chr(runner$movement$end)
    is_out <- isTRUE(scalar_lgl(runner$movement$isOut, FALSE))
    if (start %in% bases) removed[[start]] <- removed[[start]] + 1L
    if (is_out) {
      outs <- outs + 1L
    } else if (end %in% bases) {
      added[[end]] <- added[[end]] + 1L
    } else if (identical(end, "score")) {
      runs <- runs + 1L
    }
  }
  c(
    remove_1b = removed[["1B"]], remove_2b = removed[["2B"]],
    remove_3b = removed[["3B"]], add_1b = added[["1B"]],
    add_2b = added[["2B"]], add_3b = added[["3B"]],
    runs = runs, outs = outs
  )
}

parse_called_pitch <- function(play, event, game) {
  is_top <- scalar_lgl(play$about$isTopInning)
  away_id <- scalar_int(game$gameData$teams$away$id)
  home_id <- scalar_int(game$gameData$teams$home$id)
  final_call <- normalize_feed_call(
    scalar_chr(event$details$call$code),
    scalar_chr(event$details$call$description, scalar_chr(event$details$description))
  )
  movement <- concurrent_runner_movements(play, event)
  data.frame(
    game_pk = scalar_int(game$gamePk),
    game_date = as.Date(scalar_chr(game$gameData$datetime$officialDate)),
    at_bat_number = scalar_int(play$atBatIndex) + 1L,
    pitch_number = scalar_int(event$pitchNumber),
    event_index = scalar_int(event$index),
    play_id = scalar_chr(event$playId),
    pitch_start_time_utc = scalar_chr(event$startTime),
    pitch_end_time_utc = scalar_chr(event$endTime),
    inning = scalar_int(play$about$inning),
    half = if (isTRUE(is_top)) "top" else "bottom",
    is_top = is_top,
    bat_team_id = if (isTRUE(is_top)) away_id else home_id,
    fld_team_id = if (isTRUE(is_top)) home_id else away_id,
    batter_id = scalar_int(play$matchup$batter$id),
    batter_name = scalar_chr(play$matchup$batter$fullName),
    pitcher_id = scalar_int(play$matchup$pitcher$id),
    pitcher_name = scalar_chr(play$matchup$pitcher$fullName),
    final_call = final_call,
    feed_balls_after = scalar_int(event$count$balls),
    feed_strikes_after = scalar_int(event$count$strikes),
    feed_outs_after = scalar_int(event$count$outs),
    feed_plate_x = scalar_num(event$pitchData$coordinates$pX),
    feed_plate_z = scalar_num(event$pitchData$coordinates$pZ),
    feed_sz_top = scalar_num(event$pitchData$strikeZoneTop),
    feed_sz_bot = scalar_num(event$pitchData$strikeZoneBottom),
    concurrent_remove_1b = unname(movement[["remove_1b"]]),
    concurrent_remove_2b = unname(movement[["remove_2b"]]),
    concurrent_remove_3b = unname(movement[["remove_3b"]]),
    concurrent_add_1b = unname(movement[["add_1b"]]),
    concurrent_add_2b = unname(movement[["add_2b"]]),
    concurrent_add_3b = unname(movement[["add_3b"]]),
    concurrent_runs = unname(movement[["runs"]]),
    concurrent_outs = unname(movement[["outs"]]),
    stringsAsFactors = FALSE
  )
}

parse_challenge_review <- function(play, event, review, game, linkage_source) {
  is_top <- scalar_lgl(play$about$isTopInning)
  away_id <- scalar_int(game$gameData$teams$away$id)
  home_id <- scalar_int(game$gameData$teams$home$id)
  bat_team_id <- if (isTRUE(is_top)) away_id else home_id
  fld_team_id <- if (isTRUE(is_top)) home_id else away_id
  challenger_team_id <- scalar_int(review$challengeTeamId)
  challenger_player_id <- scalar_int(review$player$id)
  final_call <- normalize_feed_call(
    scalar_chr(event$details$call$code),
    scalar_chr(event$details$call$description, scalar_chr(event$details$description))
  )
  outcome <- review_outcome(review)
  initial_call <- if (outcome == "overturned") opposite_call(final_call) else final_call
  challenger_role <- if (challenger_team_id == bat_team_id) {
    "batter"
  } else if (challenger_player_id == scalar_int(play$matchup$pitcher$id)) {
    "pitcher"
  } else {
    "catcher"
  }
  data.frame(
    game_pk = scalar_int(game$gamePk),
    game_date = as.Date(scalar_chr(game$gameData$datetime$officialDate)),
    at_bat_number = scalar_int(play$atBatIndex) + 1L,
    pitch_number = scalar_int(event$pitchNumber),
    event_index = scalar_int(event$index),
    play_id = scalar_chr(event$playId),
    inning = scalar_int(play$about$inning),
    half = if (isTRUE(is_top)) "top" else "bottom",
    bat_team_id = bat_team_id,
    fld_team_id = fld_team_id,
    challenger_team_id = challenger_team_id,
    challenger_player_id = challenger_player_id,
    challenger_name = scalar_chr(review$player$fullName),
    challenger_role = challenger_role,
    final_call = final_call,
    initial_call = initial_call,
    is_overturned = identical(outcome, "overturned"),
    challenge_outcome = outcome,
    review_type = scalar_chr(review$reviewType),
    linkage_source = linkage_source,
    pitch_start_time_utc = scalar_chr(event$startTime),
    pitch_end_time_utc = scalar_chr(event$endTime),
    stringsAsFactors = FALSE
  )
}

parse_live_feed <- function(path) {
  game <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  home_plate_umpire <- extract_home_plate_umpire(game)
  plays <- game$liveData$plays$allPlays %||% list()
  called <- list()
  challenges <- list()
  ci <- 0L
  ri <- 0L

  for (play in plays) {
    eligible_events <- called_pitch_events(play)
    for (event in eligible_events) {
      ci <- ci + 1L
      called[[ci]] <- parse_called_pitch(play, event, game)
      event_reviews <- extract_mj_reviews(event$reviewDetails)
      for (review in event_reviews) {
        ri <- ri + 1L
        challenges[[ri]] <- parse_challenge_review(
          play, event, review, game,
          if (identical(scalar_chr(event$reviewDetails$reviewType), "MJ")) {
            "pitch_event"
          } else {
            "pitch_event_additional_review"
          }
        )
      }
    }

    play_reviews <- extract_mj_reviews(play$reviewDetails)
    for (play_review in play_reviews) {
      if (!length(eligible_events)) {
        stop("MJ review has no terminal called pitch in game ", scalar_int(game$gamePk),
          ", at-bat ", scalar_int(play$atBatIndex) + 1L)
      }
      terminal <- eligible_events[[length(eligible_events)]]
      terminal_reviews <- extract_mj_reviews(terminal$reviewDetails)
      duplicate_terminal_review <- review_signature(play_review) %in%
        vapply(terminal_reviews, review_signature, character(1))
      # A plate appearance may contain an earlier pitch-level challenge and a
      # second, terminal-count challenge. Only suppress the PA review when it
      # is a duplicate of a review already attached to the terminal pitch.
      if (!isTRUE(duplicate_terminal_review)) {
        ri <- ri + 1L
        challenges[[ri]] <- parse_challenge_review(
          play, terminal, play_review, game,
          if (identical(scalar_chr(play$reviewDetails$reviewType), "MJ")) {
            "plate_appearance_terminal"
          } else {
            "plate_appearance_additional_review"
          }
        )
      }
    }
  }

  game_pk <- scalar_int(game$gamePk)
  teams <- game$gameData$teams
  abs_totals <- game$gameData$absChallenges
  totals <- data.frame(
    game_pk = rep(game_pk, 2L),
    side = c("away", "home"),
    team_id = c(scalar_int(teams$away$id), scalar_int(teams$home$id)),
    used_successful = c(
      scalar_int(abs_totals$away$usedSuccessful, 0L),
      scalar_int(abs_totals$home$usedSuccessful, 0L)
    ),
    used_failed = c(
      scalar_int(abs_totals$away$usedFailed, 0L),
      scalar_int(abs_totals$home$usedFailed, 0L)
    ),
    remaining_final = c(
      scalar_int(abs_totals$away$remaining, 2L),
      scalar_int(abs_totals$home$remaining, 2L)
    ),
    stringsAsFactors = FALSE
  )

  list(
    called_pitches = safe_rbindlist(called),
    challenges = safe_rbindlist(challenges),
    totals = data.table::as.data.table(totals),
    metadata = data.frame(
      game_pk = game_pk,
      game_date = as.Date(scalar_chr(game$gameData$datetime$officialDate)),
      away_team_id = scalar_int(teams$away$id),
      home_team_id = scalar_int(teams$home$id),
      status = scalar_chr(game$gameData$status$detailedState),
      has_abs_challenges = scalar_lgl(abs_totals$hasChallenges, FALSE),
      umpire_id = home_plate_umpire$id,
      umpire_name = home_plate_umpire$name,
      source_path = normalizePath(path),
      stringsAsFactors = FALSE
    )
  )
}

parse_live_feeds <- function(paths, cores = 1L) {
  cores <- max(1L, min(as.integer(cores), length(paths)))
  parsed <- if (.Platform$OS.type != "windows" && cores > 1L) {
    parallel::mclapply(paths, parse_live_feed, mc.cores = cores, mc.preschedule = TRUE)
  } else {
    lapply(paths, parse_live_feed)
  }
  combine_parsed_live(parsed)
}

combine_parsed_live <- function(parsed) {
  list(
    called_pitches = safe_rbindlist(lapply(parsed, `[[`, "called_pitches")),
    challenges = safe_rbindlist(lapply(parsed, `[[`, "challenges")),
    totals = safe_rbindlist(lapply(parsed, `[[`, "totals")),
    metadata = safe_rbindlist(lapply(parsed, `[[`, "metadata"))
  )
}
