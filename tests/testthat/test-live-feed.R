test_that("audited game recovers every challenge and explicit upheld outcomes", {
  parsed <- parse_live_feed(fixture_path("game_823724_live.json"))
  expect_equal(nrow(parsed$challenges), 7L)
  expect_equal(sum(parsed$challenges$challenge_outcome == "overturned"), 5L)
  expect_equal(sum(parsed$challenges$challenge_outcome == "upheld"), 2L)
  expect_false(anyNA(parsed$challenges$challenge_outcome))
  expect_equal(
    parsed$challenges[linkage_source == "plate_appearance_terminal", play_id],
    "f1a9e466-82ea-3cc5-b76e-7e6ef208078f"
  )
  expect_equal(nrow(parsed$challenges[linkage_source == "plate_appearance_terminal"]), 1L)
  expect_true(all(parsed$challenges$review_type == "MJ"))
  expect_false(any(parsed$called_pitches$same_pitch_non_abs_review))
  expect_no_error(reconcile_feed_totals(parsed$challenges, parsed$totals))
  expect_identical(parsed$metadata$umpire_id, 482641L)
  expect_identical(parsed$metadata$umpire_name, "Adrian Johnson")
})

test_that("home-plate umpire extraction fails closed", {
  expect_identical(
    extract_home_plate_umpire(list()),
    list(id = NA_integer_, name = NA_character_)
  )
})

test_that("JSON false survives ingestion and is not treated as missing", {
  false_review <- list(isOverturned = FALSE)
  missing_review <- list()
  expect_identical(review_outcome(false_review), "upheld")
  expect_identical(review_outcome(missing_review), "unresolved")
})

test_that("nested ABS reviews are recovered from composite replay reviews", {
  composite <- list(
    reviewType = "MA",
    isOverturned = FALSE,
    additionalReviews = list(list(
      reviewType = "MJ",
      isOverturned = TRUE,
      challengeTeamId = 116L,
      player = list(id = 123L)
    ))
  )
  abs_reviews <- extract_mj_reviews(composite)
  expect_length(abs_reviews, 1L)
  expect_identical(abs_reviews[[1L]]$reviewType, "MJ")
  expect_identical(review_outcome(abs_reviews[[1L]]), "overturned")
})

test_that("only post-replay calls without an ABS review are disqualified", {
  expect_false(has_disqualifying_non_mj_review(list(
    reviewType = "MI", isOverturned = FALSE
  )))
  expect_true(has_disqualifying_non_mj_review(list(
    reviewType = "MI", isOverturned = TRUE
  )))
  expect_false(has_disqualifying_non_mj_review(list(
    reviewType = "MI",
    isOverturned = TRUE,
    additionalReviews = list(list(
      reviewType = "MJ", isOverturned = FALSE
    ))
  )))
  expect_false(has_disqualifying_non_mj_review(list(
    reviewType = "MJ",
    isOverturned = TRUE,
    additionalReviews = list(list(
      reviewType = "MI", isOverturned = TRUE
    ))
  )))
  expect_false(has_disqualifying_non_mj_review(list(
    reviewType = "MI"
  )))
})

test_that("called pitches retain pitcher position and exact-event review flags", {
  make_game <- function(code, name, type) {
    list(
      gamePk = 1L,
      gameData = list(
        datetime = list(officialDate = "2026-04-01"),
        teams = list(away = list(id = 10L), home = list(id = 20L)),
        players = list(ID30 = list(primaryPosition = list(
          code = code, name = name, type = type
        )))
      )
    )
  }
  play <- list(
    atBatIndex = 0L,
    about = list(inning = 1L, isTopInning = TRUE),
    matchup = list(
      batter = list(id = 40L, fullName = "Batter"),
      pitcher = list(id = 30L, fullName = "Pitcher")
    ),
    runners = list()
  )
  make_event <- function(
    review_type = NULL,
    is_overturned = FALSE,
    additional_reviews = list()
  ) {
    list(
      index = 0L,
      pitchNumber = 1L,
      playId = "pitch-1",
      details = list(call = list(code = "C", description = "Called Strike")),
      count = list(balls = 0L, strikes = 1L, outs = 0L),
      reviewDetails = if (is.null(review_type)) NULL else list(
        reviewType = review_type,
        isOverturned = is_overturned,
        additionalReviews = additional_reviews
      )
    )
  }

  pitcher <- parse_called_pitch(
    play, make_event(), make_game("1", "Pitcher", "Pitcher")
  )
  two_way <- parse_called_pitch(
    play, make_event(), make_game("Y", "Two-Way Player", "Two-Way Player")
  )
  position_player <- parse_called_pitch(
    play, make_event(), make_game("2", "Catcher", "Catcher")
  )
  upheld_non_abs_review <- parse_called_pitch(
    play, make_event("MI", FALSE), make_game("1", "Pitcher", "Pitcher")
  )
  overturned_non_abs_review <- parse_called_pitch(
    play, make_event("MI", TRUE), make_game("1", "Pitcher", "Pitcher")
  )
  simultaneous_abs_review <- parse_called_pitch(
    play,
    make_event(
      "MI", TRUE,
      list(list(reviewType = "MJ", isOverturned = FALSE))
    ),
    make_game("1", "Pitcher", "Pitcher")
  )

  expect_identical(pitcher$pitcher_primary_position_code, "1")
  expect_identical(pitcher$pitcher_primary_position_name, "Pitcher")
  expect_identical(pitcher$pitcher_primary_position_type, "Pitcher")
  expect_identical(two_way$pitcher_primary_position_code, "Y")
  expect_identical(position_player$pitcher_primary_position_code, "2")
  expect_false(pitcher$same_pitch_non_abs_review)
  expect_false(upheld_non_abs_review$same_pitch_non_abs_review)
  expect_true(overturned_non_abs_review$same_pitch_non_abs_review)
  expect_false(simultaneous_abs_review$same_pitch_non_abs_review)
})

test_that("automatic calls do not shift physical pitch keys", {
  statcast <- data.table::data.table(
    game_pk = 1L,
    at_bat_number = 1L,
    pitch_number = 1:3,
    description = c("automatic_ball", "called_strike", "ball")
  )
  statcast[, physical_pitch_number := cumsum(!description %in%
    c("automatic_ball", "automatic_strike")), by = .(game_pk, at_bat_number)]
  physical <- statcast[!description %in% c("automatic_ball", "automatic_strike")]
  expect_equal(physical$physical_pitch_number, c(1L, 2L))
})
