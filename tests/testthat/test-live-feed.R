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
  expect_no_error(reconcile_feed_totals(parsed$challenges, parsed$totals))
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
