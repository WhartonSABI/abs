test_that("rectangle geometry reproduces official Savant edge distances", {
  savant <- read_savant_team(fixture_path("savant_team_116.json"))[game_pk == 823724]
  calculated <- with(savant, abs_edge_distance_inches(
    plateX, plateZ, strikeZoneTop, strikeZoneBottom
  ))
  expect_equal(calculated, savant$edge_dist_calc, tolerance = 1e-12)
})

test_that("audited challenged pitches have exact geometry/outcome agreement", {
  config <- project_config()
  live <- parse_live_feed(fixture_path("game_823724_live.json"))
  statcast <- read_statcast_file(fixture_path("statcast_2026-04-09.csv"))[game_pk == 823724]
  ledger <- build_pitch_ledger(statcast, live, config)
  savant <- read_savant_team(fixture_path("savant_team_116.json"))[game_pk == 823724]
  expect_no_error(validate_savant_matches(ledger, savant))
  expect_no_error(validate_geometry_outcomes(ledger))
  joined <- merge(
    ledger[challenge_occurred == TRUE, .(play_id, edge_distance_inches)],
    savant[, .(play_id, edge_dist_calc)], by = "play_id"
  )
  expect_equal(joined$edge_distance_inches, joined$edge_dist_calc, tolerance = 1e-12)
})

test_that("Euclidean corner distance and boundary rule are correct", {
  half_width <- 17 / 24
  edge <- abs_edge_distance_inches(
    half_width + 0.1, 3.1 + 0.1, sz_top = 3.1, sz_bot = 1.5
  )
  expect_equal(edge, 12 * sqrt(0.1^2 + 0.1^2) - 1.45)
  expect_identical(classify_abs_call(c(-0.01, 0, 0.01)),
    c("called_strike", "called_strike", "ball"))
})

