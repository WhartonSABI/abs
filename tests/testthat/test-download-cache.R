test_that("Statcast cache validation rejects header-only placeholders", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)

  writeLines("game_pk,game_date", path)
  expect_false(statcast_cache_has_data(path))

  writeLines(c("game_pk,game_date", "123,2026-08-25"), path)
  expect_true(statcast_cache_has_data(path))
})

test_that("live-feed cache validation requires a final game state", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  writeLines('{"gameData":{"status":{"abstractGameState":"Preview"}}}', path)
  expect_false(live_feed_cache_is_final(path))

  writeLines('{"gameData":{"status":{"abstractGameState":"Final"}}}', path)
  expect_true(live_feed_cache_is_final(path))
})
