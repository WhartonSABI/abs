test_that("the public analysis bundle is complete and GitHub-sized", {
  data_directory <- file.path(project_root, "data", "analysis")
  checksum_path <- file.path(data_directory, "SHA256SUMS")
  expect_true(file.exists(checksum_path))

  checksum_lines <- readLines(checksum_path, warn = FALSE)
  checksum_parts <- strsplit(checksum_lines, "[[:space:]]+")
  expected <- vapply(checksum_parts, `[[`, character(1), 1L)
  filenames <- vapply(checksum_parts, `[[`, character(1), 2L)
  paths <- file.path(data_directory, filenames)

  expect_setequal(
    filenames,
    c(
      "challenge_events.parquet",
      "history_re_inputs.parquet",
      "pitch_ledger.parquet",
      "re288_model.rds",
      "savant_challenges.parquet"
    )
  )
  expect_true(all(file.exists(paths)))
  expect_true(all(file.info(paths)$size < 100 * 1024^2))

  actual <- vapply(
    paths, digest::digest, character(1), algo = "sha256", file = TRUE,
    serialize = FALSE
  )
  expect_identical(unname(actual), unname(expected))
})
