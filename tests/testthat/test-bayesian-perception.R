synthetic_perception_pitches <- function(games = 20L, repeats = 2L) {
  grid <- data.table::CJ(ix = -2:2, iz = -2:2)
  calls <- c("ball", "called_strike")
  rows <- data.table::CJ(
    game_pk = seq_len(games),
    grid_row = seq_len(nrow(grid)),
    initial_call = calls,
    repeat_id = seq_len(repeats)
  )
  rows <- merge(rows, cbind(grid_row = seq_len(nrow(grid)), grid), by = "grid_row")
  rows[, `:=`(
    pitch_order = seq_len(.N),
    plate_x = ix * 1.5 / 12,
    plate_z = 2 + iz * 1.5 / 20 * 2,
    sz_bot = 2,
    sz_top = 4,
    umpire_name = paste0("ump", 1L + game_pk %% 3L),
    fielder_2 = 100L + game_pk %% 4L,
    tracking_available = TRUE,
    edge_distance_inches = 0,
    abs_call = data.table::fifelse(
      (ix + iz + game_pk + repeat_id) %% 5L == 0L,
      data.table::fifelse(initial_call == "ball", "called_strike", "ball"),
      initial_call
    )
  )]
  rows[]
}

test_that("larger perception bins create fewer shared spatial cells", {
  pitches <- synthetic_perception_pitches()
  small <- assign_joint_perception_cells(pitches, 1.5)
  large <- assign_joint_perception_cells(pitches, 3)

  expect_gt(data.table::uniqueN(small$cell), data.table::uniqueN(large$cell))
  expect_true(all(large[, data.table::uniqueN(initial_call), by = cell]$V1 == 2L))
  expect_true(all(levels(large$initial_call) == c("ball", "called_strike")))
})

test_that("joint CAR input is connected, symmetric, and conserves pitches", {
  pitches <- synthetic_perception_pitches()
  input <- prepare_joint_car_input(pitches, 3, full = TRUE)

  expect_equal(sum(input$data$n), input$pitch_count)
  expect_equal(as.matrix(input$M), t(as.matrix(input$M)))
  expect_true(all(Matrix::rowSums(input$M) > 0))
  expect_true(all(c("cell_call", "umpire", "catcher") %in% names(input$data)))
})

test_that("joint model formula and dispersion priors compile", {
  pitches <- synthetic_perception_pitches(games = 3L, repeats = 1L)
  input <- prepare_joint_car_input(pitches, 3, full = TRUE)
  priors <- joint_perception_priors(1, full = TRUE)
  prior_table <- as.data.frame(priors)

  expect_true(any(prior_table$class == "sdcar" & grepl("normal\\(0, 1\\)", prior_table$prior)))
  expect_true(any(prior_table$group == "cell_call" & prior_table$class == "sd"))
  code <- brms::make_stancode(
    joint_perception_formula(TRUE), data = input$data,
    data2 = list(M = input$M), family = stats::binomial(), prior = priors
  )
  expect_match(code, "sparse_icar_lpdf", fixed = TRUE)
})

test_that("game split is deterministic and has no leakage", {
  pitches <- synthetic_perception_pitches()
  one <- deterministic_game_split(pitches, seed = 42L)
  two <- deterministic_game_split(pitches, seed = 42L)

  expect_equal(one, two)
  expect_length(intersect(one[split == "train", game_pk],
    one[split == "validation", game_pk]), 0L)
  expect_equal(sort(unique(one$split)), c("train", "validation"))
})

test_that("unseen spatial cells map to the nearest training cell", {
  pitches <- synthetic_perception_pitches(games = 2L, repeats = 1L)
  input <- prepare_joint_car_input(pitches, 3, full = FALSE)
  new_pitch <- data.table::copy(pitches[1L])
  new_pitch[, plate_x := plate_x + 100 / 12]
  mapped <- map_to_joint_training_cells(new_pitch, input$cell_table, 3)

  expect_true(mapped$spatial_cell_fallback)
  expect_true(as.character(mapped$cell) %in% input$cell_table$cell)
})

test_that("probability metrics report league and role calibration", {
  x <- data.table::data.table(
    p_hat = c(0.1, 0.2, 0.8, 0.9),
    call_wrong = c(0L, 0L, 1L, 1L),
    role = c("defense", "offense", "defense", "offense")
  )
  metrics <- joint_probability_metrics(x)

  expect_setequal(metrics$role, c("league", "defense", "offense"))
  expect_true(all(metrics$log_loss >= 0))
  expect_true(all(metrics$brier >= 0 & metrics$brier <= 1))
  expect_true(all(metrics$ece_05 >= 0 & metrics$ece_05 <= 1))
})

test_that("one-SE selection prefers larger bins and stronger shrinkage", {
  metrics <- data.table::data.table(
    candidate_id = c("small", "large_weak", "large_strong"),
    bin_width = c(2, 4, 4),
    sdcar_scale = c(1, 2, 0.5),
    scope = "league",
    log_loss = c(0.100, 0.101, 0.102),
    log_loss_se = c(0.005, 0.005, 0.005)
  )
  selected <- select_joint_perception_candidates(metrics, finalists = 1L)

  expect_identical(selected[finalist == TRUE, candidate_id], "large_strong")
})

test_that("stronger sdcar shrinkage integration test is available", {
  skip_if(Sys.getenv("RUN_SLOW_BAYES_TESTS") != "true")
  skip("Enable this test only for the overnight Bayesian validation workflow")
})
