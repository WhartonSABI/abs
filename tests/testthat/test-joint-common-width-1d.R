synthetic_joint_common_width_rows_1d <- function(
  games = 12L, rows_per_game = 36L, seed = 20260826L
) {
  set.seed(seed)
  n <- games * rows_per_game
  game_number <- rep(seq_len(games), each = rows_per_game)
  within_game <- rep(seq_len(rows_per_game), times = games)
  margin <- stats::runif(n, -4.5, 4.5)
  balls <- (within_game - 1L) %% 4L
  strikes <- ((within_game - 1L) %/% 4L) %% 3L
  cost <- ifelse(within_game %% 2L == 0L, -0.25, 0.25)
  common <- data.table::data.table(
    game_pk = as.character(8000L + game_number),
    pitch_order = within_game,
    challenged = 0L,
    stake_G = 1,
    inventory_loss = exp(cost),
    balls_before = balls,
    strikes_before = strikes,
    pitch_family = ifelse(within_game %% 2L == 0L, "fastball", "breaking"),
    matchup = ifelse(within_game %% 3L == 0L, "L-R", "R-R"),
    inning = 1 + (within_game - 1L) %% 9L,
    score_margin = ((game_number + within_game) %% 5L) - 2,
    umpire_id = paste0("U", 1L + (game_number - 1L) %% 3L),
    catcher_id = paste0("C", 1L + (game_number - 1L) %% 4L)
  )
  offense <- data.table::copy(common)
  offense[, `:=`(
    initial_call = "called_strike",
    edge_distance_inches = margin,
    batter_id = paste0("B", game_number),
    bat_team_id = paste0("O", 1L + (game_number - 1L) %% 4L),
    adverse_challenges_before = 2L
  )]
  offense_probability <- stats::pnorm(
    (margin - 0.25 - 0.20 * cost) / 1.9
  )
  offense[, challenged := stats::rbinom(.N, 1L, offense_probability)]

  defense <- data.table::copy(common)
  defense[, `:=`(
    initial_call = "ball",
    physical_edge_distance_inches = -margin,
    defensive_team_id = paste0("D", 1L + (game_number - 1L) %% 4L),
    opponent_team_id = paste0("O", 1L + game_number %% 4L),
    defense_inventory_before = 2L
  )]
  defense_probability <- stats::pnorm(
    (margin - 0.10 - 0.15 * cost) / 1.45
  )
  defense[, challenged := stats::rbinom(.N, 1L, defense_probability)]
  folds <- data.table::data.table(
    game_pk = as.character(8000L + seq_len(games)),
    fold = rep(1:3, length.out = games)
  )
  list(offense = offense[], defense = defense[], folds = folds[])
}

test_that("joint common-width normalization preserves role orientation", {
  source <- synthetic_joint_common_width_rows_1d(games = 6L, rows_per_game = 12L)
  offense <- normalize_joint_common_width_offense_1d(source$offense)
  defense <- normalize_joint_common_width_defense_1d(source$defense)

  expect_equal(offense$role_margin_inches, offense$edge_distance_inches)
  expect_equal(
    defense$role_margin_inches,
    -defense$physical_edge_distance_inches
  )
  expect_true(all(offense$role == "offense"))
  expect_true(all(defense$role == "defense"))
  expect_equal(offense$decision_unit_id, offense$batter_id)
  expect_equal(defense$decision_unit_id, defense$defensive_team_id)
})

test_that("joint common-width inputs fail closed on outcomes", {
  source <- synthetic_joint_common_width_rows_1d(games = 6L, rows_per_game = 12L)
  offense <- data.table::copy(source$offense)
  defense <- data.table::copy(source$defense)
  offense[, challenge_outcome := "upheld"]
  defense[, abs_call := "called_strike"]

  expect_error(
    normalize_joint_common_width_offense_1d(offense),
    "outcome/evaluation"
  )
  expect_error(
    normalize_joint_common_width_defense_1d(defense),
    "outcome/evaluation"
  )
})

test_that("shared folds cover the union and overlap checks fail closed", {
  source <- synthetic_joint_common_width_rows_1d(games = 6L, rows_per_game = 12L)
  games <- union(source$offense$game_pk, source$defense$game_pk)
  valid <- normalize_joint_common_width_fold_assignment_1d(source$folds, games)
  expect_equal(nrow(valid), 6L)
  expect_setequal(unique(valid$fold), 1:3)

  expect_error(
    normalize_joint_common_width_fold_assignment_1d(
      source$folds[-1L], games
    ),
    "omits"
  )
  duplicated <- data.table::rbindlist(list(source$folds, source$folds[1L]))
  expect_error(
    normalize_joint_common_width_fold_assignment_1d(duplicated, games),
    "assign each game once"
  )
  expect_error(
    assert_joint_common_width_game_separation_1d(
      source$offense[game_pk != "8006"],
      source$offense[game_pk == "8001"],
      role = "offense"
    ),
    "overlap"
  )
})

test_that("runner helper returns fully game-cross-fitted role widths", {
  skip_if_not_installed("mgcv")
  source <- synthetic_joint_common_width_rows_1d()
  result <- fit_fold_joint_common_widths_1d(
    source$offense,
    source$defense,
    source$folds,
    margin_limit_inches = 3,
    minimum_training_rows = 50L,
    minimum_training_challenges = 2L,
    nthreads = 1L,
    keep_models = FALSE,
    progress = FALSE
  )

  expect_s3_class(result, "joint_common_width_1d_crossfit")
  expect_equal(nrow(result$estimates), 6L)
  expect_setequal(result$estimates$role, c("offense", "defense"))
  expect_setequal(result$estimates$fold, 1:3)
  expect_true(all(is.finite(result$estimates$sigma_inches)))
  expect_true(all(result$estimates$sigma_inches > 0))
  expect_equal(
    result$estimates$sigma_inches,
    1 / result$estimates$slope_per_inch,
    tolerance = 1e-12
  )
  expect_equal(
    result$estimates$sigma_lower,
    1 / result$estimates$slope_upper,
    tolerance = 1e-12
  )
  expect_true(all(result$diagnostics$training_tail_rows > 0L))
  expect_true(all(result$diagnostics$heldout_tail_rows > 0L))
  expect_true(all(result$diagnostics$training_games == 8L))
  expect_true(all(result$diagnostics$heldout_games == 4L))
  expect_true(all(result$diagnostics$converged))
  expect_true(all(grepl(
    "s\\(decision_unit_id,", result$diagnostics$formula
  )))
  expect_null(result$models)

  expect_length(result$fold_perception_sigma, 3L)
  expect_true(all(vapply(
    result$fold_perception_sigma,
    function(value) identical(names(value), c("offense", "defense")),
    logical(1)
  )))
  expect_equal(
    result$fold_perception_sigma,
    joint_common_width_1d_fold_sigma(result)
  )
  expect_true(all(c(
    "offense_sigma_inches", "defense_sigma_inches",
    "offense_sigma_lower", "defense_sigma_upper"
  ) %in% names(result$fold_widths)))

  offense <- normalize_joint_common_width_offense_1d(source$offense)
  defense <- normalize_joint_common_width_defense_1d(source$defense)
  expected_local <- sum(abs(offense$role_margin_inches) <= 3) +
    sum(abs(defense$role_margin_inches) <= 3)
  expect_equal(nrow(result$oof_predictions), expected_local)
  observed_fold <- result$fold_assignment$fold[
    match(result$oof_predictions$game_pk, result$fold_assignment$game_pk)
  ]
  expect_equal(result$oof_predictions$fold, observed_fold)
  expect_true(all(
    result$oof_predictions$challenge_probability > 0 &
      result$oof_predictions$challenge_probability < 1
  ))
  expect_false(any(
    joint_common_width_1d_outcome_columns() %in%
      names(result$oof_predictions)
  ))
})
