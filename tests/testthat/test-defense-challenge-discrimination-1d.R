synthetic_defense_challenge_discrimination_1d_rows <- function(n = 240L) {
  index <- seq_len(n)
  margin <- seq(-4.5, 4.5, length.out = n)
  probability <- stats::pnorm((margin - 2) / 2)
  data.table::data.table(
    game_pk = paste0("g", 1L + (index - 1L) %% 20L),
    pitch_order = 1L + (index - 1L) %/% 20L,
    defensive_team_id = paste0("d", 1L + (index - 1L) %% 4L),
    opponent_team_id = paste0("o", 1L + index %% 5L),
    initial_call = "ball",
    challenged = as.integer(stats::runif(n) < probability),
    physical_edge_distance_inches = -margin,
    stake_G = 0.10 + (index %% 5L) / 50,
    inventory_loss = 0.08 + (index %% 3L) / 50,
    defense_inventory_before = 1L + index %% 2L,
    tracking_available = TRUE,
    abs_eligible = TRUE,
    inning = 1L + index %% 9L,
    balls_before = index %% 4L,
    strikes_before = index %% 3L,
    score_margin = (index %% 7L) - 3L,
    pitch_family = c("fastball", "breaking", "offspeed")[
      1L + index %% 3L
    ],
    matchup = c("L-L", "L-R", "R-L", "R-R")[1L + index %% 4L],
    umpire_id = paste0("u", 1L + index %% 5L),
    catcher_id = paste0("c", 1L + index %% 8L)
  )
}

fake_defense_challenge_discrimination_1d_fit <- function(
  bundle, threshold = 1, sigma = 2, opponent_shift = 0,
  umpire_shift = 0, catcher_shift = 0, draws = 8L
) {
  core <- bundle$core_bundle
  columns <- list(
    mu_threshold = rep(threshold, draws),
    mu_log_sigma = rep(log(sigma), draws),
    `tau_player[1]` = rep(0, draws),
    `tau_player[2]` = rep(0, draws),
    rho_threshold_log_sigma = rep(0, draws)
  )
  add_vector <- function(name, length, value) {
    for (index in seq_len(length)) {
      columns[[paste0(name, "[", index, "]")]] <<-
        rep(rep_len(value, length)[[index]], draws)
    }
  }
  add_vector("threshold_player", nrow(core$player_table), threshold)
  add_vector("sigma_player", nrow(core$player_table), sigma)
  add_vector("beta_context", ncol(core$data$X), 0)
  add_vector("team_shift", nrow(core$team_table), opponent_shift)
  add_vector("umpire_shift", nrow(core$umpire_table), umpire_shift)
  add_vector("catcher_shift", nrow(core$catcher_table), catcher_shift)
  fit <- list(
    stan_fit = do.call(cbind, columns),
    sigma_model = "common",
    player_table = core$player_table,
    team_table = core$team_table,
    umpire_table = core$umpire_table,
    catcher_table = core$catcher_table,
    defense_team_table = bundle$defense_team_table,
    opponent_team_table = bundle$opponent_team_table,
    context_specification = core$context_specification,
    context_columns = core$context_columns,
    context_design_columns = core$context_design_columns,
    training_games = sort(unique(bundle$normalized_rows$game_pk)),
    training_rows = nrow(bundle$rows),
    eligibility = bundle$eligibility,
    margin_limit_inches = bundle$margin_limit_inches,
    stake_log_odds_epsilon = bundle$stake_log_odds_epsilon,
    tail_diagnostics = core$tail_diagnostics,
    defense_tail_diagnostics = bundle$tail_diagnostics,
    decision_unit = "fielding_team"
  )
  class(fit) <- c(
    "defense_challenge_discrimination_1d_fit",
    "challenge_discrimination_1d_fit"
  )
  fit
}

synthetic_defense_re_table_1d <- function() {
  grid <- data.table::CJ(
    outs = 0:2,
    base_state = c("000", "001", "010", "011", "100", "101", "110", "111"),
    balls = 0:3,
    strikes = 0:2
  )
  grid[, re := 0.5 + 0.15 * balls - 0.10 * strikes - 0.08 * outs]
  grid[]
}

synthetic_defense_real_schema_1d <- function() {
  n <- 6L
  ledger <- data.table::data.table(
    game_pk = 9001L,
    pitch_order = seq_len(n),
    initial_call = c(rep("ball", 5L), "called_strike"),
    bat_team_id = 10L,
    fld_team_id = 20L,
    challenge_occurred = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    challenger_team_id = c(20L, 20L, rep(NA_integer_, 4L)),
    challenger_role = c("catcher", "pitcher", rep(NA_character_, 4L)),
    fld_team_challenges_before = c(2L, 2L, 1L, 1L, 1L, 2L),
    inning = 1L,
    half = "top",
    outs_before = c(0L, 0L, 1L, 1L, 2L, 2L),
    balls_before = c(0L, 1L, 2L, 3L, 0L, 1L),
    strikes_before = c(0L, 1L, 2L, 1L, 2L, 1L),
    bat_score = 1L,
    fld_score = 3L,
    edge_distance_inches = c(-1.5, -0.5, 0.5, 1.5, 4, -1),
    plate_x = (c(-1.5, -0.5, 0.5, 1.5, 4, -1) + 1.45) / 12 + 17 / 24,
    plate_z = 2.5,
    sz_top = 3.5,
    sz_bot = 1.5,
    tracking_available = TRUE,
    abs_eligible = TRUE,
    umpire_id = 77L,
    fielder_2 = 88L,
    pitcher_id = 99L,
    pitch_type = "FF",
    stand = "R",
    p_throws = "R",
    on_1b = NA_integer_, on_2b = NA_integer_, on_3b = NA_integer_,
    concurrent_outs = 0L, concurrent_runs = 0L,
    concurrent_remove_1b = 0L, concurrent_remove_2b = 0L,
    concurrent_remove_3b = 0L, concurrent_add_1b = 0L,
    concurrent_add_2b = 0L, concurrent_add_3b = 0L,
    challenge_outcome = c("overturned", "upheld", rep(NA_character_, 4L)),
    abs_call = c("called_strike", "ball", "ball", "ball", "ball", "ball")
  )
  remaining <- ledger[, .(
    game_pk, pitch_order, team_id = fld_team_id,
    expected_correctable_remaining = 1.5,
    expected_re_remaining = 0.18
  )]
  list(
    ledger = ledger,
    remaining = remaining,
    re_model = list(table = synthetic_defense_re_table_1d())
  )
}

test_that("real-schema preparation combines catcher and pitcher as one team action", {
  source <- synthetic_defense_real_schema_1d()
  rows <- build_defense_challenge_discrimination_1d_rows(
    source$ledger, source$remaining, source$re_model
  )

  expect_equal(nrow(rows), 5L)
  expect_equal(sum(rows$challenged), 2L)
  expect_equal(rows$defensive_team_id, rep("20", 5L))
  expect_equal(rows$opponent_team_id, rep("10", 5L))
  expect_equal(rows$defense_margin_inches, -rows$physical_edge_distance_inches)
  expect_true(rows$defense_margin_inches[[1L]] > 0)
  expect_true(all(rows$initial_call == "ball"))
  expect_true(all(rows$score_margin == 2))
  expect_true(all(is.finite(rows$stake_G) & rows$stake_G >= 0))
  expect_true(all(is.finite(rows$inventory_loss) & rows$inventory_loss >= 0))
  expect_false(any(
    defense_challenge_discrimination_1d_outcome_columns() %in% names(rows)
  ))

  bundle <- prepare_defense_challenge_discrimination_1d(rows)
  expect_equal(bundle$data$margin, bundle$rows$defense_margin_inches)
  expect_true(all(c("plate_x", "plate_z", "sz_top", "sz_bot") %in%
    names(bundle$normalized_rows)))
  expect_false(any(c("plate_x", "plate_z", "sz_top", "sz_bot") %in%
    names(bundle$alias_rows)))
})

test_that("defense preparation is outcome-free and preserves physical geometry", {
  set.seed(1)
  rows <- synthetic_defense_challenge_discrimination_1d_rows()
  rows[, `:=`(
    challenge_outcome = rep(c("overturned", "upheld"), length.out = .N),
    abs_call = rep(c("called_strike", "ball"), length.out = .N),
    official_success = challenged == 1L
  )]
  first <- prepare_defense_challenge_discrimination_1d(rows)
  rows[, `:=`(
    challenge_outcome = rev(challenge_outcome),
    abs_call = rev(abs_call),
    official_success = !official_success
  )]
  second <- prepare_defense_challenge_discrimination_1d(rows)

  expect_equal(first$data, second$data)
  expect_equal(first$data$margin, first$rows$defense_margin_inches)
  expect_equal(first$data$use_player_sigma, 0L)
  expect_equal(first$data$P, data.table::uniqueN(rows$defensive_team_id))
  expect_equal(first$data$T, data.table::uniqueN(rows$opponent_team_id))
  expect_equal(first$sigma_model, "common")
  expect_true(all(first$normalized_rows$initial_call == "ball"))
  expect_equal(
    first$normalized_rows$physical_edge_distance_inches,
    rows[order(game_pk, pitch_order)]$physical_edge_distance_inches
  )
  expect_false(any(
    defense_challenge_discrimination_1d_outcome_columns() %in%
      names(first$normalized_rows)
  ))
})

test_that("defense scoring uses oriented margin and one common width", {
  set.seed(2)
  training <- synthetic_defense_challenge_discrimination_1d_rows(200L)
  bundle <- prepare_defense_challenge_discrimination_1d(
    training, categorical_context = character(), numeric_context = character()
  )
  fit <- fake_defense_challenge_discrimination_1d_fit(
    bundle, threshold = 1, sigma = 2,
    opponent_shift = 0.5, umpire_shift = 0.25, catcher_shift = -0.25
  )
  heldout <- data.table::copy(training[abs(physical_edge_distance_inches) <= 3])
  heldout[, game_pk := paste0("heldout_", game_pk)]
  scored <- score_defense_challenge_discrimination_1d(
    fit, heldout, ndraws = 8L, return_draws = TRUE
  )
  expected <- stats::pnorm(
    (scored$summary$defense_margin_inches - 1.5) / 2
  )

  expect_equal(
    scored$summary$challenge_probability_mean, expected, tolerance = 1e-12
  )
  expect_equal(scored$sigma_inches, matrix(2, nrow(scored$summary), 8L))
  expect_true(all(scored$summary$defense_team_seen_in_training))
  expect_true(all(scored$summary$opponent_team_seen_in_training))
  expect_false("batter_id" %in% names(scored$summary))
  expect_true(all(scored$summary$decision_unit == "fielding_team"))

  unlabeled <- data.table::copy(heldout)
  unlabeled[, challenged := NULL]
  prediction <- score_defense_challenge_discrimination_1d(
    fit, unlabeled, ndraws = 8L
  )
  expect_equal(nrow(prediction), nrow(scored$summary))
})

test_that("held-out defense scoring fails closed on overlapping games", {
  rows <- synthetic_defense_challenge_discrimination_1d_rows(160L)
  bundle <- prepare_defense_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character()
  )
  fit <- fake_defense_challenge_discrimination_1d_fit(bundle)
  expect_error(
    score_defense_challenge_discrimination_1d(fit, rows, ndraws = 8L),
    "overlap"
  )
  scored <- score_defense_challenge_discrimination_1d(
    fit, rows, ndraws = 8L, allow_in_sample = TRUE
  )
  expect_true(all(scored$scoring_regime == "in_sample_explicit_opt_out"))
})

test_that("defense game folds cover full local and tail domains without leakage", {
  rows <- synthetic_defense_challenge_discrimination_1d_rows(400L)
  folds <- make_defense_challenge_discrimination_1d_game_folds(
    rows, folds = 5L, seed = 42L
  )
  again <- make_defense_challenge_discrimination_1d_game_folds(
    rows, folds = 5L, seed = 42L
  )
  split <- split_defense_challenge_discrimination_1d_fold(
    rows, folds, heldout_fold = 3L
  )

  expect_equal(folds, again)
  expect_equal(nrow(folds), data.table::uniqueN(rows$game_pk))
  expect_setequal(unique(folds$fold), 1:5)
  expect_equal(sum(folds$eligible_margin_rows), nrow(rows))
  expect_gt(sum(folds$tail_diagnostic_rows), 0L)
  expect_equal(
    nrow(split$train) + nrow(split$heldout), nrow(rows)
  )
  expect_length(
    intersect(unique(split$train$game_pk), unique(split$heldout$game_pk)), 0L
  )
  expect_equal(
    split$split_summary$eligible_margin_rows,
    split$split_summary$local_model_rows +
      split$split_summary$tail_diagnostic_rows
  )
  bad <- data.table::copy(folds[-1L])
  expect_error(
    split_defense_challenge_discrimination_1d_fold(rows, bad, 3L),
    "exactly"
  )
})

test_that("defense cache identity includes data, seed, Stan, and adapter source", {
  rows <- synthetic_defense_challenge_discrimination_1d_rows(120L)
  bundle <- prepare_defense_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character()
  )
  identity <- defense_challenge_discrimination_1d_cache_identity(
    bundle, "cmdstanr", 1L, default_challenge_discrimination_1d_stan_file(),
    2L, 100L, 100L, 0.95, 10L
  )
  changed_rows <- data.table::copy(rows)
  changed_rows[1L, challenged := 1L - challenged]
  changed <- prepare_defense_challenge_discrimination_1d(
    changed_rows, categorical_context = character(), numeric_context = character()
  )
  changed_identity <- defense_challenge_discrimination_1d_cache_identity(
    changed, "cmdstanr", 1L, default_challenge_discrimination_1d_stan_file(),
    2L, 100L, 100L, 0.95, 10L
  )
  seed_identity <- defense_challenge_discrimination_1d_cache_identity(
    bundle, "cmdstanr", 2L, default_challenge_discrimination_1d_stan_file(),
    2L, 100L, 100L, 0.95, 10L
  )

  expect_false(identical(identity, changed_identity))
  expect_false(identical(identity, seed_identity))
  cached <- structure(
    list(defense_cache_identity = identity),
    class = "defense_challenge_discrimination_1d_fit"
  )
  expect_invisible(assert_defense_challenge_discrimination_1d_cache_identity(
    cached, identity, "cache.rds"
  ))
  expect_error(
    assert_defense_challenge_discrimination_1d_cache_identity(
      cached, seed_identity, "cache.rds"
    ),
    "mismatch"
  )
})

test_that("defense summaries expose teams rather than batter aliases", {
  rows <- synthetic_defense_challenge_discrimination_1d_rows(160L)
  bundle <- prepare_defense_challenge_discrimination_1d(
    rows, categorical_context = character(), numeric_context = character()
  )
  fit <- fake_defense_challenge_discrimination_1d_fit(bundle, sigma = 2.1)
  summary <- summarize_defense_challenge_discrimination_1d(fit, ndraws = 8L)

  expect_true("defensive_team_id" %in% names(summary$defense_teams))
  expect_true("opponent_team_id" %in% names(summary$opponent_teams))
  expect_false("batter_id" %in% names(summary$defense_teams))
  expect_equal(summary$population$decision_unit, "fielding_team")
  expect_equal(summary$population$population_sigma_inches_median, 2.1)
  expect_equal(unique(summary$defense_teams$estimate_source), "league_common_sigma")
})
