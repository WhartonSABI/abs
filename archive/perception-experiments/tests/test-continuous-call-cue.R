synthetic_continuous_call_cue <- function(rows = 240L) {
  index <- seq_len(rows)
  x_inches <- seq(-14, 14, length.out = rows)
  z_inches <- 10 * sin(index / 17)
  distance <- abs_edge_distance_inches(
    x_inches / 12, 2.5 + z_inches / 12, 3.5, 1.5
  )
  data.table::data.table(
    game_pk = 1L + index %% 24L,
    at_bat_number = index,
    pitch_number = 1L,
    initial_call = ifelse(
      distance + stats::rnorm(rows, 0, 1.5) <= 0,
      "called_strike", "ball"
    ),
    swing = as.integer(index %% 7L == 0L),
    plate_x = x_inches / 12,
    plate_z = 2.5 + z_inches / 12,
    sz_top = 3.5,
    sz_bot = 1.5,
    balls = index %% 4L,
    strikes = index %% 3L,
    pitch_family = c("fastball", "breaking", "offspeed")[1L + index %% 3L],
    matchup = c("L-L", "L-R", "R-L", "R-R")[1L + index %% 4L],
    umpire_id = paste0("u", 1L + index %% 5L),
    catcher_id = paste0("c", 1L + index %% 8L)
  )
}

test_that("call cue uses only taken initial calls and ignores ABS outcomes", {
  set.seed(3)
  pitches <- synthetic_continuous_call_cue()
  pitches[, `:=`(
    abs_call = rep(c("ball", "called_strike"), length.out = .N),
    challenged = rep(0:1, length.out = .N),
    challenge_success = rep(1:0, length.out = .N)
  )]
  first <- prepare_continuous_call_cue(
    pitches, edge_degrees_freedom = 5L, residual_rank = 6L, seed = 4L
  )
  pitches[, `:=`(
    abs_call = rev(abs_call),
    challenged = 1L - challenged,
    challenge_success = 1L - challenge_success
  )]
  second <- prepare_continuous_call_cue(
    pitches, edge_degrees_freedom = 5L, residual_rank = 6L, seed = 4L
  )

  expect_equal(first$data, second$data)
  expect_equal(first$data$N, sum(pitches$swing == 0L))
  expect_true(all(first$rows$swing == 0L))
  expect_false(any(c(
    "abs_call", "challenged", "challenge_success"
  ) %in% names(first$rows)))
})

test_that("2D call residual cannot reproduce the edge spline", {
  set.seed(5)
  pitches <- synthetic_continuous_call_cue(300L)
  bundle <- prepare_continuous_call_cue(
    pitches, edge_degrees_freedom = 5L, residual_rank = 8L, seed = 5L
  )
  anchor <- cbind(intercept = 1, bundle$data$edge_basis)
  overlap <- crossprod(anchor, bundle$data$residual_basis)
  expect_lt(max(abs(overlap)), 1e-5)
})

test_that("continuous initial-call Stan program parses", {
  skip_if_not_installed("rstan")
  parsed <- rstan::stanc(file = default_continuous_call_cue_stan_file())
  expect_true(parsed$status)
})

test_that("continuous call-cue scoring exposes both candidate and observed call", {
  set.seed(7)
  bundle <- prepare_continuous_call_cue(
    synthetic_continuous_call_cue(),
    edge_degrees_freedom = 5L, residual_rank = 6L, seed = 7L
  )
  draw_count <- 3L
  columns <- list(
    intercept = rep(0, draw_count),
    sigma_umpire = rep(0.1, draw_count),
    sigma_catcher = rep(0.1, draw_count)
  )
  add_vector <- function(name, length, value) {
    if (!length) return()
    for (index in seq_len(length)) {
      columns[[paste0(name, "[", index, "]")]] <<- rep(value, draw_count)
    }
  }
  add_vector("beta_edge", bundle$data$E, 0)
  add_vector("beta_residual", bundle$data$R, 0)
  add_vector("beta_context", bundle$data$K, 0)
  add_vector("umpire_effect", nrow(bundle$umpire_table), 0)
  add_vector("catcher_effect", nrow(bundle$catcher_table), 0)
  draws <- do.call(cbind, columns)
  fit <- list(
    stan_fit = draws,
    umpire_table = bundle$umpire_table,
    catcher_table = bundle$catcher_table,
    context_specification = bundle$context_specification,
    edge_specification = bundle$edge_specification,
    residual_specification = bundle$residual_specification
  )
  class(fit) <- "continuous_call_cue_fit"
  rows <- data.table::copy(bundle$rows[1:8])
  rows[1L, `:=`(umpire_id = "new-umpire", catcher_id = "new-catcher")]
  scored <- score_continuous_call_cue(
    fit, rows, ndraws = 3L, return_draws = TRUE
  )

  expect_equal(scored$draw_id, 1:3)
  expect_equal(scored$called_strike_probability, matrix(0.5, 8L, 3L))
  expect_equal(scored$initial_call_probability, matrix(0.5, 8L, 3L))
  expect_false(scored$summary$umpire_seen_in_training[[1L]])
  expect_false(scored$summary$catcher_seen_in_training[[1L]])
})
