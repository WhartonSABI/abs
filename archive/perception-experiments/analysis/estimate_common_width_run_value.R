project_root <- rprojroot::find_root(
  rprojroot::has_file("_targets.R"), path = getwd()
)
source(file.path(project_root, "config", "project.R"))
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

environment_number <- function(name, default, lower = -Inf) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, unset = default)))
  if (length(value) != 1L || !is.finite(value) || value <= lower) {
    stop(name, " must be a finite number greater than ", lower, call. = FALSE)
  }
  value
}

environment_integer <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, unset = default)))
  if (length(value) != 1L || is.na(value) || value < 1L) {
    stop(name, " must be a positive integer", call. = FALSE)
  }
  value
}

sigma_inches <- environment_number("ABS_1D_COMMON_SIGMA", 2.850308, 0)
inventory_loss_multiplier <- environment_number(
  "ABS_1D_INVENTORY_LOSS_MULTIPLIER", 1, 0
)
bootstrap_reps <- environment_integer("ABS_1D_BOOTSTRAP_REPS", 2000L)
seed <- environment_integer("ABS_1D_RUN_VALUE_SEED", 20260825L)
output_tag <- Sys.getenv("ABS_1D_OUTPUT_TAG", unset = "primary")
output_tag <- gsub("[^A-Za-z0-9_.-]+", "_", output_tag)
if (!nzchar(output_tag)) output_tag <- "primary"

pitch_ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
remaining <- data.table::as.data.table(
  targets::tar_read(remaining_opportunities)
)
re_model <- targets::tar_read(re_model)

prior_fit <- fit_challenge_margin_prior_1d(
  pitch_ledger,
  components = 3L,
  fold_id = "all_games_exploratory",
  global_draw_id = 1L
)

rows <- data.table::copy(pitch_ledger)[
  initial_call == "called_strike" & !is.na(batter_id) &
    is.finite(edge_distance_inches)
]
rows <- continuous_context_fields(rows)
remaining_features <- remaining[, .(
  game_pk,
  pitch_order,
  bat_team_id = team_id,
  expected_correctable_remaining,
  expected_re_remaining
)]
rows <- merge(
  rows,
  remaining_features,
  by = c("game_pk", "pitch_order", "bat_team_id"),
  all.x = TRUE,
  sort = FALSE
)
if (any(!is.finite(rows$expected_correctable_remaining)) ||
    any(!is.finite(rows$expected_re_remaining))) {
  stop("Every replay opportunity must have expected remaining-opportunity features")
}

rows[, `:=`(
  stake_G = continuous_hypothetical_batter_gain(.SD, re_model),
  inventory_loss_k1 = continuous_expected_inventory_loss(
    expected_correctable_remaining, expected_re_remaining, 1L
  ) * inventory_loss_multiplier,
  inventory_loss_k2 = continuous_expected_inventory_loss(
    expected_correctable_remaining, expected_re_remaining, 2L
  ) * inventory_loss_multiplier,
  actual_wrong = edge_distance_inches > 0,
  observed_batter_challenge =
    challenge_occurred & challenger_role == "batter"
)]

# Fast exploratory replay bridge: invert the analytic q(r, count) on a fine,
# continuous signed-distance mesh. The mesh is numerical only; it is not a
# spatial model or a fitted location grid. Production/posterior scoring uses
# solve_challenge_margin_payoff_threshold_1d(); the approximation error here is
# checked directly against the same analytic q below.
invert_subjective_q <- function(gain, loss, context) {
  gain <- as.numeric(gain)
  loss <- as.numeric(loss)
  context <- as.character(context)
  if (length(gain) != length(loss) || length(gain) != length(context)) {
    stop("Payoffs and contexts must be row-aligned")
  }
  threshold <- rep(NA_real_, length(gain))
  always <- gain > 0 & loss == 0
  never <- gain == 0
  threshold[always] <- -Inf
  threshold[never] <- Inf
  interior <- gain > 0 & loss > 0
  target <- loss / (gain + loss)
  signal_grid <- seq(-40, 40, by = 0.0025)
  for (level in sort(unique(context[interior]))) {
    index <- which(interior & context == level)
    q_grid <- challenge_margin_subjective_ball_probability_1d(
      prior_fit,
      private_margin_signal = signal_grid,
      perception_sigma = sigma_inches,
      context = level
    )
    if (min(diff(q_grid)) < -1e-10) {
      stop("Subjective q is not monotone for count context ", level)
    }
    q_grid <- cummax(q_grid)
    keep <- !duplicated(q_grid)
    threshold[index] <- stats::approx(
      x = q_grid[keep], y = signal_grid[keep],
      xout = target[index], rule = 2, ties = "ordered"
    )$y
  }
  unresolved <- is.na(threshold)
  if (any(unresolved)) {
    stop("Undefined structural thresholds for ", sum(unresolved), " rows")
  }
  threshold
}

rows[, threshold_k1 := invert_subjective_q(
  stake_G, inventory_loss_k1, count_state
)]
rows[, threshold_k2 := invert_subjective_q(
  stake_G, inventory_loss_k2, count_state
)]
rows[, `:=`(
  challenge_probability_k1 = stats::pnorm(
    (edge_distance_inches - threshold_k1) / sigma_inches
  ),
  challenge_probability_k2 = stats::pnorm(
    (edge_distance_inches - threshold_k2) / sigma_inches
  )
)]

finite_threshold <- is.finite(rows$threshold_k1)
check_index <- which(finite_threshold)
set.seed(seed)
check_index <- sample(
  check_index, min(2000L, length(check_index)), replace = FALSE
)
inversion_error <- max(abs(
  challenge_margin_subjective_ball_probability_1d(
    prior_fit,
    private_margin_signal = rows$threshold_k1[check_index],
    perception_sigma = sigma_inches,
    context = rows$count_state[check_index]
  ) - rows$inventory_loss_k1[check_index] /
    (rows$stake_G[check_index] + rows$inventory_loss_k1[check_index])
))
if (!is.finite(inversion_error) || inversion_error > 1e-4) {
  stop("Structural-threshold inversion error is too large: ", inversion_error)
}

data.table::setorder(rows, game_pk, bat_team_id, pitch_order)

replay_team_game <- function(stream) {
  inventory_probability <- c(`0` = 0, `1` = 0, `2` = 1)
  previous_inning <- NA_integer_
  expected_attempts <- 0
  expected_successes <- 0
  expected_failures <- 0
  expected_gross_re <- 0
  exhaustion_opportunity_mass <- 0

  for (row in seq_len(nrow(stream))) {
    inning_now <- as.integer(stream$inning[[row]])
    if (!is.na(previous_inning) && inning_now > previous_inning &&
        inning_now > 9L) {
      inventory_probability[["1"]] <-
        inventory_probability[["1"]] + inventory_probability[["0"]]
      inventory_probability[["0"]] <- 0
    }

    action_probability <- c(
      `0` = 0,
      `1` = stream$challenge_probability_k1[[row]],
      `2` = stream$challenge_probability_k2[[row]]
    )
    challenge_probability <- sum(
      inventory_probability * action_probability
    )
    exhaustion_opportunity_mass <- exhaustion_opportunity_mass +
      inventory_probability[["0"]]
    expected_attempts <- expected_attempts + challenge_probability

    if (isTRUE(stream$actual_wrong[[row]])) {
      expected_successes <- expected_successes + challenge_probability
      expected_gross_re <- expected_gross_re +
        challenge_probability * stream$stake_G[[row]]
    } else {
      expected_failures <- expected_failures + challenge_probability
      old <- inventory_probability
      inventory_probability[["0"]] <- old[["0"]] +
        old[["1"]] * action_probability[["1"]]
      inventory_probability[["1"]] <-
        old[["1"]] * (1 - action_probability[["1"]]) +
        old[["2"]] * action_probability[["2"]]
      inventory_probability[["2"]] <-
        old[["2"]] * (1 - action_probability[["2"]])
    }
    previous_inning <- inning_now
  }

  data.table::data.table(
    expected_gross_re = expected_gross_re,
    expected_attempts = expected_attempts,
    expected_successes = expected_successes,
    expected_failures = expected_failures,
    expected_exhausted_opportunities = exhaustion_opportunity_mass,
    observed_gross_re = sum(
      stream$stake_G[
        stream$observed_batter_challenge & stream$actual_wrong
      ]
    ),
    observed_attempts = sum(stream$observed_batter_challenge),
    observed_successes = sum(
      stream$observed_batter_challenge & stream$actual_wrong
    ),
    oracle_gross_re = sum(stream$stake_G[stream$actual_wrong]),
    oracle_attempts = sum(stream$actual_wrong)
  )
}

team_game <- data.table::rbindlist(lapply(
  split(rows, interaction(rows$game_pk, rows$bat_team_id, drop = TRUE)),
  function(stream) {
    cbind(
      stream[1L, .(game_pk, bat_team_id)],
      replay_team_game(stream)
    )
  }
))

season <- team_game[, .(
  expected_gross_re = sum(expected_gross_re),
  expected_attempts = sum(expected_attempts),
  expected_successes = sum(expected_successes),
  expected_failures = sum(expected_failures),
  expected_exhausted_opportunities = sum(expected_exhausted_opportunities),
  observed_gross_re = sum(observed_gross_re),
  observed_attempts = sum(observed_attempts),
  observed_successes = sum(observed_successes),
  oracle_gross_re = sum(oracle_gross_re),
  oracle_attempts = sum(oracle_attempts)
)]
season[, `:=`(
  additional_re_vs_observed = expected_gross_re - observed_gross_re,
  model_success_rate = expected_successes / expected_attempts,
  observed_success_rate = observed_successes / observed_attempts,
  share_of_oracle = expected_gross_re / oracle_gross_re,
  observed_share_of_oracle = observed_gross_re / oracle_gross_re
)]

game <- team_game[, lapply(.SD, sum), by = game_pk, .SDcols = c(
  "expected_gross_re", "observed_gross_re", "oracle_gross_re",
  "expected_attempts", "expected_successes", "observed_attempts",
  "observed_successes"
)]
set.seed(seed + 1L)
bootstrap <- data.table::rbindlist(lapply(seq_len(bootstrap_reps), function(b) {
  sampled <- game[sample.int(nrow(game), nrow(game), replace = TRUE)]
  data.table::data.table(
    replicate = b,
    additional_re_vs_observed = sum(
      sampled$expected_gross_re - sampled$observed_gross_re
    ),
    expected_gross_re = sum(sampled$expected_gross_re),
    observed_gross_re = sum(sampled$observed_gross_re),
    oracle_gross_re = sum(sampled$oracle_gross_re)
  )
}))
interval <- bootstrap[, .(
  additional_re_lower_95 = stats::quantile(
    additional_re_vs_observed, 0.025, names = FALSE
  ),
  additional_re_upper_95 = stats::quantile(
    additional_re_vs_observed, 0.975, names = FALSE
  ),
  expected_re_lower_95 = stats::quantile(
    expected_gross_re, 0.025, names = FALSE
  ),
  expected_re_upper_95 = stats::quantile(
    expected_gross_re, 0.975, names = FALSE
  )
)]
season <- cbind(season, interval)

manifest <- data.table::data.table(
  status = "exploratory_not_cross_fitted",
  information_regime = "common-width private signal; prior conditioned on take/called strike/count",
  sigma_inches = sigma_inches,
  inventory_loss_multiplier = inventory_loss_multiplier,
  prior_components = prior_fit$components,
  opportunities = nrow(rows),
  games = data.table::uniqueN(rows$game_pk),
  teams = data.table::uniqueN(rows$bat_team_id),
  inversion_max_absolute_error = inversion_error,
  bootstrap_reps = bootstrap_reps,
  seed = seed,
  output_tag = output_tag,
  git_sha = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  branch = system2("git", c("branch", "--show-current"), stdout = TRUE)
)

output_directory <- file.path(
  project_root, "data", "processed", "perception"
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(
  season,
  file.path(
    output_directory,
    paste0("common_width_run_value_exploratory_", output_tag, ".csv")
  )
)
data.table::fwrite(
  manifest,
  file.path(
    output_directory,
    paste0("common_width_run_value_manifest_", output_tag, ".csv")
  )
)
saveRDS(
  list(
    season = season,
    team_game = team_game,
    bootstrap = bootstrap,
    manifest = manifest
  ),
  file.path(
    output_directory,
    paste0("common_width_run_value_exploratory_", output_tag, ".rds")
  )
)

print(manifest)
print(season)
