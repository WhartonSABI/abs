project_root <- rprojroot::find_root(
  rprojroot::has_file("_targets.R"), path = getwd()
)
source(file.path(project_root, "config", "project.R"))
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

environment_integer <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, unset = default)))
  if (length(value) != 1L || is.na(value) || value < 1L) {
    stop(name, " must be a positive integer", call. = FALSE)
  }
  value
}

cmdstan_path <- Sys.getenv(
  "CMDSTAN",
  unset = file.path(
    project_root, "data", "processed", "stan", "cmdstan-2.39.0"
  )
)
if (!dir.exists(cmdstan_path)) {
  stop("CmdStan was not found at ", cmdstan_path, call. = FALSE)
}
cmdstanr::set_cmdstan_path(cmdstan_path)

seed <- environment_integer("ABS_1D_SEED", 20260825L)
heldout_fold <- environment_integer("ABS_1D_HELDOUT_FOLD", 1L)
chains <- environment_integer("ABS_1D_CHAINS", 2L)
iter_warmup <- environment_integer("ABS_1D_WARMUP", 250L)
iter_sampling <- environment_integer("ABS_1D_SAMPLING", 250L)
score_draws <- environment_integer(
  "ABS_1D_SCORE_DRAWS", min(400L, chains * iter_sampling)
)

input_file <- file.path(
  project_root, "data", "processed", "continuous_decision_features.parquet"
)
if (!file.exists(input_file)) {
  stop("Missing pilot input: ", input_file, call. = FALSE)
}
rows <- data.table::as.data.table(arrow::read_parquet(input_file))
folds <- make_challenge_discrimination_1d_game_folds(
  rows, folds = 5L, seed = seed, margin_limit_inches = 3
)
split <- split_challenge_discrimination_1d_fold(
  rows, folds, heldout_fold = heldout_fold, margin_limit_inches = 3
)

sampler_health <- function(result) {
  summary <- result$core_summary
  finite_rhat <- summary$rhat[is.finite(summary$rhat)]
  finite_bulk <- summary$ess_bulk[is.finite(summary$ess_bulk)]
  finite_tail <- summary$ess_tail[is.finite(summary$ess_tail)]
  data.table::data.table(
    sigma_model = result$sigma_model,
    divergences = sum(result$diagnostic$num_divergent),
    max_treedepth_hits = sum(result$diagnostic$num_max_treedepth),
    minimum_ebfmi = min(result$diagnostic$ebfmi),
    maximum_core_rhat = max(finite_rhat),
    minimum_core_bulk_ess = min(finite_bulk),
    minimum_core_tail_ess = min(finite_tail),
    healthy = sum(result$diagnostic$num_divergent) == 0L &&
      sum(result$diagnostic$num_max_treedepth) == 0L &&
      min(result$diagnostic$ebfmi) > 0.30 &&
      max(finite_rhat) <= 1.01 &&
      min(finite_bulk) >= 100 && min(finite_tail) >= 100
  )
}

fit_and_score <- function(sigma_model) {
  fit <- fit_challenge_discrimination_1d(
    split$train,
    sigma_model = sigma_model,
    chains = chains,
    parallel_chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    seed = seed + as.integer(sigma_model == "hierarchical"),
    adapt_delta = 0.98,
    max_treedepth = 12L,
    refresh = 0L,
    force_refit = TRUE
  )
  diagnostic <- fit$stan_fit$diagnostic_summary()
  core_variables <- c(
    "mu_threshold", "mu_log_sigma", "tau_player",
    "rho_threshold_log_sigma", "tau_team", "tau_umpire", "tau_catcher",
    "beta_context"
  )
  core_summary <- fit$stan_fit$summary(core_variables)
  score <- score_challenge_discrimination_1d(
    fit, split$heldout, ndraws = score_draws, seed = seed + 2L,
    new_player = "population", require_game_separation = TRUE
  )
  posterior <- draws_challenge_discrimination_1d(
    fit, ndraws = score_draws, seed = seed + 3L
  )
  player_correlation <- vapply(
    seq_len(ncol(posterior$sigma_player)),
    function(index) stats::cor(
      posterior$threshold_player[, index],
      log(posterior$sigma_player[, index])
    ),
    numeric(1L)
  )
  list(
    sigma_model = sigma_model,
    diagnostic = diagnostic,
    core_summary = core_summary,
    population = summarize_challenge_discrimination_1d(
      fit, ndraws = score_draws, seed = seed + 4L
    )$population,
    player_posterior_correlation = player_correlation,
    score = score
  )
}

models <- c("common", "hierarchical")
if (.Platform$OS.type == "unix") {
  results <- parallel::mclapply(models, fit_and_score, mc.cores = 2L)
} else {
  results <- lapply(models, fit_and_score)
}
names(results) <- models
if (any(vapply(results, inherits, logical(1L), what = "try-error"))) {
  stop("At least one pilot model failed", call. = FALSE)
}

comparison <- compare_challenge_discrimination_1d_heldout(
  results$common$score, results$hierarchical$score
)
health <- data.table::rbindlist(lapply(results, sampler_health))
comparison$metrics[, `:=`(
  common_sampler_healthy = health[
    sigma_model == "common", healthy
  ],
  hierarchical_sampler_healthy = health[
    sigma_model == "hierarchical", healthy
  ]
)]
comparison$metrics[, advance_hierarchical_to_five_fold :=
  common_sampler_healthy && hierarchical_sampler_healthy &&
    one_game_clustered_se_pass]

pitch_ledger_file <- file.path(
  project_root, "data", "processed", "pitch_ledger.parquet"
)
pitch_ledger <- data.table::as.data.table(arrow::read_parquet(pitch_ledger_file))
outer_training_games <- sort(unique(as.character(split$train$game_pk)))
outer_heldout_games <- sort(unique(as.character(split$heldout$game_pk)))
set.seed(seed + 101L)
shuffled_training_games <- sample(outer_training_games)
inner_validation_games <- shuffled_training_games[
  seq_along(shuffled_training_games) %% 4L == 0L
]
inner_fit_games <- setdiff(outer_training_games, inner_validation_games)
prior_selection <- select_challenge_margin_prior_1d(
  pitch_ledger,
  fit_games = inner_fit_games,
  validation_games = inner_validation_games,
  components = c(1L, 3L, 6L),
  refit_selected = TRUE,
  fold_id = heldout_fold,
  global_draw_id = 1L
)
heldout_prior_rows <- pitch_ledger[
  as.character(game_pk) %in% outer_heldout_games
]
prior_scored <- score_challenge_margin_prior_1d(
  prior_selection$fit, heldout_prior_rows,
  require_game_separation = TRUE
)
prior_outer_metrics <- challenge_margin_game_clustered_log_score_1d(
  prior_scored
)
prior_scored[, `:=`(
  observed_ball = as.numeric(edge_distance_inches > 0),
  predicted_ball_rate = challenge_margin_prior_ball_rate_1d(
    prior_selection$fit, context = count_state
  )
)]
prior_calibration <- prior_scored[, .(
  observed_ball_rate = mean(observed_ball),
  predicted_ball_rate = mean(predicted_ball_rate),
  ball_rate_error = mean(predicted_ball_rate - observed_ball)
)]
prior_outer_metrics <- cbind(prior_outer_metrics, prior_calibration)
output <- list(
  manifest = list(
    seed = seed,
    heldout_fold = heldout_fold,
    local_margin_limit_inches = 3,
    chains = chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    score_draws = score_draws,
    training_eligible_rows = nrow(split$train),
    training_local_rows = split$split_summary[
      split == "training", local_model_rows
    ],
    training_tail_rows = split$split_summary[
      split == "training", tail_diagnostic_rows
    ],
    heldout_eligible_rows = nrow(split$heldout),
    heldout_local_rows = split$split_summary[
      split == "heldout", local_model_rows
    ],
    heldout_tail_rows = split$split_summary[
      split == "heldout", tail_diagnostic_rows
    ],
    training_games = data.table::uniqueN(split$train$game_pk),
    heldout_games = data.table::uniqueN(split$heldout$game_pk),
    git_sha = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
    branch = system2("git", c("branch", "--show-current"), stdout = TRUE),
    generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ),
  comparison = comparison,
  sampler_health = health,
  prior = list(
    selected_components = prior_selection$selected_components,
    candidate_metrics = prior_selection$candidate_metrics,
    outer_metrics = prior_outer_metrics,
    contextual_ball_rates = data.table::data.table(
      count_state = prior_selection$fit$context_levels,
      predicted_ball_rate = challenge_margin_prior_ball_rate_1d(
        prior_selection$fit,
        context = prior_selection$fit$context_levels
      ),
      exposure = prior_selection$fit$context_exposure
    )
  ),
  model = results
)
output_directory <- file.path(
  project_root, "data", "processed", "stan", "pilot_1d"
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(
  output_directory,
  paste0("challenge_discrimination_fold_", heldout_fold, ".rds")
)
saveRDS(output, output_file)

print(output$manifest)
print(output$comparison$metrics)
print(output$sampler_health)
print(output$prior$candidate_metrics)
print(output$prior$outer_metrics)
for (model in models) {
  cat("\n", toupper(model), " SAMPLER\n", sep = "")
  print(results[[model]]$diagnostic)
  print(results[[model]]$core_summary)
  cat("Absolute player threshold/log-sigma posterior correlations:\n")
  print(stats::quantile(
    abs(results[[model]]$player_posterior_correlation),
    c(0.5, 0.95, 1), na.rm = TRUE
  ))
}
cat("\nSaved compact pilot result to ", output_file, "\n", sep = "")
