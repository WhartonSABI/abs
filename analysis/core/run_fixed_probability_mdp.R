# This script uses the saved CAR fits to create fixed p_hat values; it does not
# refit either Bayesian model. If `scored` already exists, that object is reused.

project_root <- rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd())
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

if (exists("scored", envir = .GlobalEnv, inherits = FALSE)) {
  mdp_scored_ledger <- get("scored", envir = .GlobalEnv)
} else if (exists("ledger", envir = .GlobalEnv, inherits = FALSE) &&
           "p_hat" %in% names(get("ledger", envir = .GlobalEnv))) {
  mdp_scored_ledger <- get("ledger", envir = .GlobalEnv)
} else {
  suppressPackageStartupMessages(library(brms))
  mdp_ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
  if (!"umpire_name" %in% names(mdp_ledger)) {
    mdp_umpires <- data.table::as.data.table(readRDS(
      file.path(project_root, "data", "processed", "umpires_2026.rds")
    ))
    mdp_ledger <- merge(mdp_ledger, mdp_umpires, by = "game_pk", all.x = TRUE,
      sort = FALSE)
  }
  stop_if_missing_columns(
    mdp_ledger,
    c("umpire_name", "fielder_2", "plate_x", "plate_z", "sz_bot", "sz_top"),
    "Bayesian MDP scoring ledger"
  )
  mdp_fit_balls <- readRDS(file.path(
    project_root, "data", "processed", "fit_car_balls_final.rds"
  ))
  mdp_fit_strikes <- readRDS(file.path(
    project_root, "data", "processed", "fit_car_strikes_final.rds"
  ))
  mdp_score_fit <- function(fit, call) {
    d <- data.table::as.data.table(fit$data)
    set.seed(42)
    d[, p_hat := brms::fitted(fit, ndraws = 500)[, "Estimate"] / n]
    unique(d[, .(
      cell = as.character(cell),
      umpire = as.character(umpire),
      catcher = as.character(catcher),
      initial_call = call,
      p_hat
    )])
  }
  mdp_probabilities <- data.table::rbindlist(list(
    mdp_score_fit(mdp_fit_strikes, "called_strike"),
    mdp_score_fit(mdp_fit_balls, "ball")
  ))
  mdp_ledger[, `:=`(
    x_in = plate_x * 12,
    z_in = (plate_z - sz_bot) / (sz_top - sz_bot) * 20
  )]
  mdp_ledger[, `:=`(
    cell = paste(round(x_in / 1.5), round(z_in / 1.5), sep = "_"),
    umpire = as.character(umpire_name),
    catcher = as.character(fielder_2)
  )]
  mdp_scored_ledger <- merge(
    mdp_ledger,
    mdp_probabilities,
    by = c("cell", "umpire", "catcher", "initial_call"),
    all.x = TRUE,
    sort = FALSE
  )
}

if (exists("re_model", envir = .GlobalEnv, inherits = FALSE)) {
  mdp_re_model <- get("re_model", envir = .GlobalEnv)
} else {
  mdp_re_model <- targets::tar_read(re_model)
}

mdp_opportunities <- prepare_mdp_opportunities(
  mdp_scored_ledger,
  mdp_re_model,
  p_col = "p_hat",
  min_coverage = 0.99
)

#no crossfit
mdp_fit <- fit_challenge_mdp(
  mdp_opportunities,
  prior_n = 30,
  tol = 1e-7,
  max_iter = 3000L
)
saveRDS(
  mdp_fit,
  "data/processed/mdp_full_fit.rds"
)


arrow::write_parquet(
  mdp_fit$state_values,
  "data/processed/mdp_state_values.parquet"
)

mdp_pitch_decisions <- replay_challenge_policy(
  opportunities = mdp_opportunities,
  fit = mdp_fit,
  policy = "mdp",
  initial_inventory = 2L
)

source("scripts/functions/core/mdp_policy.R")
arrow::write_parquet(
  mdp_pitch_decisions,
  "data/processed/mdp_pitch_decisions_full_fit.parquet"
)

#with cross fit
mdp_results <- crossfit_challenge_mdp(
  mdp_opportunities,
  folds = 5L,
  seed = 42L,
  prior_n = 30,
  tol = 1e-10,
  max_iter = 10000L,
  bootstrap_reps = 500L,
  static_threshold = 0.05
)


mdp_output_dir <- file.path(project_root, "data", "processed")
mdp_state_values_file <- write_parquet_atomic(
  mdp_results$full_fit$state_values,
  file.path(mdp_output_dir, "mdp_state_values.parquet")
)
mdp_pitch_decisions_file <- write_parquet_atomic(
  mdp_results$pitch_decisions,
  file.path(mdp_output_dir, "mdp_pitch_decisions.parquet")
)
mdp_evaluation_file <- write_csv_atomic(
  mdp_results$evaluation,
  file.path(mdp_output_dir, "mdp_evaluation.csv")
)
mdp_fit_file <- atomic_write(
  file.path(mdp_output_dir, "mdp_full_fit.rds"),
  function(path) saveRDS(mdp_results$full_fit, path)
)

print(mdp_results$evaluation[order(-mean_re_team_game)])
message("Wrote MDP state values: ", mdp_state_values_file)
message("Wrote cross-fitted pitch decisions: ", mdp_pitch_decisions_file)
message("Wrote held-out evaluation: ", mdp_evaluation_file)
message("Wrote reusable full-data fit: ", mdp_fit_file)
