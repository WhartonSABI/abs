# Parallel joint Bayesian perception research pipeline.
#
# Run one stage at a time from the project root:
#   Rscript analysis/perception/run_joint_bayesian_perception.R pilot
#   Rscript analysis/perception/run_joint_bayesian_perception.R finalists
#   Rscript analysis/perception/run_joint_bayesian_perception.R full
#   Rscript analysis/perception/run_joint_bayesian_perception.R crossfit
#   Rscript analysis/perception/run_joint_bayesian_perception.R mdp

project_root <- rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd())
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)
suppressPackageStartupMessages({
  library(data.table)
  library(brms)
})

arguments <- commandArgs(trailingOnly = TRUE)
stage <- if (length(arguments)) tolower(arguments[[1L]]) else "pilot"
valid_stages <- c("pilot", "finalists", "full", "crossfit", "mdp")
if (!stage %in% valid_stages) {
  stop("Stage must be one of: ", paste(valid_stages, collapse = ", "))
}

output_dir <- file.path(project_root, "data", "processed", "joint_perception")
fit_dir <- file.path(output_dir, "fits")
dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)

env_integer <- function(name, default) {
  value <- Sys.getenv(name, unset = as.character(default))
  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed <= 0L) stop(name, " must be a positive integer")
  parsed
}

load_joint_ledger <- function() {
  ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
  if (!"umpire_name" %in% names(ledger)) {
    umpires <- data.table::as.data.table(readRDS(file.path(
      project_root, "data", "processed", "umpires_2026.rds"
    )))
    ledger <- merge(ledger, umpires, by = "game_pk", all.x = TRUE, sort = FALSE)
  }
  stop_if_missing_columns(
    ledger, joint_perception_required_columns(), "joint perception ledger"
  )
  ledger
}

candidate_id <- function(bin_width, sdcar_scale) {
  clean <- function(x) gsub("\\.", "p", format(x, trim = TRUE), fixed = FALSE)
  paste0("bin", clean(bin_width), "_sdcar", clean(sdcar_scale))
}

write_table <- function(x, path) {
  data.table::fwrite(data.table::as.data.table(x), path)
  normalizePath(path, mustWork = TRUE)
}

write_probabilities <- function(x, path) {
  arrow::write_parquet(data.table::as.data.table(x), path)
  normalizePath(path, mustWork = TRUE)
}

ledger <- load_joint_ledger()
split_file <- file.path(output_dir, "game_split.csv")
if (file.exists(split_file)) {
  game_split <- data.table::fread(split_file, colClasses = c(game_pk = "character"))
} else {
  game_split <- deterministic_game_split(ledger, train_fraction = 0.8, seed = 42L)
  write_table(game_split, split_file)
}
ledger[, game_key__ := as.character(game_pk)]
train_games <- game_split[split == "train", game_pk]
validation_games <- game_split[split == "validation", game_pk]
if (length(intersect(train_games, validation_games))) stop("Train/validation games overlap")
train_ledger <- ledger[game_key__ %in% train_games]
validation_ledger <- ledger[game_key__ %in% validation_games]

pilot_controls <- list(
  chains = env_integer("JOINT_PILOT_CHAINS", 2L),
  cores = env_integer("JOINT_PILOT_CORES", 2L),
  iter = env_integer("JOINT_PILOT_ITER", 2000L),
  warmup = env_integer("JOINT_PILOT_WARMUP", 1000L)
)
full_controls <- list(
  chains = env_integer("JOINT_FULL_CHAINS", 4L),
  cores = env_integer("JOINT_FULL_CORES", 4L),
  iter = env_integer("JOINT_FULL_ITER", 8000L),
  warmup = env_integer("JOINT_FULL_WARMUP", 4000L)
)
if (pilot_controls$warmup >= pilot_controls$iter || full_controls$warmup >= full_controls$iter) {
  stop("Warmup must be smaller than total iterations")
}

if (stage == "pilot") {
  re_model_for_candidates <- targets::tar_read(re_model)
  grid <- data.table::CJ(
    bin_width = c(2, 3, 4),
    sdcar_scale = c(0.5, 1, 2, 5)
  )
  grid[, candidate_id := candidate_id(bin_width, sdcar_scale)]
  metrics_file <- file.path(output_dir, "pilot_candidate_metrics.csv")
  completed <- if (file.exists(metrics_file)) {
    unique(data.table::fread(metrics_file)$candidate_id)
  } else character()
  all_metrics <- if (file.exists(metrics_file)) data.table::fread(metrics_file) else NULL

  for (i in seq_len(nrow(grid))) {
    spec <- grid[i]
    id <- spec$candidate_id
    if (id %in% completed) {
      message("Skipping completed pilot candidate: ", id)
      next
    }
    message("Preparing pilot candidate: ", id)
    input <- prepare_joint_car_input(train_ledger, spec$bin_width, full = FALSE)
    fit_base <- file.path(fit_dir, paste0("pilot_", id))
    fit <- do.call(fit_joint_perception_model, c(list(
      input = input, sdcar_scale = spec$sdcar_scale,
      seed = 42L + i, file = fit_base
    ), pilot_controls))
    scored <- score_joint_perception_model(
      fit, validation_ledger,
      ndraws = env_integer("JOINT_SCORE_DRAWS", 500L), seed = 42L + i
    )
    probability_file <- file.path(output_dir, paste0("pilot_probabilities_", id, ".parquet"))
    write_probabilities(scored, probability_file)
    metrics <- joint_probability_metrics(scored)
    mdp_diagnostic <- run_joint_perception_mdp(scored, re_model_for_candidates)
    mdp_rates <- mdp_diagnostic$evaluation[, .(
      role,
      mdp_challenge_rate = challenge_rate,
      mdp_success_rate = success_rate,
      mdp_captured_re = captured_re,
      mdp_zero_inventory_rate = zero_inventory_rate
    )]
    metrics[mdp_rates, on = "role", `:=`(
      mdp_challenge_rate = i.mdp_challenge_rate,
      mdp_success_rate = i.mdp_success_rate,
      mdp_captured_re = i.mdp_captured_re,
      mdp_zero_inventory_rate = i.mdp_zero_inventory_rate
    )]
    metrics[, `:=`(
      candidate_id = id,
      bin_width = spec$bin_width,
      sdcar_scale = spec$sdcar_scale,
      fit_type = "population_pilot",
      probability_file = probability_file
    )]
    all_metrics <- data.table::rbindlist(list(all_metrics, metrics), fill = TRUE)
    write_table(all_metrics, metrics_file)
    rm(fit, scored, input, mdp_diagnostic)
    invisible(gc())
  }
  ranking <- select_joint_perception_candidates(all_metrics, finalists = 2L)
  ranking_file <- file.path(output_dir, "pilot_candidate_ranking.csv")
  write_table(ranking, ranking_file)
  print(ranking)
  message("Pilot complete. Ranking: ", ranking_file)
}

if (stage == "finalists") {
  re_model_for_candidates <- targets::tar_read(re_model)
  ranking_file <- file.path(output_dir, "pilot_candidate_ranking.csv")
  if (!file.exists(ranking_file)) stop("Run the pilot stage first")
  finalists <- data.table::fread(ranking_file)[finalist == TRUE]
  validation_metrics <- list()
  for (i in seq_len(nrow(finalists))) {
    spec <- finalists[i]
    id <- spec$candidate_id
    message("Fitting full-effects finalist: ", id)
    input <- prepare_joint_car_input(train_ledger, spec$bin_width, full = TRUE)
    fit_base <- file.path(fit_dir, paste0("validation_", id))
    fit <- do.call(fit_joint_perception_model, c(list(
      input = input, sdcar_scale = spec$sdcar_scale,
      seed = 142L + i, file = fit_base
    ), full_controls))
    diagnostics <- joint_fit_diagnostics(fit)
    scored <- score_joint_perception_model(
      fit, validation_ledger,
      ndraws = env_integer("JOINT_SCORE_DRAWS", 500L), seed = 142L + i
    )
    probability_file <- file.path(output_dir, paste0("validation_probabilities_", id, ".parquet"))
    write_probabilities(scored, probability_file)
    metrics <- joint_probability_metrics(scored)
    mdp_diagnostic <- run_joint_perception_mdp(scored, re_model_for_candidates)
    mdp_rates <- mdp_diagnostic$evaluation[, .(
      role,
      mdp_challenge_rate = challenge_rate,
      mdp_success_rate = success_rate,
      mdp_captured_re = captured_re,
      mdp_zero_inventory_rate = zero_inventory_rate
    )]
    metrics[mdp_rates, on = "role", `:=`(
      mdp_challenge_rate = i.mdp_challenge_rate,
      mdp_success_rate = i.mdp_success_rate,
      mdp_captured_re = i.mdp_captured_re,
      mdp_zero_inventory_rate = i.mdp_zero_inventory_rate
    )]
    metrics[, `:=`(
      candidate_id = id, bin_width = spec$bin_width,
      sdcar_scale = spec$sdcar_scale, fit_type = "full_effects_validation",
      probability_file = probability_file,
      diagnostics_pass = diagnostics$pass
    )]
    validation_metrics[[i]] <- metrics
    write_table(diagnostics, file.path(output_dir, paste0("diagnostics_", id, ".csv")))
  }
  validation_metrics <- data.table::rbindlist(validation_metrics)
  metrics_file <- file.path(output_dir, "finalist_validation_metrics.csv")
  write_table(validation_metrics, metrics_file)
  eligible_metrics <- validation_metrics[diagnostics_pass == TRUE]
  if (!nrow(eligible_metrics)) stop("No finalist passed posterior diagnostics")
  winner <- select_joint_perception_candidates(eligible_metrics, finalists = 1L)[1L]
  winner_file <- file.path(output_dir, "selected_candidate.csv")
  write_table(winner, winner_file)

  baseline_file <- file.path(
    project_root, "data", "processed", "bayesian_current_crossfit_probabilities.parquet"
  )
  gate <- data.table::data.table(
    candidate_id = winner$candidate_id,
    status = "pending_heldout_baseline",
    reason = paste(
      "No genuinely game-held-out current-model probability artifact was found;",
      "the candidate remains parallel and cannot replace the current fits."
    )
  )
  if (file.exists(baseline_file)) {
    baseline <- data.table::as.data.table(arrow::read_parquet(baseline_file))
    baseline_metrics <- joint_probability_metrics(baseline)
    baseline_mdp <- run_joint_perception_mdp(baseline, re_model_for_candidates)
    baseline_mdp_summary <- baseline_mdp$evaluation
    write_table(baseline_metrics, file.path(output_dir, "baseline_heldout_metrics.csv"))
    write_table(
      baseline_mdp_summary,
      file.path(output_dir, "baseline_heldout_mdp_summary.csv")
    )
    candidate_league <- validation_metrics[
      candidate_id == winner$candidate_id & scope == "league"
    ]
    baseline_league <- baseline_metrics[scope == "league"]
    role_candidate <- validation_metrics[
      candidate_id == winner$candidate_id & scope == "role", .(role, ece_candidate = ece_05)
    ]
    role_baseline <- baseline_metrics[scope == "role", .(role, ece_baseline = ece_05)]
    role_check <- merge(role_candidate, role_baseline, by = "role")
    statistical_pass <-
      candidate_league$log_loss <= baseline_league$log_loss + baseline_league$log_loss_se &&
      candidate_league$brier <= baseline_league$brier + 0.001 &&
      all(role_check$ece_candidate <= role_check$ece_baseline + 0.002) &&
      candidate_league$extreme_rate < baseline_league$extreme_rate &&
      candidate_league$mdp_challenge_rate <
        baseline_mdp_summary[role == "league", challenge_rate]
    gate[, `:=`(
      status = ifelse(statistical_pass, "statistical_gate_pass", "rejected"),
      reason = ifelse(
        statistical_pass,
        "Held-out predictive and extreme-probability gates passed; MDP-rate gate remains.",
        "At least one held-out predictive or extreme-probability gate failed."
      )
    )]
  }
  write_table(gate, file.path(output_dir, "acceptance_gate.csv"))
  print(winner)
  print(gate)
}

if (stage == "full") {
  winner_file <- file.path(output_dir, "selected_candidate.csv")
  if (!file.exists(winner_file)) stop("Run the finalists stage first")
  winner <- data.table::fread(winner_file)[1L]
  message("Fitting selected candidate on all games: ", winner$candidate_id)
  input <- prepare_joint_car_input(ledger, winner$bin_width, full = TRUE)
  fit_base <- file.path(fit_dir, "joint_perception_full_final")
  fit <- do.call(fit_joint_perception_model, c(list(
    input = input, sdcar_scale = winner$sdcar_scale,
    seed = 242L, file = fit_base
  ), full_controls))
  diagnostics <- joint_fit_diagnostics(fit)
  write_table(diagnostics, file.path(output_dir, "full_fit_diagnostics.csv"))
  if (!diagnostics$pass) stop("Selected full fit failed posterior diagnostics")
  scored <- score_joint_perception_model(
    fit, ledger, ndraws = env_integer("JOINT_SCORE_DRAWS", 500L), seed = 242L
  )
  probability_file <- file.path(output_dir, "joint_perception_full_probabilities.parquet")
  write_probabilities(scored, probability_file)
  write_table(
    joint_probability_metrics(scored),
    file.path(output_dir, "full_fit_in_sample_metrics.csv")
  )
  message("Full fit and probabilities written under: ", output_dir)
}

if (stage == "crossfit") {
  winner_file <- file.path(output_dir, "selected_candidate.csv")
  if (!file.exists(winner_file)) stop("Run the finalists stage first")
  winner <- data.table::fread(winner_file)[1L]
  games <- sort(unique(as.character(ledger$game_pk)))
  set.seed(42L)
  fold_assignment <- data.table::data.table(
    game_key__ = sample(games), fold = rep(1:5, length.out = length(games))
  )
  write_table(fold_assignment, file.path(output_dir, "bayesian_fold_assignment.csv"))
  scored_folds <- vector("list", 5L)
  for (fold_id in 1:5) {
    held_out <- fold_assignment[fold == fold_id, game_key__]
    training <- ledger[!game_key__ %in% held_out]
    testing <- ledger[game_key__ %in% held_out]
    if (length(intersect(unique(training$game_key__), unique(testing$game_key__)))) {
      stop("Bayesian fold leakage detected")
    }
    message("Fitting Bayesian fold ", fold_id, " of 5")
    input <- prepare_joint_car_input(training, winner$bin_width, full = TRUE)
    fit_base <- file.path(fit_dir, paste0("joint_perception_fold", fold_id))
    fit <- do.call(fit_joint_perception_model, c(list(
      input = input, sdcar_scale = winner$sdcar_scale,
      seed = 342L + fold_id, file = fit_base
    ), full_controls))
    diagnostics <- joint_fit_diagnostics(fit)
    if (!diagnostics$pass) stop("Bayesian fold ", fold_id, " failed diagnostics")
    scored_folds[[fold_id]] <- score_joint_perception_model(
      fit, testing, ndraws = env_integer("JOINT_SCORE_DRAWS", 500L),
      seed = 342L + fold_id
    )
    scored_folds[[fold_id]][, bayesian_fold := fold_id]
    write_probabilities(
      scored_folds[[fold_id]],
      file.path(output_dir, paste0("joint_perception_fold", fold_id, "_probabilities.parquet"))
    )
    rm(fit, input)
    invisible(gc())
  }
  scored <- data.table::rbindlist(scored_folds, fill = TRUE)
  probability_file <- file.path(output_dir, "joint_perception_crossfit_probabilities.parquet")
  write_probabilities(scored, probability_file)
  write_table(
    joint_probability_metrics(scored),
    file.path(output_dir, "joint_perception_crossfit_metrics.csv")
  )
  message("Cross-fitted probabilities: ", probability_file)
}

if (stage == "mdp") {
  full_probability_file <- file.path(output_dir, "joint_perception_full_probabilities.parquet")
  crossfit_probability_file <- file.path(output_dir, "joint_perception_crossfit_probabilities.parquet")
  if (!file.exists(full_probability_file)) stop("Run the full stage first")
  re_model <- targets::tar_read(re_model)
  full_scored <- data.table::as.data.table(arrow::read_parquet(full_probability_file))
  full_mdp <- run_joint_perception_mdp(full_scored, re_model)
  saveRDS(full_mdp$fit, file.path(output_dir, "joint_mdp_full_fit.rds"))
  write_probabilities(full_mdp$fit$state_values,
    file.path(output_dir, "joint_mdp_state_values.parquet"))
  write_probabilities(full_mdp$decisions,
    file.path(output_dir, "joint_mdp_pitch_decisions_full_fit.parquet"))
  write_table(full_mdp$evaluation,
    file.path(output_dir, "joint_mdp_full_fit_summary.csv"))

  if (file.exists(crossfit_probability_file)) {
    crossfit_scored <- data.table::as.data.table(arrow::read_parquet(crossfit_probability_file))
    opportunities <- prepare_mdp_opportunities(crossfit_scored, re_model, p_col = "p_hat")
    mdp_results <- crossfit_challenge_mdp(
      opportunities, folds = 5L, seed = 42L, prior_n = 30,
      tol = 1e-10, max_iter = 10000L, bootstrap_reps = 500L,
      static_threshold = 0.05
    )
    saveRDS(mdp_results$full_fit, file.path(output_dir, "joint_mdp_crossfit_full_fit.rds"))
    write_probabilities(mdp_results$pitch_decisions,
      file.path(output_dir, "joint_mdp_pitch_decisions_crossfit.parquet"))
    write_table(mdp_results$evaluation,
      file.path(output_dir, "joint_mdp_evaluation.csv"))
  } else {
    warning("Cross-fitted Bayesian probabilities are absent; writing full-fit MDP outputs only")
  }
  message("Parallel joint Bayesian/MDP outputs written under: ", output_dir)
}
