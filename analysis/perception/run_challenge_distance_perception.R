# Empirical challenge-distance perception proxy.
#
# Fast held-out pilot:
#   Rscript analysis/perception/run_challenge_distance_perception.R fast
#
# Longer held-out confirmation fit after reviewing the pilot:
#   Rscript analysis/perception/run_challenge_distance_perception.R full
#
# Quick full-season MDP preview:
#   Rscript analysis/perception/run_challenge_distance_perception.R quick_mdp
#
# Five-fold perception and MDP analysis with fixed sharp p_hat:
#   Rscript analysis/perception/run_challenge_distance_perception.R crossfit_mdp

project_root <- rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd())
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

arguments <- commandArgs(trailingOnly = TRUE)
stage <- if (length(arguments)) tolower(arguments[[1L]]) else "fast"
if (stage == "quick") stage <- "quick_mdp"
if (stage == "crossfit") stage <- "crossfit_mdp"
valid_stages <- c("fast", "full", "quick_mdp", "crossfit_mdp")
if (!stage %in% valid_stages) {
  stop("Stage must be one of: ", paste(valid_stages, collapse = ", "))
}

output_dir <- file.path(
  project_root, "data", "processed", "challenge_distance_perception"
)
fit_dir <- file.path(output_dir, "fits")
dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)

env_integer <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, unset = as.character(default))))
  if (!is.finite(value) || value <= 0L) stop(name, " must be a positive integer")
  value
}

env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = ifelse(default, "true", "false")))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be true or false")
  }
  value %in% c("true", "1", "yes")
}

ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
probability <- data.table::as.data.table(arrow::read_parquet(file.path(
  project_root, "data", "processed", "gaussian_perception",
  "sharp_full_probabilities.parquet"
)))
if ("p_hat" %in% names(ledger)) ledger[, p_hat := NULL]
ledger <- merge(
  ledger, probability[, .(game_pk, pitch_order, p_hat)],
  by = c("game_pk", "pitch_order"), all.x = TRUE, sort = FALSE
)
re_model <- targets::tar_read(re_model)
choices_file <- file.path(output_dir, "eligible_choices.parquet")
if (file.exists(choices_file)) {
  choices <- data.table::as.data.table(arrow::read_parquet(choices_file))
} else {
  message("Building eligible hitter, catcher, and pitcher decisions")
  choices <- prepare_perception_choices(ledger, re_model, p_col = "p_hat")
  write_parquet_atomic(choices, choices_file)
}

split_file <- file.path(output_dir, "game_split.csv")
if (file.exists(split_file)) {
  split <- data.table::fread(split_file, colClasses = c(game_pk = "character"))
} else {
  split <- deterministic_game_split(choices, train_fraction = 0.8, seed = 42L)
  write_csv_atomic(split, split_file)
}
choices[, game_key__ := as.character(game_pk)]
choices[split, on = c(game_key__ = "game_pk"), split := i.split]
if (anyNA(choices$split)) stop("Some games are missing from the split")
train <- choices[split == "train"]
heldout <- choices[split == "validation"]
if (length(intersect(
  unique(as.character(train$game_pk)), unique(as.character(heldout$game_pk))
))) stop("Training and held-out games overlap")

load_clean_perception_fit <- function() {
  path <- file.path(fit_dir, "full_fit.rds")
  if (!file.exists(path)) {
    message("The clean perception fit is missing; fitting it now")
    fit_hierarchical_challenge_distance(
      train,
      chains = 4L, cores = 4L, iter = 4000L, warmup = 2000L,
      seed = 42L, adapt_delta = 0.99, file = path, refresh = 100L
    )
  } else {
    readRDS(path)
  }
}

load_tracking_opportunities <- function() {
  path <- file.path(output_dir, "tracking_opportunities.parquet")
  force <- env_flag("CHALLENGE_DISTANCE_REBUILD_OPPORTUNITIES", FALSE)
  if (file.exists(path) && !force) {
    return(data.table::as.data.table(arrow::read_parquet(path)))
  }
  value <- prepare_mdp_opportunities(ledger, re_model, p_col = "p_hat")
  coverage <- attr(value, "coverage")
  if (!is.null(coverage)) {
    write_csv_atomic(coverage, file.path(output_dir, "tracking_coverage.csv"))
  }
  write_parquet_atomic(value, path)
  value
}

write_mdp_outputs <- function(result, replays, prefix) {
  write_csv_atomic(
    result$evaluation,
    file.path(output_dir, paste0(prefix, "_mdp_evaluation.csv"))
  )
  write_csv_atomic(
    result$role_evaluation,
    file.path(output_dir, paste0(prefix, "_mdp_role_evaluation.csv"))
  )
  write_csv_atomic(
    result$decomposition,
    file.path(output_dir, paste0(prefix, "_re_decomposition.csv"))
  )
  write_parquet_atomic(
    replays,
    file.path(output_dir, paste0(prefix, "_mdp_pitch_decisions.parquet"))
  )
  write_parquet_atomic(
    result$team_game,
    file.path(output_dir, paste0(prefix, "_mdp_team_game.parquet"))
  )
  write_csv_atomic(
    result$bootstrap,
    file.path(output_dir, paste0(prefix, "_mdp_bootstrap.csv"))
  )
  subtitle <- if (startsWith(prefix, "crossfit")) {
    "Five-fold held-out comparison with fixed calibrated p-hat"
  } else {
    "Full-season descriptive preview"
  }
  ggplot2::ggsave(
    file.path(output_dir, paste0(prefix, "_mdp_re_ladder.png")),
    plot_human_mdp_ladder(result$evaluation, subtitle = subtitle),
    width = 10, height = 6.5, dpi = 180
  )
  invisible(result)
}

if (stage %in% c("fast", "full")) {
  controls <- if (stage == "fast") {
    list(chains = 2L, iter = 1200L, warmup = 600L, adapt_delta = 0.95)
  } else {
    list(chains = 4L, iter = 4000L, warmup = 2000L, adapt_delta = 0.99)
  }
  fit_file <- file.path(fit_dir, paste0(stage, "_fit.rds"))
  message(
    "Fitting ", stage, " challenge-distance model on ",
    format(sum(train$challenged), big.mark = ","), " actual challenges"
  )
  fit <- fit_hierarchical_challenge_distance(
    train,
    chains = controls$chains,
    cores = controls$chains,
    iter = controls$iter,
    warmup = controls$warmup,
    seed = 42L,
    adapt_delta = controls$adapt_delta,
    file = fit_file,
    refresh = 100L
  )

  diagnostics <- challenge_distance_diagnostics(fit)
  summaries <- summarize_challenge_distance_fit(fit)
  validation <- validate_challenge_distance_fit(fit, heldout)
  write_csv_atomic(diagnostics, file.path(output_dir, paste0(stage, "_diagnostics.csv")))
  write_csv_atomic(
    summaries$population,
    file.path(output_dir, paste0(stage, "_role_distribution.csv"))
  )
  write_parquet_atomic(
    summaries$players,
    file.path(output_dir, paste0(stage, "_player_distributions.parquet"))
  )
  write_csv_atomic(
    summaries$ordering,
    file.path(output_dir, paste0(stage, "_role_ordering.csv"))
  )
  write_csv_atomic(
    validation$comparison,
    file.path(output_dir, paste0(stage, "_heldout_comparison.csv"))
  )
  write_csv_atomic(
    validation$histogram,
    file.path(output_dir, paste0(stage, "_heldout_histogram.csv"))
  )
  ggplot2::ggsave(
    file.path(output_dir, paste0(stage, "_heldout_distribution.png")),
    plot_challenge_distance_fit(fit, heldout),
    width = 11, height = 6.5, dpi = 180
  )

  print(diagnostics)
  print(summaries$population[, .(
    role,
    mean_margin = location_mean_inches_median,
    effective_sigma = marginal_sigma_inches_median,
    between_player_mean_sd = between_player_mean_sd_inches_median
  )])
  print(summaries$ordering)
  print(validation$comparison[, .(
    role, challenges, mean_margin, fitted_mean, sd_margin,
    fitted_marginal_sd, success_rate, mean_and_sd_within_half_inch
  )])
  message("Artifacts written under: ", output_dir)
}

if (stage == "quick_mdp") {
  fit <- load_clean_perception_fit()
  diagnostics <- challenge_distance_diagnostics(fit)
  if (!isTRUE(diagnostics$pass[[1L]])) stop("The clean perception fit failed diagnostics")
  sigma <- challenge_distance_sigma_table(fit)
  spatial_scale <- estimate_perception_spatial_scale(train)
  write_csv_atomic(sigma, file.path(output_dir, "quick_sigma.csv"))
  write_csv_atomic(spatial_scale, file.path(output_dir, "quick_spatial_scale.csv"))
  opportunities <- load_tracking_opportunities()
  message("Fitting the quick tracking and human-perception MDPs")
  comparison <- fit_and_replay_human_mdp(
    opportunities, opportunities, sigma, spatial_scale,
    require_game_separation = FALSE
  )
  result <- summarize_human_mdp_comparison(
    comparison$replays,
    bootstrap_reps = env_integer("CHALLENGE_DISTANCE_BOOTSTRAP_REPS", 500L),
    seed = 42L
  )
  write_mdp_outputs(result, comparison$replays, "quick")
  write_parquet_atomic(
    comparison$tracking_fit$state_values,
    file.path(output_dir, "quick_tracking_mdp_state_values.parquet")
  )
  write_parquet_atomic(
    comparison$human_fit$state_values,
    file.path(output_dir, "quick_human_mdp_state_values.parquet")
  )
  saveRDS(comparison$tracking_fit, file.path(fit_dir, "quick_tracking_mdp.rds"))
  saveRDS(comparison$human_fit, file.path(fit_dir, "quick_human_mdp.rds"))
  print(result$evaluation[order(-total_re)])
  print(result$decomposition)
  message("Quick preview complete. These are descriptive full-season results.")
}

if (stage == "crossfit_mdp") {
  opportunities <- load_tracking_opportunities()
  opportunities[, game_key__ := as.character(game_pk)]
  fold_file <- file.path(output_dir, "crossfit_fixed_phat_game_folds.csv")
  if (file.exists(fold_file)) {
    folds <- data.table::fread(fold_file, colClasses = c(game_key__ = "character"))
  } else {
    folds <- deterministic_perception_folds(
      opportunities$game_pk, folds = 5L, seed = 42L
    )
    write_csv_atomic(folds, fold_file)
  }
  force <- env_flag("CHALLENGE_DISTANCE_FORCE_REFIT", FALSE)
  fold_iter <- env_integer("CHALLENGE_DISTANCE_FOLD_ITER", 4000L)
  fold_warmup <- env_integer(
    "CHALLENGE_DISTANCE_FOLD_WARMUP", floor(fold_iter / 2)
  )
  replay_parts <- vector("list", 5L)
  diagnostic_parts <- vector("list", 5L)
  sigma_parts <- vector("list", 5L)
  scale_parts <- vector("list", 5L)

  for (fold_id in 1:5) {
    message("Preparing fixed-p_hat fold ", fold_id, " of 5")
    test_games <- folds[fold == fold_id, game_key__]
    train_choices <- choices[!game_key__ %in% test_games]
    test_choices <- choices[game_key__ %in% test_games]
    train_opportunities <- opportunities[!game_key__ %in% test_games]
    test_opportunities <- opportunities[game_key__ %in% test_games]
    if (length(intersect(
      unique(as.character(train_opportunities$game_pk)),
      unique(as.character(test_opportunities$game_pk))
    ))) stop("Cross-fitted perception/MDP game leakage")

    replay_file <- file.path(
      output_dir, paste0("crossfit_fixed_phat_fold", fold_id, "_replays.parquet")
    )
    diagnostic_file <- file.path(
      output_dir, paste0("crossfit_fixed_phat_fold", fold_id, "_diagnostics.csv")
    )
    sigma_file <- file.path(
      output_dir, paste0("crossfit_fixed_phat_fold", fold_id, "_sigma.csv")
    )
    scale_file <- file.path(
      output_dir, paste0("crossfit_fixed_phat_fold", fold_id, "_spatial_scale.csv")
    )
    if (file.exists(replay_file) && file.exists(diagnostic_file) &&
        file.exists(sigma_file) && file.exists(scale_file) && !force) {
      replay_parts[[fold_id]] <- data.table::as.data.table(
        arrow::read_parquet(replay_file)
      )
      diagnostic_parts[[fold_id]] <- data.table::fread(diagnostic_file)
      sigma_parts[[fold_id]] <- data.table::fread(sigma_file)
      scale_parts[[fold_id]] <- data.table::fread(scale_file)
      next
    }

    perception_fit_file <- file.path(
      fit_dir, paste0("crossfit_fixed_phat_perception_fold", fold_id, ".rds")
    )
    perception_fit <- fit_hierarchical_challenge_distance(
      train_choices,
      chains = 4L, cores = 4L, iter = fold_iter, warmup = fold_warmup,
      seed = 1000L + fold_id, adapt_delta = 0.99,
      file = perception_fit_file, refresh = 100L, force_refit = force
    )
    diagnostics <- challenge_distance_diagnostics(perception_fit)
    if (!isTRUE(diagnostics$pass[[1L]]) && fold_iter < 8000L) {
      message("Fold ", fold_id, " diagnostics failed; extending to 8,000 iterations")
      perception_fit <- fit_hierarchical_challenge_distance(
        train_choices,
        chains = 4L, cores = 4L, iter = 8000L, warmup = 4000L,
        seed = 2000L + fold_id, adapt_delta = 0.995,
        file = perception_fit_file, refresh = 100L, force_refit = TRUE
      )
      diagnostics <- challenge_distance_diagnostics(perception_fit)
    }
    diagnostics[, fold := fold_id]
    if (!isTRUE(diagnostics$pass[[1L]])) {
      write_csv_atomic(diagnostics, diagnostic_file)
      stop("Challenge-distance fold ", fold_id, " failed diagnostics")
    }
    if (length(intersect(
      perception_fit$training_games, unique(as.character(test_choices$game_pk))
    ))) stop("Perception fit contains held-out games")

    sigma <- challenge_distance_sigma_table(perception_fit)
    sigma[, fold := fold_id]
    spatial_scale <- estimate_perception_spatial_scale(train_choices)
    spatial_scale[, fold := fold_id]
    comparison <- fit_and_replay_human_mdp(
      train_opportunities, test_opportunities,
      sigma[, setdiff(names(sigma), "fold"), with = FALSE],
      spatial_scale[, setdiff(names(spatial_scale), "fold"), with = FALSE],
      require_game_separation = TRUE
    )
    comparison$replays[, fold := fold_id]
    write_parquet_atomic(comparison$replays, replay_file)
    write_csv_atomic(diagnostics, diagnostic_file)
    write_csv_atomic(sigma, sigma_file)
    write_csv_atomic(spatial_scale, scale_file)
    saveRDS(
      comparison$tracking_fit,
      file.path(fit_dir, paste0("crossfit_fixed_phat_tracking_mdp_fold", fold_id, ".rds"))
    )
    saveRDS(
      comparison$human_fit,
      file.path(fit_dir, paste0("crossfit_fixed_phat_human_mdp_fold", fold_id, ".rds"))
    )
    replay_parts[[fold_id]] <- comparison$replays
    diagnostic_parts[[fold_id]] <- diagnostics
    sigma_parts[[fold_id]] <- sigma
    scale_parts[[fold_id]] <- spatial_scale
  }

  replays <- data.table::rbindlist(replay_parts, fill = TRUE)
  if (anyDuplicated(replays[, .(policy, game_pk, team_id, pitch_order)])) {
    stop("Cross-fitted MDP output duplicates policy-pitch rows")
  }
  result <- summarize_human_mdp_comparison(
    replays,
    bootstrap_reps = env_integer("CHALLENGE_DISTANCE_BOOTSTRAP_REPS", 500L),
    seed = 42L
  )
  write_mdp_outputs(result, replays, "crossfit_fixed_phat")
  write_csv_atomic(
    data.table::rbindlist(diagnostic_parts, fill = TRUE),
    file.path(output_dir, "crossfit_fixed_phat_diagnostics.csv")
  )
  write_csv_atomic(
    data.table::rbindlist(sigma_parts, fill = TRUE),
    file.path(output_dir, "crossfit_fixed_phat_sigma.csv")
  )
  write_csv_atomic(
    data.table::rbindlist(scale_parts, fill = TRUE),
    file.path(output_dir, "crossfit_fixed_phat_spatial_scale.csv")
  )
  fold_audit <- data.table::rbindlist(lapply(1:5, function(fold_id) {
    heldout_games <- folds[fold == fold_id, game_key__]
    training_games <- folds[fold != fold_id, game_key__]
    data.table::data.table(
      fold = fold_id,
      training_games = length(training_games),
      heldout_games = length(heldout_games),
      overlapping_games = length(intersect(training_games, heldout_games))
    )
  }))
  write_csv_atomic(
    fold_audit,
    file.path(output_dir, "crossfit_fixed_phat_leakage_audit.csv")
  )
  coverage_file <- file.path(output_dir, "tracking_coverage.csv")
  minimum_coverage <- if (file.exists(coverage_file)) {
    min(data.table::fread(coverage_file)$coverage)
  } else {
    1
  }
  diagnostics_all <- data.table::rbindlist(diagnostic_parts, fill = TRUE)
  validation_checks <- data.table::data.table(
    check = c(
      "minimum_scoring_coverage", "duplicate_policy_pitch_rows",
      "negative_inventory_rows", "probabilities_in_range",
      "probabilities_move_toward_half", "fold_game_overlap",
      "failed_perception_diagnostics"
    ),
    value = c(
      minimum_coverage,
      anyDuplicated(replays[, .(policy, game_pk, team_id, pitch_order)]),
      sum(replays$inventory_before_policy < 0L | replays$inventory_after_policy < 0L),
      as.numeric(all(data.table::between(replays$p_human, 0, 1))),
      as.numeric(all(
        abs(replays$p_human - 0.5) <= abs(replays$p_tracking - 0.5) + 1e-12
      )),
      sum(fold_audit$overlapping_games),
      sum(!diagnostics_all$pass)
    ),
    pass = c(
      minimum_coverage >= 0.99,
      anyDuplicated(replays[, .(policy, game_pk, team_id, pitch_order)]) == 0L,
      !any(replays$inventory_before_policy < 0L | replays$inventory_after_policy < 0L),
      all(data.table::between(replays$p_human, 0, 1)),
      all(abs(replays$p_human - 0.5) <= abs(replays$p_tracking - 0.5) + 1e-12),
      all(fold_audit$overlapping_games == 0L),
      all(diagnostics_all$pass)
    )
  )
  write_csv_atomic(
    validation_checks,
    file.path(output_dir, "crossfit_fixed_phat_validation_checks.csv")
  )
  if (!all(validation_checks$pass)) stop("Cross-fitted validation checks failed")
  print(result$evaluation[order(-total_re)])
  print(result$decomposition)
  message(
    "Five-fold fixed-p_hat comparison complete. CAR refitting remains deferred."
  )
}
