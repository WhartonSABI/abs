# Hierarchical Gaussian perception research pipeline.
#
# Run from the project root, one stage at a time:
#   Rscript analysis/perception/run_gaussian_perception.R pilot_fast
#   Rscript analysis/perception/run_gaussian_perception.R pilot_overnight
#   Rscript analysis/perception/run_gaussian_perception.R pilot_sensitivity_fast
#   Rscript analysis/perception/run_gaussian_perception.R pilot_sensitivity_overnight
#   Rscript analysis/perception/run_gaussian_perception.R crossfit
#   Rscript analysis/perception/run_gaussian_perception.R mdp
#   Rscript analysis/perception/run_gaussian_perception.R full

project_root <- rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd())
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

arguments <- commandArgs(trailingOnly = TRUE)
stage <- if (length(arguments)) {
  tolower(arguments[[1L]])
} else {
  tolower(Sys.getenv("PERCEPTION_STAGE", unset = "pilot_fast"))
}
if (stage == "pilot") stage <- "pilot_fast"
valid_stages <- c(
  "pilot_fast", "pilot_overnight",
  "pilot_sensitivity_fast", "pilot_sensitivity_overnight",
  "crossfit", "mdp", "full"
)
if (!stage %in% valid_stages) {
  stop("Stage must be one of: ", paste(valid_stages, collapse = ", "))
}

output_dir <- file.path(project_root, "data", "processed", "gaussian_perception")
fit_dir <- file.path(output_dir, "fits")
dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)

env_integer <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, unset = as.character(default))))
  if (!is.finite(value) || value <= 0L) stop(name, " must be a positive integer")
  value
}

env_number <- function(name, default) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, unset = as.character(default))))
  if (!is.finite(value)) stop(name, " must be a finite number")
  value
}

env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = ifelse(default, "true", "false")))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be true or false")
  }
  value %in% c("true", "1", "yes")
}

load_perception_ledger <- function() {
  ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
  if (!"umpire_name" %in% names(ledger)) {
    umpires <- data.table::as.data.table(readRDS(file.path(
      project_root, "data", "processed", "umpires_2026.rds"
    )))
    ledger <- merge(ledger, umpires, by = "game_pk", all.x = TRUE, sort = FALSE)
  }
  stop_if_missing_columns(
    ledger,
    c(
      "game_pk", "pitch_order", "umpire_name", "fielder_2", "batter_id",
      "pitcher_id", "plate_x", "plate_z", "sz_bot", "sz_top",
      "edge_distance_inches"
    ),
    "Gaussian perception ledger"
  )
  ledger[, game_key__ := as.character(game_pk)]
  ledger[]
}

write_perception_validation <- function(validation, prefix) {
  write_csv_atomic(
    validation$gate,
    file.path(output_dir, paste0(prefix, "_flat55_gate.csv"))
  )
  write_csv_atomic(
    validation$curve,
    file.path(output_dir, paste0(prefix, "_flat55_curve.csv"))
  )
  write_csv_atomic(
    validation$role_summary,
    file.path(output_dir, paste0(prefix, "_role_validation.csv"))
  )
  write_csv_atomic(
    validation$location_summary,
    file.path(output_dir, paste0(prefix, "_location_validation.csv"))
  )
  if (nrow(validation$timing_summary)) {
    write_csv_atomic(
      validation$timing_summary,
      file.path(output_dir, paste0(prefix, "_timing_validation.csv"))
    )
  }
  write_csv_atomic(
    validation$bootstrap_slopes,
    file.path(output_dir, paste0(prefix, "_flat55_bootstrap_slopes.csv"))
  )
  ggplot2::ggsave(
    file.path(output_dir, paste0(prefix, "_flat55_curve.png")),
    plot_flat55_validation(validation),
    width = 10, height = 6, dpi = 180
  )
  invisible(validation)
}

write_perception_summaries <- function(fit, prefix) {
  summaries <- summarize_perception_fit(fit)
  write_csv_atomic(
    summaries$population,
    file.path(output_dir, paste0(prefix, "_population_sigma.csv"))
  )
  write_parquet_atomic(
    summaries$players,
    file.path(output_dir, paste0(prefix, "_player_sigma.parquet"))
  )
  write_csv_atomic(
    summaries$ordering,
    file.path(output_dir, paste0(prefix, "_role_ordering.csv"))
  )
  write_csv_atomic(
    summaries$spread,
    file.path(output_dir, paste0(prefix, "_population_spread.csv"))
  )
  write_csv_atomic(
    summaries$sensitivity,
    file.path(output_dir, paste0(prefix, "_decision_sensitivity.csv"))
  )
  write_csv_atomic(
    summaries$identifiability,
    file.path(output_dir, paste0(prefix, "_identifiability.csv"))
  )
  invisible(summaries)
}

ledger <- load_perception_ledger()
re_model <- targets::tar_read(re_model)
score_draws <- env_integer("PERCEPTION_SCORE_DRAWS", 500L)
bootstrap_reps <- env_integer("PERCEPTION_BOOTSTRAP_REPS", 500L)
force_refit <- env_flag("PERCEPTION_FORCE_REFIT", FALSE)

full_probability_file <- file.path(output_dir, "sharp_full_probabilities.parquet")
attach_probability_file <- function(base_ledger, path) {
  probability <- data.table::as.data.table(arrow::read_parquet(path))
  if (all(c("game_pk", "pitch_order", "p_hat") %in% names(probability)) &&
      ncol(probability) == 3L) {
    base <- data.table::copy(data.table::as.data.table(base_ledger))
    if ("p_hat" %in% names(base)) base[, p_hat := NULL]
    return(merge(
      base, probability,
      by = c("game_pk", "pitch_order"), all.x = TRUE, sort = FALSE
    ))
  }
  # Backward-compatible with an interrupted earlier run that wrote the ledger.
  probability
}

write_probability_file <- function(scored_ledger, path) {
  write_parquet_atomic(
    data.table::as.data.table(scored_ledger)[, .(game_pk, pitch_order, p_hat)],
    path
  )
}

compact_policy_scores <- function(scored_choices) {
  x <- data.table::as.data.table(scored_choices)
  x <- x[
    (initial_call == "called_strike" & role == "batter") |
      (initial_call == "ball" & role == "catcher")
  ]
  x[, .(
    game_pk, pitch_order, initial_call, role, player_id,
    p_hat, p_perceived
  )]
}

compact_validation_scores <- function(scored_choices) {
  data.table::as.data.table(scored_choices)[, .(
    game_pk, pitch_order, inning, role, challenged, actual_wrong,
    edge_distance_inches, inventory_before,
    challenge_probability, challenge_probability_sigma0
  )]
}

load_or_score_full_probabilities <- function() {
  if (file.exists(full_probability_file)) {
    return(attach_probability_file(ledger, full_probability_file))
  }
  message("Scoring the existing sharp ball/strike fits")
  ball_fit <- readRDS(file.path(
    project_root, "data", "processed", "fit_car_balls_final.rds"
  ))
  strike_fit <- readRDS(file.path(
    project_root, "data", "processed", "fit_car_strikes_final.rds"
  ))
  scored <- score_saved_sharp_car_pair(
    ball_fit, strike_fit, ledger, ndraws = score_draws, seed = 42L
  )
  write_probability_file(scored, full_probability_file)
  scored
}

run_perception_pilot <- function(
  kind = c("fast", "overnight"),
  model_variant = c("fixed_slope", "role_sensitivity")
) {
  kind <- match.arg(kind)
  model_variant <- match.arg(model_variant)
  prefix <- if (model_variant == "fixed_slope") {
    paste0("pilot_", kind)
  } else {
    paste0("pilot_sensitivity_", kind)
  }
  scored_ledger <- load_or_score_full_probabilities()
  split_file <- file.path(output_dir, "pilot_game_split.csv")
  if (file.exists(split_file)) {
    split <- data.table::fread(split_file, colClasses = c(game_pk = "character"))
  } else {
    split <- deterministic_game_split(scored_ledger, train_fraction = 0.8, seed = 42L)
    write_csv_atomic(split, split_file)
  }
  scored_ledger[, game_key__ := as.character(game_pk)]
  train_games <- split[split == "train", game_pk]
  validation_games <- split[split == "validation", game_pk]
  if (length(intersect(train_games, validation_games))) stop("Pilot game leakage")
  train_choices <- prepare_perception_choices(
    scored_ledger[game_key__ %in% train_games], re_model, p_col = "p_hat"
  )
  validation_choices <- prepare_perception_choices(
    scored_ledger[game_key__ %in% validation_games], re_model, p_col = "p_hat"
  )
  scale <- estimate_perception_spatial_scale(train_choices)
  write_csv_atomic(scale, file.path(output_dir, paste0(prefix, "_spatial_scale.csv")))
  fitting_choices <- train_choices
  if (kind == "fast") {
    fitting_choices <- subsample_perception_pilot(
      train_choices,
      pass_fraction = env_number("PERCEPTION_FAST_PASS_FRACTION", 0.05),
      min_passes_per_player = env_integer("PERCEPTION_FAST_MIN_PASSES", 10L),
      seed = 42L
    )
    manifest <- attr(fitting_choices, "sampling_manifest")
    write_csv_atomic(
      manifest,
      file.path(output_dir, paste0(prefix, "_sampling_manifest.csv"))
    )
    message(
      "Fast pilot retained ", format(nrow(fitting_choices), big.mark = ","),
      " of ", format(nrow(train_choices), big.mark = ","),
      " training decisions; all challenges were retained."
    )
  }
  if (kind == "fast") {
    chains <- env_integer("PERCEPTION_FAST_CHAINS", 2L)
    cores <- env_integer("PERCEPTION_FAST_CORES", 2L)
    iter <- env_integer("PERCEPTION_FAST_ITER", 1000L)
    warmup <- env_integer("PERCEPTION_FAST_WARMUP", 500L)
    adapt_delta <- env_number("PERCEPTION_FAST_ADAPT_DELTA", 0.90)
    max_treedepth <- env_integer("PERCEPTION_FAST_MAX_TREEDEPTH", 10L)
    refresh <- env_integer("PERCEPTION_FAST_REFRESH", 25L)
  } else {
    chains <- env_integer("PERCEPTION_OVERNIGHT_CHAINS", 2L)
    cores <- env_integer("PERCEPTION_OVERNIGHT_CORES", 2L)
    iter <- env_integer("PERCEPTION_OVERNIGHT_ITER", 2000L)
    warmup <- env_integer("PERCEPTION_OVERNIGHT_WARMUP", 1000L)
    adapt_delta <- env_number("PERCEPTION_OVERNIGHT_ADAPT_DELTA", 0.95)
    max_treedepth <- env_integer("PERCEPTION_OVERNIGHT_MAX_TREEDEPTH", 12L)
    refresh <- env_integer("PERCEPTION_OVERNIGHT_REFRESH", 100L)
  }
  if (model_variant == "role_sensitivity") {
    adapt_delta <- env_number(
      "PERCEPTION_SENSITIVITY_ADAPT_DELTA", max(adapt_delta, 0.97)
    )
    max_treedepth <- env_integer(
      "PERCEPTION_SENSITIVITY_MAX_TREEDEPTH", max(max_treedepth, 13L)
    )
  }
  pilot_fit <- fit_hierarchical_perception(
    fitting_choices,
    spatial_scale = scale,
    chains = chains,
    cores = cores,
    iter = iter,
    warmup = warmup,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    model_variant = model_variant,
    refresh = refresh,
    seed = 42L,
    file = file.path(fit_dir, paste0(prefix, "_perception_fit.rds")),
    force_refit = force_refit
  )
  # Report full training opportunity counts even when the quick fit used a
  # corrected case-control sample.
  full_player_counts <- train_choices[, .(
    opportunities = .N,
    challenges = sum(challenged)
  ), by = .(player_role_id, role, player_id)]
  full_player_counts[, `:=`(
    player_role_id = as.character(player_role_id),
    role = as.character(role),
    player_id = as.character(player_id)
  )]
  pilot_fit$player_table <- data.table::as.data.table(pilot_fit$player_table)
  pilot_fit$player_table[full_player_counts, on = c(
    "player_role_id", "role", "player_id"
  ), `:=`(
    opportunities = i.opportunities,
    challenges = i.challenges
  )]
  saveRDS(pilot_fit, file.path(fit_dir, paste0(prefix, "_perception_fit.rds")))
  diagnostics <- perception_fit_diagnostics(pilot_fit)
  write_csv_atomic(
    diagnostics, file.path(output_dir, paste0(prefix, "_diagnostics.csv"))
  )
  summaries <- write_perception_summaries(pilot_fit, prefix)
  scored_validation <- score_perception(
    pilot_fit, validation_choices, ndraws = score_draws, seed = 42L
  )
  write_parquet_atomic(
    scored_validation,
    file.path(output_dir, paste0(prefix, "_heldout_choices.parquet"))
  )
  validation <- validate_flat55(
    scored_validation,
    bootstrap_reps = bootstrap_reps,
    seed = 42L
  )
  write_perception_validation(validation, prefix)
  if (model_variant == "role_sensitivity") {
    max_correlation <- max(
      abs(summaries$identifiability$sigma_beta_posterior_correlation),
      na.rm = TRUE
    )
    sensitivity_gate <- data.table::data.table(
      predictive_gate_pass = validation$gate$pass[[1L]],
      max_absolute_sigma_beta_correlation = max_correlation,
      identifiability_tolerance = 0.8,
      identifiability_pass = all(summaries$identifiability$interpretable),
      pass = validation$gate$pass[[1L]] &&
        all(summaries$identifiability$interpretable)
    )
    write_csv_atomic(
      sensitivity_gate,
      file.path(output_dir, paste0(prefix, "_candidate_gate.csv"))
    )
    print(sensitivity_gate)
  }
  print(diagnostics)
  print(validation$gate)
  if (isTRUE(validation$gate$pass[[1L]])) {
    if (model_variant == "role_sensitivity") {
      message(
        "Sensitivity pilot passed the prediction gate. Inspect the saved ",
        "sigma/beta identifiability table before interpreting eyesight."
      )
    } else if (kind == "overnight") {
      message("Overnight pilot passed. The crossfit stage is now unlocked.")
    } else {
      message(
        "Fast pilot passed its exploratory gate. Run pilot_overnight before crossfit."
      )
    }
  } else {
    message(
      tools::toTitleCase(kind),
      " pilot did not pass the flat-55 gate. Inspect the saved curve and gate; ",
      "do not use the human-eyes MDP for a performance claim."
    )
  }
  invisible(list(fit = pilot_fit, validation = validation))
}

if (stage == "pilot_fast") {
  run_perception_pilot("fast")
}

if (stage == "pilot_overnight") {
  run_perception_pilot("overnight")
}

if (stage == "pilot_sensitivity_fast") {
  run_perception_pilot("fast", model_variant = "role_sensitivity")
}

if (stage == "pilot_sensitivity_overnight") {
  run_perception_pilot("overnight", model_variant = "role_sensitivity")
}

if (stage == "crossfit") {
  pilot_gate_file <- file.path(output_dir, "pilot_overnight_flat55_gate.csv")
  if (!file.exists(pilot_gate_file)) stop("Run the overnight pilot stage first")
  pilot_gate <- data.table::fread(pilot_gate_file)
  if (!isTRUE(pilot_gate$pass[[1L]])) {
    stop("The pilot failed its flat-55 gate; full cross-fitting is intentionally locked")
  }
  fold_file <- file.path(output_dir, "crossfit_game_folds.csv")
  if (file.exists(fold_file)) {
    fold_assignment <- data.table::fread(
      fold_file, colClasses = c(game_key__ = "character")
    )
  } else {
    fold_assignment <- deterministic_perception_folds(
      ledger$game_pk, folds = 5L, seed = 42L
    )
    write_csv_atomic(fold_assignment, fold_file)
  }
  sharp_chains <- env_integer("PERCEPTION_SHARP_CHAINS", 4L)
  sharp_cores <- env_integer("PERCEPTION_SHARP_CORES", 4L)
  sharp_iter <- env_integer("PERCEPTION_SHARP_ITER", 8000L)
  sharp_warmup <- env_integer("PERCEPTION_SHARP_WARMUP", 4000L)
  perception_chains <- env_integer("PERCEPTION_FULL_CHAINS", 4L)
  perception_cores <- env_integer("PERCEPTION_FULL_CORES", 4L)
  perception_iter <- env_integer("PERCEPTION_FULL_ITER", 4000L)
  perception_warmup <- env_integer("PERCEPTION_FULL_WARMUP", 2000L)
  heldout_parts <- vector("list", 5L)

  for (fold_id in 1:5) {
    message("Preparing full perception fold ", fold_id, " of 5")
    heldout_games <- fold_assignment[fold == fold_id, game_key__]
    train_ledger <- ledger[!game_key__ %in% heldout_games]
    test_ledger <- ledger[game_key__ %in% heldout_games]
    if (length(intersect(train_ledger$game_key__, test_ledger$game_key__))) {
      stop("Sharp-probability fold leakage")
    }
    ball_fit_file <- file.path(fit_dir, paste0("sharp_ball_fold", fold_id, ".rds"))
    strike_fit_file <- file.path(fit_dir, paste0("sharp_strike_fold", fold_id, ".rds"))
    if (file.exists(ball_fit_file) && !force_refit) {
      ball_fit <- readRDS(ball_fit_file)
    } else {
      ball_fit <- fit_sharp_car_model(
        prepare_sharp_car_input(train_ledger, "ball"),
        chains = sharp_chains, cores = sharp_cores,
        iter = sharp_iter, warmup = sharp_warmup,
        seed = 100L + fold_id, file = ball_fit_file,
        force_refit = force_refit
      )
    }
    if (file.exists(strike_fit_file) && !force_refit) {
      strike_fit <- readRDS(strike_fit_file)
    } else {
      strike_fit <- fit_sharp_car_model(
        prepare_sharp_car_input(train_ledger, "called_strike"),
        chains = sharp_chains, cores = sharp_cores,
        iter = sharp_iter, warmup = sharp_warmup,
        seed = 200L + fold_id, file = strike_fit_file,
        force_refit = force_refit
      )
    }
    sharp_diagnostics <- data.table::rbindlist(list(
      cbind(model = "ball", joint_fit_diagnostics(ball_fit)),
      cbind(model = "called_strike", joint_fit_diagnostics(strike_fit))
    ), fill = TRUE)
    write_csv_atomic(
      sharp_diagnostics,
      file.path(output_dir, paste0("fold", fold_id, "_sharp_diagnostics.csv"))
    )
    if (!all(sharp_diagnostics$pass)) {
      stop("Sharp CAR fold ", fold_id, " failed posterior diagnostics")
    }
    train_probability_file <- file.path(
      output_dir, paste0("fold", fold_id, "_train_sharp_probabilities.parquet")
    )
    test_probability_file <- file.path(
      output_dir, paste0("fold", fold_id, "_test_sharp_probabilities.parquet")
    )
    train_scored_ledger <- if (file.exists(train_probability_file)) {
      attach_probability_file(train_ledger, train_probability_file)
    } else {
      value <- score_sharp_car_pair(
        ball_fit, strike_fit, train_ledger, ndraws = score_draws,
        seed = 300L + fold_id
      )
      write_probability_file(value, train_probability_file)
      value
    }
    test_scored_ledger <- if (file.exists(test_probability_file)) {
      attach_probability_file(test_ledger, test_probability_file)
    } else {
      value <- score_sharp_car_pair(
        ball_fit, strike_fit, test_ledger, ndraws = score_draws,
        seed = 400L + fold_id
      )
      write_probability_file(value, test_probability_file)
      value
    }
    train_choices <- prepare_perception_choices(train_scored_ledger, re_model)
    test_choices <- prepare_perception_choices(test_scored_ledger, re_model)
    perception_fit_file <- file.path(
      fit_dir, paste0("perception_fold", fold_id, ".rds")
    )
    perception_fit <- fit_hierarchical_perception(
      train_choices,
      chains = perception_chains, cores = perception_cores,
      iter = perception_iter, warmup = perception_warmup,
      seed = 500L + fold_id,
      file = perception_fit_file,
      force_refit = force_refit
    )
    if (length(intersect(
      perception_fit$training_games,
      unique(as.character(test_choices$game_pk))
    ))) {
      stop("Perception fold ", fold_id, " contains held-out games in training")
    }
    diagnostics <- perception_fit_diagnostics(perception_fit)
    write_csv_atomic(
      diagnostics,
      file.path(output_dir, paste0("fold", fold_id, "_perception_diagnostics.csv"))
    )
    if (!isTRUE(diagnostics$pass[[1L]])) {
      stop("Perception fold ", fold_id, " failed posterior diagnostics")
    }
    train_choice_file <- file.path(
      output_dir, paste0("fold", fold_id, "_train_perception_choices.parquet")
    )
    test_choice_file <- file.path(
      output_dir, paste0("fold", fold_id, "_test_perception_choices.parquet")
    )
    train_policy_choices <- prepare_perception_choices(
      train_scored_ledger, re_model,
      available_only = FALSE, exclude_preempted = FALSE
    )
    test_policy_choices <- prepare_perception_choices(
      test_scored_ledger, re_model,
      available_only = FALSE, exclude_preempted = FALSE
    )
    train_scored <- score_perception(
      perception_fit, train_policy_choices, ndraws = score_draws,
      seed = 600L + fold_id
    )
    test_scored <- score_perception(
      perception_fit, test_policy_choices, ndraws = score_draws,
      seed = 700L + fold_id
    )
    test_validation_scored <- score_perception(
      perception_fit, test_choices, ndraws = score_draws,
      seed = 800L + fold_id
    )
    train_scored[, perception_fold := fold_id]
    test_scored[, perception_fold := fold_id]
    write_parquet_atomic(compact_policy_scores(train_scored), train_choice_file)
    write_parquet_atomic(compact_policy_scores(test_scored), test_choice_file)
    heldout_parts[[fold_id]] <- compact_validation_scores(test_validation_scored)
    rm(
      ball_fit, strike_fit, perception_fit, train_scored_ledger,
      test_scored_ledger, train_scored, test_scored, test_validation_scored,
      train_policy_choices, test_policy_choices
    )
    invisible(gc())
  }
  heldout <- data.table::rbindlist(heldout_parts, fill = TRUE)
  write_parquet_atomic(
    heldout,
    file.path(output_dir, "crossfit_heldout_perception_choices.parquet")
  )
  validation <- validate_flat55(
    heldout, bootstrap_reps = bootstrap_reps, seed = 42L
  )
  write_perception_validation(validation, "crossfit")
  print(validation$gate)
  if (isTRUE(validation$gate$pass[[1L]])) {
    message("Full held-out validation passed. The mdp and full stages are unlocked.")
  } else {
    message("Full held-out validation failed. Human-eyes MDP remains locked.")
  }
}

if (stage == "mdp") {
  gate_file <- file.path(output_dir, "crossfit_flat55_gate.csv")
  fold_file <- file.path(output_dir, "crossfit_game_folds.csv")
  if (!file.exists(gate_file) || !file.exists(fold_file)) {
    stop("Run the crossfit stage first")
  }
  gate <- data.table::fread(gate_file)
  if (!isTRUE(gate$pass[[1L]])) {
    stop("The cross-fitted perception model failed the flat-55 gate; MDP is locked")
  }
  fold_assignment <- data.table::fread(
    fold_file, colClasses = c(game_key__ = "character")
  )
  fold_objects <- lapply(1:5, function(fold_id) {
    heldout_games <- fold_assignment[fold == fold_id, game_key__]
    list(
      train_ledger = ledger[!game_key__ %in% heldout_games],
      test_ledger = ledger[game_key__ %in% heldout_games],
      train_scored = data.table::as.data.table(arrow::read_parquet(file.path(
        output_dir, paste0("fold", fold_id, "_train_perception_choices.parquet")
      ))),
      test_scored = data.table::as.data.table(arrow::read_parquet(file.path(
        output_dir, paste0("fold", fold_id, "_test_perception_choices.parquet")
      )))
    )
  })
  heldout <- data.table::as.data.table(arrow::read_parquet(file.path(
    output_dir, "crossfit_heldout_perception_choices.parquet"
  )))
  validation <- validate_flat55(
    heldout, bootstrap_reps = bootstrap_reps, seed = 42L
  )
  result <- crossfit_perception_mdp(
    fold_objects, re_model, validation = validation,
    bootstrap_reps = bootstrap_reps, seed = 42L
  )
  if (result$status != "complete") stop("Perception MDP did not complete")
  write_csv_atomic(result$evaluation, file.path(output_dir, "mdp_evaluation.csv"))
  write_csv_atomic(
    result$role_evaluation,
    file.path(output_dir, "mdp_role_evaluation.csv")
  )
  write_csv_atomic(result$decomposition, file.path(output_dir, "re_decomposition.csv"))
  write_parquet_atomic(result$replays, file.path(output_dir, "mdp_pitch_decisions.parquet"))
  write_parquet_atomic(result$team_game, file.path(output_dir, "mdp_team_game.parquet"))
  write_csv_atomic(result$bootstrap, file.path(output_dir, "mdp_bootstrap.csv"))
  print(result$evaluation)
  print(result$decomposition)
}

if (stage == "full") {
  gate_file <- file.path(output_dir, "crossfit_flat55_gate.csv")
  if (!file.exists(gate_file)) stop("Run the crossfit stage first")
  gate <- data.table::fread(gate_file)
  if (!isTRUE(gate$pass[[1L]])) {
    stop("The cross-fitted perception model failed its validation gate")
  }
  scored_ledger <- load_or_score_full_probabilities()
  choices <- prepare_perception_choices(scored_ledger, re_model)
  fit <- fit_hierarchical_perception(
    choices,
    chains = env_integer("PERCEPTION_FULL_CHAINS", 4L),
    cores = env_integer("PERCEPTION_FULL_CORES", 4L),
    iter = env_integer("PERCEPTION_FULL_ITER", 4000L),
    warmup = env_integer("PERCEPTION_FULL_WARMUP", 2000L),
    seed = 900L,
    file = file.path(fit_dir, "perception_full.rds"),
    force_refit = force_refit
  )
  diagnostics <- perception_fit_diagnostics(fit)
  write_csv_atomic(diagnostics, file.path(output_dir, "full_diagnostics.csv"))
  if (!isTRUE(diagnostics$pass[[1L]])) {
    stop("Full perception fit failed diagnostics; extend sampling before using it")
  }
  write_perception_summaries(fit, "full")
  policy_choices <- prepare_perception_choices(
    scored_ledger, re_model,
    available_only = FALSE, exclude_preempted = FALSE
  )
  scored_choices <- score_perception(
    fit, policy_choices, ndraws = score_draws, seed = 900L
  )
  write_parquet_atomic(
    scored_choices,
    file.path(output_dir, "full_perception_choices.parquet")
  )
  policy_ledger <- merge_perception_probabilities(scored_ledger, scored_choices)
  opportunities <- prepare_mdp_opportunities(
    policy_ledger, re_model, p_col = "p_perceived"
  )
  mdp_fit <- fit_challenge_mdp(
    opportunities, prior_n = 30, tol = 1e-10, max_iter = 10000L
  )
  decisions <- replay_challenge_policy(opportunities, mdp_fit, "mdp")
  saveRDS(mdp_fit, file.path(output_dir, "full_human_eyes_mdp_fit.rds"))
  write_parquet_atomic(
    mdp_fit$state_values,
    file.path(output_dir, "full_human_eyes_mdp_state_values.parquet")
  )
  write_parquet_atomic(
    decisions,
    file.path(output_dir, "full_human_eyes_mdp_decisions.parquet")
  )
  write_csv_atomic(
    summarize_joint_mdp_replay(decisions, model = "human_eyes_full_fit"),
    file.path(output_dir, "full_human_eyes_mdp_summary.csv")
  )
  print(diagnostics)
  message("Final perception artifacts written under: ", output_dir)
}
