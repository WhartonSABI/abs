project_root <- rprojroot::find_root(
  rprojroot::has_file("_targets.R"), path = getwd()
)
source(file.path(project_root, "config", "project.R"))
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

joint_environment_number <- function(name, default, lower = -Inf) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, unset = default)))
  if (length(value) != 1L || !is.finite(value) || value <= lower) {
    stop(name, " must be one finite number greater than ", lower)
  }
  value
}

joint_environment_integer <- function(name, default, minimum = 1L) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, unset = default)))
  if (length(value) != 1L || is.na(value) || value < minimum) {
    stop(name, " must be one integer at least ", minimum)
  }
  value
}

joint_environment_flag <- function(name, default = TRUE) {
  raw <- tolower(Sys.getenv(name, unset = if (default) "true" else "false"))
  if (!raw %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be true or false")
  }
  raw %in% c("true", "1", "yes")
}

offense_sigma <- joint_environment_number(
  "ABS_1D_JOINT_OFFENSE_SIGMA", 2.947823, 0
)
defense_sigma <- joint_environment_number(
  "ABS_1D_JOINT_DEFENSE_SIGMA", 1.827574, 0
)
folds <- joint_environment_integer("ABS_1D_JOINT_FOLDS", 5L, 2L)
seed <- joint_environment_integer("ABS_1D_JOINT_SEED", 20260826L)
bootstrap_reps <- joint_environment_integer(
  "ABS_1D_JOINT_BOOTSTRAP_REPS", 2000L, 0L
)
offense_components <- joint_environment_integer(
  "ABS_1D_JOINT_OFFENSE_COMPONENTS", 3L
)
defense_components <- joint_environment_integer(
  "ABS_1D_JOINT_DEFENSE_COMPONENTS", 6L
)
output_tag <- Sys.getenv("ABS_1D_JOINT_OUTPUT_TAG", unset = "primary")
output_tag <- gsub("[^A-Za-z0-9_.-]+", "_", output_tag)
if (!nzchar(output_tag)) output_tag <- "primary"
nested_prior_selection <- joint_environment_flag(
  "ABS_1D_JOINT_NESTED_PRIOR_SELECTION", TRUE
)
refresh_core_targets <- joint_environment_flag(
  "ABS_1D_JOINT_REFRESH_CORE_TARGETS", TRUE
)

if (refresh_core_targets) {
  targets::tar_make(
    names = c(
      pitch_ledger, re_model, remaining_opportunities,
      continuous_decision_features
    ),
    callr_function = NULL,
    reporter = "silent"
  )
}

target_metadata <- data.table::as.data.table(targets::tar_meta(
  names = c(
    "pitch_ledger", "re_model", "remaining_opportunities",
    "continuous_decision_features"
  ),
  fields = c("name", "data", "command", "depend", "time", "size", "bytes")
))
if (nrow(target_metadata) != 4L || anyNA(target_metadata$data)) {
  stop("Required target metadata and hashes are unavailable")
}
pitch_ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
re_model <- targets::tar_read(re_model)
remaining_opportunities <- data.table::as.data.table(
  targets::tar_read(remaining_opportunities)
)
offense_decision_rows <- data.table::as.data.table(
  targets::tar_read(continuous_decision_features)
)
opportunities <- prepare_joint_challenge_signal_mdp_opportunities_1d(
  pitch_ledger, re_model
)
fold_assignment <- continuous_game_folds(
  opportunities$game_pk, folds = folds, seed = seed
)
defense_decision_rows <- build_defense_challenge_discrimination_1d_rows(
  pitch_ledger, remaining_opportunities, re_model
)
width_crossfit <- crossfit_joint_common_width_1d(
  offense_decision_rows,
  defense_decision_rows,
  fold_assignment = fold_assignment,
  categorical_context = c("count_state", "pitch_family", "matchup"),
  numeric_context = c("stake_G", "inning", "score_margin"),
  margin_limit_inches = 3,
  confidence_level = 0.95,
  nthreads = 1L,
  keep_models = FALSE,
  progress = TRUE
)

select_joint_fold_priors <- function(
  opportunities, fold_assignment, folds, seed
) {
  opportunity_rows <- data.table::copy(opportunities)
  opportunity_rows[, game_pk := as.character(game_pk)]
  assigned <- merge(
    opportunity_rows,
    data.table::copy(fold_assignment)[, .(
      game_pk = as.character(game_pk), outer_fold = as.integer(fold)
    )],
    by = "game_pk", all.x = TRUE, sort = FALSE
  )
  if (anyNA(assigned$outer_fold)) stop("Outer prior folds are incomplete")
  fit_list <- metric_list <- vector("list", folds)
  for (outer_fold in seq_len(folds)) {
    fold_index <- outer_fold
    training_games <- sort(unique(assigned[
      outer_fold != fold_index, as.character(game_pk)
    ]))
    inner <- continuous_game_folds(
      training_games, folds = 4L, seed = seed + 1000L + outer_fold
    )
    validation_games <- sort(as.character(inner[fold == 1L, game_pk]))
    fit_games <- setdiff(training_games, validation_games)
    selections <- lapply(
      joint_challenge_signal_mdp_roles_1d(),
      function(role) {
        input <- joint_challenge_margin_prior_input_1d(assigned, role)
        selection <- select_challenge_margin_prior_1d(
          input,
          fit_games = fit_games,
          validation_games = validation_games,
          components = c(1L, 3L, 6L),
          refit_selected = TRUE,
          fold_id = paste0("joint_outer_", outer_fold, "_", role),
          global_draw_id = 1L
        )
        selection$fit$joint_role <- role
        selection$fit$conditioning <- c(
          "taken", if (role == "offense") "called_strike" else "called_ball"
        )
        selection$fit$success_event <- if (role == "offense") {
          "true signed ABS distance > 0 (called strike should be ball)"
        } else {
          "true signed ABS distance <= 0 (called ball should be strike)"
        }
        selection
      }
    )
    names(selections) <- joint_challenge_signal_mdp_roles_1d()
    fit_list[[outer_fold]] <- lapply(selections, `[[`, "fit")
    metric_list[[outer_fold]] <- data.table::rbindlist(lapply(
      names(selections),
      function(role) {
        role_value <- role
        out <- data.table::copy(selections[[role]]$candidate_metrics)
        out[, `:=`(outer_fold = fold_index, role = role_value)]
        out
      }
    ), fill = TRUE)
  }
  list(
    fits = fit_list,
    metrics = data.table::rbindlist(metric_list, fill = TRUE)
  )
}

role_sigma <- c(offense = offense_sigma, defense = defense_sigma)
role_components <- c(
  offense = offense_components,
  defense = defense_components
)
prior_selection <- if (nested_prior_selection) {
  select_joint_fold_priors(
    opportunities, fold_assignment, folds = folds, seed = seed
  )
} else {
  list(fits = NULL, metrics = data.table::data.table())
}
result <- crossfit_joint_challenge_signal_mdp_1d(
  opportunities,
  perception_sigma = role_sigma,
  fold_assignment = fold_assignment,
  folds = folds,
  seed = seed,
  prior_components = role_components,
  fold_prior_fits = prior_selection$fits,
  fold_perception_sigma = width_crossfit$fold_perception_sigma,
  prior_n = 30,
  tol = 1e-7,
  max_iter = 10000L,
  lookup_grid_step = 0.005,
  bootstrap_reps = bootstrap_reps,
  progress = TRUE
)

state_values <- data.table::rbindlist(lapply(seq_len(folds), function(fold) {
  value <- data.table::copy(result$fold_fits[[fold]]$state_values)
  value[, fold := fold]
  value
}), fill = TRUE)

prior_parameters <- data.table::rbindlist(lapply(seq_len(folds), function(fold) {
  data.table::rbindlist(lapply(names(result$prior_fits[[fold]]), function(role) {
    fit <- result$prior_fits[[fold]][[role]]
    data.table::data.table(
      fold = fold,
      role = role,
      component = seq_len(fit$components),
      weight = fit$weight,
      mean_inches = fit$mean,
      sd_inches = fit$sd,
      training_rows = fit$training_rows,
      converged = fit$converged,
      iterations = fit$iterations,
      training_data_fingerprint = fit$training_fingerprint
    )
  }))
}))

prior_context_weights <- data.table::rbindlist(lapply(
  seq_len(folds),
  function(fold) {
    data.table::rbindlist(lapply(
      names(result$prior_fits[[fold]]),
      function(role) {
        fit <- result$prior_fits[[fold]][[role]]
        value <- data.table::copy(fit$context_weight_table)
        if (!nrow(value)) return(NULL)
        value[, `:=`(
          fold = fold,
          role = role,
          context_prior_strength = fit$context_prior_strength,
          context_weight_method = fit$context_weight_method,
          prior_fold_id = fit$fold_id,
          global_draw_id = fit$global_draw_id,
          training_fingerprint = fit$training_fingerprint
        )]
        value
      }
    ), fill = TRUE)
  }
), fill = TRUE)

lookup_diagnostics <- data.table::rbindlist(lapply(
  seq_len(folds),
  function(fold) {
    data.table::rbindlist(lapply(
      names(result$fold_fits[[fold]]$lookups),
      function(role) {
        lookup <- result$fold_fits[[fold]]$lookups[[role]]
        data.table::data.table(
          fold = fold,
          role = role,
          contexts = length(lookup$contexts),
          signal_min_inches = lookup$signal_range[[1L]],
          signal_max_inches = lookup$signal_range[[2L]],
          grid_step_inches = lookup$grid_step,
          tail_standard_deviations = lookup$tail_standard_deviations,
          maximum_prior_mass_closure_error =
            lookup$maximum_prior_mass_closure_error
        )
      }
    ))
  }
))

selected_prior_components <- data.table::rbindlist(lapply(
  seq_len(folds),
  function(fold) {
    data.table::rbindlist(lapply(
      names(result$prior_fits[[fold]]),
      function(role) data.table::data.table(
        fold = fold,
        role = role,
        selected_components = result$prior_fits[[fold]][[role]]$components
      )
    ))
  }
))

calibration <- result$replay[, .(
  rows = .N,
  model_attempts = sum(policy_challenge_probability),
  truth_successes = sum(expected_policy_success),
  truth_selected_success_rate = if (sum(policy_challenge_probability) > 0) {
    sum(expected_policy_success) / sum(policy_challenge_probability)
  } else {
    NA_real_
  },
  prior_attempts = sum(prior_policy_challenge_probability),
  prior_successes = sum(prior_policy_success_probability),
  prior_selected_success_rate = if (
    sum(prior_policy_challenge_probability) > 0
  ) {
    sum(prior_policy_success_probability) /
      sum(prior_policy_challenge_probability)
  } else {
    NA_real_
  }
), by = .(fold, role)]
calibration[, selected_success_calibration_error :=
  truth_selected_success_rate - prior_selected_success_rate]

tail_diagnostics <- result$replay[, .(
  rows = .N,
  observed_attempts = sum(observed_team_challenge),
  model_attempts = sum(policy_challenge_probability),
  model_successes = sum(expected_policy_success),
  model_failures = sum(expected_policy_failure),
  model_captured_re = sum(expected_captured_re),
  prior_attempts = sum(prior_policy_challenge_probability),
  context_fallback_k1 = sum(context_fallback_k1),
  context_fallback_k2 = sum(context_fallback_k2),
  exact_root_fallback_k1 = sum(exact_root_fallback_k1),
  exact_root_fallback_k2 = sum(exact_root_fallback_k2)
), by = .(
  fold,
  role,
  sigma_domain = data.table::fcase(
    decision_mode != "structural", "passive_unscored",
    abs(role_margin_inches) <= 3, "local_abs_margin_le_3",
    default = "tail_abs_margin_gt_3"
  )
)]

clock_diagnostics <- opportunities[, .(
  rows = .N,
  structural_rows = sum(decision_mode == "structural"),
  passive_rows = sum(decision_mode == "passive"),
  exogenous_rows = sum(decision_mode == "exogenous"),
  observed_challenges = sum(observed_team_challenge),
  geometry_wrong = sum(geometry_wrong %in% TRUE),
  positive_stake_rows = sum(stake_G > 0),
  nonpositive_stake_rows = sum(stake_G <= 0)
), by = role]

model_source_files <- file.path(project_root, c(
  "scripts/functions/perception/joint_challenge_signal_mdp_1d.R",
  "scripts/functions/perception/challenge_signal_mdp_1d.R",
  "scripts/functions/perception/challenge_margin_prior_1d.R",
  "scripts/functions/perception/challenge_discrimination_1d.R",
  "scripts/functions/perception/defense_challenge_discrimination_1d.R",
  "scripts/functions/perception/joint_common_width_1d.R",
  "scripts/functions/perception/continuous_model_utils.R",
  "scripts/functions/perception/continuous_perception_geometry.R",
  "scripts/functions/perception/continuous_perception_pipeline.R",
  "scripts/functions/core/geometry.R",
  "scripts/functions/core/mdp_policy.R",
  "scripts/functions/core/utils.R",
  "scripts/functions/core/valuation.R",
  "config/project.R",
  "_targets.R",
  "analysis/perception/run_joint_shared_bellman_1d.R"
))
model_source_hash <- digest::digest(
  vapply(
    model_source_files,
    function(path) digest::digest(file = path, algo = "sha256"),
    character(1)
  ),
  algo = "sha256"
)

manifest <- data.table::data.table(
  status = paste(
    "game-heldout role widths, nested-selected priors, and",
    "shared-inventory Bellman; exploratory"
  ),
  policy_scope = paste(
    "joint offense batter and defense catcher/pitcher-unit policy with",
    "one shared MLB challenge inventory"
  ),
  information_regime = result$information_regime,
  offense_common_sigma_inches = paste(
    format(width_crossfit$estimates[
      role == "offense", sigma_inches
    ], digits = 8),
    collapse = ","
  ),
  defense_common_sigma_inches = paste(
    format(width_crossfit$estimates[
      role == "defense", sigma_inches
    ], digits = 8),
    collapse = ","
  ),
  sigma_cross_fitted = TRUE,
  offense_sigma_source = paste(
    "outer-training-game fast common-slope probit with batter/team/",
    "umpire/catcher threshold effects; challenge/pass only"
  ),
  defense_sigma_source = paste(
    "outer-training-game fast common-slope probit with defense-team/",
    "opponent/umpire/catcher threshold effects; challenge/pass only"
  ),
  sigma_training_margin_limit_inches = 3,
  width_payoff_features = paste(
    "current correction gain G included; precomputed Poisson inventory loss",
    "excluded; policy threshold uses endogenous Bellman L"
  ),
  policy_scoring_margin_limit_inches = Inf,
  tail_extrapolation = TRUE,
  offense_prior_components = paste(sort(unique(selected_prior_components[
    role == "offense", selected_components
  ])), collapse = ","),
  defense_prior_components = paste(sort(unique(selected_prior_components[
    role == "defense", selected_components
  ])), collapse = ","),
  prior_components_nested_selected_each_fold = nested_prior_selection,
  prior_component_rationale = paste(
    if (nested_prior_selection) {
      "K in {1,3,6} selected inside every outer training fold by"
    } else {
      "fixed pilot K values;"
    },
    "game-clustered heldout location log score and one-SE rule"
  ),
  folds = folds,
  opportunities = nrow(opportunities),
  structural_opportunities = sum(opportunities$decision_mode == "structural"),
  passive_opportunities = sum(opportunities$decision_mode == "passive"),
  exogenous_opportunities = sum(opportunities$decision_mode == "exogenous"),
  games = data.table::uniqueN(opportunities$game_pk),
  team_games = data.table::uniqueN(opportunities[, .(game_pk, team_id)]),
  observed_challenges = sum(opportunities$observed_team_challenge),
  official_geometry_mismatches = sum(
    opportunities$official_geometry_mismatch %in% TRUE
  ),
  bellman_loss_source = "self-consistent cross-fitted shared continuation marginal",
  future_truth_used_by_policy = FALSE,
  challenge_outcomes_used_by_policy = FALSE,
  challenge_choices_used_in_sigma = TRUE,
  heldout_rows_used_in_sigma = FALSE,
  official_inventory_scope = TRUE,
  comparison_to_observed_is_like_for_like_inventory = TRUE,
  observed_success_truth = paste(
    "ABS geometry for primary evaluation; official result retained as",
    "evaluation-only mismatch diagnostic"
  ),
  fixed_factual_pitch_trace = TRUE,
  core_targets_refreshed_before_run = refresh_core_targets,
  calibration_inventory_regime = paste(
    "truth-conditioned factual-trace inventory diagnostic; not a fully",
    "prior-predictive inventory trajectory"
  ),
  bootstrap_uncertainty_scope = paste(
    "game resampling conditional on point-estimated fold widths, nested",
    "selected mixture families, Bellman specification, and factual trace"
  ),
  bootstrap_reps = bootstrap_reps,
  seed = seed,
  output_tag = output_tag,
  pitch_ledger_target_hash = target_metadata[
    name == "pitch_ledger", data
  ],
  re_model_target_hash = target_metadata[name == "re_model", data],
  remaining_opportunities_target_hash = target_metadata[
    name == "remaining_opportunities", data
  ],
  continuous_decision_features_target_hash = target_metadata[
    name == "continuous_decision_features", data
  ],
  uncommitted_model_source_hash = model_source_hash,
  git_worktree_dirty = length(system2(
    "git", c("status", "--porcelain"), stdout = TRUE
  )) > 0L,
  git_sha = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  branch = system2("git", c("branch", "--show-current"), stdout = TRUE)
)

output_directory <- file.path(project_root, "data", "processed", "perception")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
prefix <- file.path(
  output_directory,
  paste0("joint_shared_bellman_1d_", output_tag)
)
data.table::fwrite(result$season, paste0(prefix, "_season.csv"))
data.table::fwrite(result$fold_diagnostics, paste0(prefix, "_folds.csv"))
data.table::fwrite(
  width_crossfit$estimates, paste0(prefix, "_width_estimates.csv")
)
data.table::fwrite(
  width_crossfit$diagnostics, paste0(prefix, "_width_diagnostics.csv")
)
data.table::fwrite(state_values, paste0(prefix, "_state_values.csv"))
data.table::fwrite(prior_parameters, paste0(prefix, "_prior_parameters.csv"))
data.table::fwrite(
  prior_context_weights, paste0(prefix, "_prior_context_weights.csv")
)
data.table::fwrite(
  selected_prior_components, paste0(prefix, "_selected_components.csv")
)
data.table::fwrite(
  prior_selection$metrics, paste0(prefix, "_component_selection.csv")
)
data.table::fwrite(
  lookup_diagnostics, paste0(prefix, "_lookup_diagnostics.csv")
)
data.table::fwrite(calibration, paste0(prefix, "_calibration.csv"))
data.table::fwrite(tail_diagnostics, paste0(prefix, "_tails.csv"))
data.table::fwrite(clock_diagnostics, paste0(prefix, "_clock.csv"))
data.table::fwrite(result$truth_diagnostics, paste0(prefix, "_truth.csv"))
data.table::fwrite(result$bootstrap, paste0(prefix, "_bootstrap.csv"))
data.table::fwrite(result$comparators, paste0(prefix, "_comparators.csv"))
data.table::fwrite(
  result$team_game_role, paste0(prefix, "_team_game_role.csv")
)
data.table::fwrite(target_metadata, paste0(prefix, "_target_metadata.csv"))
data.table::fwrite(manifest, paste0(prefix, "_manifest.csv"))
data.table::fwrite(
  result$fold_assignment, paste0(prefix, "_fold_assignment.csv")
)
arrow::write_parquet(
  width_crossfit$oof_predictions,
  paste0(prefix, "_width_oof_predictions.parquet")
)
arrow::write_parquet(
  result$replay[, .(
    game_pk, team_id, pitch_order, fold, inning, stage, role, count_state,
    decision_mode, edge_distance_inches, role_margin_inches, stake_G,
    actual_wrong, geometry_wrong, official_success,
    official_geometry_mismatch, observed_team_challenge, observed_success,
    inventory_probability_0, inventory_probability_1,
    inventory_probability_2, signal_threshold_k1_inches,
    signal_threshold_k2_inches, marginal_inventory_re_k1,
    marginal_inventory_re_k2, policy_challenge_probability,
    context_fallback_k1, context_fallback_k2,
    exact_root_fallback_k1, exact_root_fallback_k2,
    expected_policy_success, expected_policy_failure, expected_captured_re,
    prior_policy_challenge_probability, prior_policy_success_probability,
    prior_policy_selected_success_probability
  )],
  paste0(prefix, "_replay.parquet")
)
saveRDS(
  list(
    season = result$season,
    fold_diagnostics = result$fold_diagnostics,
    width_estimates = width_crossfit$estimates,
    width_diagnostics = width_crossfit$diagnostics,
    state_values = state_values,
    prior_parameters = prior_parameters,
    prior_context_weights = prior_context_weights,
    selected_prior_components = selected_prior_components,
    component_selection = prior_selection$metrics,
    lookup_diagnostics = lookup_diagnostics,
    calibration = calibration,
    tail_diagnostics = tail_diagnostics,
    clock_diagnostics = clock_diagnostics,
    truth_diagnostics = result$truth_diagnostics,
    comparators = result$comparators,
    team_game_role = result$team_game_role,
    bootstrap_interval = result$bootstrap_interval,
    target_metadata = target_metadata,
    manifest = manifest
  ),
  paste0(prefix, "_compact.rds")
)

print(manifest)
print(clock_diagnostics)
print(result$fold_diagnostics)
print(calibration)
print(result$truth_diagnostics)
print(result$season)
