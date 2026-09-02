project_root <- rprojroot::find_root(
  rprojroot::has_file("_targets.R"), path = getwd()
)
source(file.path(project_root, "config", "project.R"))
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

revealed_environment_flag <- function(name, default = TRUE) {
  raw <- tolower(Sys.getenv(name, unset = if (default) "true" else "false"))
  if (!raw %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be true or false")
  }
  raw %in% c("true", "1", "yes")
}

profile_name <- Sys.getenv("ABS_REVEALED_POLICY_PROFILE", unset = "full")
profile <- revealed_challenge_policy_profile_1d(profile_name)
controls <- list(
  local_margin_limit_inches = 3,
  slow_speed_cutoff_mph = 75,
  prior_context_strength = 100,
  prior_em_tolerance = 1e-4,
  prior_em_max_iterations = 500L,
  bellman_transition_prior_n = 30,
  bellman_tolerance = 1e-7,
  bellman_max_iterations = 10000L,
  threshold_bias_bounds_inches = c(-20, 20),
  action_probability_epsilon = 1e-12,
  density_tail_standard_deviations = 12,
  density_direct_truth_tolerance = 0.05,
  subjective_belief_grid_min_inches = -20,
  subjective_belief_grid_max_inches = 20,
  subjective_belief_grid_step_inches = 0.1,
  selection_nthreads = profile$nthreads,
  prior_inner_folds = profile$inner_folds,
  bellman_lookup_grid_step_inches = profile$lookup_grid_step,
  density_grid_step_inches = profile$density_grid_step,
  bootstrap_reps = profile$bootstrap_reps
)
refresh_targets <- revealed_environment_flag(
  "ABS_REVEALED_POLICY_REFRESH_TARGETS", TRUE
)
keep_bootstrap_draws <- revealed_environment_flag(
  "ABS_REVEALED_POLICY_KEEP_BOOTSTRAP_DRAWS", FALSE
)

branch <- system2("git", c("branch", "--show-current"), stdout = TRUE)
if (!identical(branch, "jp")) {
  stop("The revealed-selection policy workflow must run on branch jp")
}

required_targets <- c("pitch_ledger", "re_model")
if (refresh_targets) {
  targets::tar_make(
    names = tidyselect::any_of(required_targets),
    callr_function = NULL,
    reporter = "silent"
  )
}
# `_targets.R` deliberately gives `config_env` an always cue, so
# `tar_outdated()` necessarily reports all downstream targets even immediately
# after a successful build. Object hashes and the snapshot fingerprint below
# are the fail-closed contract instead.
target_metadata <- data.table::as.data.table(targets::tar_meta(
  names = tidyselect::any_of(required_targets),
  fields = c("name", "data", "command", "depend", "time", "size", "bytes")
))
if (nrow(target_metadata) != length(required_targets) ||
    anyNA(target_metadata[, .(name, data)])) {
  stop("Required target hashes are unavailable")
}

pitch_ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
re_model <- targets::tar_read(re_model)
games <- sort(unique(as.character(pitch_ledger$game_pk)))
fold_assignment <- continuous_game_folds(
  games, folds = profile$folds, seed = profile$seed
)
validate_continuous_game_folds(fold_assignment, profile$folds)

message("Building inventory-available offense/defense selection rows")
selection_rows <- build_revealed_challenge_selection_1d(
  pitch_ledger,
  re_model = re_model,
  slow_cutoff_mph = controls$slow_speed_cutoff_mph,
  require_positive_inventory = TRUE
)
if (!setequal(unique(selection_rows$game_pk), games)) {
  stop("Selection rows do not cover every current game")
}
selection <- crossfit_revealed_challenge_selection_1d(
  selection_rows,
  fold_assignment = fold_assignment,
  folds = profile$folds,
  seed = profile$seed,
  local_margin_limit_inches = controls$local_margin_limit_inches,
  nthreads = profile$nthreads,
  keep_models = TRUE,
  progress = TRUE
)

message("Scoring the complete structural clock at k=1 and k=2 inventory")
policy_clock_rows <- build_revealed_challenge_selection_1d(
  pitch_ledger,
  re_model = re_model,
  slow_cutoff_mph = controls$slow_speed_cutoff_mph,
  require_positive_inventory = FALSE
)
policy_clock_predictions <- score_revealed_challenge_selection_policy_clock_1d(
  policy_clock_rows, selection
)
# The retained BAM bundles are needed only for this out-of-fold clock score.
# Release them before the 25-scenario Bellman/profile allocations.
selection$models <- NULL
invisible(gc(verbose = FALSE))

message("Fitting nested-selected role priors from all tracked adverse calls")
prior_rows <- build_revealed_challenge_prior_1d(pitch_ledger)
prior <- crossfit_revealed_challenge_prior_1d(
  prior_rows,
  fold_assignment = fold_assignment,
  folds = profile$folds,
  inner_folds = profile$inner_folds,
  seed = profile$seed,
  components = c(1L, 3L, 6L),
  context_prior_strength = controls$prior_context_strength,
  tolerance = controls$prior_em_tolerance,
  max_iterations = controls$prior_em_max_iterations,
  progress = TRUE
)

message("Preparing one chronological shared-inventory opportunity stream")
opportunities <- prepare_revealed_challenge_policy_opportunities_1d(
  pitch_ledger, re_model
)
if (!setequal(unique(opportunities$game_pk), games) ||
    nrow(opportunities[decision_mode == "structural"]) != nrow(prior_rows) ||
    nrow(policy_clock_predictions) != nrow(prior_rows)) {
  stop("Current-snapshot opportunity universes do not align")
}

scenarios <- revealed_perception_joint_scenarios_1d()
if (!is.null(profile$active_scenarios)) {
  scenarios <- scenarios[scenario_id %in% profile$active_scenarios]
}
message("Solving scenario-specific outer-fold Bellman continuation values")
scenario_bellman <- crossfit_revealed_policy_bellman_scenarios_1d(
  opportunities,
  fold_assignment = fold_assignment,
  fold_prior_fits = prior$primary_fits,
  width_estimates = selection$width_estimates,
  scenarios = scenarios,
  prior_n = controls$bellman_transition_prior_n,
  tol = controls$bellman_tolerance,
  max_iter = controls$bellman_max_iterations,
  lookup_grid_step = profile$lookup_grid_step,
  keep_replays = FALSE,
  progress = TRUE
)

profile_rows <- prepare_revealed_perception_action_rows_1d(
  selection_rows, opportunities, fold_assignment
)
message("Scoring all joint sensory/action decompositions")
perception <- crossfit_revealed_joint_perception_profile_1d(
  profile_rows,
  fold_prior_fits = prior$primary_fits,
  width_estimates = selection$width_estimates,
  scenario_state_values = scenario_bellman$state_values,
  scenarios = scenarios,
  direct_selection_oof = selection$oof_predictions,
  lookup_grid_step = profile$lookup_grid_step,
  bias_bounds_inches = controls$threshold_bias_bounds_inches,
  probability_epsilon = controls$action_probability_epsilon,
  keep_oof_profiles = FALSE,
  progress = TRUE
)
accepted <- perception$scenario_metrics[one_se_accepted == TRUE, .(
  scenario_id, offense_kappa, defense_kappa
)]
if (!nrow(accepted)) stop("The perception identified set is empty")

message("Replaying accepted normative policies on held-out games")
threshold_parts <- normative_parts <- accepted_diagnostic_parts <- vector(
  "list", nrow(accepted)
)
for (scenario_index in seq_len(nrow(accepted))) {
  message(
    "accepted policy ", scenario_index, "/", nrow(accepted), ": ",
    accepted$scenario_id[[scenario_index]]
  )
  accepted_result <- crossfit_revealed_policy_bellman_scenarios_1d(
    opportunities,
    fold_assignment = fold_assignment,
    fold_prior_fits = prior$primary_fits,
    width_estimates = selection$width_estimates,
    scenarios = accepted[scenario_index],
    prior_n = controls$bellman_transition_prior_n,
    tol = controls$bellman_tolerance,
    max_iter = controls$bellman_max_iterations,
    lookup_grid_step = profile$lookup_grid_step,
    keep_replays = TRUE,
    progress = FALSE
  )
  threshold_parts[[scenario_index]] <- build_revealed_policy_thresholds_1d(
    accepted_result$replays, selection$width_estimates
  )
  normative_parts[[scenario_index]] <-
    build_revealed_normative_probability_rows_1d(
      accepted_result$replays, selection$width_estimates
    )
  accepted_diagnostic_parts[[scenario_index]] <- accepted_result$diagnostics
  rm(accepted_result)
}
policy_thresholds <- data.table::rbindlist(
  threshold_parts, use.names = TRUE, fill = TRUE
)
normative_probabilities <- data.table::rbindlist(
  normative_parts, use.names = TRUE, fill = TRUE
)
accepted_bellman_diagnostics <- data.table::rbindlist(
  accepted_diagnostic_parts, use.names = TRUE, fill = TRUE
)
rm(threshold_parts, normative_parts, accepted_diagnostic_parts)

message("Deriving challenged-margin distributions from OOF propensities")
margin_distributions <- build_revealed_challenge_margin_distributions_1d(
  prior,
  selection$oof_predictions,
  grid_step = profile$density_grid_step,
  tail_standard_deviations = controls$density_tail_standard_deviations,
  direct_truth_tolerance = controls$density_direct_truth_tolerance
)
perception_profile <- build_revealed_perception_profile_table_1d(
  perception, selection$width_estimates
)
subjective_belief_envelope <- build_revealed_subjective_belief_envelope_1d(
  prior$primary_fits,
  selection$width_estimates,
  accepted,
  signal_grid = seq(
    controls$subjective_belief_grid_min_inches,
    controls$subjective_belief_grid_max_inches,
    by = controls$subjective_belief_grid_step_inches
  )
)

message("Valuing observed, fitted, normative, no-challenge, and oracle policies")
policy_value <- build_revealed_policy_value_comparison_1d(
  policy_clock_predictions,
  normative_probabilities,
  bootstrap_reps = profile$bootstrap_reps,
  seed = profile$seed
)

official_labels <- pitch_ledger[
  challenge_occurred %in% TRUE &
    challenge_outcome %in% c("overturned", "upheld") &
    adverse_role %in% c("offense", "defense"),
  .(
    game_pk = as.character(game_pk),
    team_id = as.character(adverse_team_id),
    pitch_order = as.integer(pitch_order),
    role = as.character(adverse_role),
    official_success = challenge_outcome == "overturned"
  )
]
official_policy_validation <- evaluate_revealed_policy_official_outcomes_1d(
  policy_value$replay, official_labels
)
selection_official_labels <- official_labels[, .(
  game_pk, pitch_order, role, official_success
)]
selection_validation <- evaluate_revealed_challenge_margin_selection_1d(
  selection$oof_predictions,
  official_labels = selection_official_labels,
  probability_column = "challenge_probability"
)

snapshot_id <- revealed_policy_snapshot_id_1d(target_metadata, games)
pitch_hash <- target_metadata[name == "pitch_ledger", data][[1L]]
selection$oof_predictions[, `:=`(
  action = as.integer(challenged),
  true_margin_inches = as.numeric(role_margin_inches),
  oof_propensity = as.numeric(challenge_probability),
  tail_stratum = data.table::fcase(
    margin_stratum == "local_abs_margin_le_3", "near_zone",
    speed_stratum %in% c("slow_non_eephus", "eephus"),
      "slow_or_eephus_tail",
    speed_stratum == "speed_missing", "speed_missing_tail",
    default = "ordinary_speed_tail"
  )
)]
selection_oof <- attach_revealed_policy_snapshot_1d(
  selection$oof_predictions, snapshot_id, pitch_hash
)
margin_distributions <- attach_revealed_policy_snapshot_1d(
  margin_distributions, snapshot_id, pitch_hash
)
perception_profile <- attach_revealed_policy_snapshot_1d(
  perception_profile, snapshot_id, pitch_hash
)
subjective_belief_envelope <- attach_revealed_policy_snapshot_1d(
  subjective_belief_envelope, snapshot_id, pitch_hash
)
policy_thresholds <- attach_revealed_policy_snapshot_1d(
  policy_thresholds, snapshot_id, pitch_hash
)
policy_value_summary <- attach_revealed_policy_snapshot_1d(
  policy_value$summary$season, snapshot_id, pitch_hash
)

package_names <- c(
  "R", "arrow", "data.table", "digest", "jsonlite", "mgcv", "mvtnorm",
  "targets", "testthat"
)
package_versions <- stats::setNames(vapply(package_names, function(package) {
  if (package == "R") return(as.character(getRversion()))
  if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(package))
}, character(1L)), package_names)
selected_components <- prior$parameter_table[, .(
  selected_components = data.table::uniqueN(component)
), by = .(outer_fold, role)]
manifest <- list(
  model = "revealed_selection_perception_aware_shared_inventory_policy_1d",
  status = "five-fold cross-validated exploratory analysis",
  profile = profile$name,
  snapshot_id = snapshot_id,
  target_hashes = as.list(stats::setNames(
    target_metadata$data, target_metadata$name
  )),
  games = length(games),
  called_pitches = nrow(pitch_ledger),
  inventory_available_selection_rows = nrow(selection_rows),
  all_tracked_adverse_prior_rows = nrow(prior_rows),
  structural_policy_clock_rows = nrow(policy_clock_predictions),
  observed_challenges = sum(selection_rows$challenged),
  folds = profile$folds,
  fold_seed = profile$seed,
  fold_fingerprint = digest::digest(
    data.table::setDF(fold_assignment[order(game_pk)]), algo = "sha256"
  ),
  gmm_candidates = c(1L, 3L, 6L),
  gmm_selected_by_fold_role = selected_components,
  primary_prior_context = "count x coarse pitch family",
  prior_sensitivity = "count-only weights on identical selected shapes",
  profile_grid = revealed_perception_kappa_grid_1d(),
  evaluated_joint_scenarios = scenarios$scenario_id,
  accepted_scenarios = accepted$scenario_id,
  subjective_belief_output = paste(
    "count-specific and all-count q(r) envelope across accepted scenarios,",
    "folds, and contextual pitch-family priors"
  ),
  best_scenario = perception$scenario_metrics[
    best_scenario == TRUE, scenario_id
  ],
  structural_support_gate = perception$direct_benchmark_gate,
  selection_promotion_gate = selection$gate,
  selection_tail_diagnostics = selection$comparison[
    scope %in% c("ordinary_speed_tail", "slow_or_eephus_tail")
  ],
  leakage_declarations = list(
    priors_use_actions = FALSE,
    priors_use_official_outcomes = FALSE,
    selection_uses_official_outcomes = FALSE,
    profile_uses_challenge_actions = TRUE,
    profile_uses_challenge_outcomes = FALSE,
    Bellman_uses_future_truth = FALSE,
    geometry_enters = "held-out counterfactual success/value evaluation only",
    official_outcomes_enter = "final validation only"
  ),
  policy_objective = "run expectancy; WPA excluded",
  inventory = "one chronological offense/defense inventory per team-game",
  bootstrap_reps = profile$bootstrap_reps,
  numerical_controls = controls,
  no_untouched_confirmation_set = TRUE,
  branch = branch,
  git_sha = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  git_worktree_dirty = length(system2(
    "git", c("status", "--porcelain"), stdout = TRUE
  )) > 0L,
  software_versions = as.list(package_versions),
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
)

output_directory <- file.path(
  project_root, "data", "processed", "perception", "revealed_policy"
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(
  selection_oof,
  file.path(output_directory, "challenge_selection_oof.parquet")
)
arrow::write_parquet(
  margin_distributions,
  file.path(output_directory, "challenge_margin_distributions.parquet")
)
arrow::write_parquet(
  perception_profile,
  file.path(output_directory, "perception_profile.parquet")
)
arrow::write_parquet(
  subjective_belief_envelope,
  file.path(output_directory, "subjective_belief_envelope.parquet")
)
arrow::write_parquet(
  policy_thresholds,
  file.path(output_directory, "policy_thresholds.parquet")
)
data.table::fwrite(
  policy_value_summary,
  file.path(output_directory, "policy_value_summary.csv")
)
jsonlite::write_json(
  manifest,
  file.path(output_directory, "model_manifest.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null",
  dataframe = "rows"
)

diagnostic_tables <- list(
  target_metadata = target_metadata,
  fold_assignment = fold_assignment,
  selection_comparison = selection$comparison,
  selection_gate = selection$gate,
  selection_fit_diagnostics = selection$diagnostics,
  width_estimates = selection$width_estimates,
  prior_component_selection = prior$candidate_metrics,
  prior_oof_log_scores = prior$oof_log_scores,
  prior_parameters = prior$parameter_table,
  prior_context_weights = prior$context_table,
  prior_diagnostics = prior$diagnostics,
  scenario_metrics = perception$scenario_metrics,
  scenario_role_metrics = perception$role_metrics,
  threshold_bias = perception$threshold_bias,
  threshold_diagnostics = perception$threshold_diagnostics,
  bellman_diagnostics = scenario_bellman$diagnostics,
  accepted_bellman_diagnostics = accepted_bellman_diagnostics,
  selection_validation = selection_validation$diagnostics,
  selection_official_validation = selection_validation$official_diagnostics,
  policy_official_validation = official_policy_validation,
  policy_bootstrap_intervals = policy_value$summary$bootstrap_intervals
)
for (name in names(diagnostic_tables)) {
  data.table::fwrite(
    diagnostic_tables[[name]],
    file.path(output_directory, paste0(name, ".csv"))
  )
}
if (isTRUE(keep_bootstrap_draws) &&
    nrow(policy_value$summary$bootstrap)) {
  arrow::write_parquet(
    policy_value$summary$bootstrap,
    file.path(output_directory, "policy_value_bootstrap_draws.parquet")
  )
}

required_outputs <- file.path(output_directory, c(
  "challenge_selection_oof.parquet",
  "challenge_margin_distributions.parquet",
  "perception_profile.parquet",
  "subjective_belief_envelope.parquet",
  "policy_thresholds.parquet",
  "policy_value_summary.csv",
  "model_manifest.json"
))
if (!all(file.exists(required_outputs))) {
  stop("At least one primary revealed-policy artifact was not written")
}
print(selection$gate)
print(perception$scenario_metrics)
print(perception$direct_benchmark_gate)
print(policy_value_summary)
cat("Revealed-policy artifacts:", output_directory, "\n")
