project_root <- rprojroot::find_root(
  rprojroot::has_file("_targets.R"), path = getwd()
)
source(file.path(project_root, "config", "project.R"))
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

environment_number <- function(name, default, lower = -Inf) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, unset = default)))
  if (length(value) != 1L || !is.finite(value) || value <= lower) {
    stop(name, " must be one finite number greater than ", lower)
  }
  value
}

environment_integer <- function(name, default, minimum = 1L) {
  value <- suppressWarnings(as.integer(Sys.getenv(name, unset = default)))
  if (length(value) != 1L || is.na(value) || value < minimum) {
    stop(name, " must be one integer at least ", minimum)
  }
  value
}

sigma_inches <- environment_number("ABS_1D_COMMON_SIGMA", 2.850308, 0)
folds <- environment_integer("ABS_1D_BELLMAN_FOLDS", 5L, 2L)
seed <- environment_integer("ABS_1D_BELLMAN_SEED", 20260825L)
bootstrap_reps <- environment_integer(
  "ABS_1D_BELLMAN_BOOTSTRAP_REPS", 2000L
)
prior_components <- environment_integer(
  "ABS_1D_BELLMAN_PRIOR_COMPONENTS", 3L
)
output_tag <- Sys.getenv("ABS_1D_BELLMAN_OUTPUT_TAG", unset = "primary")
output_tag <- gsub("[^A-Za-z0-9_.-]+", "_", output_tag)
if (!nzchar(output_tag)) output_tag <- "primary"

pitch_ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
re_model <- targets::tar_read(re_model)
opportunities <- prepare_challenge_signal_mdp_opportunities_1d(
  pitch_ledger, re_model
)
fold_assignment <- continuous_game_folds(
  opportunities$game_pk, folds = folds, seed = seed
)

result <- crossfit_challenge_signal_mdp_1d(
  opportunities,
  perception_sigma = sigma_inches,
  fold_assignment = fold_assignment,
  folds = folds,
  seed = seed,
  prior_components = prior_components,
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

model_source_files <- file.path(project_root, c(
  "scripts/functions/perception/challenge_signal_mdp_1d.R",
  "scripts/functions/perception/challenge_margin_prior_1d.R",
  "scripts/functions/core/mdp_policy.R",
  "analysis/perception/run_common_width_bellman_1d.R"
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
  status = "game_heldout_bellman_and_prior_fixed_pilot_sigma_exploratory",
  policy_scope = "batter called-strikes only with an artificial batter-only inventory",
  information_regime = result$information_regime,
  common_sigma_inches = sigma_inches,
  sigma_cross_fitted = FALSE,
  sigma_source = "fixed one-fold common-width pilot estimate",
  sigma_training_margin_limit_inches = 3,
  policy_scoring_margin_limit_inches = Inf,
  tail_extrapolation = TRUE,
  tail_opportunities = sum(abs(opportunities$edge_distance_inches) > 3),
  prior_components = prior_components,
  prior_components_nested_selected_each_fold = FALSE,
  folds = folds,
  opportunities = nrow(opportunities),
  games = data.table::uniqueN(opportunities$game_pk),
  teams = data.table::uniqueN(opportunities$team_id),
  bellman_loss_source = "self-consistent cross-fitted continuation marginal",
  future_truth_used_by_policy = FALSE,
  challenge_outcomes_used_by_policy = FALSE,
  official_inventory_scope = FALSE,
  comparison_to_observed_is_like_for_like = FALSE,
  observed_success_truth = "ABS geometry; not official overturn label",
  fixed_factual_pitch_trace = TRUE,
  bootstrap_uncertainty_scope = paste(
    "game resampling conditional on fixed sigma, K=3 prior family,",
    "Bellman specification, and factual pitch trace"
  ),
  bootstrap_reps = bootstrap_reps,
  seed = seed,
  output_tag = output_tag,
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
  paste0("common_width_bellman_1d_", output_tag)
)
data.table::fwrite(result$season, paste0(prefix, "_season.csv"))
data.table::fwrite(result$fold_diagnostics, paste0(prefix, "_folds.csv"))
data.table::fwrite(state_values, paste0(prefix, "_state_values.csv"))
data.table::fwrite(manifest, paste0(prefix, "_manifest.csv"))
arrow::write_parquet(
  result$replay[, .(
    game_pk, team_id, pitch_order, fold, inning, stage, count_state,
    edge_distance_inches, stake_G, actual_wrong, observed_batter_challenge,
    inventory_probability_0, inventory_probability_1,
    inventory_probability_2, signal_threshold_k1_inches,
    signal_threshold_k2_inches, marginal_inventory_re_k1,
    marginal_inventory_re_k2, policy_challenge_probability,
    expected_policy_success, expected_policy_failure, expected_captured_re,
    prior_policy_challenge_probability, prior_policy_success_probability,
    prior_policy_selected_success_probability
  )],
  paste0(prefix, "_replay.parquet")
)
saveRDS(
  list(
    season = result$season,
    team_game = result$team_game,
    bootstrap = result$bootstrap,
    fold_diagnostics = result$fold_diagnostics,
    state_values = state_values,
    manifest = manifest
  ),
  paste0(prefix, "_compact.rds")
)

print(manifest)
print(result$fold_diagnostics)
print(result$season)
