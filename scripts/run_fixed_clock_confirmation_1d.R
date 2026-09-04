# Production: Rscript scripts/run_fixed_clock_confirmation_1d.R
# Bounded end-to-end validation:
#   ABS_FIXED_CLOCK_PROFILE=smoke ABS_FIXED_CLOCK_REFRESH_TARGETS=false \
#     Rscript scripts/run_fixed_clock_confirmation_1d.R
# Optional controls: ABS_FIXED_CLOCK_BOOTSTRAP_REPS,
# ABS_FIXED_CLOCK_PROCEDURE_BOOTSTRAP_STRIDE, ABS_FIXED_CLOCK_WORKERS,
# ABS_FIXED_CLOCK_CANDIDATE_WORKERS, ABS_FIXED_CLOCK_POLICY_CV,
# ABS_FIXED_CLOCK_FIXED_STAGE_DF, ABS_FIXED_CLOCK_FIXED_RIDGE,
# ABS_FIXED_CLOCK_WARM_START_POLICY, ABS_FIXED_CLOCK_EMPIRICAL_ALPHA_GRID,
# and ABS_FIXED_CLOCK_CONTEXT_RE.

project_root <- rprojroot::find_root(
  rprojroot::has_file("_targets.R"), path = getwd()
)
source(file.path(project_root, "config", "project.R"))
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

fixed_clock_env_flag <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, unset = if (default) "true" else "false"))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop(name, " must be true or false", call. = FALSE)
  }
  value %in% c("true", "1", "yes")
}

fixed_clock_runner_profile <- function(name = c("full", "report", "smoke")) {
  name <- match.arg(name)
  if (name == "smoke") {
    return(list(
      name = name,
      folds = 2L,
      inner_folds = 2L,
      scenarios = data.table::data.table(
        scenario_id = "offense_kappa_0.50__defense_kappa_0.50",
        offense_kappa = 0.5,
        defense_kappa = 0.5
      ),
      tuning_grid = data.table::data.table(stage_df = 4L, ridge = 1e-3),
      run_policy_cv = FALSE,
      policy_cv_grid = "fixed",
      fixed_stage_df = 4L,
      fixed_ridge = 1e-3,
      policy_game_limit = 30L,
      nuisance_game_limit = 200L,
      prior_family = "empirical_binned",
      empirical_bin_width = 0.01,
      empirical_shrinkage = 200,
      empirical_alpha_grid = 200,
      prior_components = 1L,
      bootstrap_reps = 0L,
      procedure_bootstrap_stride = 1L,
      optimizer_control = list(maxit = 2L, reltol = 1e-5),
      cv_optimizer_control = list(maxit = 2L, reltol = 1e-5),
      local_optimizer_control = list(maxit = 1L, reltol = 1e-5),
      lookup_grid_step = 0.1,
      bellman_lookup_grid_step = 0.1,
      bellman_max_iterations = 1000L,
      fit_bellman = FALSE,
      fit_context_re = FALSE,
      bias_nodes = 3L,
      nthreads = 1L,
      candidate_workers = 1L,
      workers = 1L
    ))
  }
  scenarios <- revealed_perception_joint_scenarios_1d()
  detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if (length(detected_cores) != 1L || !is.finite(detected_cores)) {
    detected_cores <- 2L
  }
  list(
    name = name,
    folds = 5L,
    inner_folds = 3L,
    scenarios = scenarios,
    tuning_grid = data.table::CJ(
      stage_df = c(4L, 5L),
      ridge = c(1e-4, 1e-3),
      sorted = TRUE
    ),
    run_policy_cv = TRUE,
    policy_cv_grid = "search",
    fixed_stage_df = 4L,
    fixed_ridge = 1e-3,
    policy_game_limit = Inf,
    nuisance_game_limit = Inf,
    prior_family = "empirical_binned",
    empirical_bin_width = 0.01,
    empirical_shrinkage = 200,
    empirical_alpha_grid = c(0, 25, 50, 100, 200),
    prior_components = c(1L, 3L, 6L),
    bootstrap_reps = if (name == "full") 300L else 0L,
    procedure_bootstrap_stride = 3L,
    optimizer_control = list(maxit = 100L, reltol = 1e-8),
    cv_optimizer_control = list(maxit = 25L, reltol = 1e-7),
    local_optimizer_control = list(maxit = 25L, reltol = 1e-7),
    lookup_grid_step = 0.02,
    bellman_lookup_grid_step = 0.02,
    bellman_max_iterations = 10000L,
    fit_bellman = TRUE,
    fit_context_re = name == "full",
    bias_nodes = 5L,
    nthreads = 1L,
    candidate_workers = max(
      1L, min(8L, as.integer(detected_cores) - 1L)
    ),
    workers = max(1L, min(4L, as.integer(detected_cores) - 1L))
  )
}

profile_name <- Sys.getenv("ABS_FIXED_CLOCK_PROFILE", unset = "full")
profile <- fixed_clock_runner_profile(profile_name)
policy_cv_override <- Sys.getenv("ABS_FIXED_CLOCK_POLICY_CV", unset = "")
if (nzchar(policy_cv_override)) {
  profile$run_policy_cv <- fixed_clock_env_flag("ABS_FIXED_CLOCK_POLICY_CV")
}
policy_cv_grid_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_POLICY_CV_GRID", unset = ""
)
if (nzchar(policy_cv_grid_override)) {
  profile$policy_cv_grid <- match.arg(
    policy_cv_grid_override, c("search", "fixed")
  )
}
fixed_stage_df_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_FIXED_STAGE_DF", unset = ""
)
if (nzchar(fixed_stage_df_override)) {
  profile$fixed_stage_df <- suppressWarnings(as.integer(
    fixed_stage_df_override
  ))
  if (is.na(profile$fixed_stage_df) || profile$fixed_stage_df < 2L) {
    stop("ABS_FIXED_CLOCK_FIXED_STAGE_DF must be an integer of at least 2")
  }
}
fixed_ridge_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_FIXED_RIDGE", unset = ""
)
if (nzchar(fixed_ridge_override)) {
  profile$fixed_ridge <- suppressWarnings(as.numeric(fixed_ridge_override))
  if (!is.finite(profile$fixed_ridge) || profile$fixed_ridge < 0) {
    stop("ABS_FIXED_CLOCK_FIXED_RIDGE must be finite and non-negative")
  }
}
prior_family_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_PRIOR_FAMILY", unset = ""
)
if (nzchar(prior_family_override)) {
  profile$prior_family <- match.arg(
    prior_family_override, c("gaussian_mixture", "empirical_binned")
  )
}
empirical_bin_width_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_EMPIRICAL_BIN_WIDTH", unset = ""
)
if (nzchar(empirical_bin_width_override)) {
  profile$empirical_bin_width <- suppressWarnings(as.numeric(
    empirical_bin_width_override
  ))
  if (!is.finite(profile$empirical_bin_width) ||
      profile$empirical_bin_width <= 0) {
    stop("ABS_FIXED_CLOCK_EMPIRICAL_BIN_WIDTH must be positive and finite")
  }
}
empirical_shrinkage_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_EMPIRICAL_SHRINKAGE", unset = ""
)
if (nzchar(empirical_shrinkage_override)) {
  profile$empirical_shrinkage <- suppressWarnings(as.numeric(
    empirical_shrinkage_override
  ))
  if (!is.finite(profile$empirical_shrinkage) ||
      profile$empirical_shrinkage < 0) {
    stop("ABS_FIXED_CLOCK_EMPIRICAL_SHRINKAGE must be non-negative and finite")
  }
  profile$empirical_alpha_grid <- profile$empirical_shrinkage
}
empirical_alpha_grid_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_EMPIRICAL_ALPHA_GRID", unset = ""
)
if (nzchar(empirical_alpha_grid_override)) {
  profile$empirical_alpha_grid <- suppressWarnings(as.numeric(strsplit(
    empirical_alpha_grid_override, ",", fixed = TRUE
  )[[1L]]))
  if (!length(profile$empirical_alpha_grid) ||
      anyNA(profile$empirical_alpha_grid) ||
      any(!is.finite(profile$empirical_alpha_grid)) ||
      any(profile$empirical_alpha_grid < 0)) {
    stop(
      "ABS_FIXED_CLOCK_EMPIRICAL_ALPHA_GRID must be comma-separated, finite, and non-negative"
    )
  }
  profile$empirical_alpha_grid <- sort(unique(profile$empirical_alpha_grid))
}
bootstrap_override <- Sys.getenv("ABS_FIXED_CLOCK_BOOTSTRAP_REPS", unset = "")
if (nzchar(bootstrap_override)) {
  profile$bootstrap_reps <- as.integer(bootstrap_override)
  if (is.na(profile$bootstrap_reps) || profile$bootstrap_reps < 0L) {
    stop("ABS_FIXED_CLOCK_BOOTSTRAP_REPS must be a non-negative integer")
  }
}
procedure_stride_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_PROCEDURE_BOOTSTRAP_STRIDE", unset = ""
)
if (nzchar(procedure_stride_override)) {
  profile$procedure_bootstrap_stride <- suppressWarnings(as.integer(
    procedure_stride_override
  ))
  if (is.na(profile$procedure_bootstrap_stride) ||
      profile$procedure_bootstrap_stride < 1L) {
    stop(
      "ABS_FIXED_CLOCK_PROCEDURE_BOOTSTRAP_STRIDE must be a positive integer"
    )
  }
}
worker_override <- Sys.getenv("ABS_FIXED_CLOCK_WORKERS", unset = "")
if (nzchar(worker_override)) {
  worker_value <- suppressWarnings(as.integer(worker_override))
  if (is.na(worker_value) || worker_value < 1L) {
    stop("ABS_FIXED_CLOCK_WORKERS must be a positive integer")
  }
  profile$workers <- worker_value
}
candidate_worker_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_CANDIDATE_WORKERS", unset = ""
)
if (nzchar(candidate_worker_override)) {
  candidate_worker_value <- suppressWarnings(
    as.integer(candidate_worker_override)
  )
  if (is.na(candidate_worker_value) || candidate_worker_value < 1L) {
    stop("ABS_FIXED_CLOCK_CANDIDATE_WORKERS must be a positive integer")
  }
  profile$candidate_workers <- candidate_worker_value
}
final_candidate_workers <- profile$candidate_workers
final_candidate_worker_override <- Sys.getenv(
  "ABS_FIXED_CLOCK_FINAL_CANDIDATE_WORKERS", unset = ""
)
if (nzchar(final_candidate_worker_override)) {
  final_candidate_workers <- suppressWarnings(as.integer(
    final_candidate_worker_override
  ))
  if (is.na(final_candidate_workers) || final_candidate_workers < 1L) {
    stop("ABS_FIXED_CLOCK_FINAL_CANDIDATE_WORKERS must be a positive integer")
  }
}
data.table::setDTthreads(profile$nthreads)
warm_start_policy_path <- Sys.getenv(
  "ABS_FIXED_CLOCK_WARM_START_POLICY", unset = ""
)
warm_start_file_sha256 <- if (nzchar(warm_start_policy_path)) {
  if (!file.exists(warm_start_policy_path)) {
    stop("ABS_FIXED_CLOCK_WARM_START_POLICY does not exist")
  }
  digest::digest(
    warm_start_policy_path, file = TRUE, algo = "sha256"
  )
} else {
  "not_used"
}
seed <- 20260826L
refresh_targets <- fixed_clock_env_flag(
  "ABS_FIXED_CLOCK_REFRESH_TARGETS", profile$name == "full"
)
run_context_re <- fixed_clock_env_flag(
  "ABS_FIXED_CLOCK_CONTEXT_RE", profile$fit_context_re
)

required_targets <- c("pitch_ledger", "re_model", "history_statcast")
public_input_directory <- Sys.getenv(
  "ABS_FIXED_CLOCK_INPUT_DIR", unset = ""
)
if (nzchar(public_input_directory)) {
  if (!grepl("^/", public_input_directory)) {
    public_input_directory <- file.path(project_root, public_input_directory)
  }
  public_input_directory <- normalizePath(
    public_input_directory, mustWork = TRUE
  )
  public_input_files <- c(
    pitch_ledger = "pitch_ledger.parquet",
    re_model = "re288_model.rds",
    history_statcast = "history_re_inputs.parquet"
  )
  public_input_paths <- stats::setNames(
    file.path(public_input_directory, unname(public_input_files)),
    names(public_input_files)
  )
  missing_public_inputs <- names(public_input_files)[
    !file.exists(public_input_paths)
  ]
  if (length(missing_public_inputs)) {
    stop(
      "Public analysis input bundle is incomplete: ",
      paste(missing_public_inputs, collapse = ", "), call. = FALSE
    )
  }
  target_metadata <- data.table::data.table(
    name = names(public_input_files),
    data = vapply(
      public_input_paths,
      digest::digest,
      character(1L),
      file = TRUE,
      algo = "sha256"
    )
  )
  pitch_ledger <- data.table::as.data.table(
    arrow::read_parquet(public_input_paths[["pitch_ledger"]])
  )
  re_model <- readRDS(public_input_paths[["re_model"]])
  history_statcast <- data.table::as.data.table(
    arrow::read_parquet(public_input_paths[["history_statcast"]])
  )
  message("Using the versioned public analysis-input bundle")
} else {
  if (refresh_targets) {
    targets::tar_make(
      names = tidyselect::any_of(required_targets),
      callr_function = NULL,
      reporter = "silent"
    )
  }
  target_metadata <- data.table::as.data.table(targets::tar_meta(
    names = tidyselect::any_of(required_targets),
    fields = c("name", "data", "command", "depend", "time", "size", "bytes")
  ))
  if (nrow(target_metadata) != length(required_targets) ||
      anyNA(target_metadata[, .(name, data)])) {
    stop("Required target hashes are unavailable", call. = FALSE)
  }
  pitch_ledger <- data.table::as.data.table(targets::tar_read(pitch_ledger))
  re_model <- targets::tar_read(re_model)
  history_statcast <- data.table::as.data.table(
    targets::tar_read(history_statcast)
  )
}
pitch_ledger <- exclude_fixed_clock_unavailable_games_1d(pitch_ledger)

message("Validating and hashing the immutable July 20 confirmation split")
snapshot <- validate_fixed_clock_confirmation_snapshot_1d(pitch_ledger)
split <- snapshot$split
split_validation <- snapshot$validation
development_ledger <- data.table::copy(split$development)
confirmation_ledger <- data.table::copy(split$confirmation)
development_games <- sort(unique(as.character(development_ledger$game_pk)))
confirmation_games <- sort(unique(as.character(confirmation_ledger$game_pk)))
if (length(intersect(development_games, confirmation_games))) {
  stop("Development and confirmation games overlap", call. = FALSE)
}

output_base_directory <- file.path(
  project_root, "data", "processed", "perception",
  "fixed_clock_confirmation"
)
source_hashes <- stats::setNames(target_metadata$data, target_metadata$name)
runner_path <- file.path(
  project_root, "scripts", "run_fixed_clock_confirmation_1d.R"
)
function_paths <- abs_function_files(project_root)
code_paths <- c(runner_path, function_paths)
code_hashes <- stats::setNames(
  vapply(code_paths, digest::digest, character(1L), file = TRUE, algo = "sha256"),
  sub(paste0("^", project_root, "/?"), "", code_paths)
)
semantic_runner_sha256 <- tolower(Sys.getenv(
  "ABS_FIXED_CLOCK_SEMANTIC_RUNNER_SHA256", unset = ""
))
if (nzchar(semantic_runner_sha256)) {
  if (!grepl("^[0-9a-f]{64}$", semantic_runner_sha256)) {
    stop("ABS_FIXED_CLOCK_SEMANTIC_RUNNER_SHA256 must be a SHA-256 digest")
  }
  runner_code_key <- sub(
    paste0("^", project_root, "/?"), "", runner_path
  )
  code_hashes[[runner_code_key]] <- semantic_runner_sha256
  message(
    "Using the frozen semantic runner hash for an execution-only resume"
  )
}
analysis_profile <- profile
analysis_profile[c("workers", "candidate_workers", "bootstrap_reps")] <- NULL
run_configuration <- list(
  schema = "fixed_clock_confirmation_run_configuration_v1",
  profile = analysis_profile,
  seed = seed,
  context_re = run_context_re,
  warm_start_file_sha256 = warm_start_file_sha256,
  target_hashes = source_hashes,
  code_hashes = code_hashes
)
run_configuration_sha256 <- fixed_clock_hash_object_1d(run_configuration)
execution_configuration <- list(
  schema = "fixed_clock_confirmation_execution_configuration_v1",
  run_configuration_sha256 = run_configuration_sha256,
  bootstrap_reps = profile$bootstrap_reps,
  procedure_bootstrap_stride = profile$procedure_bootstrap_stride,
  candidate_workers = profile$candidate_workers,
  final_candidate_workers = final_candidate_workers,
  workers = profile$workers,
  data_table_threads = data.table::getDTthreads(),
  slurm_job_id = Sys.getenv("SLURM_JOB_ID", unset = NA_character_),
  slurm_cpus_per_task = Sys.getenv(
    "SLURM_CPUS_PER_TASK", unset = NA_character_
  )
)
execution_configuration_sha256 <- fixed_clock_hash_object_1d(
  execution_configuration
)
output_directory <- file.path(
  output_base_directory, "runs",
  paste0(profile$name, "_", substr(run_configuration_sha256, 1L, 20L))
)
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
checkpoint_directory <- file.path(output_directory, "bootstrap_checkpoints")
dir.create(checkpoint_directory, recursive = TRUE, showWarnings = FALSE)
stage_checkpoint_directory <- file.path(
  output_directory, "stage_checkpoints"
)
dir.create(stage_checkpoint_directory, recursive = TRUE, showWarnings = FALSE)
resume_stages <- fixed_clock_env_flag("ABS_FIXED_CLOCK_RESUME_STAGES", TRUE)

write_immutable_rds <- function(value, path) {
  if (file.exists(path)) {
    existing <- readRDS(path)
    if (!identical(
      fixed_clock_hash_object_1d(existing), fixed_clock_hash_object_1d(value)
    )) {
      stop("Immutable artifact already exists with different content: ", path)
    }
    return(invisible(path))
  }
  temporary <- tempfile(pattern = basename(path), tmpdir = dirname(path))
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(value, temporary, version = 3)
  if (!file.rename(temporary, path)) stop("Could not write immutable artifact")
  invisible(path)
}

load_or_compute_stage <- function(stage_id, fingerprint_inputs, compute) {
  stage_id <- as.character(stage_id)
  if (length(stage_id) != 1L || is.na(stage_id) || !nzchar(stage_id) ||
      !grepl("^[A-Za-z0-9_.-]+$", stage_id)) {
    stop("stage_id must be one filesystem-safe string", call. = FALSE)
  }
  if (!is.function(compute)) stop("compute must be a function", call. = FALSE)
  fingerprint <- fixed_clock_hash_object_1d(list(
    run_configuration_sha256 = run_configuration_sha256,
    stage_id = stage_id,
    inputs = fingerprint_inputs
  ))
  path <- file.path(
    stage_checkpoint_directory,
    paste0(stage_id, "_", fingerprint, ".rds")
  )
  if (isTRUE(resume_stages) && file.exists(path)) {
    checkpoint <- readRDS(path)
    valid <- is.list(checkpoint) &&
      identical(checkpoint$schema, "fixed_clock_stage_checkpoint_v1") &&
      identical(checkpoint$stage_id, stage_id) &&
      identical(checkpoint$fingerprint, fingerprint) &&
      identical(
        checkpoint$result_sha256,
        fixed_clock_hash_object_1d(checkpoint$result)
      )
    if (!isTRUE(valid)) {
      stop("Stage checkpoint failed integrity validation: ", path)
    }
    message("Resuming stage checkpoint: ", stage_id)
    return(checkpoint$result)
  }
  message("Computing stage: ", stage_id)
  started <- proc.time()[["elapsed"]]
  result <- compute()
  elapsed_seconds <- proc.time()[["elapsed"]] - started
  .fixed_clock_write_rds_atomic_1d(
    list(
      schema = "fixed_clock_stage_checkpoint_v1",
      stage_id = stage_id,
      fingerprint = fingerprint,
      elapsed_seconds = elapsed_seconds,
      result = result,
      result_sha256 = fixed_clock_hash_object_1d(result)
    ),
    path
  )
  message(
    "Completed stage ", stage_id, " in ",
    sprintf("%.1f", elapsed_seconds), " seconds"
  )
  result
}

split_artifact <- list(
  schema = "fixed_clock_temporal_split_v1",
  cutoff = format(split$cutoff, "%Y-%m-%d"),
  split_sha256 = split$split_sha256,
  validation = split_validation,
  game_split = split$game_split[order(game_pk)]
)
write_immutable_rds(
  split_artifact, file.path(output_directory, "split_manifest.rds")
)
write_immutable_rds(
  run_configuration,
  file.path(output_directory, "run_configuration.rds")
)
write_immutable_rds(
  execution_configuration,
  file.path(
    output_directory,
    paste0(
      "execution_configuration_", execution_configuration_sha256, ".rds"
    )
  )
)
data.table::fwrite(
  split$game_split[order(game_date, game_pk)],
  file.path(output_directory, "split_games.csv")
)

message("Constructing development-only nuisance samples")
nuisance_development_games <- development_ledger[, .(
  game_date = min(as.Date(game_date))
), by = .(game_pk = as.character(game_pk))][order(game_date, game_pk)]$game_pk
if (is.finite(profile$nuisance_game_limit)) {
  nuisance_development_games <- head(
    nuisance_development_games, as.integer(profile$nuisance_game_limit)
  )
}
nuisance_development_ledger <- development_ledger[
  as.character(game_pk) %in% nuisance_development_games
]
fold_assignment <- continuous_game_folds(
  nuisance_development_games, folds = profile$folds, seed = seed
)
validate_continuous_game_folds(fold_assignment, profile$folds)
development_selection_rows <- build_revealed_challenge_selection_1d(
  nuisance_development_ledger,
  re_model = re_model,
  require_positive_inventory = TRUE
)
confirmation_selection_rows <- build_revealed_challenge_selection_1d(
  confirmation_ledger,
  re_model = re_model,
  require_positive_inventory = TRUE
)
use_empirical_prior <- identical(profile$prior_family, "empirical_binned")
development_prior_rows <- if (use_empirical_prior) {
  build_revealed_empirical_margin_prior_rows_1d(
    nuisance_development_ledger, require_abs_eligible = TRUE
  )
} else {
  build_revealed_challenge_prior_1d(nuisance_development_ledger)
}
confirmation_prior_rows <- if (use_empirical_prior) {
  NULL
} else {
  build_revealed_challenge_prior_1d(confirmation_ledger)
}

message("Cross-fitting development selection widths and margin priors")
development_selection_cv <- crossfit_revealed_challenge_selection_1d(
  development_selection_rows,
  fold_assignment = fold_assignment,
  folds = profile$folds,
  seed = seed,
  nthreads = profile$nthreads,
  keep_models = FALSE,
  progress = TRUE
)
empirical_alpha_selection <- NULL
selected_prior_alpha <- NULL
if (use_empirical_prior) {
  message(
    "Selecting empirical role/count shrinkage by development-game OOF CRPS"
  )
  empirical_alpha_selection <- load_or_compute_stage(
    "empirical_prior_alpha_selection",
    list(
      prior_rows_sha256 = fixed_clock_hash_object_1d(
        development_prior_rows[, .(
          game_pk, pitch_order, role, count_state_original,
          edge_distance_inches
        )]
      ),
      fold_assignment = fold_assignment,
      alpha_grid = profile$empirical_alpha_grid,
      bin_width_inches = profile$empirical_bin_width
    ),
    function() {
      value <- select_revealed_empirical_margin_alpha_1d(
        development_prior_rows,
        fold_assignment = fold_assignment,
        folds = profile$folds,
        seed = seed,
        alpha_grid = profile$empirical_alpha_grid,
        bin_width_inches = profile$empirical_bin_width
      )
      # Retain game-level selection evidence, not the much larger row-level
      # score table, in the content-addressed stage checkpoint.
      value$scores <- NULL
      value
    }
  )
  selected_prior_alpha <- empirical_alpha_selection$selected_alpha[
    revealed_challenge_prior_1d_roles()
  ]
  if (anyNA(selected_prior_alpha)) {
    stop("Empirical shrinkage selection omitted an offense/defense role")
  }
  development_prior_cv <- crossfit_revealed_empirical_margin_prior_1d(
    development_prior_rows,
    fold_assignment = fold_assignment,
    folds = profile$folds,
    seed = seed,
    alpha = selected_prior_alpha,
    bin_width_inches = profile$empirical_bin_width,
    progress = TRUE
  )
} else {
  development_prior_cv <- crossfit_revealed_challenge_prior_1d(
    development_prior_rows,
    fold_assignment = fold_assignment,
    folds = profile$folds,
    inner_folds = profile$inner_folds,
    seed = seed,
    components = profile$prior_components,
    progress = TRUE
  )
}

message("Refitting selected nuisance models on the development sample")
if (use_empirical_prior) {
  full_prior_results <- NULL
  full_prior_fits <- fit_revealed_empirical_margin_priors_1d(
    development_prior_rows,
    alpha = selected_prior_alpha,
    bin_width_inches = profile$empirical_bin_width,
    fold_id = "full_development"
  )
  selected_components <- NULL
  prior_model_spec <- list(
    family = empirical_challenge_margin_prior_1d_type(),
    context = "role by raw baseball count",
    eligible_rows = "tracked structural ABS-eligible adverse-call opportunities",
    bin_width_inches = profile$empirical_bin_width,
    selected_alpha = selected_prior_alpha,
    alpha_grid = profile$empirical_alpha_grid,
    alpha_selection = empirical_alpha_selection$selection_rule,
    action_outcome_columns_used = character()
  )
} else {
  full_prior_results <- lapply(
    revealed_challenge_prior_1d_roles(),
    function(role_value) {
      fit_revealed_challenge_prior_outer_fold_1d(
        development_prior_rows,
        confirmation_prior_rows,
        role = role_value,
        outer_fold = 0L,
        components = profile$prior_components,
        inner_folds = profile$inner_folds,
        seed = seed,
        context_prior_strength = 100,
        tolerance = 1e-4,
        max_iterations = 500L
      )
    }
  )
  names(full_prior_results) <- revealed_challenge_prior_1d_roles()
  full_prior_fits <- lapply(full_prior_results, `[[`, "primary_fit")
  selected_components <- stats::setNames(vapply(
    full_prior_results,
    function(value) as.integer(value$diagnostic$selected_components[[1L]]),
    integer(1L)
  ), names(full_prior_results))
  prior_model_spec <- list(
    family = "gaussian_mixture",
    context = "role by count and pitch family",
    selected_components = selected_components,
    context_prior_strength = 100
  )
}

full_selection_results <- lapply(
  revealed_challenge_selection_1d_roles(),
  function(role_value) {
    fit_revealed_challenge_selection_role_fold_1d(
      development_selection_rows,
      confirmation_selection_rows,
      role = role_value,
      local_margin_limit_inches = 3,
      nthreads = profile$nthreads,
      keep_models = TRUE
    )
  }
)
names(full_selection_results) <- revealed_challenge_selection_1d_roles()
full_widths <- data.table::rbindlist(lapply(
  names(full_selection_results), function(role_value) {
    diagnostic <- full_selection_results[[role_value]]$diagnostics
    data.table::data.table(
      role = role_value,
      sigma_inches = diagnostic$effective_width_inches,
      margin_probit_slope = diagnostic$local_margin_probit_slope,
      training_rows = diagnostic$training_rows_local,
      training_challenges = diagnostic$training_challenges_local
    )
  }
))
effective_width <- stats::setNames(
  full_widths$sigma_inches[
    match(fixed_clock_direct_policy_roles_1d(), full_widths$role)
  ],
  fixed_clock_direct_policy_roles_1d()
)

prepare_fixed_clock_opportunities_for_prior <- function(ledger, re_value) {
  out <- prepare_revealed_challenge_policy_opportunities_1d(ledger, re_value)
  if (use_empirical_prior) {
    out <- route_revealed_empirical_count_context_1d(out)
  }
  out
}

message("Building public development and confirmation opportunity clocks")
development_opportunities <- prepare_fixed_clock_opportunities_for_prior(
  development_ledger, re_model
)
confirmation_opportunities <- prepare_fixed_clock_opportunities_for_prior(
  confirmation_ledger, re_model
)
development_clock <- fixed_clock_policy_clock_1d(development_opportunities)
confirmation_clock <- fixed_clock_policy_clock_1d(confirmation_opportunities)
confirmation_truth <- fixed_clock_policy_truth_1d(confirmation_opportunities)
confirmation_observed <- fixed_clock_policy_observed_actions_1d(
  confirmation_opportunities
)
confirmation_expectations <-
  fixed_clock_confirmation_snapshot_expectations_1d()[
    partition == "confirmation"
  ]
if (nrow(confirmation_clock) !=
      confirmation_expectations$eligible_clock_pitches ||
    sum(confirmation_observed$observed_challenge) !=
      confirmation_expectations$observed_challenges) {
  stop(
    "Confirmation structural clock does not reproduce ",
    format(confirmation_expectations$eligible_clock_pitches, big.mark = ","),
    " rows/",
    format(confirmation_expectations$observed_challenges, big.mark = ","),
    " attempts", call. = FALSE
  )
}

policy_development_games <- development_ledger[, .(
  game_date = min(as.Date(game_date))
), by = .(game_pk = as.character(game_pk))][order(game_date, game_pk)]$game_pk
if (is.finite(profile$policy_game_limit)) {
  policy_development_games <- head(
    policy_development_games, as.integer(profile$policy_game_limit)
  )
}
policy_development_clock <- development_clock[
  game_pk %in% policy_development_games
]
policy_development_opportunities <- development_opportunities[
  as.character(game_pk) %in% policy_development_games
]
if (!nrow(policy_development_clock)) {
  stop("The policy-development clock is empty", call. = FALSE)
}

if (isTRUE(profile$run_policy_cv)) {
  policy_cv_tuning_grid <- if (identical(profile$policy_cv_grid, "fixed")) {
    data.table::data.table(
      stage_df = as.integer(profile$fixed_stage_df),
      ridge = as.numeric(profile$fixed_ridge)
    )
  } else {
    profile$tuning_grid
  }
  message(
    "Selecting direct-policy smoothing on ", profile$folds,
    " development game folds using ", nrow(policy_cv_tuning_grid),
    " prespecified tuning setting(s)"
  )
  direct_cv <- cross_validate_fixed_clock_direct_policy_1d(
    policy_development_clock,
    fold_assignment = fold_assignment[game_pk %in% policy_development_games],
    fold_prior_fits = development_prior_cv$primary_fits,
    width_estimates = development_selection_cv$width_estimates,
    scenarios = profile$scenarios,
    tuning_grid = policy_cv_tuning_grid,
    lookup_grid_step = profile$lookup_grid_step,
    optimizer_control = profile$cv_optimizer_control,
    workers = profile$candidate_workers,
    checkpoint_dir = file.path(output_directory, "direct_cv_checkpoints"),
    checkpoint_key = paste0(
      "direct_cv_", run_configuration_sha256
    ),
    resume = TRUE,
    progress = TRUE
  )
} else {
  message(
    "Using the prespecified fixed smoothing setting: stage_df=",
    profile$fixed_stage_df, ", ridge=", profile$fixed_ridge
  )
  selected_setting <- data.table::data.table(
    stage_df = as.integer(profile$fixed_stage_df),
    ridge = as.numeric(profile$fixed_ridge)
  )
  selected_setting[, tuning_id := sprintf(
    "df_%s__ridge_%0.8g", stage_df, ridge
  )]
  direct_cv <- list(
    selected = selected_setting[],
    summary = selected_setting[, .(
      tuning_id, stage_df, ridge,
      mean_worst_scenario_re_per_team_game = NA_real_,
      se_worst_scenario_re_per_team_game = NA_real_,
      mean_scenario_re_per_team_game = NA_real_,
      folds = 0L,
      within_one_se = NA,
      selected = TRUE,
      profile_note = paste(
        "development-only smoothing setting prespecified through the runner;",
        "policy cross-validation skipped"
      )
    )],
    fold_scores = data.table::data.table(),
    scenario_scores = data.table::data.table(),
    fold_assignment = fold_assignment[game_pk %in% policy_development_games],
    selection_rule = paste(
      "prespecified fixed setting; set ABS_FIXED_CLOCK_POLICY_CV=true",
      "to rerun development-only OOF selection"
    )
  )
}
selected_stage_df <- as.integer(direct_cv$selected$stage_df[[1L]])
selected_ridge <- as.numeric(direct_cv$selected$ridge[[1L]])

warm_start <- list(
  parameters = NULL,
  provenance = list(
    used = FALSE,
    file_sha256 = warm_start_file_sha256,
    source_policy_sha256 = NA_character_,
    parameter_sha256 = NA_character_,
    scenarios = 0L
  )
)
if (nzchar(warm_start_policy_path)) {
  source_frozen_policy <- readRDS(warm_start_policy_path)
  validate_frozen_fixed_clock_policy_1d(source_frozen_policy)
  source_policy <- .fixed_clock_direct_restore_policy_1d(
    source_frozen_policy
  )
  if (length(intersect(
    as.character(source_frozen_policy$training_game_ids), confirmation_games
  ))) {
    stop("Warm-start policy includes confirmation games", call. = FALSE)
  }
  shared_scenarios <- intersect(
    profile$scenarios$scenario_id, names(source_policy$candidate_fits)
  )
  if (!length(shared_scenarios)) {
    stop("Warm-start policy has no matching scenarios", call. = FALSE)
  }
  warm_start$parameters <- stats::setNames(lapply(
    shared_scenarios,
    function(id) source_policy$candidate_fits[[id]]$parameter
  ), shared_scenarios)
  warm_start$provenance <- list(
    used = TRUE,
    file_sha256 = warm_start_file_sha256,
    source_policy_sha256 = source_frozen_policy$policy_sha256,
    parameter_sha256 = fixed_clock_hash_object_1d(warm_start$parameters),
    scenarios = length(shared_scenarios),
    role = "optimization warm start only; objective fully reoptimized"
  )
  rm(source_frozen_policy, source_policy)
  message(
    "Loaded ", length(shared_scenarios),
    " development-policy parameter warm starts"
  )
}

message("Fitting the all-development maximin direct policy")
direct_fit <- load_or_compute_stage(
  "all_development_direct_fit",
  list(
    clock_sha256 = fixed_clock_hash_object_1d(policy_development_clock),
    prior_sha256 = fixed_clock_hash_object_1d(full_prior_fits),
    scenarios = profile$scenarios,
    effective_width = effective_width,
    stage_df = selected_stage_df,
    ridge = selected_ridge,
    lookup_grid_step = profile$lookup_grid_step,
    optimizer_control = profile$optimizer_control,
    warm_start = warm_start$provenance
  ),
  function() fit_fixed_clock_direct_policy_1d(
    policy_development_clock,
    prior_fits = full_prior_fits,
    scenarios = profile$scenarios,
    effective_width = effective_width,
    compliance = "perfect",
    stage_df = selected_stage_df,
    ridge = selected_ridge,
    initial_inventory = 2L,
    lookup_grid_step = profile$lookup_grid_step,
    optimizer_control = profile$optimizer_control,
    initial_parameters = warm_start$parameters,
    workers = final_candidate_workers,
    progress = TRUE
  )
)
primary_re_hash <- fixed_clock_re288_hash_1d(re_model)
development_prior_hash_rows <- if (use_empirical_prior) {
  development_prior_rows[, .(
    game_pk, pitch_order, role,
    signed_margin_inches = edge_distance_inches,
    raw_count_state = count_state_original,
    abs_eligible
  )]
} else {
  development_prior_rows[, .(
    game_pk, pitch_order, role,
    signed_margin_inches = edge_distance_inches,
    context_count_family
  )]
}
development_hashes <- c(
  split = split$split_sha256,
  policy_development_clock = fixed_clock_hash_object_1d(
    policy_development_clock
  ),
  development_prior = fixed_clock_hash_object_1d(
    development_prior_hash_rows
  ),
  development_selection = fixed_clock_hash_object_1d(
    development_selection_rows[, .(
      game_pk, pitch_order, role, challenged, role_margin_inches
    )]
  )
)
information_allowlist <- c(
  "game_pk", "team_id", "pitch_order", "inning", "stage", "role",
  "count_state", "stake_G", "decision_mode"
)
frozen_policy <- freeze_fixed_clock_policy_1d(
  direct_fit,
  training_game_ids = nuisance_development_games,
  cutoff = split$cutoff,
  metadata = list(
    policy_family = "robust_signal_assisted_fixed_clock",
    objective = "expected modeled RE captured on the factual opportunity clock",
    state_basis = direct_fit$basis_spec,
    coefficient_candidate = direct_fit$selected_candidate_id,
    inventory_loss_definition = "L1=softplus(k2 surface)+softplus(gap surface); L2=softplus(k2 surface)",
    q_star_definition = "L/(G_decision+L) for G_decision>0 and k>0",
    primary_decision_re288_sha256 = primary_re_hash,
    margin_prior = prior_model_spec,
    scenario_grid = direct_fit$scenarios,
    development_hashes = development_hashes,
    direct_training_game_sha256 = fixed_clock_hash_object_1d(
      direct_fit$training_games
    ),
    nuisance_training_game_sha256 = fixed_clock_hash_object_1d(
      nuisance_development_games
    ),
    optimization_diagnostics = list(
      cross_validation = direct_cv$summary,
      candidate_summary = direct_fit$candidate_summary,
      candidate_optimization = direct_fit$candidate_optimization,
      robust_refinement = direct_fit$robust_refinement,
      warm_start = warm_start$provenance
    ),
    confirmation_status = paste(
      "temporally held-out from all fitted coefficients and nuisance models;",
      "methodological reanalysis after the earlier GMM confirmation run had",
      "begun, so not a pristine first-look confirmation"
    ),
    information_allowlist = information_allowlist,
    forbidden = fixed_clock_policy_forbidden_columns_1d()
  )
)
validate_frozen_fixed_clock_policy_1d(frozen_policy)

frozen_policy_path <- file.path(
  output_directory,
  paste0("frozen_policy_", frozen_policy$policy_sha256, ".rds")
)
write_immutable_rds(frozen_policy, frozen_policy_path)
write_immutable_rds(
  frozen_policy, file.path(output_directory, "frozen_policy.rds")
)

message("Fitting public-only and Bellman structural benchmarks")
public_fit <- load_or_compute_stage(
  "all_development_public_fit",
  list(
    clock_sha256 = fixed_clock_hash_object_1d(policy_development_clock),
    prior_sha256 = fixed_clock_hash_object_1d(full_prior_fits),
    stage_df = selected_stage_df,
    ridge = selected_ridge,
    optimizer_control = profile$optimizer_control
  ),
  function() fit_fixed_clock_public_policy_1d(
    policy_development_clock,
    prior_fits = full_prior_fits,
    stage_df = selected_stage_df,
    ridge = selected_ridge,
    optimizer_control = profile$optimizer_control
  )
)
frozen_public <- freeze_fixed_clock_policy_1d(
  public_fit,
  training_game_ids = nuisance_development_games,
  cutoff = split$cutoff,
  metadata = list(
    policy_family = "public_information_only",
    primary_decision_re288_sha256 = primary_re_hash,
    information_allowlist = information_allowlist
  )
)
bellman_policies <- if (isTRUE(profile$fit_bellman)) {
  load_or_compute_stage(
    "all_development_bellman_fit",
    list(
      opportunities_sha256 = fixed_clock_hash_object_1d(
        policy_development_opportunities
      ),
      prior_sha256 = fixed_clock_hash_object_1d(full_prior_fits),
      scenarios = profile$scenarios,
      effective_width = effective_width,
      max_iter = profile$bellman_max_iterations,
      lookup_grid_step = profile$bellman_lookup_grid_step
    ),
    function() fit_fixed_clock_bellman_policies_1d(
      policy_development_opportunities,
      prior_fits = full_prior_fits,
      scenarios = profile$scenarios,
      effective_width = effective_width,
      max_iter = profile$bellman_max_iterations,
      lookup_grid_step = profile$bellman_lookup_grid_step,
      progress = TRUE
    )
  )
} else {
  NULL
}

message("Scoring fitted human behavior on confirmation geometry")
confirmation_policy_rows <- build_revealed_challenge_selection_1d(
  confirmation_ledger,
  re_model = re_model,
  require_positive_inventory = FALSE
)
confirmation_model_bundle <- list(
  fold_assignment = data.table::data.table(
    game_pk = confirmation_games, fold = 1L
  ),
  gate = development_selection_cv$gate,
  models = list(stats::setNames(lapply(
    revealed_challenge_selection_1d_roles(),
    function(role_value) {
      value <- full_selection_results[[role_value]]
      list(full = value$full_model, local = value$local_model)
    }
  ), revealed_challenge_selection_1d_roles()))
)
confirmation_fitted <- score_revealed_challenge_selection_policy_clock_1d(
  confirmation_policy_rows, confirmation_model_bundle
)

message("Evaluating frozen policies on quarantined confirmation geometry")
direct_evaluation <- evaluate_fixed_clock_policy_1d(
  frozen_policy,
  confirmation_clock,
  confirmation_truth,
  scenario_ids = profile$scenarios$scenario_id
)
arrow::write_parquet(
  direct_evaluation$policy_actions,
  file.path(output_directory, "confirmation_direct_policy_actions.parquet")
)
arrow::write_parquet(
  direct_evaluation$replay,
  file.path(output_directory, "confirmation_direct_replay.parquet")
)
direct_noisy_evaluation <- evaluate_fixed_clock_policy_1d(
  frozen_policy,
  confirmation_clock,
  confirmation_truth,
  scenario_ids = profile$scenarios$scenario_id,
  compliance_override = "noisy",
  evaluation_mode = "sensitivity",
  override_provenance = list(
    label = "action-noise practical-compliance sensitivity"
  ),
  return_level = "game_role"
)
public_evaluation <- evaluate_fixed_clock_public_policy_1d(
  frozen_public, confirmation_clock, confirmation_truth
)
comparator_evaluation <- evaluate_fixed_clock_comparators_1d(
  confirmation_clock,
  confirmation_truth,
  confirmation_observed,
  fitted_probability_rows = confirmation_fitted
)
bellman_evaluation <- if (!is.null(bellman_policies)) {
  evaluate_fixed_clock_bellman_policies_1d(
    bellman_policies,
    confirmation_clock,
    confirmation_truth,
    scenario_ids = profile$scenarios$scenario_id,
    return_level = "game_role"
  )
} else {
  NULL
}

add_combined_game_roles <- function(values) {
  x <- data.table::copy(data.table::as.data.table(values))
  measures <- c(
    "captured_re", "attempts", "successes", "failures",
    "exhausted_opportunity_mass", "opportunity_exposure"
  )
  group <- c("game_pk", "team_id", "policy", "policy_family", "scenario_id")
  combined <- x[, lapply(.SD, sum), by = group, .SDcols = measures]
  combined[, role := "combined"]
  data.table::rbindlist(list(x, combined), use.names = TRUE, fill = TRUE)
}

assemble_game_scores <- function(
  direct_value,
  public_value,
  comparator_value,
  bellman_value = NULL,
  procedure_parameter = NULL,
  nuisance_override = NULL,
  evaluation_gain_rows = NULL,
  procedure_override_provenance = NULL,
  procedure_lookup_cache = NULL
) {
  scenario_ids <- profile$scenarios$scenario_id
  direct_game <- data.table::copy(direct_value$game_role)
  direct_game[, `:=`(
    scenario_id = sub("^fixed_clock__", "", policy),
    policy = "robust_signal_assisted"
  )]
  if (!is.null(procedure_parameter)) {
    procedure <- evaluate_fixed_clock_policy_1d(
      frozen_policy,
      confirmation_clock,
      confirmation_truth,
      scenario_ids = scenario_ids,
      evaluation_gain_rows = evaluation_gain_rows,
      prior_fits_override = nuisance_override$prior_fits,
      scenarios_override = profile$scenarios,
      effective_width_override = nuisance_override$effective_width,
      parameter_override = procedure_parameter,
      evaluation_mode = "bootstrap_procedure",
      override_provenance = procedure_override_provenance,
      return_level = "game_role",
      lookup_cache = procedure_lookup_cache
    )$game_role
    procedure[, `:=`(
      scenario_id = sub("^fixed_clock__", "", policy),
      policy = "direct_learning_procedure"
    )]
    direct_game <- data.table::rbindlist(
      list(direct_game, procedure), use.names = TRUE
    )
  }
  public_game <- data.table::rbindlist(lapply(scenario_ids, function(id) {
    out <- data.table::copy(public_value$game_role)
    out[, `:=`(scenario_id = id, policy = "public_information_only")]
    out
  }))
  comparator_game <- data.table::rbindlist(lapply(scenario_ids, function(id) {
    out <- data.table::copy(comparator_value$game_role)
    out[, scenario_id := id]
    out
  }))
  pieces <- list(direct_game, public_game, comparator_game)
  if (!is.null(bellman_value)) {
    bellman_game <- data.table::copy(bellman_value$game_role)
    bellman_game[, `:=`(
      scenario_id = sub("^bellman__", "", policy),
      policy = "bellman_structural"
    )]
    pieces[[length(pieces) + 1L]] <- bellman_game
  }
  all <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  all <- add_combined_game_roles(all)
  observed <- all[policy == "observed", .(
    game_pk, team_id, role, scenario_id, observed_re = captured_re
  )]
  oracle <- all[policy == "exact_location_oracle", .(
    game_pk, team_id, role, scenario_id, oracle_re = captured_re
  )]
  all <- merge(
    all, observed,
    by = c("game_pk", "team_id", "role", "scenario_id"),
    all.x = TRUE, sort = FALSE
  )
  all <- merge(
    all, oracle,
    by = c("game_pk", "team_id", "role", "scenario_id"),
    all.x = TRUE, sort = FALSE
  )
  if (anyNA(all[, .(observed_re, oracle_re)])) {
    stop("Observed/oracle references do not cover all policy game scores")
  }
  all[, team_games := 1]
  all[]
}

point_game_scores <- assemble_game_scores(
  direct_evaluation,
  public_evaluation,
  comparator_evaluation,
  bellman_evaluation
)
procedure_point_game_scores <- data.table::copy(
  point_game_scores[policy == "robust_signal_assisted"]
)
procedure_point_game_scores[, policy := "direct_learning_procedure"]
point_game_scores <- data.table::rbindlist(
  list(point_game_scores, procedure_point_game_scores),
  use.names = TRUE, fill = TRUE
)
point_summary <- summarize_fixed_clock_frozen_policy_1d(point_game_scores)
point_summary[, `:=`(
  games = length(confirmation_games),
  captured_re_per_game = captured_re / length(confirmation_games),
  gain_over_observed_re_per_game = gain_over_observed_re /
    length(confirmation_games),
  fraction_observed_to_oracle_gap_closed = ifelse(
    oracle_re > observed_re,
    gain_over_observed_re / (oracle_re - observed_re),
    NA_real_
  )
)]
point_envelope <- fixed_clock_scenario_envelope_1d(
  point_summary,
  expected_scenario_ids = profile$scenarios$scenario_id
)

noisy_game_scores <- assemble_game_scores(
  direct_noisy_evaluation,
  public_evaluation,
  comparator_evaluation
)[policy == "robust_signal_assisted"]
noisy_game_scores[, policy := "robust_signal_assisted_noisy_compliance"]
noisy_compliance_summary <- summarize_fixed_clock_frozen_policy_1d(
  noisy_game_scores
)
noisy_compliance_summary[, `:=`(
  games = length(confirmation_games),
  captured_re_per_game = captured_re / length(confirmation_games),
  gain_over_observed_re_per_game = gain_over_observed_re /
    length(confirmation_games),
  fraction_observed_to_oracle_gap_closed = ifelse(
    oracle_re > observed_re,
    gain_over_observed_re / (oracle_re - observed_re),
    NA_real_
  )
)]

message("Running persistent team-game perception-bias sensitivity")
direct_combined <- point_summary[
  policy == "robust_signal_assisted" & role == "combined"
]
representative_scenarios <- unique(c(
  direct_combined[which.min(gain_over_observed_re), scenario_id],
  "offense_kappa_0.50__defense_kappa_0.50",
  direct_combined[which.max(gain_over_observed_re), scenario_id]
))
representative_scenarios <- intersect(
  representative_scenarios, profile$scenarios$scenario_id
)
bias_sensitivity <- evaluate_fixed_clock_policy_bias_sensitivity_1d(
  frozen_policy,
  confirmation_clock,
  confirmation_truth,
  bias_sd_inches = 0.25 * effective_width,
  scenario_ids = representative_scenarios,
  quadrature_nodes = profile$bias_nodes
)
direct_policy_action_audit <- direct_evaluation$policy_actions[, .(
  game_pk, team_id, pitch_order, evaluation_scenario_id,
  G_decision, inventory_loss_k1, inventory_loss_k2,
  q_star_k1, q_star_k2,
  signal_threshold_k1_inches, signal_threshold_k2_inches
)]
rm(direct_evaluation)
invisible(gc(FALSE))

message("Calculating boundary, shrinkage, and optional contextual RE sensitivities")
boundary_keys <- confirmation_opportunities[
  decision_mode == "structural" & abs(role_margin_inches) > 0.25,
  .(game_pk = as.character(game_pk), team_id = as.character(team_id), pitch_order)
]
boundary_clock <- confirmation_clock[boundary_keys,
  on = .(game_pk, team_id, pitch_order), nomatch = 0L
]
boundary_truth <- confirmation_truth[boundary_keys,
  on = .(game_pk, team_id, pitch_order), nomatch = 0L
]
boundary_evaluation <- evaluate_fixed_clock_policy_1d(
  frozen_policy,
  boundary_clock,
  boundary_truth,
  scenario_ids = representative_scenarios,
  return_level = "game_role"
)

history_observations <- prepare_re_observations(history_statcast)
alternative_re_tables <- stats::setNames(lapply(c(0, 50), function(shrinkage) {
  fixed_clock_re288_weighted_table_1d(
    history_observations, shrinkage = shrinkage
  )
}), c("shrinkage_0", "shrinkage_50"))
alternative_re_scores <- data.table::rbindlist(lapply(
  names(alternative_re_tables), function(label) {
    opportunity <- prepare_fixed_clock_opportunities_for_prior(
      confirmation_ledger,
      list(table = alternative_re_tables[[label]])
    )
    gain <- fixed_clock_policy_clock_1d(opportunity)[, .(
      game_pk, team_id, pitch_order, G_evaluation = stake_G
    )]
    value <- evaluate_fixed_clock_policy_1d(
      frozen_policy,
      confirmation_clock,
      confirmation_truth,
      scenario_ids = representative_scenarios,
      evaluation_gain_rows = gain,
      return_level = "game_role"
    )$season
    value[, re_sensitivity := label]
    value
  }
), use.names = TRUE, fill = TRUE)

context_re_status <- data.table::data.table(
  requested = run_context_re,
  available = all(c("batter", "pitcher", "home_team") %in%
    names(history_statcast)),
  evaluated = FALSE,
  reason = "not requested"
)
context_re_scores <- data.table::data.table()
if (run_context_re) {
  if (context_re_status$available) {
    context_fit <- fit_fixed_clock_context_re_1d(
      history_statcast, nthreads = profile$nthreads
    )
    context_gain_ledger <- fixed_clock_context_pitch_gains_1d(
      confirmation_ledger, context_fit
    )
    context_opportunities <- prepare_fixed_clock_opportunities_for_prior(
      context_gain_ledger, re_model
    )
    context_gain <- fixed_clock_policy_clock_1d(context_opportunities)[, .(
      game_pk, team_id, pitch_order
    )]
    ledger_gain <- context_gain_ledger[, .(
      game_pk = as.character(game_pk),
      pitch_order = as.integer(pitch_order),
      G_context_evaluation
    )]
    context_gain <- merge(
      context_gain, ledger_gain,
      by = c("game_pk", "pitch_order"), all.x = TRUE, sort = FALSE
    )
    context_gain[, G_evaluation := G_context_evaluation]
    context_re_scores <- evaluate_fixed_clock_policy_1d(
      frozen_policy,
      confirmation_clock,
      confirmation_truth,
      scenario_ids = representative_scenarios,
      evaluation_gain_rows = context_gain[, .(
        game_pk, team_id, pitch_order, G_evaluation
      )],
      return_level = "game_role"
    )$season
    context_re_status[, `:=`(evaluated = TRUE, reason = "evaluated")]
  } else {
    context_re_status[, reason := paste(
      "history_statcast target lacks batter/pitcher/home_team; refresh targets",
      "after the extended history reader"
    )]
  }
}

message("Building support, drift, calibration, reconciliation, and concentration diagnostics")
support_diagnostics <- confirmation_policy_rows[, .(
  rows = .N,
  challenges = sum(challenged),
  correctable = sum(role_margin_inches > 0),
  margin_min = min(role_margin_inches),
  margin_max = max(role_margin_inches),
  margin_q01 = stats::quantile(role_margin_inches, 0.01),
  margin_q99 = stats::quantile(role_margin_inches, 0.99)
), by = .(role, count_state)]
temporal_drift <- data.table::rbindlist(list(
  development_selection_rows[, .(
    partition = "development", rows = .N,
    challenge_rate = mean(challenged),
    mean_margin = mean(role_margin_inches),
    sd_margin = stats::sd(role_margin_inches)
  ), by = .(role, count_state)],
  confirmation_selection_rows[, .(
    partition = "confirmation", rows = .N,
    challenge_rate = mean(challenged),
    mean_margin = mean(role_margin_inches),
    sd_margin = stats::sd(role_margin_inches)
  ), by = .(role, count_state)]
), use.names = TRUE)
calibration_diagnostics <- confirmation_fitted[, .(
  rows = .N,
  observed_challenge_rate = mean(challenged),
  predicted_challenge_rate_k1 = mean(probability_k1),
  predicted_challenge_rate_k2 = mean(probability_k2),
  calibration_error_k1 = mean(probability_k1) - mean(challenged),
  calibration_error_k2 = mean(probability_k2) - mean(challenged)
), by = .(role, count_state)]
fallback_diagnostics <- confirmation_fitted[, .(
  rows = .N,
  unit_fallback_rate_k1 = mean(unit_fallback_k1),
  unit_fallback_rate_k2 = mean(unit_fallback_k2),
  team_fallback_rate_k1 = mean(team_fallback_k1),
  team_fallback_rate_k2 = mean(team_fallback_k2)
), by = role]
official <- confirmation_ledger[challenge_occurred %in% TRUE, .(
  recorded_attempts = .N,
  official_resolved = sum(challenge_outcome %in% c("overturned", "upheld")),
  official_successes = sum(challenge_outcome == "overturned", na.rm = TRUE),
  geometry_successes = sum(correctable_opportunity %in% TRUE),
  official_geometry_mismatches = sum(
    challenge_outcome %in% c("overturned", "upheld") &
      (challenge_outcome == "overturned") !=
        (correctable_opportunity %in% TRUE),
    na.rm = TRUE
  )
)]
if (official$recorded_attempts != 2275L) {
  stop("Official confirmation reconciliation does not reproduce 2,275 attempts")
}
direct_game_gain <- point_game_scores[
  policy == "robust_signal_assisted" & role == "combined",
  .(gain = sum(captured_re - observed_re)),
  by = .(scenario_id, game_pk)
]
gain_concentration <- direct_game_gain[order(-gain), {
  positive_total <- sum(pmax(gain, 0))
  .(
    games = .N,
    positive_gain_total = positive_total,
    top_1pct_share = if (positive_total > 0) {
      sum(head(pmax(gain, 0), max(1L, ceiling(.N * 0.01)))) / positive_total
    } else NA_real_,
    top_5pct_share = if (positive_total > 0) {
      sum(head(pmax(gain, 0), max(1L, ceiling(.N * 0.05)))) / positive_total
    } else NA_real_,
    top_10pct_share = if (positive_total > 0) {
      sum(head(pmax(gain, 0), max(1L, ceiling(.N * 0.10)))) / positive_total
    } else NA_real_
  )
}, by = scenario_id]

manifest <- build_fixed_clock_confirmation_manifest_1d(
  split,
  frozen_policy,
  scenario_ids = profile$scenarios$scenario_id,
  source_hashes = c(source_hashes, primary_re288 = primary_re_hash),
  seed = seed,
  metadata = list(
    profile = profile$name,
    run_configuration_sha256 = run_configuration_sha256,
    estimand = paste(
      "expected additional modeled RE captured by one frozen",
      "information-restricted policy on the factual opportunity clock"
    ),
    G_decision = "frozen primary RE288 shrinkage 20",
    G_evaluation = "primary, coordinated historical-game bootstrap, and sensitivities",
    observed_confirmation_attempts = 2275L,
    official_outcomes = "validation only",
    future_context = "excluded",
    confirmation_geometry = "joined after thresholds are frozen"
  )
)
validate_fixed_clock_confirmation_manifest_1d(
  manifest, split, frozen_policy
)
leakage_audit <- audit_fixed_clock_confirmation_leakage_1d(
  split,
  frozen_policy,
  manifest,
  confirmation_actions = direct_policy_action_audit,
  confirmation_truth = confirmation_truth,
  fail = TRUE
)

bootstrap_result <- NULL
bootstrap_intervals <- simultaneous_intervals <- worst_lower_bound <-
  data.table::data.table()
bootstrap_envelopes <- data.table::data.table()
if (profile$bootstrap_reps > 0L) {
  message(
    "Running ", profile$bootstrap_reps,
    " coordinated whole-game bootstrap replicates"
  )
  historical_games <- sort(unique(as.character(history_observations$game_pk)))
  source_game_ids <- list(
    historical_re = historical_games,
    development = development_games,
    confirmation = confirmation_games
  )
  refit_historical <- function(weights, replicate_id, seed, context) {
    table <- fixed_clock_re288_weighted_table_1d(
      context$history_observations,
      game_weights = weights,
      shrinkage = 20
    )
    list(
      table = table,
      table_sha256 = fixed_clock_re288_hash_1d(table)
    )
  }
  refit_development <- function(
    weights, historical_re_fit, replicate_id, seed, context
  ) {
    nuisance <- if (identical(context$prior_family, "empirical_binned")) {
      refit_fixed_clock_empirical_development_nuisance_1d(
        context$development_prior_rows,
        context$development_selection_rows,
        game_weights = weights,
        alpha = context$selected_prior_alpha,
        bin_width_inches = context$empirical_bin_width,
        nthreads = 1L,
        fold_id = paste0("bootstrap_", replicate_id)
      )
    } else {
      refit_fixed_clock_development_nuisance_1d(
        context$development_prior_rows,
        context$development_selection_rows,
        selected_components = context$selected_components,
        game_weights = weights,
        nthreads = 1L,
        fold_id = paste0("bootstrap_", replicate_id)
      )
    }
    lookup_cache <- new.env(parent = emptyenv())
    run_procedure_refit <-
      (as.integer(replicate_id) - 1L) %%
        context$procedure_bootstrap_stride == 0L
    local <- if (run_procedure_refit) {
      bootstrap_clock <- expand_fixed_clock_game_bootstrap_1d(
        context$development_clock, weights
      )
      locally_refit_fixed_clock_direct_policy_1d(
        context$frozen_policy,
        bootstrap_clock,
        prior_fits = nuisance$prior_fits,
        scenarios = context$scenarios,
        effective_width = nuisance$effective_width,
        optimizer_control = context$local_optimizer_control,
        lookup_cache = lookup_cache
      )
    } else {
      NULL
    }
    positive_training_games <- weights[bootstrap_weight > 0, game_pk]
    base_provenance <- list(
      replicate_id = as.integer(replicate_id),
      source_game_ids = as.character(positive_training_games),
      game_weight_sha256 = fixed_clock_hash_object_1d(weights),
      source_sha256 = fixed_clock_hash_object_1d(list(
        prior_rows = context$development_prior_rows,
        selection_rows = context$development_selection_rows,
        clock = context$development_clock
      ))
    )
    nuisance_provenance <- c(base_provenance, list(
      fit_sha256 = fixed_clock_hash_object_1d(list(
        prior_fits = nuisance$prior_fits,
        effective_width = nuisance$effective_width
      ))
    ))
    procedure_provenance <- if (run_procedure_refit) {
      c(base_provenance, list(
        fit_sha256 = fixed_clock_hash_object_1d(list(
          prior_fits = nuisance$prior_fits,
          effective_width = nuisance$effective_width,
          parameter = local$parameter
        ))
      ))
    } else {
      NULL
    }
    c(nuisance, list(
      local_refit = local,
      procedure_refit_included = run_procedure_refit,
      lookup_cache = lookup_cache,
      nuisance_override_provenance = nuisance_provenance,
      procedure_override_provenance = procedure_provenance
    ))
  }
  score_confirmation <- function(
    weights, historical_re_fit, development_fit,
    replicate_id, seed, context
  ) {
    re_opportunity <- prepare_fixed_clock_opportunities_for_prior(
      context$confirmation_ledger, historical_re_fit
    )
    evaluation_gain <- fixed_clock_policy_clock_1d(re_opportunity)[, .(
      game_pk, team_id, pitch_order, G_evaluation = stake_G
    )]
    direct <- evaluate_fixed_clock_policy_1d(
      context$frozen_policy,
      context$confirmation_clock,
      context$confirmation_truth,
      scenario_ids = context$scenarios$scenario_id,
      evaluation_gain_rows = evaluation_gain,
      prior_fits_override = development_fit$prior_fits,
      scenarios_override = context$scenarios,
      effective_width_override = development_fit$effective_width,
      evaluation_mode = "bootstrap_nuisance",
      override_provenance = development_fit$nuisance_override_provenance,
      return_level = "game_role",
      lookup_cache = development_fit$lookup_cache
    )
    public <- evaluate_fixed_clock_public_policy_1d(
      context$frozen_public,
      context$confirmation_clock,
      context$confirmation_truth,
      evaluation_gain_rows = evaluation_gain,
      prior_fits_override = development_fit$prior_fits,
      evaluation_mode = "bootstrap_nuisance",
      override_provenance = development_fit$nuisance_override_provenance
    )
    comparator <- evaluate_fixed_clock_comparators_1d(
      context$confirmation_clock,
      context$confirmation_truth,
      context$confirmation_observed,
      fitted_probability_rows = context$confirmation_fitted,
      evaluation_gain_rows = evaluation_gain
    )
    bellman <- if (is.null(context$bellman_policies)) NULL else {
      evaluate_fixed_clock_bellman_policies_1d(
        context$bellman_policies,
        context$confirmation_clock,
        context$confirmation_truth,
        scenario_ids = context$scenarios$scenario_id,
        evaluation_gain_rows = evaluation_gain,
        return_level = "game_role"
      )
    }
    procedure <- if (isTRUE(development_fit$procedure_refit_included)) {
      evaluate_fixed_clock_policy_1d(
        context$frozen_policy,
        context$confirmation_clock,
        context$confirmation_truth,
        scenario_ids = context$scenarios$scenario_id,
        evaluation_gain_rows = evaluation_gain,
        prior_fits_override = development_fit$prior_fits,
        scenarios_override = context$scenarios,
        effective_width_override = development_fit$effective_width,
        parameter_override = development_fit$local_refit$parameter,
        evaluation_mode = "bootstrap_procedure",
        override_provenance =
          development_fit$procedure_override_provenance,
        return_level = "game_role",
        lookup_cache = development_fit$lookup_cache
      )$game_role
    } else {
      NULL
    }
    summarize_fixed_clock_bootstrap_evaluations_1d(
      direct_game_role = direct$game_role,
      public_game_role = public$game_role,
      comparator_game_role = comparator$game_role,
      bellman_game_role = if (is.null(bellman)) NULL else bellman$game_role,
      procedure_game_role = procedure,
      confirmation_weights = weights,
      scenario_ids = context$scenarios$scenario_id
    )
  }
  bootstrap_context <- list(
    history_observations = history_observations,
    development_prior_rows = development_prior_rows,
    development_selection_rows = development_selection_rows,
    development_clock = development_clock,
    confirmation_ledger = confirmation_ledger,
    confirmation_clock = confirmation_clock,
    confirmation_truth = confirmation_truth,
    confirmation_observed = confirmation_observed,
    confirmation_fitted = confirmation_fitted,
    prior_family = profile$prior_family,
    selected_prior_alpha = selected_prior_alpha,
    empirical_bin_width = profile$empirical_bin_width,
    selected_components = selected_components,
    frozen_policy = frozen_policy,
    frozen_public = frozen_public,
    bellman_policies = bellman_policies,
    scenarios = profile$scenarios,
    local_optimizer_control = profile$local_optimizer_control,
    procedure_bootstrap_stride = profile$procedure_bootstrap_stride
  )
  checkpoint_provenance <- list(
    schema = "fixed_clock_bootstrap_callbacks_v3",
    run_configuration_sha256 = run_configuration_sha256,
    confirmation_manifest_sha256 = manifest$manifest_sha256,
    target_metadata_sha256 = fixed_clock_hash_object_1d(target_metadata),
    frozen_policy_sha256 = frozen_policy$policy_sha256,
    frozen_public_policy_sha256 = frozen_public$policy_sha256,
    bellman_policy_sha256 = if (is.null(bellman_policies)) {
      "not_fit"
    } else {
      fixed_clock_hash_object_1d(bellman_policies)
    },
    confirmation_clock_sha256 = fixed_clock_hash_object_1d(confirmation_clock),
    confirmation_truth_sha256 = fixed_clock_hash_object_1d(confirmation_truth),
    confirmation_observed_sha256 = fixed_clock_hash_object_1d(
      confirmation_observed
    ),
    confirmation_fitted_sha256 = fixed_clock_hash_object_1d(
      confirmation_fitted
    ),
    scenario_grid_sha256 = fixed_clock_hash_object_1d(profile$scenarios),
    prior_model_spec = prior_model_spec,
    effective_width = effective_width,
    optimizer_control = profile$local_optimizer_control,
    procedure_bootstrap_stride = profile$procedure_bootstrap_stride,
    score_storage = "summary_only_after_complete_game_path_weighting"
  )
  checkpoint_key <- paste0(
    "fixed_clock_",
    fixed_clock_hash_object_1d(checkpoint_provenance)
  )
  replicate_checkpoint_directory <- file.path(
    checkpoint_directory, checkpoint_key
  )
  bootstrap_result <- bootstrap_fixed_clock_policy_1d(
    source_game_ids,
    refit_historical,
    refit_development,
    score_confirmation,
    reps = profile$bootstrap_reps,
    seed = seed,
    context = bootstrap_context,
    checkpoint_dir = replicate_checkpoint_directory,
    checkpoint_key = checkpoint_key,
    resume = TRUE,
    workers = profile$workers
  )
  draws <- bootstrap_result$results
  bootstrap_intervals <- draws[, .(
    bootstrap_replicates = data.table::uniqueN(replicate),
    captured_re_lower_95 = stats::quantile(captured_re, 0.025),
    captured_re_upper_95 = stats::quantile(captured_re, 0.975),
    gain_lower_95 = stats::quantile(gain_over_observed_re, 0.025),
    gain_upper_95 = stats::quantile(gain_over_observed_re, 0.975),
    oracle_share_lower_95 = stats::quantile(share_of_oracle, 0.025),
    oracle_share_upper_95 = stats::quantile(share_of_oracle, 0.975)
  ), by = .(policy, scenario_id, role)]
  bootstrap_envelopes <- fixed_clock_scenario_envelope_1d(
    draws,
    replicate_column = "replicate",
    expected_scenario_ids = profile$scenarios$scenario_id
  )
  procedure_policy <- "direct_learning_procedure"
  interval_parts <- list(
    list(
      point = point_summary[policy != procedure_policy],
      draws = draws[policy != procedure_policy]
    )
  )
  if (nrow(draws[policy == procedure_policy])) {
    interval_parts[[2L]] <- list(
      point = point_summary[policy == procedure_policy],
      draws = draws[policy == procedure_policy]
    )
  }
  simultaneous_intervals <- data.table::rbindlist(lapply(
    interval_parts,
    function(value) summarize_fixed_clock_simultaneous_intervals_1d(
      value$point,
      value$draws,
      expected_scenario_ids = profile$scenarios$scenario_id
    )
  ), use.names = TRUE, fill = TRUE)
  worst_lower_bound <- data.table::rbindlist(lapply(
    interval_parts,
    function(value) fixed_clock_worst_scenario_lower_bound_1d(
      value$draws,
      expected_scenario_ids = profile$scenarios$scenario_id
    )
  ), use.names = TRUE, fill = TRUE)
}

message("Writing confirmation artifacts")
write_immutable_rds(
  manifest, file.path(output_directory, "confirmation_manifest.rds")
)
write_immutable_rds(
  frozen_public, file.path(output_directory, "frozen_public_policy.rds")
)
if (!is.null(bellman_policies)) {
  write_immutable_rds(
    bellman_policies,
    file.path(output_directory, "bellman_structural_comparison.rds")
  )
}
arrow::write_parquet(
  point_game_scores,
  file.path(output_directory, "confirmation_game_values.parquet")
)
arrow::write_parquet(
  noisy_game_scores,
  file.path(output_directory, "noisy_compliance_game_values.parquet")
)
arrow::write_parquet(
  bias_sensitivity$game_role,
  file.path(output_directory, "persistent_bias_sensitivity.parquet")
)
arrow::write_parquet(
  boundary_evaluation$game_role,
  file.path(output_directory, "boundary_exclusion_sensitivity.parquet")
)
data.table::fwrite(
  point_summary,
  file.path(output_directory, "compact_reporting_table.csv")
)
data.table::fwrite(
  point_envelope,
  file.path(output_directory, "partial_identification_envelope.csv")
)
data.table::fwrite(
  noisy_compliance_summary,
  file.path(output_directory, "noisy_compliance_summary.csv")
)
data.table::fwrite(
  direct_cv$summary,
  file.path(output_directory, "direct_policy_cross_validation.csv")
)
data.table::fwrite(
  direct_fit$candidate_summary,
  file.path(output_directory, "direct_policy_candidate_diagnostics.csv")
)
data.table::fwrite(
  direct_fit$candidate_optimization,
  file.path(output_directory, "direct_policy_optimizer_diagnostics.csv")
)
data.table::fwrite(
  full_widths,
  file.path(output_directory, "effective_widths.csv")
)
if (use_empirical_prior) {
  data.table::fwrite(
    empirical_alpha_selection$metrics,
    file.path(output_directory, "empirical_prior_alpha_selection.csv")
  )
  empirical_prior_support <- data.table::rbindlist(lapply(
    names(full_prior_fits),
    function(role_value) {
      fit <- full_prior_fits[[role_value]]
      data.table::data.table(
        role = role_value,
        count_state = names(fit$context_exposure),
        opportunities = as.integer(fit$context_exposure),
        bins = fit$components,
        bin_width_inches = fit$bin_width_inches,
        selected_alpha = fit$context_prior_strength
      )
    }
  ))
  data.table::fwrite(
    empirical_prior_support,
    file.path(output_directory, "empirical_prior_support.csv")
  )
}
data.table::fwrite(
  support_diagnostics,
  file.path(output_directory, "support_diagnostics.csv")
)
data.table::fwrite(
  temporal_drift,
  file.path(output_directory, "temporal_drift.csv")
)
data.table::fwrite(
  calibration_diagnostics,
  file.path(output_directory, "role_count_calibration.csv")
)
data.table::fwrite(
  fallback_diagnostics,
  file.path(output_directory, "fallback_rates.csv")
)
data.table::fwrite(
  gain_concentration,
  file.path(output_directory, "gain_concentration.csv")
)
data.table::fwrite(
  official,
  file.path(output_directory, "official_geometry_reconciliation.csv")
)
data.table::fwrite(
  leakage_audit,
  file.path(output_directory, "leakage_diagnostics.csv")
)
data.table::fwrite(
  alternative_re_scores,
  file.path(output_directory, "alternative_re_shrinkage.csv")
)
data.table::fwrite(
  context_re_status,
  file.path(output_directory, "context_adjusted_re_status.csv")
)
if (nrow(context_re_scores)) data.table::fwrite(
  context_re_scores,
  file.path(output_directory, "context_adjusted_re_sensitivity.csv")
)
if (!is.null(bootstrap_result)) {
  arrow::write_parquet(
    bootstrap_result$results,
    file.path(output_directory, "coordinated_bootstrap_draws.parquet")
  )
  data.table::fwrite(
    bootstrap_result$plan,
    file.path(output_directory, "coordinated_bootstrap_plan.csv")
  )
  data.table::fwrite(
    bootstrap_result$timing,
    file.path(output_directory, "coordinated_bootstrap_timing.csv")
  )
  data.table::fwrite(
    bootstrap_intervals,
    file.path(output_directory, "scenario_percentile_intervals.csv")
  )
  data.table::fwrite(
    bootstrap_envelopes,
    file.path(output_directory, "bootstrap_scenario_envelopes.csv")
  )
  data.table::fwrite(
    simultaneous_intervals,
    file.path(output_directory, "simultaneous_scenario_intervals.csv")
  )
  data.table::fwrite(
    worst_lower_bound,
    file.path(output_directory, "worst_scenario_one_sided_lower_bound.csv")
  )
}

run_manifest <- list(
  schema = "fixed_clock_confirmation_run_v2",
  run_configuration_sha256 = run_configuration_sha256,
  run_id = basename(output_directory),
  profile = profile$name,
  split_sha256 = split$split_sha256,
  frozen_policy_sha256 = frozen_policy$policy_sha256,
  confirmation_manifest_sha256 = manifest$manifest_sha256,
  primary_re288_sha256 = primary_re_hash,
  development_games = length(development_games),
  nuisance_development_games = length(nuisance_development_games),
  direct_policy_search_games = length(direct_fit$training_games),
  confirmation_games = length(confirmation_games),
  confirmation_attempts = sum(confirmation_observed$observed_challenge),
  scenarios = profile$scenarios$scenario_id,
  bootstrap_reps = profile$bootstrap_reps,
  learning_procedure_bootstrap_reps = if (profile$bootstrap_reps > 0L) {
    length(seq.int(
      1L, profile$bootstrap_reps,
      by = profile$procedure_bootstrap_stride
    ))
  } else {
    0L
  },
  learning_procedure_bootstrap_stride = profile$procedure_bootstrap_stride,
  learning_procedure_bootstrap_ids = if (profile$bootstrap_reps > 0L) {
    seq.int(
      1L, profile$bootstrap_reps,
      by = profile$procedure_bootstrap_stride
    )
  } else {
    integer()
  },
  candidate_workers = profile$candidate_workers,
  final_candidate_workers = final_candidate_workers,
  workers = profile$workers,
  margin_prior = prior_model_spec,
  confirmation_status = paste(
    "temporally held-out from all fitted coefficients and nuisance models;",
    "methodological reanalysis after prior confirmation processing, not a",
    "pristine first-look confirmation"
  ),
  artifacts = list(
    bellman_structural_comparison = !is.null(bellman_policies),
    context_adjusted_re = nrow(context_re_scores) > 0L,
    coordinated_bootstrap = !is.null(bootstrap_result),
    noisy_compliance = TRUE,
    persistent_game_bias = TRUE,
    boundary_exclusion = TRUE,
    alternative_re_shrinkage = TRUE
  ),
  estimand = paste(
    "additional modeled RE captured on the held-out factual opportunity clock;",
    "not actual runs caused and not a regenerated game counterfactual"
  ),
  headline_template = paste(
    "On the held-out factual opportunity clock, the frozen no-oracle policy",
    "captured an estimated X additional modeled RE."
  )
)
write_immutable_rds(
  run_manifest,
  file.path(output_directory, "run_manifest.rds")
)
jsonlite::write_json(
  run_manifest,
  file.path(output_directory, "run_manifest.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

required_outputs <- file.path(output_directory, c(
  "split_manifest.rds",
  "run_configuration.rds",
  "frozen_policy.rds",
  "confirmation_manifest.rds",
  "confirmation_game_values.parquet",
  "compact_reporting_table.csv",
  "partial_identification_envelope.csv",
  "noisy_compliance_summary.csv",
  "leakage_diagnostics.csv",
  "official_geometry_reconciliation.csv",
  "run_manifest.rds",
  "run_manifest.json"
))
if (!all(file.exists(required_outputs))) {
  stop("At least one required fixed-clock confirmation artifact is missing")
}
print(split_validation)
print(point_summary[role == "combined"])
print(point_envelope[role == "combined"])
cat("Fixed-clock confirmation artifacts:", output_directory, "\n")
