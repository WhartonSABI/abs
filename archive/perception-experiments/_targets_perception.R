library(targets)

source("config/project.R")
source("scripts/load_functions.R")
tar_source(abs_function_files())

tar_option_set(
  packages = c(
    "arrow", "cmdstanr", "data.table", "jsonlite", "Matrix", "mvtnorm",
    "posterior", "rprojroot", "splines"
  ),
  format = "rds",
  error = "stop",
  memory = "transient",
  garbage_collection = TRUE
)

perception_dir <- file.path("data", "processed", "perception")

list(
  tar_target(
    perception_profile,
    continuous_perception_profile_from_env(),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    cmdstan_configuration,
    configure_continuous_cmdstan(),
    cue = tar_cue(mode = "always")
  ),

  # Safe source tables contain only the information sets declared for each
  # model. Challenge actions live in a separate decision table, and official
  # outcomes remain quarantined in a leaf-only label table.
  tar_target(
    continuous_location_features_file,
    file.path("data", "processed", "continuous_location_features.parquet"),
    format = "file"
  ),
  tar_target(
    continuous_swing_features_file,
    file.path("data", "processed", "continuous_swing_features.parquet"),
    format = "file"
  ),
  tar_target(
    continuous_call_features_file,
    file.path("data", "processed", "continuous_call_features.parquet"),
    format = "file"
  ),
  tar_target(
    continuous_decision_features_file,
    file.path("data", "processed", "continuous_decision_features.parquet"),
    format = "file"
  ),
  tar_target(
    continuous_challenge_labels_file,
    file.path("data", "processed", "continuous_challenge_labels.parquet"),
    format = "file"
  ),
  tar_target(
    perception_location_rows,
    read_continuous_parquet(
      continuous_location_features_file, "continuous location features"
    )
  ),
  tar_target(
    perception_swing_rows,
    read_continuous_parquet(
      continuous_swing_features_file, "continuous swing features"
    )
  ),
  tar_target(
    perception_call_rows,
    read_continuous_parquet(
      continuous_call_features_file, "continuous initial-call features"
    )
  ),
  tar_target(
    perception_decision_rows,
    read_continuous_parquet(
      continuous_decision_features_file, "continuous decision features"
    )
  ),

  tar_target(
    perception_game_folds,
    continuous_game_folds(
      c(
        perception_location_rows$game_pk,
        perception_swing_rows$game_pk,
        perception_call_rows$game_pk,
        perception_decision_rows$game_pk
      ),
      folds = perception_profile$folds,
      seed = perception_profile$seed
    )
  ),
  tar_target(
    perception_fold_validation,
    validate_continuous_game_folds(
      perception_game_folds, perception_profile$folds
    )
  ),
  tar_target(
    perception_active_fold,
    seq_len(perception_profile$active_folds),
    iteration = "vector"
  ),
  tar_target(
    perception_fold_fit,
    {
      cmdstan_configuration
      perception_fold_validation
      fit_continuous_perception_fold(
        fold = perception_active_fold,
        folds = perception_game_folds,
        location_rows = perception_location_rows,
        swing_rows = perception_swing_rows,
        call_rows = perception_call_rows,
        profile = perception_profile,
        backend = "cmdstanr"
      )
    },
    pattern = map(perception_active_fold)
  ),

  # Shared anisotropy is a candidate, not a default. It must recover a known
  # synthetic ratio and improve game-held-out swing prediction by one SE.
  tar_target(
    perception_anisotropy_fold_score,
    score_continuous_anisotropy_fold(
      perception_fold_fit, perception_swing_rows,
      perception_game_folds, perception_profile
    ),
    pattern = map(perception_fold_fit)
  ),
  tar_target(
    perception_anisotropy_recovery,
    {
      cmdstan_configuration
      run_continuous_anisotropy_recovery(
        perception_swing_rows, perception_profile, backend = "cmdstanr"
      )
    }
  ),
  tar_target(
    perception_anisotropy_gate,
    aggregate_continuous_anisotropy_gate(
      perception_anisotropy_fold_score,
      perception_anisotropy_recovery$validation
    )
  ),
  tar_target(
    perception_selected_fold_fit,
    apply_continuous_anisotropy_gate(
      perception_fold_fit, perception_anisotropy_gate
    ),
    pattern = map(perception_fold_fit)
  ),

  tar_target(
    perception_component_validation_fold,
    score_continuous_component_validation_fold(
      perception_selected_fold_fit,
      perception_location_rows,
      perception_swing_rows,
      perception_call_rows,
      perception_game_folds,
      perception_profile
    ),
    pattern = map(perception_selected_fold_fit)
  ),
  tar_target(
    perception_component_validation,
    aggregate_continuous_component_validation(
      perception_component_validation_fold
    )
  ),

  # Challenge/pass decisions enter only here. Fixed trust variants and the
  # sigma=0 decision baseline are fit first and scored on held-out games. The
  # estimated shared-omega stage is not fit unless omega=.5 or 1 improves on
  # omega=0 by at least one paired game-clustered SE.
  tar_target(
    perception_fixed_trust_fold,
    {
      cmdstan_configuration
      score_continuous_fixed_trust_variants(
        perception_selected_fold_fit,
        perception_decision_rows,
        perception_game_folds,
        perception_profile
      )
    },
    pattern = map(perception_selected_fold_fit)
  ),
  tar_target(
    perception_fixed_trust,
    aggregate_continuous_fixed_trust(perception_fixed_trust_fold)
  ),
  tar_target(
    perception_trust_pre_gate,
    continuous_fixed_trust_pre_gate(perception_fixed_trust$scores)
  ),
  tar_target(
    perception_estimated_trust_fold,
    {
      cmdstan_configuration
      run_continuous_estimated_trust_fold(
        perception_selected_fold_fit,
        perception_decision_rows,
        perception_game_folds,
        perception_profile,
        perception_trust_pre_gate,
        backend = "cmdstanr"
      )
    },
    pattern = map(perception_selected_fold_fit)
  ),
  tar_target(
    perception_estimated_trust,
    aggregate_continuous_estimated_trust(
      perception_estimated_trust_fold,
      perception_fixed_trust$scores,
      perception_trust_pre_gate
    )
  ),
  tar_target(
    perception_trust_gate,
    continuous_primary_trust_gate(
      perception_fixed_trust$fixed_choice_scores,
      perception_estimated_trust$gate
    )
  ),
  tar_target(
    perception_posteriors_without_outcomes,
    format_continuous_human_decision_posteriors(
      perception_fixed_trust$scores,
      perception_trust_gate,
      perception_estimated_trust$scores
    )
  ),

  # This is the first and only edge in the DAG that can see official outcomes.
  tar_target(
    perception_challenge_labels,
    read_continuous_parquet(
      continuous_challenge_labels_file,
      "quarantined continuous challenge labels"
    )
  ),
  tar_target(
    human_decision_posteriors,
    attach_continuous_outcome_evaluation(
      perception_posteriors_without_outcomes,
      perception_challenge_labels
    )
  ),
  tar_target(
    perception_chosen_calibration,
    continuous_chosen_calibration(human_decision_posteriors)
  ),

  tar_target(
    perception_fold_diagnostics,
    continuous_fold_diagnostics(perception_selected_fold_fit),
    pattern = map(perception_selected_fold_fit)
  ),
  tar_target(
    perception_decision_diagnostics,
    continuous_fixed_trust_diagnostics(perception_fixed_trust_fold),
    pattern = map(perception_fixed_trust_fold)
  ),
  tar_target(
    perception_strategy_correlation,
    continuous_perception_strategy_correlation(
      perception_selected_fold_fit,
      ndraws = perception_profile$posterior_draws,
      seed = perception_profile$seed
    ),
    pattern = map(perception_selected_fold_fit)
  ),
  tar_target(
    perception_all_diagnostics,
    data.table::rbindlist(
      c(
        perception_fold_diagnostics,
        perception_decision_diagnostics,
        list(perception_estimated_trust$diagnostics)
      ),
      fill = TRUE
    )
  ),
  tar_target(
    perception_validation_metrics,
    list(
      fold_separation = TRUE,
      full_five_fold = perception_profile$active_folds == 5L,
      sampler_healthy = continuous_sampler_gate(perception_all_diagnostics),
      swing_improvement = perception_component_validation$swing_improvement,
      swing_improvement_se = perception_component_validation$swing_improvement_se,
      location_calibrated = perception_component_validation$location_calibrated,
      call_performance = perception_component_validation$call_performance,
      pitch_family_log_score_difference =
        perception_component_validation$pitch_family_log_score_difference,
      pitch_family_log_score_difference_se =
        perception_component_validation$pitch_family_log_score_difference_se,
      max_perception_strategy_correlation = max(
        data.table::rbindlist(perception_strategy_correlation)$
          max_perception_strategy_correlation
      ),
      choice_noninferior = perception_fixed_trust$choice_noninferior,
      shared_call_trust_promoted = perception_trust_gate$pass,
      shared_call_trust_pre_gate = perception_trust_gate$pre_gate_pass,
      shared_call_trust_improvement_se =
        perception_trust_gate$improvement_in_se,
      shared_call_trust_interval =
        perception_trust_gate$informative_interval_pass,
      shared_call_trust_identifiable =
        perception_trust_gate$identifiability_pass,
      shared_call_trust_grid_converged =
        perception_trust_gate$grid_convergence_pass,
      quadrature_mean_error = max(
        perception_fixed_trust$quadrature_diagnostics$
          mean_absolute_difference
      ),
      quadrature_p99_error = max(
        perception_fixed_trust$quadrature_diagnostics$
          p99_absolute_difference
      ),
      chosen_calibrated = perception_chosen_calibration$calibrated
    )
  ),
  tar_target(
    perception_validation_gates,
    build_continuous_validation_gates(perception_validation_metrics)
  ),
  tar_target(
    batter_perception_parameters,
    summarize_continuous_batter_parameters(
      perception_selected_fold_fit,
      ndraws = perception_profile$posterior_draws,
      seed = perception_profile$seed,
      swing_rows = perception_swing_rows
    )
  ),

  tar_target(
    batter_perception_parameters_file,
    write_continuous_parquet(
      batter_perception_parameters,
      file.path(perception_dir, "batter_perception_parameters.parquet")
    ),
    format = "file"
  ),
  tar_target(
    human_decision_posteriors_file,
    write_continuous_parquet(
      human_decision_posteriors,
      file.path(perception_dir, "human_decision_posteriors.parquet")
    ),
    format = "file"
  ),
  tar_target(
    perception_validation_gates_file,
    write_continuous_csv(
      perception_validation_gates,
      file.path(perception_dir, "validation_gates.csv")
    ),
    format = "file"
  ),
  tar_target(
    perception_diagnostics_file,
    write_continuous_csv(
      perception_all_diagnostics,
      file.path(perception_dir, "posterior_diagnostics.csv")
    ),
    format = "file"
  ),
  tar_target(
    perception_manifest_file,
    write_continuous_perception_manifest(
      path = file.path(perception_dir, "model_manifest.json"),
      profile = perception_profile$name,
      folds = perception_game_folds,
      gates = as.list(perception_trust_gate[1L]),
      extra = list(
        exploratory = TRUE,
        confirmation_set_reserved = FALSE,
        active_folds = perception_profile$active_folds,
        seeds = perception_profile$seed,
        quadrature_orders = c(7L, 11L),
        fixed_call_trust = continuous_call_trust_candidates(),
        prior_specifications = continuous_perception_prior_specifications(
          perception_profile
        ),
        mixture_candidates = c(1L, 3L, 6L),
        pitch_family_sensitivity = list(
          with_minus_without_log_score =
            perception_component_validation$pitch_family_log_score_difference,
          game_clustered_se =
            perception_component_validation$pitch_family_log_score_difference_se
        ),
        global_draw_alignment = continuous_global_draw_alignment_manifest(
          perception_fixed_trust_fold,
          perception_estimated_trust_fold,
          perception_profile$gh_order
        ),
        estimated_call_trust_pre_gate = data.table::as.data.frame(
          perception_trust_pre_gate$summary
        ),
        estimated_call_trust_grid_convergence = data.table::as.data.frame(
          perception_estimated_trust$grid_convergence
        ),
        cmdstanr_version = as.character(utils::packageVersion("cmdstanr")),
        cmdstan_version = continuous_cmdstan_version(),
        anisotropy_gate = data.table::as.data.frame(
          perception_anisotropy_gate
        ),
        mixture_selection = lapply(
          perception_selected_fold_fit,
          function(value) list(
            fold = value$fold,
            selected_components = value$mixture_selection$components,
            one_se_floor = value$mixture_selection$one_se_floor
          )
        ),
        label_quarantine = paste(
          "Official ABS calls and overturn outcomes first enter at",
          "human_decision_posteriors; all upstream fits are label-free."
        )
      )
    ),
    format = "file"
  )
)
