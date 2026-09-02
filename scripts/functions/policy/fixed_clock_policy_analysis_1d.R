# Analysis-level helpers for fitting, tuning, and evaluating the fixed-clock
# no-oracle challenge policy.  These helpers keep the public opportunity clock,
# frozen decision rule, and quarantined geometry/evaluation score separate.

fixed_clock_policy_clock_1d <- function(
  opportunities,
  gain_column = "stake_G",
  structural_only = TRUE
) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  required <- c(
    fixed_clock_direct_policy_key_columns_1d(),
    "inning", "stage", "role", "count_state", "decision_mode", gain_column
  )
  stop_if_missing_columns(x, required, "fixed-clock source opportunities")
  if (isTRUE(structural_only)) x <- x[decision_mode == "structural"]
  out <- x[, .(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    inning = as.integer(inning),
    stage = as.integer(stage),
    role = as.character(role),
    count_state = as.character(count_state),
    stake_G = as.numeric(get(gain_column)),
    decision_mode = as.character(decision_mode)
  )]
  .fixed_clock_direct_assert_truth_free_1d(out)
  .fixed_clock_direct_normalize_clock_1d(out)
}

fixed_clock_policy_truth_1d <- function(opportunities, structural_only = TRUE) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  stop_if_missing_columns(
    x,
    c(
      fixed_clock_direct_policy_key_columns_1d(), "role",
      "role_margin_inches", "decision_mode"
    ),
    "fixed-clock truth opportunities"
  )
  if (isTRUE(structural_only)) x <- x[decision_mode == "structural"]
  truth <- normalize_revealed_policy_truth_1d(x[, .(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    role = as.character(role),
    role_margin_inches = as.numeric(role_margin_inches)
  )])
  truth[, .(game_pk, team_id, pitch_order, role, role_margin_inches)]
}

fixed_clock_policy_observed_actions_1d <- function(
  opportunities, structural_only = TRUE
) {
  x <- data.table::copy(data.table::as.data.table(opportunities))
  stop_if_missing_columns(
    x,
    c(
      fixed_clock_direct_policy_key_columns_1d(), "observed_team_challenge",
      "decision_mode"
    ),
    "fixed-clock observed actions"
  )
  if (isTRUE(structural_only)) x <- x[decision_mode == "structural"]
  observed <- x$observed_team_challenge
  if (anyNA(observed) || any(!observed %in% c(FALSE, TRUE, 0L, 1L))) {
    stop("Observed fixed-clock actions must be non-missing and binary",
      call. = FALSE
    )
  }
  out <- x[, .(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    observed_challenge = as.integer(observed_team_challenge)
  )]
  if (anyDuplicated(out[, .(game_pk, team_id, pitch_order)])) {
    stop("Observed fixed-clock actions have duplicate keys", call. = FALSE)
  }
  out[]
}

expand_fixed_clock_game_bootstrap_1d <- function(
  rows,
  game_weights,
  game_column = "game_pk"
) {
  x <- data.table::copy(data.table::as.data.table(rows))
  weights <- data.table::copy(data.table::as.data.table(game_weights))
  stop_if_missing_columns(x, game_column, "game-bootstrap rows")
  stop_if_missing_columns(
    weights, c("game_pk", "bootstrap_weight"), "game-bootstrap weights"
  )
  x[, (game_column) := as.character(get(game_column))]
  numeric_weight <- suppressWarnings(as.numeric(weights$bootstrap_weight))
  integer_weight <- suppressWarnings(as.integer(numeric_weight))
  if (anyNA(numeric_weight) || any(!is.finite(numeric_weight)) ||
      any(numeric_weight < 0) || any(numeric_weight != integer_weight)) {
    stop("Game-bootstrap weights must be finite non-negative integers",
      call. = FALSE
    )
  }
  weights <- weights[, .(
    game_pk = as.character(game_pk),
    bootstrap_weight = integer_weight
  )]
  if (anyNA(weights) ||
      anyDuplicated(weights$game_pk)) {
    stop("Game-bootstrap weights are invalid", call. = FALSE)
  }
  positive <- weights[bootstrap_weight > 0L]
  pieces <- vector("list", sum(positive$bootstrap_weight))
  index <- 0L
  for (weight_row in seq_len(nrow(positive))) {
    source_game <- positive$game_pk[[weight_row]]
    block <- x[get(game_column) == source_game]
    if (!nrow(block)) {
      stop("A positive bootstrap game is absent from the supplied rows",
        call. = FALSE
      )
    }
    for (copy_id in seq_len(positive$bootstrap_weight[[weight_row]])) {
      index <- index + 1L
      value <- data.table::copy(block)
      value[, (game_column) := paste0(
        source_game, "__bootstrap_", sprintf("%03d", copy_id)
      )]
      pieces[[index]] <- value
    }
  }
  if (!length(pieces)) stop("The game bootstrap selected no rows", call. = FALSE)
  data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
}

refit_fixed_clock_effective_widths_1d <- function(
  selection_rows,
  local_margin_limit_inches = 3,
  nthreads = 1L
) {
  x <- data.table::copy(data.table::as.data.table(selection_rows))
  assert_revealed_challenge_selection_1d_outcome_free(
    x, "fixed-clock width-refit rows"
  )
  rows <- lapply(revealed_challenge_selection_1d_roles(), function(role_value) {
    train <- x[
      role == role_value &
        abs(role_margin_inches) <= local_margin_limit_inches
    ]
    if (nrow(train) < 100L || sum(train$challenged) < 5L ||
        sum(1L - train$challenged) < 5L) {
      stop(role_value, " width bootstrap lacks challenge/pass support",
        call. = FALSE
      )
    }
    specification <- .revealed_selection_training_spec_1d(train, FALSE)
    formula <- .revealed_selection_formula_1d(specification)
    model <- mgcv::bam(
      formula$formula,
      data = specification$data,
      family = stats::binomial(link = "probit"),
      method = "fREML",
      discrete = TRUE,
      nthreads = as.integer(nthreads),
      gc.level = 1L
    )
    slope <- unname(stats::coef(model)[["role_margin_inches"]])
    if (!is.finite(slope) || slope <= 0) {
      stop(role_value, " bootstrap effective width is not identified",
        call. = FALSE
      )
    }
    data.table::data.table(
      role = role_value,
      sigma_inches = 1 / slope,
      margin_probit_slope = slope,
      rows = nrow(train),
      challenges = sum(train$challenged)
    )
  })
  data.table::rbindlist(rows)
}

refit_fixed_clock_development_nuisance_1d <- function(
  prior_rows,
  selection_rows,
  selected_components,
  game_weights = NULL,
  context_prior_strength = 100,
  tolerance = 1e-4,
  max_iterations = 500L,
  local_margin_limit_inches = 3,
  nthreads = 1L,
  fold_id = "bootstrap"
) {
  prior <- data.table::copy(data.table::as.data.table(prior_rows))
  selection <- data.table::copy(data.table::as.data.table(selection_rows))
  if (!is.null(game_weights)) {
    prior <- expand_fixed_clock_game_bootstrap_1d(prior, game_weights)
    selection <- expand_fixed_clock_game_bootstrap_1d(selection, game_weights)
  }
  components <- unlist(selected_components, use.names = TRUE)
  roles <- revealed_challenge_prior_1d_roles()
  if (is.null(names(components)) || !all(roles %in% names(components))) {
    stop("selected_components must be named for offense and defense",
      call. = FALSE
    )
  }
  priors <- lapply(roles, function(role_value) {
    rows <- prior[role == role_value]
    fit <- fit_challenge_margin_prior_1d(
      .revealed_prior_adapter_1d(rows, "context_count_family"),
      components = as.integer(components[[role_value]]),
      fold_id = paste0(fold_id, "_", role_value),
      context_prior_strength = context_prior_strength,
      tolerance = tolerance,
      max_iterations = max_iterations
    )
    fit$role <- role_value
    fit$context_source_column <- "context_count_family"
    fit
  })
  names(priors) <- roles
  widths <- refit_fixed_clock_effective_widths_1d(
    selection,
    local_margin_limit_inches = local_margin_limit_inches,
    nthreads = nthreads
  )
  list(
    prior_fits = priors,
    width_estimates = widths[],
    effective_width = stats::setNames(
      widths$sigma_inches[match(roles, widths$role)], roles
    ),
    selected_components = stats::setNames(
      as.integer(components[roles]), roles
    ),
    resampled = !is.null(game_weights)
  )
}

.fixed_clock_public_prior_probability_1d <- function(fit, context) {
  context <- as.character(context)
  levels <- unique(context)
  probability <- vapply(levels, function(value) {
    challenge_margin_prior_ball_rate_1d(fit, context = value)
  }, numeric(1L))
  unname(probability[match(context, levels)])
}

.fixed_clock_public_quantities_1d <- function(
  clock, design, parameter, prior_fits
) {
  losses <- .fixed_clock_direct_losses_1d(parameter, design)
  out <- data.table::data.table(
    inventory_loss_k1 = losses$k1,
    inventory_loss_k2 = losses$k2,
    q_star_k1 = NA_real_,
    q_star_k2 = NA_real_,
    prior_wrong_probability = 0,
    prior_action_probability_k0 = 0,
    prior_action_probability_k1 = 0,
    prior_action_probability_k2 = 0,
    prior_success_probability_k1 = 0,
    prior_success_probability_k2 = 0,
    prior_failure_probability_k1 = 0,
    prior_failure_probability_k2 = 0
  )
  active <- clock$eligible & clock$stake_G > 0
  for (role in fixed_clock_direct_policy_roles_1d()) {
    index <- which(active & clock$role == role)
    if (!length(index)) next
    prior_q <- .fixed_clock_public_prior_probability_1d(
      prior_fits[[role]], clock$count_state[index]
    )
    out$prior_wrong_probability[index] <- prior_q
    for (inventory in 1:2) {
      loss <- losses[[paste0("k", inventory)]][index]
      q_star <- loss / (clock$stake_G[index] + loss)
      challenge <- as.numeric(prior_q > q_star)
      out[[paste0("q_star_k", inventory)]][index] <- q_star
      out[[paste0("prior_action_probability_k", inventory)]][index] <-
        challenge
      out[[paste0("prior_success_probability_k", inventory)]][index] <-
        challenge * prior_q
      out[[paste0("prior_failure_probability_k", inventory)]][index] <-
        challenge * (1 - prior_q)
    }
  }
  out[]
}

fit_fixed_clock_public_policy_1d <- function(
  clock_rows,
  prior_fits,
  stage_df = 4L,
  ridge = 1e-4,
  initial_loss = c(k1 = 0.06, k2 = 0.03),
  initial_inventory = 2L,
  optimizer_control = list(maxit = 100L, reltol = 1e-8)
) {
  clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  priors <- .fixed_clock_direct_validate_priors_1d(prior_fits)
  basis_spec <- .fixed_clock_direct_stage_basis_spec_1d(stage_df)
  design <- .fixed_clock_direct_design_1d(clock$stage, clock$role, basis_spec)
  ridge <- .fixed_clock_direct_scalar_number_1d(ridge, "ridge", lower = 0)
  initial_inventory <- .fixed_clock_direct_scalar_integer_1d(
    initial_inventory, "initial_inventory", minimum = 0L
  )
  initial <- .fixed_clock_direct_initial_parameters_1d(
    design, basis_spec, initial_loss
  )
  objective <- function(parameter) {
    quantities <- .fixed_clock_public_quantities_1d(
      clock, design, parameter, priors
    )
    value <- .fixed_clock_direct_trace_value_1d(
      clock, quantities, initial_inventory = initial_inventory
    )$expected_re_per_team_game
    -value + ridge * .fixed_clock_direct_penalty_1d(parameter, basis_spec)
  }
  optimized <- tryCatch(
    stats::optim(
      initial, objective, method = "BFGS", control = optimizer_control
    ),
    error = function(error) NULL
  )
  if (is.null(optimized) || !is.finite(optimized$value)) {
    optimized <- stats::optim(
      initial, objective, method = "Nelder-Mead", control = optimizer_control
    )
  }
  quantities <- .fixed_clock_public_quantities_1d(
    clock, design, optimized$par, priors
  )
  value <- .fixed_clock_direct_trace_value_1d(
    clock, quantities, initial_inventory = initial_inventory
  )
  empirical_prior <- all(vapply(
    priors,
    function(prior) identical(
      as.character(prior$prior_type %||% ""), "empirical_binned_count"
    ),
    logical(1L)
  ))
  result <- list(
    schema = "fixed_clock_public_policy_v1",
    parameter = optimized$par,
    basis_spec = basis_spec,
    design_columns = colnames(design),
    prior_fits = priors,
    ridge = ridge,
    initial_loss = as.numeric(initial_loss),
    initial_inventory = initial_inventory,
    convergence = optimized$convergence,
    objective = optimized$value,
    total_expected_re = value$total_expected_re,
    expected_re_per_team_game = value$expected_re_per_team_game,
    training_games = sort(unique(clock$game_pk)),
    training_rows = nrow(clock),
    information_regime = if (empirical_prior) {
      paste(
        "public role-by-raw-count empirical margin prior, game stage, outs,",
        "role, and inventory only; no private signal or pitch location"
      )
    } else {
      paste(
        "public count-by-pitch-family margin prior, game stage, outs, role,",
        "and inventory only; no private signal or pitch location"
      )
    }
  )
  class(result) <- "fixed_clock_public_policy_1d"
  result
}

.fixed_clock_public_restore_1d <- function(frozen_policy) {
  validate_frozen_fixed_clock_policy_1d(frozen_policy)
  policy <- frozen_policy$policy
  if (!is.list(policy) ||
      !identical(policy$schema, "fixed_clock_public_policy_v1")) {
    stop("Frozen object does not contain a public fixed-clock policy",
      call. = FALSE
    )
  }
  policy$prior_fits <- lapply(
    policy$prior_fits, .fixed_clock_direct_restore_prior_fit_1d
  )
  invisible(lapply(policy$prior_fits, .challenge_margin_validate_fit))
  policy
}

evaluate_fixed_clock_public_policy_1d <- function(
  frozen_policy,
  clock_rows,
  truth_rows,
  evaluation_gain_rows = NULL,
  require_game_separation = TRUE,
  prior_fits_override = NULL,
  evaluation_gain = NULL,
  evaluation_mode = c("frozen", "bootstrap_nuisance"),
  override_provenance = NULL
) {
  evaluation_mode <- match.arg(evaluation_mode)
  if (!is.null(evaluation_gain_rows) && !is.null(evaluation_gain)) {
    stop("Supply only evaluation_gain_rows; evaluation_gain is deprecated",
      call. = FALSE
    )
  }
  if (is.null(evaluation_gain_rows) && !is.null(evaluation_gain)) {
    evaluation_gain_rows <- evaluation_gain
  }
  if (!is.null(prior_fits_override) && evaluation_mode == "frozen") {
    stop("Public-policy nuisance overrides require bootstrap_nuisance mode",
      call. = FALSE
    )
  }
  override_sha256 <- NA_character_
  if (!is.null(prior_fits_override)) {
    required_provenance <- c(
      "replicate_id", "source_game_ids", "game_weight_sha256",
      "source_sha256", "fit_sha256"
    )
    if (!is.list(override_provenance) ||
        length(setdiff(required_provenance, names(override_provenance)))) {
      stop("Public-policy bootstrap nuisance provenance is incomplete",
        call. = FALSE
      )
    }
    source_games <- as.character(unlist(
      override_provenance$source_game_ids,
      recursive = TRUE, use.names = FALSE
    ))
    clock_games <- unique(as.character(
      data.table::as.data.table(clock_rows)$game_pk
    ))
    if (length(intersect(clock_games, source_games))) {
      stop("Public-policy nuisance games overlap the evaluation clock",
        call. = FALSE
      )
    }
    override_sha256 <- fixed_clock_hash_object_1d(override_provenance)
  } else if (!is.null(override_provenance)) {
    stop("override_provenance was supplied without a public-policy override",
      call. = FALSE
    )
  }
  policy <- .fixed_clock_public_restore_1d(frozen_policy)
  priors <- if (is.null(prior_fits_override)) {
    policy$prior_fits
  } else {
    .fixed_clock_direct_validate_priors_1d(prior_fits_override)
  }
  clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  if (isTRUE(require_game_separation) && length(intersect(
    unique(clock$game_pk), frozen_policy$training_game_ids
  ))) {
    stop("Public-policy confirmation clock overlaps training games",
      call. = FALSE
    )
  }
  design <- .fixed_clock_direct_design_1d(
    clock$stage, clock$role, policy$basis_spec
  )
  quantities <- .fixed_clock_public_quantities_1d(
    clock, design, policy$parameter, priors
  )
  keys <- fixed_clock_direct_policy_key_columns_1d()
  probability <- clock[, ..keys]
  probability[, `:=`(
    policy = "public_information_only",
    policy_family = "fixed_clock_public_policy_1d",
    probability_k1 = quantities$prior_action_probability_k1,
    probability_k2 = quantities$prior_action_probability_k2,
    truth_used_by_decision_rule = FALSE,
    geometry_used_for_signal_integration = FALSE,
    truth_used_by_action_rule = FALSE
  )]
  evaluation_clock <- .fixed_clock_evaluation_clock_1d(
    clock, evaluation_gain_rows
  )
  replay <- replay_revealed_policy_value_1d(
    evaluation_clock, probability, truth_rows,
    initial_inventory = policy$initial_inventory
  )
  list(
    replay = replay[],
    game_role = .revealed_policy_game_role_summary_1d(replay),
    quantities = cbind(data.table::copy(clock), quantities),
    evaluation_mode = evaluation_mode,
    override_sha256 = override_sha256,
    frozen_policy_sha256 = frozen_policy$policy_sha256,
    decision_spec_sha256 = fixed_clock_hash_object_1d(list(
      parameter = policy$parameter,
      prior_fits = priors,
      basis_spec = policy$basis_spec,
      initial_inventory = policy$initial_inventory
    ))
  )
}

.fixed_clock_evaluation_clock_1d <- function(
  clock_rows, evaluation_gain_rows = NULL
) {
  clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  if (is.null(evaluation_gain_rows)) return(clock[])
  keys <- fixed_clock_direct_policy_key_columns_1d()
  gain <- data.table::copy(data.table::as.data.table(evaluation_gain_rows))
  .fixed_clock_direct_assert_truth_free_1d(
    gain, "fixed-clock evaluation gain table"
  )
  stop_if_missing_columns(
    gain, c(keys, "G_evaluation"), "fixed-clock evaluation gain table"
  )
  gain <- gain[, .(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    G_evaluation = as.numeric(G_evaluation)
  )]
  if (anyNA(gain) || any(!is.finite(gain$G_evaluation)) ||
      anyDuplicated(gain[, ..keys])) {
    stop("Fixed-clock evaluation gains are invalid", call. = FALSE)
  }
  out <- merge(clock, gain, by = keys, all.x = TRUE, sort = FALSE)
  if (anyNA(out$G_evaluation)) {
    stop("Fixed-clock evaluation gains do not cover every opportunity",
      call. = FALSE
    )
  }
  out[, stake_G := G_evaluation][, G_evaluation := NULL]
  out[]
}

evaluate_fixed_clock_comparators_1d <- function(
  clock_rows,
  truth_rows,
  observed_actions,
  fitted_probability_rows = NULL,
  evaluation_gain_rows = NULL,
  initial_inventory = 2L
) {
  decision_clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  clock <- .fixed_clock_evaluation_clock_1d(
    decision_clock, evaluation_gain_rows
  )
  truth <- normalize_revealed_policy_truth_1d(truth_rows)
  keys <- fixed_clock_direct_policy_key_columns_1d()
  observed <- data.table::copy(data.table::as.data.table(observed_actions))
  stop_if_missing_columns(
    observed, c(keys, "observed_challenge"), "fixed-clock observed actions"
  )
  observed <- observed[, .(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    observed_challenge = as.integer(observed_challenge)
  )]
  if (anyNA(observed) || any(!observed$observed_challenge %in% 0:1) ||
      anyDuplicated(observed[, ..keys])) {
    stop("Fixed-clock observed actions are invalid", call. = FALSE)
  }
  base_key <- do.call(paste, c(clock[, ..keys], sep = "\r"))
  observed_key <- do.call(paste, c(observed[, ..keys], sep = "\r"))
  index <- match(base_key, observed_key)
  if (anyNA(index) || nrow(observed) != nrow(clock)) {
    stop("Observed actions do not cover the fixed clock", call. = FALSE)
  }
  observed <- observed[index]
  make_probability <- function(
    policy, family, k1, k2, truth_used = FALSE,
    geometry_integrated = FALSE
  ) {
    out <- clock[, ..keys]
    out[, `:=`(
      policy = policy,
      policy_family = family,
      probability_k1 = as.numeric(k1),
      probability_k2 = as.numeric(k2),
      truth_used_by_decision_rule = truth_used,
      geometry_used_for_signal_integration = geometry_integrated,
      truth_used_by_action_rule = truth_used
    )]
    out
  }
  probabilities <- list(make_probability(
    "observed", "observed",
    observed$observed_challenge, observed$observed_challenge
  ))
  if (!is.null(fitted_probability_rows)) {
    fitted <- data.table::copy(
      data.table::as.data.table(fitted_probability_rows)
    )
    stop_if_missing_columns(
      fitted,
      c(keys, "probability_k1", "probability_k2"),
      "fitted-human fixed-clock probabilities"
    )
    fitted[, `:=`(
      game_pk = as.character(game_pk),
      team_id = as.character(team_id),
      pitch_order = as.integer(pitch_order)
    )]
    fitted_key <- do.call(paste, c(fitted[, ..keys], sep = "\r"))
    fitted_index <- match(base_key, fitted_key)
    if (anyNA(fitted_index) || nrow(fitted) != nrow(clock)) {
      stop("Fitted-human probabilities do not cover the fixed clock",
        call. = FALSE
      )
    }
    fitted <- fitted[fitted_index]
    probabilities[[length(probabilities) + 1L]] <- make_probability(
      "fitted_human_selection",
      "fitted_human_selection",
      fitted$probability_k1,
      fitted$probability_k2,
      geometry_integrated = TRUE
    )
  }
  normalized_truth <- normalize_revealed_policy_truth_1d(
    truth[, c(keys, "role", "role_margin_inches"), with = FALSE]
  )
  truth_key <- do.call(paste, c(normalized_truth[, ..keys], sep = "\r"))
  truth_index <- match(base_key, truth_key)
  if (anyNA(truth_index) || nrow(normalized_truth) != nrow(clock)) {
    stop("Comparator truth does not cover the fixed clock", call. = FALSE)
  }
  normalized_truth <- normalized_truth[truth_index]
  oracle <- as.numeric(
    normalized_truth$geometry_success & clock$stake_G > 0
  )
  probabilities[[length(probabilities) + 1L]] <- make_probability(
    "exact_location_oracle", "exact_location_oracle",
    oracle, oracle, truth_used = TRUE
  )
  probability <- data.table::rbindlist(probabilities, use.names = TRUE)
  replay <- replay_revealed_policy_value_1d(
    clock,
    probability,
    truth[, c(keys, "role", "role_margin_inches"), with = FALSE],
    initial_inventory = initial_inventory
  )
  list(
    replay = replay[],
    game_role = .revealed_policy_game_role_summary_1d(replay),
    season = summarize_revealed_policy_value_1d(
      replay, bootstrap_reps = 0L
    )$season
  )
}

fit_fixed_clock_bellman_policies_1d <- function(
  development_opportunities,
  prior_fits,
  scenarios = revealed_perception_joint_scenarios_1d(),
  effective_width,
  prior_n = 30,
  tol = 1e-7,
  max_iter = 10000L,
  lookup_grid_step = 0.02,
  progress = interactive()
) {
  opportunities <- data.table::copy(
    data.table::as.data.table(development_opportunities)
  )
  opportunities[
    decision_mode == "structural" & stake_G <= 0,
    decision_mode := "passive"
  ]
  scenario_table <- .fixed_clock_direct_normalize_scenarios_1d(
    scenarios, effective_width = effective_width
  )
  fits <- vector("list", nrow(scenario_table))
  names(fits) <- scenario_table$scenario_id
  for (index in seq_len(nrow(scenario_table))) {
    scenario <- scenario_table[index]
    if (isTRUE(progress)) message(
      "fixed-clock Bellman scenario ", index, "/", nrow(scenario_table),
      ": ", scenario$scenario_id
    )
    sigma <- stats::setNames(vapply(
      fixed_clock_direct_policy_roles_1d(),
      function(role) as.numeric(
        scenario[[paste0(role, "_sensory_sigma_inches")]]
      ),
      numeric(1L)
    ), fixed_clock_direct_policy_roles_1d())
    fits[[index]] <- fit_joint_challenge_signal_mdp_1d(
      opportunities,
      prior_fits = prior_fits,
      perception_sigma = sigma,
      prior_n = prior_n,
      tol = tol,
      max_iter = max_iter,
      lookup_grid_step = lookup_grid_step,
      allow_factual_exogenous = FALSE
    )
  }
  result <- list(
    schema = "fixed_clock_bellman_comparison_v1",
    scenarios = scenario_table[],
    fits = fits,
    training_games = sort(unique(as.character(opportunities$game_pk))),
    prior_n = prior_n,
    tol = tol,
    max_iter = max_iter,
    lookup_grid_step = lookup_grid_step,
    information_regime = paste(
      "development-only empirical Bellman structural comparison;",
      "scenario-specific private signal and shared team inventory"
    )
  )
  class(result) <- "fixed_clock_bellman_policies_1d"
  result
}

evaluate_fixed_clock_bellman_policies_1d <- function(
  bellman_policies,
  clock_rows,
  truth_rows,
  scenario_ids = NULL,
  evaluation_gain_rows = NULL,
  compliance = c("perfect", "noisy"),
  initial_inventory = 2L,
  require_game_separation = TRUE,
  return_level = c("full", "game_role")
) {
  if (!inherits(bellman_policies, "fixed_clock_bellman_policies_1d")) {
    stop("bellman_policies must be a fixed-clock Bellman comparison",
      call. = FALSE
    )
  }
  compliance <- match.arg(compliance)
  return_level <- match.arg(return_level)
  clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  if (isTRUE(require_game_separation) && length(intersect(
    unique(clock$game_pk), bellman_policies$training_games
  ))) {
    stop("Bellman confirmation clock overlaps training games", call. = FALSE)
  }
  truth <- normalize_revealed_policy_truth_1d(truth_rows)
  scenarios <- data.table::as.data.table(bellman_policies$scenarios)
  if (is.null(scenario_ids)) scenario_ids <- scenarios$scenario_id
  scenario_ids <- unique(as.character(scenario_ids))
  if (any(!scenario_ids %in% scenarios$scenario_id)) {
    stop("Unknown Bellman evaluation scenario", call. = FALSE)
  }
  keys <- fixed_clock_direct_policy_key_columns_1d()
  base_key <- do.call(paste, c(clock[, ..keys], sep = "\r"))
  truth_key <- do.call(paste, c(truth[, ..keys], sep = "\r"))
  truth <- truth[match(base_key, truth_key)]
  if (anyNA(truth$role_margin_inches)) {
    stop("Bellman truth does not cover the fixed clock", call. = FALSE)
  }
  evaluation_clock <- .fixed_clock_evaluation_clock_1d(
    clock, evaluation_gain_rows
  )
  probability_parts <- action_parts <- game_parts <-
    vector("list", length(scenario_ids))
  for (index in seq_along(scenario_ids)) {
    scenario_id <- scenario_ids[[index]]
    scenario_value <- scenario_id
    scenario <- scenarios[get("scenario_id") == scenario_value]
    fit <- bellman_policies$fits[[scenario_id]]
    action <- data.table::copy(clock)
    active_index <- which(action$eligible & action$stake_G > 0)
    for (inventory in 1:2) {
      threshold <- rep(Inf, nrow(action))
      q_star <- loss <- rep(NA_real_, nrow(action))
      if (length(active_index)) {
        decision <- joint_challenge_signal_mdp_action_1d(
          fit,
          stage = clock$stage[active_index],
          role = clock$role[active_index],
          inventory = inventory,
          gain = clock$stake_G[active_index],
          context = clock$count_state[active_index],
          true_margin = NULL
        )
        threshold[active_index] <- decision$signal_threshold_inches
        q_star[active_index] <- decision$q_threshold
        loss[active_index] <- decision$marginal_inventory_re
      }
      action[, (paste0("signal_threshold_k", inventory, "_inches")) :=
        threshold]
      action[, (paste0("q_star_k", inventory)) := q_star]
      action[, (paste0("inventory_loss_k", inventory)) := loss]
    }
    action[, `:=`(
      G_decision = stake_G,
      evaluation_scenario_id = scenario_id,
      truth_used_by_decision_rule = FALSE,
      geometry_used_for_signal_integration = FALSE
    )]
    sensory <- ifelse(
      action$role == "offense",
      scenario$offense_sensory_sigma_inches,
      scenario$defense_sensory_sigma_inches
    )
    action_noise <- ifelse(
      action$role == "offense",
      scenario$offense_action_sigma_inches,
      scenario$defense_action_sigma_inches
    )
    sigma <- if (compliance == "perfect") sensory else {
      sqrt(sensory^2 + action_noise^2)
    }
    active <- action$eligible & action$G_decision > 0
    probability <- action[, ..keys]
    probability[, `:=`(
      policy = paste0("bellman__", scenario_id),
      policy_family = "fixed_clock_bellman_comparison_1d",
      probability_k1 = revealed_policy_signal_integration_probability_1d(
        truth$role_margin_inches,
        action$signal_threshold_k1_inches,
        sigma
      ) * as.numeric(active),
      probability_k2 = revealed_policy_signal_integration_probability_1d(
        truth$role_margin_inches,
        action$signal_threshold_k2_inches,
        sigma
      ) * as.numeric(active),
      truth_used_by_decision_rule = FALSE,
      geometry_used_for_signal_integration = TRUE,
      truth_used_by_action_rule = FALSE
    )]
    if (return_level == "game_role") {
      scenario_replay <- replay_revealed_policy_value_1d(
        evaluation_clock,
        probability,
        truth[, c(keys, "role", "role_margin_inches"), with = FALSE],
        initial_inventory = initial_inventory
      )
      scenario_replay[, evaluation_scenario_id := scenario_id]
      game_parts[[index]] <- .revealed_policy_game_role_summary_1d(
        scenario_replay
      )
    } else {
      action_parts[[index]] <- action
      probability_parts[[index]] <- probability
    }
  }
  if (return_level == "game_role") {
    return(list(
      policy_actions = data.table::data.table(),
      replay = data.table::data.table(),
      game_role = data.table::rbindlist(game_parts, use.names = TRUE),
      detail_retained = FALSE
    ))
  }
  replay <- replay_revealed_policy_value_1d(
    evaluation_clock,
    data.table::rbindlist(probability_parts, use.names = TRUE),
    truth[, c(keys, "role", "role_margin_inches"), with = FALSE],
    initial_inventory = initial_inventory
  )
  replay[, evaluation_scenario_id := sub("^bellman__", "", policy)]
  list(
    policy_actions = data.table::rbindlist(action_parts, use.names = TRUE),
    replay = replay[],
    game_role = .revealed_policy_game_role_summary_1d(replay),
    detail_retained = TRUE
  )
}

score_fixed_clock_direct_policy_prior_1d <- function(
  policy,
  clock_rows,
  scenario_ids = NULL,
  candidate_id = NULL
) {
  if (inherits(policy, "fixed_clock_frozen_policy_1d")) {
    policy <- .fixed_clock_direct_restore_policy_1d(policy)
  }
  if (!inherits(policy, "fixed_clock_direct_policy_1d") &&
      !identical(policy$schema, "fixed_clock_direct_policy_v1")) {
    stop("policy must be a fitted or frozen direct policy", call. = FALSE)
  }
  clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  if (is.null(candidate_id)) candidate_id <- policy$selected_candidate_id
  if (is.null(scenario_ids)) scenario_ids <- policy$scenarios$scenario_id
  resources <- .fixed_clock_direct_build_resources_1d(
    clock,
    policy$prior_fits,
    data.table::as.data.table(policy$scenarios)[scenario_id %in% scenario_ids],
    policy$compliance,
    policy$lookup_grid_step
  )
  design <- .fixed_clock_direct_design_1d(
    clock$stage, clock$role, policy$basis_spec
  )
  parameter <- policy$candidate_fits[[candidate_id]]$parameter
  data.table::rbindlist(lapply(scenario_ids, function(scenario_id) {
    quantities <- .fixed_clock_direct_policy_quantities_1d(
      clock, design, parameter, resources[[scenario_id]]
    )
    value <- .fixed_clock_direct_trace_value_1d(
      clock, quantities, initial_inventory = policy$initial_inventory
    )
    data.table::data.table(
      candidate_id = candidate_id,
      scenario_id = scenario_id,
      total_expected_re = value$total_expected_re,
      expected_re_per_team_game = value$expected_re_per_team_game,
      team_games = value$team_games
    )
  }))
}

locally_refit_fixed_clock_direct_policy_1d <- function(
  frozen_policy,
  clock_rows,
  prior_fits,
  scenarios = revealed_perception_joint_scenarios_1d(),
  effective_width,
  optimizer_control = list(maxit = 25L, reltol = 1e-7),
  lookup_cache = NULL
) {
  policy <- .fixed_clock_direct_restore_policy_1d(frozen_policy)
  clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  priors <- .fixed_clock_direct_validate_priors_1d(prior_fits)
  scenario_table <- .fixed_clock_direct_normalize_scenarios_1d(
    scenarios, effective_width = effective_width
  )
  resources <- .fixed_clock_direct_build_resources_1d(
    clock,
    priors,
    scenario_table,
    policy$compliance,
    policy$lookup_grid_step,
    lookup_cache = lookup_cache
  )
  design <- .fixed_clock_direct_design_1d(
    clock$stage, clock$role, policy$basis_spec
  )
  candidate_id <- policy$selected_candidate_id
  baseline_parameter <- policy$candidate_fits[[candidate_id]]$parameter
  baseline <- .fixed_clock_direct_cross_evaluate_1d(
    "frozen_start",
    baseline_parameter,
    clock,
    design,
    scenario_table,
    resources,
    policy$initial_inventory
  )
  binding <- baseline[
    order(expected_re_per_team_game, evaluation_scenario_id),
    evaluation_scenario_id[[1L]]
  ]
  proposal <- .fixed_clock_direct_fit_candidate_1d(
    clock,
    design,
    policy$basis_spec,
    resources[[binding]],
    policy$initial_inventory,
    policy$ridge,
    policy$initial_loss,
    optimizer_control,
    initial_parameter = baseline_parameter
  )
  proposed <- .fixed_clock_direct_cross_evaluate_1d(
    "local_refit",
    proposal$parameter,
    clock,
    design,
    scenario_table,
    resources,
    policy$initial_inventory
  )
  baseline_score <- .fixed_clock_direct_candidate_summary_1d(baseline)
  proposal_score <- .fixed_clock_direct_candidate_summary_1d(proposed)
  accepted <-
    proposal_score$worst_case_expected_re_per_team_game[[1L]] >
      baseline_score$worst_case_expected_re_per_team_game[[1L]] + 1e-12 ||
    (
      abs(
        proposal_score$worst_case_expected_re_per_team_game[[1L]] -
          baseline_score$worst_case_expected_re_per_team_game[[1L]]
      ) <= 1e-12 &&
        proposal_score$mean_expected_re_per_team_game[[1L]] >
          baseline_score$mean_expected_re_per_team_game[[1L]] + 1e-12
    )
  list(
    parameter = if (accepted) proposal$parameter else baseline_parameter,
    accepted = accepted,
    binding_scenario_id = binding,
    baseline = baseline[],
    proposal = proposed[],
    baseline_score = baseline_score[],
    proposal_score = proposal_score[],
    convergence = proposal$convergence,
    method = "one_local_bootstrap_refit_from_frozen_solution"
  )
}

.fixed_clock_gauss_hermite_normal_1d <- function(nodes = 5L) {
  nodes <- .fixed_clock_scalar_integer_1d(nodes, "quadrature_nodes", minimum = 1L)
  if (nodes == 1L) {
    return(data.table::data.table(node = 0, weight = 1))
  }
  off_diagonal <- sqrt(seq_len(nodes - 1L) / 2)
  jacobi <- matrix(0, nrow = nodes, ncol = nodes)
  jacobi[cbind(seq_len(nodes - 1L), 2:nodes)] <- off_diagonal
  jacobi[cbind(2:nodes, seq_len(nodes - 1L))] <- off_diagonal
  decomposition <- eigen(jacobi, symmetric = TRUE)
  order <- order(decomposition$values)
  data.table::data.table(
    node = sqrt(2) * decomposition$values[order],
    weight = decomposition$vectors[1L, order]^2
  )
}

evaluate_fixed_clock_policy_bias_sensitivity_1d <- function(
  frozen_policy,
  clock_rows,
  truth_rows,
  bias_sd_inches,
  scenario_ids = NULL,
  quadrature_nodes = 5L,
  evaluation_gain_rows = NULL,
  compliance_override = NULL
) {
  bias_sd <- .fixed_clock_direct_named_roles_1d(
    bias_sd_inches, "bias_sd_inches", positive = FALSE
  )
  baseline <- evaluate_fixed_clock_policy_1d(
    frozen_policy,
    clock_rows,
    truth_rows,
    scenario_ids = scenario_ids,
    evaluation_gain_rows = evaluation_gain_rows,
    compliance_override = compliance_override,
    evaluation_mode = if (is.null(compliance_override)) {
      "frozen"
    } else {
      "sensitivity"
    },
    override_provenance = if (is.null(compliance_override)) {
      NULL
    } else {
      list(label = "persistent-bias compliance sensitivity")
    }
  )
  truth <- normalize_revealed_policy_truth_1d(truth_rows)
  keys <- fixed_clock_direct_policy_key_columns_1d()
  evaluation_clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  if (!is.null(evaluation_gain_rows)) {
    gain <- data.table::copy(data.table::as.data.table(evaluation_gain_rows))
    stop_if_missing_columns(
      gain, c(keys, "G_evaluation"), "bias-sensitivity evaluation gains"
    )
    gain <- gain[, .(
      game_pk = as.character(game_pk),
      team_id = as.character(team_id),
      pitch_order = as.integer(pitch_order),
      G_evaluation = as.numeric(G_evaluation)
    )]
    evaluation_clock <- merge(
      evaluation_clock, gain, by = keys, all.x = TRUE, sort = FALSE
    )
    if (anyNA(evaluation_clock$G_evaluation)) {
      stop("Bias-sensitivity evaluation gains do not cover the clock",
        call. = FALSE
      )
    }
    evaluation_clock[, stake_G := G_evaluation][, G_evaluation := NULL]
  }
  quadrature <- .fixed_clock_gauss_hermite_normal_1d(quadrature_nodes)
  nodes <- data.table::CJ(
    offense_index = seq_len(nrow(quadrature)),
    defense_index = seq_len(nrow(quadrature)),
    sorted = TRUE
  )
  nodes[, `:=`(
    offense_bias = quadrature$node[offense_index] * bias_sd[["offense"]],
    defense_bias = quadrature$node[defense_index] * bias_sd[["defense"]],
    quadrature_weight = quadrature$weight[offense_index] *
      quadrature$weight[defense_index]
  )]
  actions <- data.table::copy(baseline$policy_actions)
  actions <- merge(
    actions,
    truth[, c(keys, "role", "role_margin_inches"), with = FALSE],
    by = keys, all.x = TRUE, allow.cartesian = TRUE, sort = FALSE,
    suffixes = c("", ".truth")
  )
  if (anyNA(actions$role_margin_inches) ||
      any(actions$role != actions$role.truth)) {
    stop("Bias-sensitivity truth does not align with frozen actions",
      call. = FALSE
    )
  }
  scenario_values <- vector("list", length(baseline$scenario_ids))
  names(scenario_values) <- baseline$scenario_ids
  for (scenario_id in baseline$scenario_ids) {
    scenario_actions <- actions[evaluation_scenario_id == scenario_id]
    team_groups <- split(
      seq_len(nrow(scenario_actions)),
      interaction(
        scenario_actions$game_pk, scenario_actions$team_id, drop = TRUE
      )
    )
    group_values <- lapply(team_groups, function(index) {
      action_group <- scenario_actions[index]
      game_value <- evaluation_clock[
        game_pk == action_group$game_pk[[1L]] &
          team_id == action_group$team_id[[1L]]
      ]
      truth_group <- truth[
        game_pk == action_group$game_pk[[1L]] &
          team_id == action_group$team_id[[1L]]
      ]
      node_values <- lapply(seq_len(nrow(nodes)), function(node_index) {
        node <- nodes[node_index]
        bias <- ifelse(
          action_group$role == "offense",
          node$offense_bias[[1L]], node$defense_bias[[1L]]
        )
        active <- action_group$eligible & action_group$G_decision > 0
        probability <- action_group[, ..keys]
        probability[, `:=`(
          policy = paste0("fixed_clock_bias__", scenario_id),
          policy_family = "fixed_clock_direct_policy_bias_sensitivity_1d",
          probability_k1 = .fixed_clock_direct_conditional_action_1d(
            action_group$role_margin_inches + bias,
            action_group$signal_threshold_k1_inches,
            action_group$action_total_sigma_inches,
            active
          ),
          probability_k2 = .fixed_clock_direct_conditional_action_1d(
            action_group$role_margin_inches + bias,
            action_group$signal_threshold_k2_inches,
            action_group$action_total_sigma_inches,
            active
          ),
          truth_used_by_decision_rule = FALSE,
          geometry_used_for_signal_integration = TRUE,
          truth_used_by_action_rule = FALSE
        )]
        replay <- replay_revealed_policy_value_1d(
          game_value,
          probability,
          truth_group[, c(keys, "role", "role_margin_inches"), with = FALSE],
          initial_inventory = frozen_policy$policy$initial_inventory
        )
        value <- .revealed_policy_game_role_summary_1d(replay)
        value[, quadrature_weight := node$quadrature_weight[[1L]]]
        value
      })
      values <- data.table::rbindlist(node_values, use.names = TRUE)
      measures <- c(
        "captured_re", "attempts", "successes", "failures",
        "exhausted_opportunity_mass", "opportunity_exposure"
      )
      values[, lapply(.SD, function(value) {
        sum(value * quadrature_weight)
      }), by = .(game_pk, team_id, policy, policy_family, role),
      .SDcols = measures]
    })
    value <- data.table::rbindlist(group_values, use.names = TRUE)
    value[, `:=`(
      scenario_id = scenario_id,
      offense_bias_sd_inches = bias_sd[["offense"]],
      defense_bias_sd_inches = bias_sd[["defense"]],
      quadrature_nodes_per_role = nrow(quadrature)
    )]
    scenario_values[[scenario_id]] <- value
  }
  game_role <- data.table::rbindlist(scenario_values, use.names = TRUE)
  list(
    game_role = game_role[],
    baseline = baseline,
    quadrature = nodes[],
    information_regime = paste(
      "independent persistent role-by-team-game Gaussian perception biases",
      "integrated by two-dimensional Gauss-Hermite quadrature before",
      "averaging shared-inventory paths"
    )
  )
}

cross_validate_fixed_clock_direct_policy_1d <- function(
  clock_rows,
  fold_assignment,
  fold_prior_fits,
  width_estimates,
  scenarios = revealed_perception_joint_scenarios_1d(),
  tuning_grid = data.table::CJ(
    stage_df = c(4L, 5L), ridge = c(1e-4, 1e-3), sorted = TRUE
  ),
  initial_inventory = 2L,
  lookup_grid_step = 0.02,
  optimizer_control = list(maxit = 75L, reltol = 1e-7),
  workers = 1L,
  checkpoint_dir = NULL,
  checkpoint_key = "fixed_clock_direct_cv_v1",
  resume = TRUE,
  progress = interactive()
) {
  clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  folds <- data.table::copy(data.table::as.data.table(fold_assignment))
  stop_if_missing_columns(folds, c("game_pk", "fold"), "direct-policy folds")
  folds[, `:=`(game_pk = as.character(game_pk), fold = as.integer(fold))]
  if (anyNA(folds) || anyDuplicated(folds$game_pk) ||
      !setequal(folds$game_pk, unique(clock$game_pk))) {
    stop("Direct-policy folds must cover every development game", call. = FALSE)
  }
  clock[, fold := folds$fold[match(game_pk, folds$game_pk)]]
  fold_ids <- sort(unique(folds$fold))
  if (length(fold_prior_fits) != length(fold_ids)) {
    stop("fold_prior_fits must contain one role-prior list per fold",
      call. = FALSE
    )
  }
  if (is.null(names(fold_prior_fits)) ||
      !setequal(names(fold_prior_fits), as.character(fold_ids))) {
    stop("fold_prior_fits must be named by fold ID", call. = FALSE)
  }
  widths <- data.table::copy(data.table::as.data.table(width_estimates))
  stop_if_missing_columns(
    widths, c("fold", "role", "sigma_inches"), "direct-policy fold widths"
  )
  grid <- data.table::copy(data.table::as.data.table(tuning_grid))
  stop_if_missing_columns(grid, c("stage_df", "ridge"), "direct-policy tuning grid")
  grid[, tuning_id := sprintf("df_%s__ridge_%0.8g", stage_df, ridge)]
  checkpoint_key <- as.character(checkpoint_key)
  if (length(checkpoint_key) != 1L || is.na(checkpoint_key) ||
      !nzchar(checkpoint_key)) {
    stop("checkpoint_key must be one non-empty string", call. = FALSE)
  }
  if (!is.null(checkpoint_dir)) {
    checkpoint_dir <- file.path(
      normalizePath(checkpoint_dir, mustWork = FALSE), checkpoint_key
    )
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cv_contract_sha256 <- fixed_clock_hash_object_1d(list(
    schema = "fixed_clock_direct_cv_contract_v1",
    clock = clock[, setdiff(names(clock), "fold"), with = FALSE],
    folds = folds,
    fold_prior_fits = fold_prior_fits,
    widths = widths,
    scenarios = scenarios,
    grid = grid,
    initial_inventory = initial_inventory,
    lookup_grid_step = lookup_grid_step,
    optimizer_control = optimizer_control,
    workers = workers
  ))

  parts <- vector("list", nrow(grid) * length(fold_ids))
  fits <- vector("list", length(parts))
  index <- 0L
  for (grid_index in seq_len(nrow(grid))) {
    for (fold_id in fold_ids) {
      index <- index + 1L
      if (isTRUE(progress)) message(
        "direct-policy CV ", index, "/", length(parts), ": ",
        grid$tuning_id[[grid_index]], " fold ", fold_id
      )
      width <- widths[fold == fold_id]
      effective <- stats::setNames(
        width$sigma_inches[match(fixed_clock_direct_policy_roles_1d(), width$role)],
        fixed_clock_direct_policy_roles_1d()
      )
      if (anyNA(effective)) stop("A direct-policy fold width is missing")
      fold_priors <- fold_prior_fits[[as.character(fold_id)]]
      heldout_games <- folds[fold == fold_id, game_pk]
      prior_training_games <- unique(unlist(lapply(
        fold_priors, function(value) as.character(value$training_games)
      ), use.names = FALSE))
      if (length(intersect(heldout_games, prior_training_games))) {
        stop("A direct-policy fold prior contains held-out games",
          call. = FALSE
        )
      }
      cell_fingerprint <- fixed_clock_hash_object_1d(list(
        cv_contract_sha256 = cv_contract_sha256,
        tuning_id = grid$tuning_id[[grid_index]],
        fold = fold_id
      ))
      checkpoint_path <- if (is.null(checkpoint_dir)) NULL else file.path(
        checkpoint_dir,
        sprintf(
          "%s__fold_%02d.rds",
          gsub("[^A-Za-z0-9_.-]", "_", grid$tuning_id[[grid_index]]),
          fold_id
        )
      )
      checkpoint <- NULL
      if (!is.null(checkpoint_path) && isTRUE(resume) &&
          file.exists(checkpoint_path)) {
        checkpoint <- readRDS(checkpoint_path)
        if (!is.list(checkpoint) ||
            !identical(checkpoint$schema, "fixed_clock_direct_cv_cell_v1") ||
            !identical(checkpoint$fingerprint, cell_fingerprint) ||
            !identical(
              checkpoint$fit_sha256,
              fixed_clock_hash_object_1d(checkpoint$fit)
            ) || !identical(
              checkpoint$score_sha256,
              fixed_clock_hash_object_1d(checkpoint$score)
            )) {
          stop("Direct-policy CV checkpoint fingerprint mismatch",
            call. = FALSE
          )
        }
      }
      if (is.null(checkpoint)) {
        fit <- fit_fixed_clock_direct_policy_1d(
          clock[fold != fold_id, setdiff(names(clock), "fold"), with = FALSE],
          prior_fits = fold_priors,
          scenarios = scenarios,
          effective_width = effective,
          compliance = "perfect",
          stage_df = grid$stage_df[[grid_index]],
          ridge = grid$ridge[[grid_index]],
          initial_inventory = initial_inventory,
          lookup_grid_step = lookup_grid_step,
          optimizer_control = optimizer_control,
          workers = workers,
          progress = FALSE
        )
        score <- score_fixed_clock_direct_policy_prior_1d(
          fit,
          clock[fold == fold_id, setdiff(names(clock), "fold"), with = FALSE]
        )
        if (!is.null(checkpoint_path)) {
          checkpoint <- list(
            schema = "fixed_clock_direct_cv_cell_v1",
            fingerprint = cell_fingerprint,
            fit = fit,
            score = score,
            fit_sha256 = fixed_clock_hash_object_1d(fit),
            score_sha256 = fixed_clock_hash_object_1d(score)
          )
          .fixed_clock_write_rds_atomic_1d(checkpoint, checkpoint_path)
        }
      } else {
        fit <- checkpoint$fit
        score <- data.table::copy(data.table::as.data.table(checkpoint$score))
      }
      selected_optimization <- fit$candidate_optimization[
        candidate_id == fit$selected_candidate_id
      ]
      score[, `:=`(
        tuning_id = grid$tuning_id[[grid_index]],
        stage_df = grid$stage_df[[grid_index]],
        ridge = grid$ridge[[grid_index]],
        fold = fold_id,
        selected_optimizer_convergence =
          selected_optimization$convergence[[1L]],
        all_candidates_converged = all(fit$candidate_optimization$converged),
        candidate_convergence_rate = mean(
          fit$candidate_optimization$converged
        ),
        candidate_function_evaluations = sum(
          fit$candidate_optimization$function_evaluations,
          na.rm = TRUE
        )
      )]
      parts[[index]] <- score
      fits[[index]] <- fit
    }
  }
  scores <- data.table::rbindlist(parts, use.names = TRUE)
  fold_scores <- scores[, .(
    worst_scenario_re_per_team_game = min(expected_re_per_team_game),
    mean_scenario_re_per_team_game = mean(expected_re_per_team_game),
    selected_optimizer_convergence = unique(selected_optimizer_convergence),
    all_candidates_converged = unique(all_candidates_converged),
    candidate_convergence_rate = unique(candidate_convergence_rate),
    candidate_function_evaluations = unique(candidate_function_evaluations)
  ), by = .(tuning_id, stage_df, ridge, fold)]
  summary <- fold_scores[, .(
    mean_worst_scenario_re_per_team_game = mean(
      worst_scenario_re_per_team_game
    ),
    se_worst_scenario_re_per_team_game = stats::sd(
      worst_scenario_re_per_team_game
    ) / sqrt(.N),
    mean_scenario_re_per_team_game = mean(mean_scenario_re_per_team_game),
    folds = .N,
    all_candidates_converged = all(all_candidates_converged),
    mean_candidate_convergence_rate = mean(candidate_convergence_rate),
    candidate_function_evaluations = sum(candidate_function_evaluations)
  ), by = .(tuning_id, stage_df, ridge)]
  best <- summary[which.max(mean_worst_scenario_re_per_team_game)]
  cutoff <- best$mean_worst_scenario_re_per_team_game -
    best$se_worst_scenario_re_per_team_game
  summary[, within_one_se :=
    mean_worst_scenario_re_per_team_game >= cutoff
  ]
  eligible <- summary[within_one_se == TRUE]
  data.table::setorder(eligible, stage_df, -ridge, -mean_scenario_re_per_team_game)
  selected <- eligible[1L]
  summary[, selected := tuning_id == selected$tuning_id]
  list(
    selected = selected[],
    summary = summary[],
    fold_scores = fold_scores[],
    scenario_scores = scores[],
    fold_assignment = folds[],
    cv_contract_sha256 = cv_contract_sha256,
    checkpoint_dir = checkpoint_dir,
    selection_rule = paste(
      "one-standard-error rule on fold-level worst-scenario RE; ties favor",
      "fewer stage basis functions and stronger smoothing"
    )
  )
}

bootstrap_fixed_clock_policy_1d <- function(
  source_game_ids,
  refit_historical_re,
  refit_development,
  score_confirmation,
  reps = 500L,
  seed = 20260826L,
  context = list(),
  checkpoint_dir = NULL,
  checkpoint_key = "fixed_clock_confirmation_v1",
  resume = TRUE,
  workers = 1L
) {
  reps <- .fixed_clock_scalar_integer_1d(reps, "reps", minimum = 1L)
  workers <- .fixed_clock_scalar_integer_1d(workers, "workers", minimum = 1L)
  replicate_ids <- seq_len(reps)
  normalized_sources <- .fixed_clock_bootstrap_sources_1d(source_game_ids)
  bootstrap_contract_fingerprint <-
    .fixed_clock_bootstrap_contract_fingerprint_1d(
      normalized_sources,
      refit_historical_re,
      refit_development,
      score_confirmation,
      context
    )
  run_one <- function(replicate_id) {
    run_fixed_clock_confirmation_bootstrap_1d(
      source_game_ids = source_game_ids,
      replicate_ids = replicate_id,
      refit_historical_re = refit_historical_re,
      refit_development = refit_development,
      score_confirmation = score_confirmation,
      seed = seed,
      context = context,
      checkpoint_dir = checkpoint_dir,
      checkpoint_key = checkpoint_key,
      resume = resume,
      .bootstrap_contract_fingerprint = bootstrap_contract_fingerprint
    )
  }
  pieces <- if (workers == 1L || .Platform$OS.type == "windows") {
    lapply(replicate_ids, run_one)
  } else {
    parallel::mclapply(
      replicate_ids, run_one,
      mc.cores = workers, mc.preschedule = FALSE, mc.set.seed = FALSE
    )
  }
  result <- list(
    results = data.table::rbindlist(lapply(pieces, `[[`, "results"), fill = TRUE),
    plan = data.table::rbindlist(lapply(pieces, `[[`, "plan"), fill = TRUE),
    timing = data.table::rbindlist(lapply(pieces, `[[`, "timing"), fill = TRUE),
    replicate_ids = replicate_ids,
    seed = as.integer(seed),
    checkpoint_key = checkpoint_key,
    workers = workers,
    resampling_unit = "complete game within each of three coordinated sources"
  )
  data.table::setorder(result$results, replicate, replicate_row)
  data.table::setorder(result$plan, replicate, source)
  data.table::setorder(result$timing, replicate)
  class(result) <- "fixed_clock_confirmation_bootstrap_1d"
  result
}

.fixed_clock_complete_scenario_grid_1d <- function(
  draws,
  point = NULL,
  group_columns = c("policy", "role"),
  scenario_column = "scenario_id",
  replicate_column = "replicate",
  expected_scenario_ids = NULL
) {
  draws <- data.table::copy(data.table::as.data.table(draws))
  group_columns <- as.character(group_columns)
  draw_key <- c(replicate_column, group_columns, scenario_column)
  stop_if_missing_columns(draws, draw_key, "bootstrap scenario grid")
  if (anyNA(draws[, ..draw_key]) || anyDuplicated(draws[, ..draw_key])) {
    stop("Bootstrap scenario grid contains missing or duplicate keys",
      call. = FALSE
    )
  }
  if (is.null(expected_scenario_ids)) {
    expected_scenario_ids <- sort(unique(as.character(draws[[scenario_column]])))
  } else {
    expected_scenario_ids <- sort(unique(as.character(expected_scenario_ids)))
  }
  if (!length(expected_scenario_ids) || anyNA(expected_scenario_ids) ||
      any(!nzchar(expected_scenario_ids))) {
    stop("expected_scenario_ids must be non-empty and non-missing",
      call. = FALSE
    )
  }
  cross_join <- function(left, right) {
    left <- data.table::copy(left)[, join__ := 1L]
    right <- data.table::copy(right)[, join__ := 1L]
    merge(left, right, by = "join__", allow.cartesian = TRUE)[, join__ := NULL]
  }
  groups <- unique(draws[, ..group_columns])
  scenario_table <- data.table::data.table(scenario__ = expected_scenario_ids)
  data.table::setnames(scenario_table, "scenario__", scenario_column)
  group_scenarios <- cross_join(groups, scenario_table)
  replicates <- data.table::data.table(
    replicate__ = sort(unique(draws[[replicate_column]]))
  )
  data.table::setnames(replicates, "replicate__", replicate_column)
  expected_draw_keys <- cross_join(replicates, group_scenarios)
  observed_draw_keys <- unique(draws[, ..draw_key])
  data.table::setcolorder(expected_draw_keys, draw_key)
  data.table::setcolorder(observed_draw_keys, draw_key)
  if (!data.table::fsetequal(expected_draw_keys, observed_draw_keys)) {
    stop(
      "Bootstrap values do not contain the complete common scenario grid",
      call. = FALSE
    )
  }
  if (!is.null(point)) {
    point <- data.table::copy(data.table::as.data.table(point))
    point_key <- c(group_columns, scenario_column)
    stop_if_missing_columns(point, point_key, "point scenario grid")
    if (anyNA(point[, ..point_key]) || anyDuplicated(point[, ..point_key])) {
      stop("Point scenario grid contains missing or duplicate keys",
        call. = FALSE
      )
    }
    point_groups <- unique(point[, ..group_columns])
    expected_point_keys <- cross_join(point_groups, scenario_table)
    observed_point_keys <- unique(point[, ..point_key])
    data.table::setcolorder(expected_point_keys, point_key)
    data.table::setcolorder(observed_point_keys, point_key)
    if (!data.table::fsetequal(expected_point_keys, observed_point_keys)) {
      stop("Point values do not contain the complete scenario grid",
        call. = FALSE
      )
    }
    if (!data.table::fsetequal(point_groups, groups)) {
      stop("Bootstrap policies/roles do not match point estimates",
        call. = FALSE
      )
    }
  }
  list(
    scenario_grid_sha256 = fixed_clock_hash_object_1d(expected_scenario_ids),
    scenario_count = length(expected_scenario_ids),
    replicate_count = nrow(replicates),
    scenario_grid_validated = TRUE
  )
}

summarize_fixed_clock_simultaneous_intervals_1d <- function(
  point_values,
  bootstrap_values,
  estimate_column = "gain_over_observed_re",
  group_columns = c("policy", "role"),
  scenario_column = "scenario_id",
  replicate_column = "replicate",
  confidence = 0.95,
  expected_scenario_ids = NULL
) {
  point <- data.table::copy(data.table::as.data.table(point_values))
  draws <- data.table::copy(data.table::as.data.table(bootstrap_values))
  required_point <- c(group_columns, scenario_column, estimate_column)
  required_draw <- c(required_point, replicate_column)
  stop_if_missing_columns(point, required_point, "point scenario values")
  stop_if_missing_columns(draws, required_draw, "bootstrap scenario values")
  confidence <- as.numeric(confidence)
  if (length(confidence) != 1L || !is.finite(confidence) ||
      confidence <= 0 || confidence >= 1) {
    stop("confidence must lie strictly between zero and one", call. = FALSE)
  }
  key <- c(group_columns, scenario_column)
  if (anyDuplicated(point[, ..key]) ||
      anyDuplicated(draws[, c(replicate_column, key), with = FALSE])) {
    stop("Scenario interval inputs contain duplicate keys", call. = FALSE)
  }
  grid <- .fixed_clock_complete_scenario_grid_1d(
    draws = draws,
    point = point,
    group_columns = group_columns,
    scenario_column = scenario_column,
    replicate_column = replicate_column,
    expected_scenario_ids = expected_scenario_ids
  )
  reference <- point[, c(key, estimate_column), with = FALSE]
  data.table::setnames(reference, estimate_column, "point_estimate__")
  joined <- merge(draws, reference, by = key, all.x = TRUE, sort = FALSE)
  if (anyNA(joined$point_estimate__)) {
    stop("Bootstrap scenario values do not align with point estimates",
      call. = FALSE
    )
  }
  joined[, deviation__ :=
    abs(as.numeric(get(estimate_column)) - point_estimate__)
  ]
  replicate_group <- c(replicate_column, group_columns)
  maxima <- joined[, .(
    maximum_absolute_deviation = max(deviation__)
  ), by = replicate_group]
  critical <- maxima[, .(
    simultaneous_critical_value = stats::quantile(
      maximum_absolute_deviation, confidence, names = FALSE
    )
  ), by = group_columns]
  out <- merge(point, critical, by = group_columns, all.x = TRUE, sort = FALSE)
  out[, `:=`(
    simultaneous_lower = as.numeric(get(estimate_column)) -
      simultaneous_critical_value,
    simultaneous_upper = as.numeric(get(estimate_column)) +
      simultaneous_critical_value,
    simultaneous_confidence = confidence,
    scenario_grid_sha256 = grid$scenario_grid_sha256,
    scenario_count = grid$scenario_count,
    bootstrap_replicates = grid$replicate_count,
    scenario_grid_validated = grid$scenario_grid_validated
  )]
  data.table::setorderv(out, c(group_columns, scenario_column))
  out[]
}

fixed_clock_worst_scenario_lower_bound_1d <- function(
  bootstrap_values,
  estimate_column = "gain_over_observed_re",
  group_columns = c("policy", "role"),
  scenario_column = "scenario_id",
  replicate_column = "replicate",
  confidence = 0.95,
  expected_scenario_ids = NULL
) {
  draws <- data.table::copy(data.table::as.data.table(bootstrap_values))
  stop_if_missing_columns(
    draws,
    c(group_columns, scenario_column, replicate_column, estimate_column),
    "worst-scenario bootstrap values"
  )
  grid <- .fixed_clock_complete_scenario_grid_1d(
    draws = draws,
    group_columns = group_columns,
    scenario_column = scenario_column,
    replicate_column = replicate_column,
    expected_scenario_ids = expected_scenario_ids
  )
  replicate_group <- c(replicate_column, group_columns)
  worst <- draws[, .(
    worst_scenario_estimate = min(as.numeric(get(estimate_column)))
  ), by = replicate_group]
  worst[, .(
    one_sided_lower_bound = stats::quantile(
      worst_scenario_estimate, 1 - confidence, names = FALSE
    ),
    confidence = confidence,
    bootstrap_replicates = data.table::uniqueN(get(replicate_column)),
    scenario_grid_sha256 = grid$scenario_grid_sha256,
    scenario_count = grid$scenario_count,
    scenario_grid_validated = grid$scenario_grid_validated
  ), by = group_columns]
}
