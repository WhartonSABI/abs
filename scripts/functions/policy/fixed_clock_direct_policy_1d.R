# Direct policy search on a fixed, factual challenge-opportunity clock.
#
# The policy sees public context and a private noisy margin signal. Geometry is
# admitted only by evaluate_fixed_clock_policy_1d(), after the fitted policy has
# been frozen. The future opportunity clock is held fixed by construction, so
# the estimand is captured correction RE on that clock rather than a downstream
# game counterfactual.

fixed_clock_direct_policy_roles_1d <- function() c("offense", "defense")

fixed_clock_direct_policy_key_columns_1d <- function() {
  c("game_pk", "team_id", "pitch_order")
}

.fixed_clock_direct_truth_columns_1d <- function() {
  inherited <- character()
  if (exists(
    "revealed_perception_profile_outcome_columns_1d",
    mode = "function", inherits = TRUE
  )) {
    inherited <- revealed_perception_profile_outcome_columns_1d()
  }
  unique(c(
    inherited,
    "role_margin_inches", "edge_distance_inches", "call_wrong",
    "actual_wrong", "geometry_wrong", "geometry_success",
    "official_success", "observed_success", "challenge_outcome",
    "is_overturned", "overturned", "abs_call", "final_call", "outcome"
  ))
}

.fixed_clock_direct_assert_truth_free_1d <- function(
  rows, label = "fixed-clock policy input"
) {
  columns <- if (is.character(rows)) rows else names(data.table::as.data.table(rows))
  leaked <- intersect(columns, .fixed_clock_direct_truth_columns_1d())
  if (length(leaked)) {
    stop(
      label, " contains confirmation truth/evaluation columns: ",
      paste(sort(leaked), collapse = ", "), call. = FALSE
    )
  }
  invisible(TRUE)
}

.fixed_clock_direct_scalar_integer_1d <- function(value, name, minimum = 0L) {
  numeric_value <- suppressWarnings(as.numeric(value))
  integer_value <- suppressWarnings(as.integer(numeric_value))
  if (length(numeric_value) != 1L || !is.finite(numeric_value) ||
      numeric_value != integer_value || integer_value < minimum) {
    stop(name, " must be one integer at least ", minimum, call. = FALSE)
  }
  integer_value
}

.fixed_clock_direct_scalar_number_1d <- function(
  value, name, lower = -Inf, upper = Inf, lower_open = FALSE
) {
  value <- suppressWarnings(as.numeric(value))
  bad_lower <- if (isTRUE(lower_open)) value <= lower else value < lower
  if (length(value) != 1L || !is.finite(value) || bad_lower || value > upper) {
    left <- if (isTRUE(lower_open)) "(" else "["
    stop(
      name, " must be one finite number in ", left, lower, ", ", upper,
      "]", call. = FALSE
    )
  }
  value
}

.fixed_clock_direct_named_roles_1d <- function(value, name, positive = FALSE) {
  roles <- fixed_clock_direct_policy_roles_1d()
  value <- unlist(value, use.names = TRUE)
  if (length(value) == 1L && is.null(names(value))) {
    value <- stats::setNames(rep(as.numeric(value), length(roles)), roles)
  }
  if (is.null(names(value)) || anyDuplicated(names(value)) ||
      !all(roles %in% names(value))) {
    stop(name, " must be scalar or named for offense and defense", call. = FALSE)
  }
  value <- as.numeric(value[roles])
  names(value) <- roles
  invalid <- if (isTRUE(positive)) value <= 0 else value < 0
  if (anyNA(value) || any(!is.finite(value)) || any(invalid)) {
    stop(
      name, " must contain finite ", if (positive) "positive" else "nonnegative",
      " role values", call. = FALSE
    )
  }
  value
}

.fixed_clock_direct_normalize_clock_1d <- function(rows) {
  .fixed_clock_direct_assert_truth_free_1d(rows, "fixed-clock opportunity clock")
  x <- data.table::copy(data.table::as.data.table(rows))
  required <- c(
    fixed_clock_direct_policy_key_columns_1d(),
    "inning", "stage", "role", "count_state", "stake_G"
  )
  stop_if_missing_columns(x, required, "fixed-clock opportunity clock")
  if (!"decision_mode" %in% names(x)) x[, decision_mode := "structural"]
  retained <- unique(c(required, "decision_mode"))
  x <- x[, ..retained]
  x[, `:=`(
    game_pk = as.character(game_pk),
    team_id = as.character(team_id),
    pitch_order = as.integer(pitch_order),
    inning = as.integer(inning),
    stage = collapse_mdp_stage(as.integer(stage)),
    role = as.character(role),
    count_state = as.character(count_state),
    stake_G = as.numeric(stake_G),
    decision_mode = as.character(decision_mode)
  )]
  x[, eligible := decision_mode == "structural"]
  if (!nrow(x) || anyNA(x[, .(
    game_pk, team_id, pitch_order, inning, stage, role, count_state,
    stake_G, decision_mode
  )]) || any(!nzchar(x$game_pk)) || any(!nzchar(x$team_id)) ||
      any(x$pitch_order < 1L) || any(x$inning < 1L) ||
      any(!x$role %in% fixed_clock_direct_policy_roles_1d()) ||
      any(!nzchar(x$count_state)) || any(!is.finite(x$stake_G)) ||
      anyDuplicated(x[, .(game_pk, team_id, pitch_order)])) {
    stop("Fixed-clock opportunity rows are invalid", call. = FALSE)
  }
  data.table::setorder(x, game_pk, team_id, pitch_order)
  chronology <- x[, any(diff(pitch_order) <= 0), by = .(game_pk, team_id)]
  if (any(chronology$V1)) {
    stop("Pitch order must increase within each team-game", call. = FALSE)
  }
  x[]
}

.fixed_clock_direct_validate_priors_1d <- function(prior_fits) {
  roles <- fixed_clock_direct_policy_roles_1d()
  if (!is.list(prior_fits) || is.null(names(prior_fits)) ||
      anyDuplicated(names(prior_fits)) || !all(roles %in% names(prior_fits))) {
    stop("prior_fits must be a uniquely named offense/defense list", call. = FALSE)
  }
  out <- prior_fits[roles]
  invisible(lapply(out, .challenge_margin_validate_fit))
  out
}

.fixed_clock_direct_restore_prior_fit_1d <- function(value) {
  if (identical(
    as.character(value$prior_type %||% ""),
    "empirical_binned_count"
  )) {
    return(structure(
      value,
      class = c(
        "empirical_challenge_margin_prior_1d_fit",
        "challenge_margin_prior_1d_fit"
      )
    ))
  }
  structure(value, class = "challenge_margin_prior_1d_fit")
}

.fixed_clock_direct_canonical_scenarios_1d <- function() {
  if (exists(
    "revealed_perception_joint_scenarios_1d",
    mode = "function", inherits = TRUE
  )) {
    return(revealed_perception_joint_scenarios_1d())
  }
  grid <- c(0, 0.25, 0.5, 0.75, 1)
  out <- data.table::CJ(
    offense_kappa = grid, defense_kappa = grid, sorted = TRUE
  )
  out[, scenario_id := sprintf(
    "offense_kappa_%0.2f__defense_kappa_%0.2f",
    offense_kappa, defense_kappa
  )]
  data.table::setcolorder(out, c(
    "scenario_id", "offense_kappa", "defense_kappa"
  ))
  out[]
}

.fixed_clock_direct_find_column_1d <- function(rows, candidates) {
  match <- candidates[candidates %in% names(rows)]
  if (length(match)) match[[1L]] else NULL
}

.fixed_clock_direct_normalize_scenarios_1d <- function(
  scenarios = NULL, effective_width = NULL
) {
  x <- if (is.null(scenarios)) {
    .fixed_clock_direct_canonical_scenarios_1d()
  } else {
    data.table::copy(data.table::as.data.table(scenarios))
  }
  stop_if_missing_columns(x, "scenario_id", "fixed-clock perception scenarios")
  x[, scenario_id := as.character(scenario_id)]
  if (!nrow(x) || anyNA(x$scenario_id) || any(!nzchar(x$scenario_id)) ||
      anyDuplicated(x$scenario_id)) {
    stop("Scenario identifiers must be unique and non-missing", call. = FALSE)
  }

  roles <- fixed_clock_direct_policy_roles_1d()
  kappa_columns <- paste0(roles, "_kappa")
  if (all(kappa_columns %in% names(x))) {
    width <- .fixed_clock_direct_named_roles_1d(
      effective_width, "effective_width", positive = TRUE
    )
    for (role in roles) {
      kappa <- as.numeric(x[[paste0(role, "_kappa")]])
      if (anyNA(kappa) || any(!is.finite(kappa)) || any(kappa < 0 | kappa > 1)) {
        stop("Scenario kappas must lie in [0, 1]", call. = FALSE)
      }
      x[, (paste0(role, "_sensory_sigma_inches")) :=
        kappa * width[[role]]]
      x[, (paste0(role, "_action_sigma_inches")) :=
        width[[role]] * sqrt(pmax(0, 1 - kappa^2))]
      x[, (paste0(role, "_effective_sigma_inches")) := width[[role]]]
    }
  } else {
    for (role in roles) {
      sensory_column <- .fixed_clock_direct_find_column_1d(x, c(
        paste0(role, "_sensory_sigma_inches"),
        paste0(role, "_sensory_sigma"),
        paste0(role, "_sigma_inches"),
        paste0(role, "_sigma")
      ))
      if (is.null(sensory_column)) {
        stop(
          "Scenarios need role kappas plus effective_width, or explicit ",
          role, " sensory sigma", call. = FALSE
        )
      }
      sensory <- as.numeric(x[[sensory_column]])
      action_column <- .fixed_clock_direct_find_column_1d(x, c(
        paste0(role, "_action_sigma_inches"),
        paste0(role, "_action_sigma")
      ))
      action <- if (is.null(action_column)) rep(0, nrow(x)) else {
        as.numeric(x[[action_column]])
      }
      if (anyNA(sensory) || any(!is.finite(sensory)) || any(sensory < 0) ||
          anyNA(action) || any(!is.finite(action)) || any(action < 0)) {
        stop("Scenario sensory/action sigmas must be finite and nonnegative",
          call. = FALSE
        )
      }
      x[, (paste0(role, "_sensory_sigma_inches")) := sensory]
      x[, (paste0(role, "_action_sigma_inches")) := action]
      x[, (paste0(role, "_effective_sigma_inches")) :=
        sqrt(sensory^2 + action^2)]
    }
  }
  retained <- c(
    "scenario_id", intersect(kappa_columns, names(x)),
    as.vector(outer(
      roles,
      c(
        "sensory_sigma_inches", "action_sigma_inches",
        "effective_sigma_inches"
      ),
      paste, sep = "_"
    ))
  )
  x[, ..retained]
}

.fixed_clock_direct_stage_basis_spec_1d <- function(stage_df = 4L) {
  stage_df <- .fixed_clock_direct_scalar_integer_1d(
    stage_df, "stage_df", minimum = 1L
  )
  if (stage_df > 6L) {
    stop("stage_df must not exceed 6 for the low-dimensional basis",
      call. = FALSE
    )
  }
  list(
    df = stage_df,
    regulation_boundary = c(0, 53),
    columns_per_role = stage_df + 3L,
    basis = paste(
      "fixed-boundary regulation-stage B-spline plus explicit outs-before",
      "and extra-inning indicator"
    )
  )
}

.fixed_clock_direct_stage_basis_1d <- function(stage, spec) {
  stage <- collapse_mdp_stage(as.integer(stage))
  regulation_stage <- pmin(stage, spec$regulation_boundary[[2L]])
  smooth <- if (spec$df <= 1L) {
    matrix(numeric(), nrow = length(stage), ncol = 0L)
  } else {
    smooth_df <- spec$df - 1L
    splines::bs(
      regulation_stage,
      df = smooth_df,
      degree = min(3L, max(1L, smooth_df)),
      Boundary.knots = spec$regulation_boundary,
      intercept = FALSE
    )
  }
  out <- cbind(
    regulation_intercept = 1,
    smooth,
    outs_before_1 = as.numeric(stage %% 3L == 1L),
    outs_before_2 = as.numeric(stage %% 3L == 2L),
    extra_inning = as.numeric(stage >= 54L)
  )
  smooth_columns <- seq_len(ncol(smooth))
  if (length(smooth_columns)) {
    colnames(out)[1L + smooth_columns] <- paste0(
      "regulation_spline_", smooth_columns
    )
  }
  if (ncol(out) != spec$columns_per_role) {
    stop("Fixed-clock stage basis has an unexpected dimension", call. = FALSE)
  }
  out
}

.fixed_clock_direct_design_1d <- function(stage, role, basis_spec) {
  role <- as.character(role)
  roles <- fixed_clock_direct_policy_roles_1d()
  if (anyNA(role) || any(!role %in% roles)) {
    stop("Fixed-clock design roles must be offense or defense", call. = FALSE)
  }
  basis <- .fixed_clock_direct_stage_basis_1d(stage, basis_spec)
  p <- ncol(basis)
  out <- matrix(0, nrow = nrow(basis), ncol = p * length(roles))
  for (role_index in seq_along(roles)) {
    rows <- role == roles[[role_index]]
    columns <- (role_index - 1L) * p + seq_len(p)
    out[rows, columns] <- basis[rows, , drop = FALSE]
  }
  colnames(out) <- unlist(lapply(roles, function(value) {
    paste0(value, "_", colnames(basis))
  }), use.names = FALSE)
  out
}

.fixed_clock_direct_softplus_1d <- function(value) {
  value <- as.numeric(value)
  pmax(value, 0) + log1p(exp(-abs(value)))
}

.fixed_clock_direct_inverse_softplus_1d <- function(value) {
  value <- as.numeric(value)
  if (anyNA(value) || any(!is.finite(value)) || any(value <= 0)) {
    stop("Inverse softplus needs positive finite values", call. = FALSE)
  }
  value + log1p(-exp(-value))
}

.fixed_clock_direct_initial_parameters_1d <- function(
  design, basis_spec, initial_loss
) {
  initial_loss <- as.numeric(initial_loss)
  if (length(initial_loss) != 2L || anyNA(initial_loss) ||
      any(!is.finite(initial_loss)) || any(initial_loss <= 0) ||
      initial_loss[[1L]] < initial_loss[[2L]]) {
    stop("initial_loss must contain positive c(k1, k2) with k1 >= k2",
      call. = FALSE
    )
  }
  dimension <- ncol(design)
  p <- basis_spec$columns_per_role
  beta_k2 <- beta_gap <- numeric(dimension)
  intercepts <- 1L + (seq_along(fixed_clock_direct_policy_roles_1d()) - 1L) * p
  beta_k2[intercepts] <- .fixed_clock_direct_inverse_softplus_1d(
    initial_loss[[2L]]
  )
  beta_gap[intercepts] <- .fixed_clock_direct_inverse_softplus_1d(
    initial_loss[[1L]] - initial_loss[[2L]] + 1e-8
  )
  c(beta_k2, beta_gap)
}

.fixed_clock_direct_losses_1d <- function(parameter, design) {
  dimension <- ncol(design)
  parameter <- as.numeric(parameter)
  if (length(parameter) != 2L * dimension || anyNA(parameter) ||
      any(!is.finite(parameter))) {
    stop("Fixed-clock parameter vector is invalid", call. = FALSE)
  }
  loss_k2 <- .fixed_clock_direct_softplus_1d(
    as.numeric(design %*% parameter[seq_len(dimension)])
  )
  loss_k1 <- loss_k2 + .fixed_clock_direct_softplus_1d(
    as.numeric(design %*% parameter[dimension + seq_len(dimension)])
  )
  if (any(loss_k1 + 1e-14 < loss_k2) || any(loss_k2 < 0)) {
    stop("Fixed-clock loss parameterization violated L1 >= L2 >= 0")
  }
  list(k1 = loss_k1, k2 = loss_k2)
}

.fixed_clock_direct_lookup_threshold_masses_1d <- function(
  lookup, threshold, context
) {
  if (!inherits(lookup, "challenge_signal_lookup_1d")) {
    stop("action lookup must be a challenge_signal_lookup_1d", call. = FALSE)
  }
  size <- max(length(threshold), length(context))
  if (!size || any(c(length(threshold), length(context)) != 1L &
      c(length(threshold), length(context)) != size)) {
    stop("Threshold mass inputs must be scalar or aligned", call. = FALSE)
  }
  threshold <- rep_len(as.numeric(threshold), size)
  context <- rep_len(as.character(context), size)
  if (anyNA(threshold) || anyNA(context)) {
    stop("Threshold masses require non-missing inputs", call. = FALSE)
  }
  action <- success <- numeric(size)
  fallback <- logical(size)
  for (level in unique(context)) {
    index <- which(context == level)
    table <- .challenge_signal_lookup_table_1d(lookup, level)
    fallback[index] <- table$context_fallback || !level %in% names(lookup$tables)
    always <- index[threshold[index] == -Inf]
    never <- index[threshold[index] == Inf]
    finite <- setdiff(index[is.finite(threshold[index])], c(always, never))
    if (length(always)) {
      action[always] <- 1
      success[always] <- table$prior_ball_rate
    }
    if (length(finite)) {
      component_sd <- sqrt(
        lookup$prior_fit$sd^2 + lookup$perception_sigma^2
      )
      for (component in seq_along(table$component_weight)) {
        action[finite] <- action[finite] +
          table$component_weight[[component]] * stats::pnorm(
            threshold[finite],
            mean = lookup$prior_fit$mean[[component]],
            sd = component_sd[[component]],
            lower.tail = FALSE
          )
      }
      success[finite] <- stats::approx(
        table$signal, table$success_tail,
        xout = threshold[finite], rule = 2, ties = "ordered"
      )$y
      outside <- finite[
        threshold[finite] < min(table$signal) |
          threshold[finite] > max(table$signal)
      ]
      if (length(outside)) {
        success[outside] <- .challenge_signal_success_tail_exact_1d(
          lookup, threshold[outside], table$component_weight
        )
      }
    }
  }
  action <- pmin(1, pmax(0, action))
  success <- pmin(action, pmax(0, success))
  data.table::data.table(
    action_probability = action,
    success_and_action_probability = success,
    failure_and_action_probability = action - success,
    context_fallback = fallback
  )
}

.fixed_clock_direct_build_resources_1d <- function(
  clock, prior_fits, scenarios, compliance, lookup_grid_step,
  lookup_cache = NULL
) {
  roles <- fixed_clock_direct_policy_roles_1d()
  if (is.null(lookup_cache)) lookup_cache <- new.env(parent = emptyenv())
  if (!is.environment(lookup_cache)) {
    stop("lookup_cache must be an environment", call. = FALSE)
  }
  prior_hashes <- stats::setNames(vapply(
    roles,
    function(role) fixed_clock_hash_object_1d(prior_fits[[role]]),
    character(1L)
  ), roles)
  cached_lookup <- function(cache, key, builder) {
    if (!exists(key, envir = cache, inherits = FALSE)) {
      assign(key, builder(), envir = cache)
    }
    get(key, envir = cache, inherits = FALSE)
  }
  out <- vector("list", nrow(scenarios))
  names(out) <- scenarios$scenario_id
  for (scenario_index in seq_len(nrow(scenarios))) {
    scenario <- scenarios[scenario_index]
    role_resources <- lapply(roles, function(role) {
      role_value <- role
      role_rows <- clock[
        eligible == TRUE & get("role") == role_value
      ]
      contexts <- unique(as.character(role_rows$count_state))
      if (!length(contexts)) {
        stop("Every role needs at least one structural training opportunity",
          call. = FALSE
        )
      }
      sensory <- as.numeric(scenario[[paste0(role, "_sensory_sigma_inches")]])
      action_noise <- as.numeric(
        scenario[[paste0(role, "_action_sigma_inches")]]
      )
      total <- if (identical(compliance, "perfect")) {
        sensory
      } else {
        sqrt(sensory^2 + action_noise^2)
      }
      lookup_key <- function(sigma) fixed_clock_hash_object_1d(list(
        schema = "fixed_clock_signal_lookup_cache_v1",
        role = role,
        sigma = sigma,
        contexts = sort(contexts),
        grid_step = lookup_grid_step,
        prior_fit_sha256 = prior_hashes[[role]]
      ))
      q_key <- lookup_key(sensory)
      q_lookup <- cached_lookup(
        lookup_cache, q_key,
        function() build_challenge_signal_lookup_1d(
          prior_fits[[role]], sensory, contexts, grid_step = lookup_grid_step
        )
      )
      action_lookup <- if (abs(total - sensory) <= 1e-15) {
        q_lookup
      } else {
        action_key <- lookup_key(total)
        cached_lookup(
          lookup_cache, action_key,
          function() build_challenge_signal_lookup_1d(
            prior_fits[[role]], total, contexts, grid_step = lookup_grid_step
          )
        )
      }
      list(
        q_lookup = q_lookup,
        action_lookup = action_lookup,
        sensory_sigma_inches = sensory,
        action_sigma_inches = if (identical(compliance, "perfect")) {
          0
        } else {
          action_noise
        },
        action_total_sigma_inches = total
      )
    })
    names(role_resources) <- roles
    out[[scenario_index]] <- list(
      scenario_id = scenario$scenario_id[[1L]],
      roles = role_resources
    )
  }
  out
}

.fixed_clock_direct_policy_quantities_1d <- function(
  clock, design, parameter, resources
) {
  losses <- .fixed_clock_direct_losses_1d(parameter, design)
  n <- nrow(clock)
  result <- data.table::data.table(
    inventory_loss_k1 = losses$k1,
    inventory_loss_k2 = losses$k2,
    q_star_k1 = NA_real_,
    q_star_k2 = NA_real_,
    signal_threshold_k1_inches = Inf,
    signal_threshold_k2_inches = Inf,
    prior_action_probability_k1 = 0,
    prior_action_probability_k2 = 0,
    prior_success_probability_k1 = 0,
    prior_success_probability_k2 = 0,
    prior_failure_probability_k1 = 0,
    prior_failure_probability_k2 = 0,
    context_fallback_k1 = FALSE,
    context_fallback_k2 = FALSE,
    action_total_sigma_inches = 0,
    prior_action_probability_k0 = 0
  )
  if (nrow(result) != n) stop("Fixed-clock quantity allocation failed")
  active <- clock$eligible & clock$stake_G > 0
  for (role in fixed_clock_direct_policy_roles_1d()) {
    index <- which(active & clock$role == role)
    if (!length(index)) next
    role_resource <- resources$roles[[role]]
    result$action_total_sigma_inches[index] <-
      role_resource$action_total_sigma_inches
    for (inventory in 1:2) {
      loss <- losses[[paste0("k", inventory)]][index]
      terms <- challenge_signal_payoff_terms_1d(
        role_resource$q_lookup,
        gain = clock$stake_G[index],
        inventory_loss = loss,
        context = clock$count_state[index]
      )
      masses <- .fixed_clock_direct_lookup_threshold_masses_1d(
        role_resource$action_lookup,
        terms$threshold_inches,
        clock$count_state[index]
      )
      result[[paste0("q_star_k", inventory)]][index] <- terms$q_target
      result[[paste0("signal_threshold_k", inventory, "_inches")]][index] <-
        terms$threshold_inches
      result[[paste0("prior_action_probability_k", inventory)]][index] <-
        masses$action_probability
      result[[paste0("prior_success_probability_k", inventory)]][index] <-
        masses$success_and_action_probability
      result[[paste0("prior_failure_probability_k", inventory)]][index] <-
        masses$failure_and_action_probability
      result[[paste0("context_fallback_k", inventory)]][index] <-
        terms$context_fallback | masses$context_fallback
    }
  }
  # Inventory zero and non-positive gains are structurally forced to wait.
  result[]
}

.fixed_clock_direct_trace_value_1d <- function(
  clock, quantities, initial_inventory = 2L, keep_rows = FALSE
) {
  initial_inventory <- .fixed_clock_direct_scalar_integer_1d(
    initial_inventory, "initial_inventory", minimum = 0L
  )
  if (!initial_inventory %in% 0:2) {
    stop("initial_inventory must be 0, 1, or 2", call. = FALSE)
  }
  n <- nrow(clock)
  if (nrow(quantities) != n) {
    stop("Fixed-clock quantities must align row-for-row with the clock",
      call. = FALSE
    )
  }
  group_start <- c(
    TRUE,
    clock$game_pk[-1L] != clock$game_pk[-n] |
      clock$team_id[-1L] != clock$team_id[-n]
  )
  group_id <- cumsum(group_start)
  team_games <- max(group_id)
  group_sizes <- tabulate(group_id, nbins = team_games)
  position <- sequence(group_sizes)
  rows_by_position <- split(seq_len(n), position)

  mass <- matrix(0, nrow = team_games, ncol = 3L)
  mass[, initial_inventory + 1L] <- 1
  previous_inning <- rep(NA_integer_, team_games)
  total_expected_re <- 0
  if (isTRUE(keep_rows)) {
    inventory_before <- matrix(0, nrow = n, ncol = 3L)
    expected_reward <- expected_action <- expected_success <-
      expected_failure <- numeric(n)
    extra_grant <- logical(n)
  }

  for (rows in rows_by_position) {
    groups <- group_id[rows]
    inning_now <- clock$inning[rows]
    prior_inning <- previous_inning[groups]
    replenish <- (is.na(prior_inning) & inning_now > 9L) |
      (!is.na(prior_inning) & inning_now > prior_inning & inning_now > 9L)
    old <- mass[groups, , drop = FALSE]
    if (any(replenish)) {
      old[replenish, 2L] <- old[replenish, 2L] + old[replenish, 1L]
      old[replenish, 1L] <- 0
    }
    if (isTRUE(keep_rows)) {
      inventory_before[rows, ] <- old
      extra_grant[rows] <- replenish
    }

    action_k1 <- quantities$prior_action_probability_k1[rows]
    action_k2 <- quantities$prior_action_probability_k2[rows]
    success_k1 <- quantities$prior_success_probability_k1[rows]
    success_k2 <- quantities$prior_success_probability_k2[rows]
    failure_k1 <- quantities$prior_failure_probability_k1[rows]
    failure_k2 <- quantities$prior_failure_probability_k2[rows]
    row_action <- old[, 2L] * action_k1 + old[, 3L] * action_k2
    row_success <- old[, 2L] * success_k1 + old[, 3L] * success_k2
    row_failure <- old[, 2L] * failure_k1 + old[, 3L] * failure_k2
    row_reward <- row_success * clock$stake_G[rows]
    total_expected_re <- total_expected_re + sum(row_reward)

    updated <- old
    updated[, 1L] <- old[, 1L] + old[, 2L] * failure_k1
    updated[, 2L] <- old[, 2L] * (1 - failure_k1) +
      old[, 3L] * failure_k2
    updated[, 3L] <- old[, 3L] * (1 - failure_k2)
    row_mass <- rowSums(updated)
    if (any(updated < -1e-12) || any(abs(row_mass - 1) > 1e-10)) {
      stop("Fixed-clock prior inventory probability mass failed to close",
        call. = FALSE
      )
    }
    positive <- updated
    positive[positive < 0] <- 0
    updated <- positive / rowSums(positive)
    mass[groups, ] <- updated
    previous_inning[groups] <- inning_now

    if (isTRUE(keep_rows)) {
      expected_action[rows] <- row_action
      expected_success[rows] <- row_success
      expected_failure[rows] <- row_failure
      expected_reward[rows] <- row_reward
    }
  }
  result <- list(
    total_expected_re = total_expected_re,
    expected_re_per_team_game = total_expected_re / team_games,
    team_games = team_games
  )
  if (isTRUE(keep_rows)) {
    rows <- data.table::copy(clock)
    rows[, `:=`(
      inventory_probability_0 = inventory_before[, 1L],
      inventory_probability_1 = inventory_before[, 2L],
      inventory_probability_2 = inventory_before[, 3L],
      extra_inning_replenishment = extra_grant,
      prior_expected_challenges = expected_action,
      prior_expected_successes = expected_success,
      prior_expected_failures = expected_failure,
      prior_expected_captured_re = expected_reward
    )]
    result$rows <- cbind(rows, quantities)
  }
  result
}

.fixed_clock_direct_penalty_1d <- function(parameter, basis_spec) {
  p <- basis_spec$columns_per_role
  dimension <- length(parameter) / 2L
  intercept <- 1L +
    (seq_along(fixed_clock_direct_policy_roles_1d()) - 1L) * p
  penalized <- setdiff(seq_len(dimension), intercept)
  indices <- c(penalized, dimension + penalized)
  if (!length(indices)) return(0)
  mean(parameter[indices]^2)
}

.fixed_clock_direct_fit_candidate_1d <- function(
  clock, design, basis_spec, resources, initial_inventory, ridge,
  initial_loss, optimizer_control, initial_parameter = NULL
) {
  initial <- if (is.null(initial_parameter)) {
    .fixed_clock_direct_initial_parameters_1d(
      design, basis_spec, initial_loss
    )
  } else {
    value <- as.numeric(initial_parameter)
    if (length(value) != 2L * ncol(design) || anyNA(value) ||
        any(!is.finite(value))) {
      stop("initial_parameter is invalid for the fixed-clock design",
        call. = FALSE
      )
    }
    value
  }
  objective <- function(parameter) {
    quantities <- .fixed_clock_direct_policy_quantities_1d(
      clock, design, parameter, resources
    )
    value <- .fixed_clock_direct_trace_value_1d(
      clock, quantities, initial_inventory = initial_inventory
    )$expected_re_per_team_game
    -value + ridge * .fixed_clock_direct_penalty_1d(parameter, basis_spec)
  }
  run_optim <- function(method, parameter) {
    tryCatch(
      stats::optim(
        parameter, objective, method = method, control = optimizer_control
      ),
      error = function(error) structure(list(error = error), class = "optim_error")
    )
  }
  optimized <- run_optim("BFGS", initial)
  if (inherits(optimized, "optim_error") ||
      !is.finite(optimized$value %||% NA_real_)) {
    optimized <- run_optim("Nelder-Mead", initial)
  }
  if (inherits(optimized, "optim_error") ||
      !is.finite(optimized$value %||% NA_real_)) {
    stop("Fixed-clock direct policy optimization failed", call. = FALSE)
  }
  quantities <- .fixed_clock_direct_policy_quantities_1d(
    clock, design, optimized$par, resources
  )
  value <- .fixed_clock_direct_trace_value_1d(
    clock, quantities, initial_inventory = initial_inventory
  )
  list(
    parameter = optimized$par,
    objective = optimized$value,
    convergence = optimized$convergence,
    message = optimized$message %||% "",
    counts = optimized$counts,
    total_expected_re = value$total_expected_re,
    expected_re_per_team_game = value$expected_re_per_team_game
  )
}

.fixed_clock_direct_cross_evaluate_1d <- function(
  candidate_id, parameter, clock, design, scenarios, resources,
  initial_inventory
) {
  data.table::rbindlist(lapply(scenarios$scenario_id, function(scenario_id) {
    quantities <- .fixed_clock_direct_policy_quantities_1d(
      clock, design, parameter, resources[[scenario_id]]
    )
    value <- .fixed_clock_direct_trace_value_1d(
      clock, quantities, initial_inventory = initial_inventory
    )
    data.table::data.table(
      candidate_id = candidate_id,
      evaluation_scenario_id = scenario_id,
      total_expected_re = value$total_expected_re,
      expected_re_per_team_game = value$expected_re_per_team_game
    )
  }))
}

.fixed_clock_direct_candidate_summary_1d <- function(cross_evaluation) {
  out <- cross_evaluation[, .(
    worst_case_expected_re_per_team_game = min(expected_re_per_team_game),
    mean_expected_re_per_team_game = mean(expected_re_per_team_game),
    best_case_expected_re_per_team_game = max(expected_re_per_team_game)
  ), by = candidate_id]
  data.table::setorder(
    out,
    -worst_case_expected_re_per_team_game,
    -mean_expected_re_per_team_game,
    candidate_id
  )
  out
}

#' Fit robust direct fixed-clock challenge policies.
#'
#' One candidate is optimized under each perception scenario. Every candidate
#' is then evaluated under every scenario, and the candidate with the largest
#' worst-case expected RE per team-game is selected.
fit_fixed_clock_direct_policy_1d <- function(
  clock_rows,
  prior_fits,
  scenarios = NULL,
  effective_width = NULL,
  compliance = c("perfect", "noisy"),
  stage_df = 4L,
  ridge = 1e-4,
  initial_loss = c(k1 = 0.06, k2 = 0.03),
  initial_inventory = 2L,
  lookup_grid_step = 0.02,
  optimizer_control = list(maxit = 100L, reltol = 1e-8),
  initial_parameters = NULL,
  workers = 1L,
  progress = interactive()
) {
  compliance <- match.arg(compliance)
  clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  priors <- .fixed_clock_direct_validate_priors_1d(prior_fits)
  scenarios <- .fixed_clock_direct_normalize_scenarios_1d(
    scenarios, effective_width = effective_width
  )
  basis_spec <- .fixed_clock_direct_stage_basis_spec_1d(stage_df)
  design <- .fixed_clock_direct_design_1d(
    clock$stage, clock$role, basis_spec
  )
  ridge <- .fixed_clock_direct_scalar_number_1d(
    ridge, "ridge", lower = 0
  )
  lookup_grid_step <- .fixed_clock_direct_scalar_number_1d(
    lookup_grid_step, "lookup_grid_step", lower = 0, lower_open = TRUE
  )
  initial_inventory <- .fixed_clock_direct_scalar_integer_1d(
    initial_inventory, "initial_inventory", minimum = 0L
  )
  if (!initial_inventory %in% 0:2) {
    stop("initial_inventory must be 0, 1, or 2", call. = FALSE)
  }
  if (!is.list(optimizer_control)) {
    stop("optimizer_control must be a list", call. = FALSE)
  }
  if (is.null(initial_parameters)) {
    initial_parameters <- stats::setNames(
      vector("list", nrow(scenarios)), scenarios$scenario_id
    )
  } else {
    if (!is.list(initial_parameters) || is.null(names(initial_parameters)) ||
        anyNA(names(initial_parameters)) || any(!nzchar(names(initial_parameters))) ||
        anyDuplicated(names(initial_parameters))) {
      stop(
        "initial_parameters must be a uniquely named scenario list",
        call. = FALSE
      )
    }
    unknown_initial_parameters <- setdiff(
      names(initial_parameters), scenarios$scenario_id
    )
    if (length(unknown_initial_parameters)) {
      stop(
        "initial_parameters contains unknown scenarios: ",
        paste(unknown_initial_parameters, collapse = ", "), call. = FALSE
      )
    }
    missing_initial_parameters <- setdiff(
      scenarios$scenario_id, names(initial_parameters)
    )
    initial_parameters[missing_initial_parameters] <- vector(
      "list", length(missing_initial_parameters)
    )
    initial_parameters <- initial_parameters[scenarios$scenario_id]
  }
  invalid_initial_parameters <- vapply(
    initial_parameters,
    function(value) {
      !is.null(value) && (
        !is.numeric(value) || length(value) != 2L * ncol(design) ||
          anyNA(value) || any(!is.finite(value))
      )
    },
    logical(1L)
  )
  if (any(invalid_initial_parameters)) {
    stop(
      "initial_parameters is invalid for scenarios: ",
      paste(names(initial_parameters)[invalid_initial_parameters], collapse = ", "),
      call. = FALSE
    )
  }
  workers <- .fixed_clock_direct_scalar_integer_1d(
    workers, "workers", minimum = 1L
  )
  resources <- .fixed_clock_direct_build_resources_1d(
    clock, priors, scenarios, compliance, lookup_grid_step
  )

  fit_candidate <- function(scenario_index) {
    scenario_id <- scenarios$scenario_id[[scenario_index]]
    if (isTRUE(progress) && workers == 1L) {
      message(
        "fixed-clock candidate ", scenario_index, "/", nrow(scenarios),
        ": ", scenario_id
      )
    }
    candidate <- .fixed_clock_direct_fit_candidate_1d(
      clock, design, basis_spec, resources[[scenario_id]],
      initial_inventory, ridge, initial_loss, optimizer_control,
      initial_parameter = initial_parameters[[scenario_id]]
    )
    candidate$candidate_id <- scenario_id
    candidate$training_scenario_id <- scenario_id
    candidate
  }
  scenario_indices <- seq_len(nrow(scenarios))
  if (isTRUE(progress) && workers > 1L) {
    message(
      "fitting ", nrow(scenarios), " fixed-clock candidates on ",
      min(workers, nrow(scenarios)), " fork workers"
    )
  }
  candidate_fits <- if (
    workers > 1L && nrow(scenarios) > 1L && .Platform$OS.type != "windows"
  ) {
    parallel::mclapply(
      scenario_indices,
      fit_candidate,
      mc.cores = min(workers, nrow(scenarios)),
      mc.preschedule = FALSE,
      mc.set.seed = FALSE
    )
  } else {
    lapply(scenario_indices, fit_candidate)
  }
  failed_candidates <- vapply(
    candidate_fits, inherits, logical(1L), what = "try-error"
  )
  if (any(failed_candidates)) {
    stop(
      "At least one parallel fixed-clock candidate fit failed: ",
      paste(scenarios$scenario_id[failed_candidates], collapse = ", "),
      call. = FALSE
    )
  }
  names(candidate_fits) <- scenarios$scenario_id

  cross_evaluate_candidate <- function(candidate_id) {
      .fixed_clock_direct_cross_evaluate_1d(
        candidate_id,
        candidate_fits[[candidate_id]]$parameter,
        clock,
        design,
        scenarios,
        resources,
        initial_inventory
      )
  }
  cross_parts <- if (
    workers > 1L && length(candidate_fits) > 1L &&
      .Platform$OS.type != "windows"
  ) {
    parallel::mclapply(
      names(candidate_fits),
      cross_evaluate_candidate,
      mc.cores = min(workers, length(candidate_fits)),
      mc.preschedule = FALSE,
      mc.set.seed = FALSE
    )
  } else {
    lapply(names(candidate_fits), cross_evaluate_candidate)
  }
  failed_cross_evaluations <- vapply(
    cross_parts, inherits, logical(1L), what = "try-error"
  )
  if (any(failed_cross_evaluations)) {
    stop("At least one parallel candidate cross-evaluation failed",
      call. = FALSE
    )
  }
  cross_evaluation <- data.table::rbindlist(cross_parts)
  candidate_summary <- .fixed_clock_direct_candidate_summary_1d(
    cross_evaluation
  )
  selected_candidate_id <- candidate_summary$candidate_id[[1L]]

  # One scenario-exchange refinement: optimize locally against the currently
  # binding scenario, then retain the proposal only when a full cross-check
  # improves the maximin criterion (with mean value as the deterministic tie
  # breaker). This adds one local fit, not a costly nested robust optimizer.
  selected_rows <- cross_evaluation[
    candidate_id == selected_candidate_id
  ][order(expected_re_per_team_game, evaluation_scenario_id)]
  binding_scenario_id <- selected_rows$evaluation_scenario_id[[1L]]
  baseline_fit <- candidate_fits[[selected_candidate_id]]
  proposal_fit <- .fixed_clock_direct_fit_candidate_1d(
    clock,
    design,
    basis_spec,
    resources[[binding_scenario_id]],
    initial_inventory,
    ridge,
    initial_loss,
    optimizer_control,
    initial_parameter = baseline_fit$parameter
  )
  proposal_rows <- .fixed_clock_direct_cross_evaluate_1d(
    selected_candidate_id,
    proposal_fit$parameter,
    clock,
    design,
    scenarios,
    resources,
    initial_inventory
  )
  baseline_score <- candidate_summary[candidate_id == selected_candidate_id]
  proposal_score <- .fixed_clock_direct_candidate_summary_1d(proposal_rows)
  tolerance <- 1e-12
  refinement_accepted <-
    proposal_score$worst_case_expected_re_per_team_game[[1L]] >
      baseline_score$worst_case_expected_re_per_team_game[[1L]] + tolerance ||
    (
      abs(
        proposal_score$worst_case_expected_re_per_team_game[[1L]] -
          baseline_score$worst_case_expected_re_per_team_game[[1L]]
      ) <= tolerance &&
        proposal_score$worst_case_expected_re_per_team_game[[1L]] >=
          baseline_score$worst_case_expected_re_per_team_game[[1L]] &&
        proposal_score$mean_expected_re_per_team_game[[1L]] >
          baseline_score$mean_expected_re_per_team_game[[1L]] + tolerance
    )
  robust_refinement <- list(
    method = "one_step_binding_scenario_exchange",
    binding_scenario_id = binding_scenario_id,
    accepted = refinement_accepted,
    baseline_parameter = baseline_fit$parameter,
    proposal_parameter = proposal_fit$parameter,
    baseline_score = baseline_score[],
    proposal_score = proposal_score[],
    proposal_convergence = proposal_fit$convergence,
    proposal_message = proposal_fit$message
  )
  if (isTRUE(refinement_accepted)) {
    proposal_fit$candidate_id <- selected_candidate_id
    proposal_fit$training_scenario_id <- baseline_fit$training_scenario_id
    proposal_fit$robust_refined <- TRUE
    proposal_fit$robust_refinement_scenario_id <- binding_scenario_id
    candidate_fits[[selected_candidate_id]] <- proposal_fit
    cross_evaluation <- data.table::rbindlist(list(
      cross_evaluation[candidate_id != selected_candidate_id],
      proposal_rows
    ))
    candidate_summary <- .fixed_clock_direct_candidate_summary_1d(
      cross_evaluation
    )
    selected_candidate_id <- candidate_summary$candidate_id[[1L]]
  }
  candidate_summary[, maximin_selected := candidate_id == selected_candidate_id]
  candidate_optimization <- data.table::rbindlist(lapply(
    names(candidate_fits), function(candidate_id) {
      candidate <- candidate_fits[[candidate_id]]
      data.table::data.table(
        candidate_id = candidate_id,
        convergence = as.integer(candidate$convergence),
        converged = identical(as.integer(candidate$convergence), 0L),
        objective = as.numeric(candidate$objective),
        function_evaluations = as.integer(
          candidate$counts[["function"]] %||% NA_integer_
        ),
        gradient_evaluations = as.integer(
          candidate$counts[["gradient"]] %||% NA_integer_
        ),
        message = as.character(candidate$message %||% "")
      )
    }
  ))

  fit <- list(
    schema = "fixed_clock_direct_policy_v1",
    scenarios = scenarios[],
    candidate_fits = candidate_fits,
    cross_evaluation = cross_evaluation[],
    candidate_summary = candidate_summary[],
    candidate_optimization = candidate_optimization[],
    selected_candidate_id = selected_candidate_id,
    robust_refinement = robust_refinement,
    prior_fits = priors,
    basis_spec = basis_spec,
    design_columns = colnames(design),
    compliance = compliance,
    ridge = ridge,
    initial_loss = as.numeric(initial_loss),
    initial_inventory = initial_inventory,
    lookup_grid_step = lookup_grid_step,
    training_games = sort(unique(clock$game_pk)),
    training_team_games = data.table::uniqueN(clock[, .(game_pk, team_id)]),
    training_rows = nrow(clock),
    training_structural_rows = sum(clock$eligible),
    candidate_workers = workers,
    warm_started_scenarios = names(initial_parameters)[vapply(
      initial_parameters, Negate(is.null), logical(1L)
    )],
    initial_parameters_sha256 = fixed_clock_hash_object_1d(
      initial_parameters
    ),
    prior_training_games = lapply(priors, function(value) {
      sort(as.character(value$training_games %||% character()))
    }),
    excluded_confirmation_columns = .fixed_clock_direct_truth_columns_1d(),
    information_regime = paste(
      "direct policy search on the factual opportunity clock;",
      "public context plus latent noisy signal; confirmation geometry excluded;",
      "one shared offense/defense team inventory"
    )
  )
  class(fit) <- "fixed_clock_direct_policy_1d"
  fit
}

.fixed_clock_direct_conditional_action_1d <- function(
  margin, threshold, sigma, active
) {
  margin <- as.numeric(margin)
  threshold <- as.numeric(threshold)
  sigma <- as.numeric(sigma)
  active <- as.logical(active)
  if (anyNA(margin) || any(!is.finite(margin)) || anyNA(threshold) ||
      anyNA(sigma) || any(!is.finite(sigma)) || any(sigma < 0) ||
      anyNA(active)) {
    stop("Conditional fixed-clock action inputs are invalid", call. = FALSE)
  }
  out <- numeric(length(margin))
  deterministic <- active & sigma == 0
  stochastic <- active & sigma > 0
  out[deterministic] <- as.numeric(
    margin[deterministic] > threshold[deterministic]
  )
  out[stochastic] <- stats::pnorm(
    (margin[stochastic] - threshold[stochastic]) / sigma[stochastic]
  )
  out[threshold == -Inf & active] <- 1
  out[threshold == Inf | !active] <- 0
  pmin(1, pmax(0, out))
}

.fixed_clock_direct_evaluation_summary_1d <- function(replay, mapping) {
  game_role <- .revealed_policy_game_role_summary_1d(replay)
  by_role <- game_role[, .(
    captured_re = sum(captured_re),
    attempts = sum(attempts),
    successes = sum(successes),
    failures = sum(failures),
    exhausted_opportunity_mass = sum(exhausted_opportunity_mass),
    opportunity_exposure = sum(opportunity_exposure)
  ), by = .(policy, policy_family, role)]
  combined <- by_role[, .(
    captured_re = sum(captured_re),
    attempts = sum(attempts),
    successes = sum(successes),
    failures = sum(failures),
    exhausted_opportunity_mass = sum(exhausted_opportunity_mass),
    opportunity_exposure = sum(opportunity_exposure),
    role = "combined"
  ), by = .(policy, policy_family)]
  season <- data.table::rbindlist(list(by_role, combined), use.names = TRUE)
  season[, `:=`(
    success_rate = ifelse(attempts > 0, successes / attempts, NA_real_),
    inventory_exhaustion_rate = ifelse(
      opportunity_exposure > 0,
      exhausted_opportunity_mass / opportunity_exposure,
      NA_real_
    )
  )]
  season <- merge(season, mapping, by = c("policy", "policy_family"), all.x = TRUE)
  data.table::setorder(season, evaluation_scenario_id, role)
  list(game_role = game_role[], season = season[])
}

.fixed_clock_direct_restore_policy_1d <- function(frozen_policy) {
  validate_frozen_fixed_clock_policy_1d(frozen_policy)
  policy <- frozen_policy$policy
  if (!is.list(policy) ||
      !identical(policy$schema, "fixed_clock_direct_policy_v1")) {
    stop("Frozen object does not contain a fixed-clock direct policy",
      call. = FALSE
    )
  }
  required <- c(
    "scenarios", "candidate_fits", "selected_candidate_id", "prior_fits",
    "basis_spec", "compliance", "initial_inventory", "lookup_grid_step"
  )
  absent <- setdiff(required, names(policy))
  if (length(absent)) {
    stop(
      "Frozen direct policy is missing: ", paste(absent, collapse = ", "),
      call. = FALSE
    )
  }
  policy$scenarios <- data.table::as.data.table(policy$scenarios)
  roles <- fixed_clock_direct_policy_roles_1d()
  if (!is.list(policy$prior_fits) ||
      !all(roles %in% names(policy$prior_fits))) {
    stop("Frozen direct policy omits role-specific priors", call. = FALSE)
  }
  policy$prior_fits <- lapply(
    policy$prior_fits[roles], .fixed_clock_direct_restore_prior_fit_1d
  )
  invisible(lapply(policy$prior_fits, .challenge_margin_validate_fit))
  policy
}

#' Evaluate a content-frozen policy with confirmation geometry.
#'
#' `frozen_policy` is created by the content-addressed
#' freeze_fixed_clock_policy_1d(). Public confirmation rows are transformed
#' into thresholds before geometry is joined. Geometry then integrates the
#' already-fixed noisy-signal action probability and scores success.
evaluate_fixed_clock_policy_1d <- function(
  frozen_policy,
  clock_rows,
  truth_rows,
  candidate_id = NULL,
  scenario_ids = NULL,
  initial_inventory = NULL,
  require_game_separation = TRUE,
  evaluation_gain_rows = NULL,
  prior_fits_override = NULL,
  scenarios_override = NULL,
  effective_width_override = NULL,
  compliance_override = NULL,
  parameter_override = NULL,
  evaluation_mode = c(
    "frozen", "bootstrap_nuisance", "bootstrap_procedure", "sensitivity"
  ),
  override_provenance = NULL,
  return_level = c("full", "game_role"),
  lookup_cache = NULL
) {
  evaluation_mode <- match.arg(evaluation_mode)
  return_level <- match.arg(return_level)
  if (is.null(lookup_cache)) lookup_cache <- new.env(parent = emptyenv())
  if (!is.environment(lookup_cache)) {
    stop("lookup_cache must be an environment", call. = FALSE)
  }
  policy <- .fixed_clock_direct_restore_policy_1d(frozen_policy)
  clock <- .fixed_clock_direct_normalize_clock_1d(clock_rows)
  if (isTRUE(require_game_separation)) {
    overlap <- intersect(
      unique(clock$game_pk), as.character(frozen_policy$training_game_ids)
    )
    if (length(overlap)) {
      stop("Confirmation clock overlaps frozen-policy training games",
        call. = FALSE
      )
    }
  }
  override_flags <- c(
    candidate = !is.null(candidate_id),
    initial_inventory = !is.null(initial_inventory),
    prior_fits = !is.null(prior_fits_override),
    scenarios = !is.null(scenarios_override) ||
      !is.null(effective_width_override),
    compliance = !is.null(compliance_override),
    parameter = !is.null(parameter_override)
  )
  allowed <- switch(
    evaluation_mode,
    frozen = character(),
    bootstrap_nuisance = c("prior_fits", "scenarios"),
    bootstrap_procedure = c("prior_fits", "scenarios", "parameter"),
    sensitivity = names(override_flags)
  )
  disallowed <- names(override_flags)[override_flags & !names(override_flags) %in% allowed]
  if (length(disallowed)) {
    stop(
      "Action-changing overrides are not allowed in ", evaluation_mode,
      " mode: ", paste(disallowed, collapse = ", "), call. = FALSE
    )
  }
  override_sha256 <- NA_character_
  if (any(override_flags)) {
    if (!is.list(override_provenance) || is.null(names(override_provenance))) {
      stop("Action-changing overrides require named override_provenance",
        call. = FALSE
      )
    }
    if (evaluation_mode %in% c("bootstrap_nuisance", "bootstrap_procedure")) {
      required_provenance <- c(
        "replicate_id", "source_game_ids", "game_weight_sha256",
        "source_sha256", "fit_sha256"
      )
      missing_provenance <- setdiff(
        required_provenance, names(override_provenance)
      )
      if (length(missing_provenance)) {
        stop(
          "Bootstrap override provenance is missing: ",
          paste(missing_provenance, collapse = ", "), call. = FALSE
        )
      }
      override_training_games <- sort(unique(as.character(unlist(
        override_provenance$source_game_ids,
        recursive = TRUE, use.names = FALSE
      ))))
      if (length(intersect(unique(clock$game_pk), override_training_games))) {
        stop("Override training games overlap the evaluation clock",
          call. = FALSE
        )
      }
    } else if (evaluation_mode == "sensitivity" &&
        (is.null(override_provenance$label) ||
          !nzchar(as.character(override_provenance$label[[1L]])))) {
      stop("Sensitivity overrides require a provenance label", call. = FALSE)
    }
    override_sha256 <- fixed_clock_hash_object_1d(override_provenance)
  } else if (!is.null(override_provenance)) {
    stop("override_provenance was supplied without an override", call. = FALSE)
  }
  if (is.null(candidate_id)) candidate_id <- policy$selected_candidate_id
  candidate_id <- as.character(candidate_id)
  if (length(candidate_id) != 1L || is.na(candidate_id) ||
      !candidate_id %in% names(policy$candidate_fits)) {
    stop("candidate_id is not present in the frozen policy", call. = FALSE)
  }
  priors <- if (is.null(prior_fits_override)) {
    policy$prior_fits
  } else {
    .fixed_clock_direct_validate_priors_1d(prior_fits_override)
  }
  scenarios <- if (is.null(scenarios_override)) {
    policy$scenarios
  } else {
    .fixed_clock_direct_normalize_scenarios_1d(
      scenarios_override, effective_width = effective_width_override
    )
  }
  compliance <- if (is.null(compliance_override)) {
    policy$compliance
  } else {
    match.arg(as.character(compliance_override), c("perfect", "noisy"))
  }
  if (is.null(scenario_ids)) scenario_ids <- scenarios$scenario_id
  scenario_ids <- unique(as.character(scenario_ids))
  if (!length(scenario_ids) || anyNA(scenario_ids) ||
      any(!scenario_ids %in% scenarios$scenario_id)) {
    stop("scenario_ids must select available perception scenarios", call. = FALSE)
  }
  if (return_level == "game_role" && length(scenario_ids) > 1L) {
    game_parts <- season_parts <- vector("list", length(scenario_ids))
    scenario_metadata <- vector("list", length(scenario_ids))
    candidate_argument <- if (isTRUE(override_flags[["candidate"]])) {
      candidate_id
    } else {
      NULL
    }
    for (scenario_index in seq_along(scenario_ids)) {
      scenario_id <- scenario_ids[[scenario_index]]
      one <- evaluate_fixed_clock_policy_1d(
        frozen_policy = frozen_policy,
        clock_rows = clock,
        truth_rows = truth_rows,
        candidate_id = candidate_argument,
        scenario_ids = scenario_id,
        initial_inventory = initial_inventory,
        require_game_separation = require_game_separation,
        evaluation_gain_rows = evaluation_gain_rows,
        prior_fits_override = prior_fits_override,
        scenarios_override = scenarios_override,
        effective_width_override = effective_width_override,
        compliance_override = compliance_override,
        parameter_override = parameter_override,
        evaluation_mode = evaluation_mode,
        override_provenance = override_provenance,
        return_level = "full",
        lookup_cache = lookup_cache
      )
      game_parts[[scenario_index]] <- one$game_role
      season_parts[[scenario_index]] <- one$season
      scenario_metadata[[scenario_index]] <- data.table::data.table(
        scenario_id = scenario_id,
        decision_spec_sha256 = one$decision_spec_sha256
      )
      rm(one)
      if (scenario_index %% 5L == 0L) invisible(gc(FALSE))
    }
    combined_game_role <- data.table::rbindlist(game_parts, use.names = TRUE)
    combined_season <- data.table::rbindlist(season_parts, use.names = TRUE)
    data.table::setorder(combined_game_role, policy, game_pk, team_id, role)
    data.table::setorder(combined_season, policy, role)
    out <- list(
      policy_actions = data.table::data.table(),
      replay = data.table::data.table(),
      game_role = combined_game_role,
      season = combined_season,
      candidate_id = candidate_id,
      scenario_ids = scenario_ids,
      compliance = compliance,
      evaluation_mode = evaluation_mode,
      override_flags = override_flags,
      override_sha256 = override_sha256,
      frozen_policy_sha256 = frozen_policy$policy_sha256,
      decision_spec_sha256 = fixed_clock_hash_object_1d(
        data.table::rbindlist(scenario_metadata)
      ),
      scenario_decision_specs = data.table::rbindlist(scenario_metadata),
      G_decision_sha256 = fixed_clock_hash_object_1d(
        clock[, c(
          fixed_clock_direct_policy_key_columns_1d(), "stake_G"
        ), with = FALSE]
      ),
      G_evaluation_differs = !is.null(evaluation_gain_rows),
      parameter_overridden = !is.null(parameter_override),
      detail_retained = FALSE,
      information_regime = paste(
        "confirmation geometry joined only after policy freeze;",
        "truth conditions evaluation probabilities but never the action rule;",
        "scenarios evaluated sequentially with game-role summaries retained"
      )
    )
    class(out) <- "fixed_clock_policy_evaluation_1d"
    return(out)
  }
  resources <- .fixed_clock_direct_build_resources_1d(
    clock,
    priors,
    scenarios[scenario_id %in% scenario_ids],
    compliance,
    policy$lookup_grid_step,
    lookup_cache = lookup_cache
  )
  design <- .fixed_clock_direct_design_1d(
    clock$stage, clock$role, policy$basis_spec
  )
  candidate <- policy$candidate_fits[[candidate_id]]
  parameter <- if (is.null(parameter_override)) {
    candidate$parameter
  } else {
    value <- as.numeric(parameter_override)
    if (length(value) != length(candidate$parameter) || anyNA(value) ||
        any(!is.finite(value))) {
      stop("parameter_override is invalid for the frozen policy",
        call. = FALSE
      )
    }
    value
  }
  actions <- data.table::rbindlist(lapply(scenario_ids, function(scenario_id) {
    quantities <- .fixed_clock_direct_policy_quantities_1d(
      clock, design, parameter, resources[[scenario_id]]
    )
    out <- cbind(data.table::copy(clock), quantities)
    out[, `:=`(
      G_decision = stake_G,
      candidate_id = candidate_id,
      evaluation_scenario_id = scenario_id,
      compliance = compliance,
      truth_used_by_decision_rule = FALSE,
      geometry_used_for_signal_integration = FALSE,
      truth_used_by_action_rule = FALSE
    )]
    out
  }))
  truth <- normalize_revealed_policy_truth_1d(truth_rows)
  keys <- fixed_clock_direct_policy_key_columns_1d()
  base_clock <- unique(actions[, c(
    keys, "inning", "role", "stake_G", "stage", "count_state",
    "decision_mode"
  ), with = FALSE])
  if (anyDuplicated(base_clock[, ..keys])) {
    stop("Frozen policy has inconsistent scenario clocks", call. = FALSE)
  }
  evaluation_clock <- data.table::copy(base_clock)
  if (!is.null(evaluation_gain_rows)) {
    gain <- data.table::copy(data.table::as.data.table(evaluation_gain_rows))
    .fixed_clock_direct_assert_truth_free_1d(
      gain, "fixed-clock evaluation gains"
    )
    stop_if_missing_columns(
      gain, c(keys, "G_evaluation"), "fixed-clock evaluation gains"
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
    evaluation_clock <- merge(
      evaluation_clock, gain, by = keys, all.x = TRUE, sort = FALSE
    )
    if (anyNA(evaluation_clock$G_evaluation)) {
      stop("Evaluation gains must cover the confirmation clock exactly",
        call. = FALSE
      )
    }
    evaluation_clock[, stake_G := G_evaluation][, G_evaluation := NULL]
  }
  truth_key <- do.call(paste, c(truth[, ..keys], sep = "\r"))
  clock_key <- do.call(paste, c(base_clock[, ..keys], sep = "\r"))
  if (length(truth_key) != length(clock_key) ||
      !setequal(truth_key, clock_key)) {
    stop("Confirmation geometry must cover the frozen clock exactly",
      call. = FALSE
    )
  }
  joined <- merge(
    actions,
    truth[, c(keys, "role", "role_margin_inches"), with = FALSE],
    by = keys, all.x = TRUE, allow.cartesian = TRUE, sort = FALSE,
    suffixes = c("", ".truth")
  )
  if (any(joined$role != joined$role.truth)) {
    stop("Frozen clock and confirmation geometry disagree on role",
      call. = FALSE
    )
  }
  active <- joined$eligible & joined$stake_G > 0
  probability_k1 <- .fixed_clock_direct_conditional_action_1d(
    joined$role_margin_inches,
    joined$signal_threshold_k1_inches,
    joined$action_total_sigma_inches,
    active
  )
  probability_k2 <- .fixed_clock_direct_conditional_action_1d(
    joined$role_margin_inches,
    joined$signal_threshold_k2_inches,
    joined$action_total_sigma_inches,
    active
  )
  policy_mapping <- unique(joined[, .(
    evaluation_scenario_id,
    candidate_id,
    compliance
  )])
  policy_mapping[, `:=`(
    policy = paste0("fixed_clock__", evaluation_scenario_id),
    policy_family = "fixed_clock_direct_policy_1d"
  )]
  probability <- joined[, c(keys), with = FALSE]
  probability[, `:=`(
    evaluation_scenario_id = joined$evaluation_scenario_id,
    probability_k1 = probability_k1,
    probability_k2 = probability_k2
  )]
  probability <- merge(
    probability,
    policy_mapping[, .(
      evaluation_scenario_id, policy, policy_family
    )],
    by = "evaluation_scenario_id", all.x = TRUE, sort = FALSE
  )
  probability[, `:=`(
    truth_used_by_decision_rule = FALSE,
    geometry_used_for_signal_integration = TRUE,
    truth_used_by_action_rule = FALSE
  )]
  probability <- probability[, .(
    game_pk, team_id, pitch_order, policy, policy_family,
    probability_k1, probability_k2,
    truth_used_by_decision_rule,
    geometry_used_for_signal_integration,
    truth_used_by_action_rule
  )]
  if (is.null(initial_inventory)) initial_inventory <- policy$initial_inventory
  replay <- replay_revealed_policy_value_1d(
    evaluation_clock,
    probability,
    truth[, c(keys, "role", "role_margin_inches"), with = FALSE],
    initial_inventory = initial_inventory
  )
  mapping <- policy_mapping[, .(
    policy, policy_family, evaluation_scenario_id,
    candidate_id, compliance
  )]
  replay <- merge(
    replay, mapping, by = c("policy", "policy_family"), all.x = TRUE,
    sort = FALSE
  )
  data.table::setorder(
    replay, evaluation_scenario_id, game_pk, team_id, pitch_order
  )
  summary <- .fixed_clock_direct_evaluation_summary_1d(replay, mapping)
  out <- list(
    policy_actions = actions[],
    replay = replay[],
    game_role = summary$game_role,
    season = summary$season,
    candidate_id = candidate_id,
    scenario_ids = scenario_ids,
    compliance = compliance,
    evaluation_mode = evaluation_mode,
    override_flags = override_flags,
    override_sha256 = override_sha256,
    frozen_policy_sha256 = frozen_policy$policy_sha256,
    decision_spec_sha256 = fixed_clock_hash_object_1d(list(
      candidate_id = candidate_id,
      parameter = parameter,
      prior_fits = priors,
      scenarios = scenarios[scenario_id %in% scenario_ids],
      compliance = compliance,
      initial_inventory = initial_inventory,
      basis_spec = policy$basis_spec
    )),
    G_decision_sha256 = fixed_clock_hash_object_1d(
      base_clock[, c(keys, "stake_G"), with = FALSE]
    ),
    G_evaluation_differs = !is.null(evaluation_gain_rows),
    parameter_overridden = !is.null(parameter_override),
    detail_retained = return_level == "full",
    information_regime = paste(
      "confirmation geometry joined only after policy freeze;",
      "truth conditions evaluation probabilities but never the action rule"
    )
  )
  if (return_level == "game_role") {
    out$policy_actions <- data.table::data.table()
    out$replay <- data.table::data.table()
  }
  class(out) <- "fixed_clock_policy_evaluation_1d"
  out
}
