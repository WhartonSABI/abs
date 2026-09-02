joint_perception_required_columns <- function() {
  c(
    "game_pk", "pitch_order", "initial_call", "abs_call", "plate_x",
    "plate_z", "sz_bot", "sz_top", "umpire_name", "fielder_2"
  )
}

assign_joint_perception_cells <- function(pitches, bin_width) {
  x <- data.table::copy(data.table::as.data.table(pitches))
  stop_if_missing_columns(x, joint_perception_required_columns(),
    "joint Bayesian perception pitches")
  bin_width <- as.numeric(bin_width)
  if (length(bin_width) != 1L || !is.finite(bin_width) || bin_width <= 0) {
    stop("bin_width must be one positive number")
  }
  if (!"tracking_available" %in% names(x)) x[, tracking_available := TRUE]
  if (!"edge_distance_inches" %in% names(x)) x[, edge_distance_inches := 0]
  x <- x[
    tracking_available %in% TRUE & is.finite(edge_distance_inches) &
      abs(edge_distance_inches) <= 12 &
      initial_call %in% c("ball", "called_strike") &
      is.finite(plate_x) & is.finite(plate_z) & is.finite(sz_bot) &
      is.finite(sz_top) & sz_top > sz_bot & !is.na(umpire_name) &
      !is.na(fielder_2)
  ]
  x[, `:=`(
    x_in = plate_x * 12,
    z_in = (plate_z - sz_bot) / (sz_top - sz_bot) * 20,
    call_wrong = as.integer(initial_call != abs_call),
    role = data.table::fifelse(initial_call == "called_strike", "offense", "defense")
  )]
  x[, `:=`(
    ix = as.integer(round(x_in / bin_width)),
    iz = as.integer(round(z_in / bin_width))
  )]
  x[, `:=`(
    cell = paste(ix, iz, sep = "_"),
    initial_call = factor(initial_call, levels = c("ball", "called_strike")),
    umpire = factor(umpire_name),
    catcher = factor(fielder_2)
  )]
  x[, cell_call := interaction(cell, initial_call, sep = "|", drop = TRUE)]
  data.table::setattr(x, "bin_width", bin_width)
  x[]
}

rook_neighbor_list <- function(cell_table) {
  ct <- data.table::as.data.table(cell_table)
  lapply(seq_len(nrow(ct)), function(i) {
    which(
      (abs(ct$ix - ct$ix[[i]]) == 1L & ct$iz == ct$iz[[i]]) |
        (ct$ix == ct$ix[[i]] & abs(ct$iz - ct$iz[[i]]) == 1L)
    )
  })
}

largest_rook_component <- function(cell_table) {
  ct <- data.table::copy(data.table::as.data.table(cell_table))
  data.table::setorder(ct, ix, iz)
  if (!nrow(ct)) stop("No spatial cells are available")
  neighbors <- rook_neighbor_list(ct)
  component <- integer(nrow(ct))
  component_id <- 0L
  for (start in seq_len(nrow(ct))) {
    if (component[[start]] != 0L) next
    component_id <- component_id + 1L
    queue <- start
    component[[start]] <- component_id
    while (length(queue)) {
      current <- queue[[1L]]
      queue <- queue[-1L]
      new <- neighbors[[current]][component[neighbors[[current]]] == 0L]
      if (length(new)) {
        component[new] <- component_id
        queue <- c(queue, new)
      }
    }
  }
  sizes <- tabulate(component)
  ct[component == which.max(sizes)]
}

joint_rook_adjacency <- function(cell_table) {
  ct <- data.table::copy(data.table::as.data.table(cell_table))
  data.table::setorder(ct, ix, iz)
  neighbors <- rook_neighbor_list(ct)
  edges <- do.call(rbind, lapply(seq_along(neighbors), function(i) {
    if (!length(neighbors[[i]])) return(NULL)
    cbind(i, neighbors[[i]])
  }))
  if (is.null(edges) || !nrow(edges)) stop("Joint spatial graph has no edges")
  matrix <- Matrix::sparseMatrix(
    i = edges[, 1L], j = edges[, 2L], x = 1,
    dims = c(nrow(ct), nrow(ct)),
    dimnames = list(ct$cell, ct$cell)
  )
  matrix
}

prepare_joint_car_input <- function(pitches, bin_width, full = TRUE) {
  x <- assign_joint_perception_cells(pitches, bin_width)
  all_cells <- unique(x[, .(ix, iz, cell)])
  cells <- largest_rook_component(all_cells)
  x <- x[cell %in% cells$cell]
  data.table::setorder(cells, ix, iz)
  M <- joint_rook_adjacency(cells)
  group <- c("cell", "initial_call", "cell_call")
  if (isTRUE(full)) group <- c(group, "umpire", "catcher")
  aggregated <- x[, .(n = .N, wrong = sum(call_wrong)), by = group]
  aggregated[, cell := factor(as.character(cell), levels = cells$cell)]
  aggregated[, initial_call := factor(initial_call, levels = c("ball", "called_strike"))]
  aggregated[, cell_call := factor(cell_call)]
  if (isTRUE(full)) {
    aggregated[, `:=`(umpire = factor(umpire), catcher = factor(catcher))]
  }
  if (sum(aggregated$n) != nrow(x)) stop("Joint aggregation did not conserve pitches")
  list(
    data = aggregated,
    M = M,
    cell_table = cells,
    bin_width = as.numeric(bin_width),
    full = isTRUE(full),
    pitch_count = nrow(x),
    dropped_disconnected = nrow(all_cells) - nrow(cells)
  )
}

joint_perception_formula <- function(full = TRUE) {
  if (isTRUE(full)) {
    brms::bf(
      wrong | trials(n) ~ initial_call +
        car(M, gr = cell, type = "esicar") +
        (1 | cell_call) + (1 | umpire) + (1 | catcher)
    )
  } else {
    brms::bf(
      wrong | trials(n) ~ initial_call +
        car(M, gr = cell, type = "esicar") +
        (1 | cell_call)
    )
  }
}

joint_perception_priors <- function(sdcar_scale, full = TRUE) {
  sdcar_scale <- as.numeric(sdcar_scale)
  if (length(sdcar_scale) != 1L || !is.finite(sdcar_scale) || sdcar_scale <= 0) {
    stop("sdcar_scale must be one positive number")
  }
  priors <- c(
    brms::prior(normal(0, 1), class = "b"),
    brms::prior(student_t(3, 0, 2.5), class = "Intercept"),
    brms::prior_string(sprintf("normal(0, %s)", format(sdcar_scale)), class = "sdcar"),
    brms::prior(normal(0, 0.5), class = "sd", group = "cell_call")
  )
  if (isTRUE(full)) {
    priors <- c(
      priors,
      brms::prior(normal(0, 0.2), class = "sd", group = "umpire"),
      brms::prior(normal(0, 0.2), class = "sd", group = "catcher")
    )
  }
  priors
}

fit_joint_perception_model <- function(
  input, sdcar_scale, chains = 4L, cores = 4L, iter = 8000L,
  warmup = 4000L, seed = 42L, file = NULL, refresh = 100L
) {
  if (!is.list(input) || is.null(input$data) || is.null(input$M)) {
    stop("input must come from prepare_joint_car_input()")
  }
  arguments <- list(
    formula = joint_perception_formula(input$full),
    data = input$data,
    data2 = list(M = input$M),
    family = stats::binomial(),
    prior = joint_perception_priors(sdcar_scale, input$full),
    chains = as.integer(chains), cores = as.integer(cores),
    iter = as.integer(iter), warmup = as.integer(warmup),
    seed = as.integer(seed), init = 0,
    control = list(adapt_delta = 0.95), refresh = as.integer(refresh)
  )
  if (!is.null(file)) arguments$file <- file
  fit <- do.call(brms::brm, arguments)
  attr(fit, "joint_perception_spec") <- list(
    bin_width = input$bin_width,
    sdcar_scale = as.numeric(sdcar_scale),
    full = input$full,
    cell_table = input$cell_table
  )
  if (!is.null(file)) {
    fit_path <- if (grepl("\\.rds$", file, ignore.case = TRUE)) file else paste0(file, ".rds")
    saveRDS(fit, fit_path)
  }
  fit
}

deterministic_game_split <- function(pitches, train_fraction = 0.8, seed = 42L) {
  games <- sort(unique(as.character(pitches$game_pk)))
  if (length(games) < 2L) stop("At least two games are required")
  if (!is.finite(train_fraction) || train_fraction <= 0 || train_fraction >= 1) {
    stop("train_fraction must be strictly between zero and one")
  }
  set.seed(seed)
  shuffled <- sample(games)
  n_train <- max(1L, min(length(games) - 1L, floor(length(games) * train_fraction)))
  data.table::data.table(
    game_pk = shuffled,
    split = ifelse(seq_along(shuffled) <= n_train, "train", "validation")
  )
}

map_to_joint_training_cells <- function(pitches, cell_table, bin_width) {
  x <- assign_joint_perception_cells(pitches, bin_width)
  cells <- data.table::as.data.table(cell_table)
  known <- x$cell %in% cells$cell
  x[, `:=`(original_cell = cell, spatial_cell_fallback = !known)]
  if (any(!known)) {
    missing_cells <- unique(x[!known, .(ix, iz, original_cell)])
    replacements <- vapply(seq_len(nrow(missing_cells)), function(i) {
      distance <- (cells$ix - missing_cells$ix[[i]])^2 +
        (cells$iz - missing_cells$iz[[i]])^2
      cells$cell[[which.min(distance)]]
    }, character(1L))
    map <- stats::setNames(replacements, missing_cells$original_cell)
    x[!known, cell := unname(map[original_cell])]
  }
  x[, cell := factor(as.character(cell), levels = cells$cell)]
  x[, cell_call := interaction(cell, initial_call, sep = "|", drop = TRUE)]
  x[]
}

score_joint_perception_model <- function(
  fit, pitches, ndraws = 500L, chunk_size = 5000L, seed = 42L
) {
  spec <- attr(fit, "joint_perception_spec")
  if (is.null(spec$bin_width) || is.null(spec$cell_table)) {
    stop("Joint fit is missing its scoring specification")
  }
  x <- map_to_joint_training_cells(pitches, spec$cell_table, spec$bin_width)
  x[, n := 1L]
  output <- numeric(nrow(x))
  chunks <- split(seq_len(nrow(x)), ceiling(seq_len(nrow(x)) / as.integer(chunk_size)))
  set.seed(seed)
  for (indices in chunks) {
    draws <- brms::posterior_epred(
      fit, newdata = x[indices], ndraws = as.integer(ndraws),
      allow_new_levels = TRUE, sample_new_levels = "gaussian"
    )
    output[indices] <- colMeans(draws)
  }
  x[, p_hat := output]
  if (any(!is.finite(x$p_hat)) || any(!data.table::between(x$p_hat, 0, 1))) {
    stop("Joint perception scoring produced invalid probabilities")
  }
  x[]
}

fixed_bin_calibration_error <- function(probability, outcome, width = 0.05) {
  breaks <- seq(0, 1, by = width)
  bin <- cut(probability, breaks = breaks, include.lowest = TRUE)
  d <- data.table::data.table(probability, outcome, bin)
  calibration <- d[, .(
    n = .N, predicted = mean(probability), observed = mean(outcome)
  ), by = bin]
  sum(calibration$n * abs(calibration$predicted - calibration$observed)) /
    sum(calibration$n)
}

joint_probability_metrics <- function(scored, p_col = "p_hat", y_col = "call_wrong") {
  x <- data.table::as.data.table(scored)
  stop_if_missing_columns(x, c(p_col, y_col, "role"), "joint probability metrics")
  p <- pmin(1 - 1e-12, pmax(1e-12, as.numeric(x[[p_col]])))
  y <- as.numeric(x[[y_col]])
  x[, `:=`(.metric_p = p, .metric_y = y)]
  summarize <- function(d, scope, role) {
    loss <- -(d$.metric_y * log(d$.metric_p) +
      (1 - d$.metric_y) * log(1 - d$.metric_p))
    calibration_bin <- cut(
      d$.metric_p, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE
    )
    calibration_counts <- table(calibration_bin)
    data.table::data.table(
      scope = scope, role = role, n = nrow(d),
      log_loss = mean(loss), log_loss_se = stats::sd(loss) / sqrt(length(loss)),
      brier = mean((d$.metric_p - d$.metric_y)^2),
      ece_05 = fixed_bin_calibration_error(d$.metric_p, d$.metric_y),
      occupied_calibration_bins = sum(calibration_counts > 0),
      sparse_calibration_bins = sum(calibration_counts > 0 & calibration_counts < 100),
      extreme_rate = mean(d$.metric_p < 0.01 | d$.metric_p > 0.99),
      mean_probability = mean(d$.metric_p), wrong_rate = mean(d$.metric_y)
    )
  }
  result <- list(summarize(x, "league", "league"))
  for (value in sort(unique(x$role))) {
    result[[length(result) + 1L]] <- summarize(x[role == value], "role", value)
  }
  x[, c(".metric_p", ".metric_y") := NULL]
  data.table::rbindlist(result)
}

select_joint_perception_candidates <- function(candidate_metrics, finalists = 2L) {
  x <- data.table::as.data.table(candidate_metrics)[scope == "league"]
  stop_if_missing_columns(
    x, c("candidate_id", "bin_width", "sdcar_scale", "log_loss", "log_loss_se"),
    "joint candidate metrics"
  )
  best <- x[which.min(log_loss)]
  x[, within_one_se := log_loss <= best$log_loss + best$log_loss_se]
  data.table::setorder(x, -within_one_se, -bin_width, sdcar_scale, log_loss)
  x[, finalist := seq_len(.N) <= as.integer(finalists)]
  x[]
}

joint_fit_diagnostics <- function(fit) {
  summary <- posterior::summarise_draws(
    posterior::as_draws_array(fit), "rhat", "ess_bulk"
  )
  sampler <- brms::nuts_params(fit)
  max_rhat <- suppressWarnings(max(summary$rhat, na.rm = TRUE))
  min_bulk_ess <- suppressWarnings(min(summary$ess_bulk, na.rm = TRUE))
  divergences <- sum(sampler$Parameter == "divergent__" & sampler$Value > 0)
  data.table::data.table(
    max_rhat = max_rhat,
    min_bulk_ess = min_bulk_ess,
    divergences = divergences,
    pass = max_rhat <= 1.01 & min_bulk_ess >= 400 & divergences == 0
  )
}

summarize_joint_mdp_replay <- function(decisions, model = "joint_perception") {
  x <- data.table::as.data.table(decisions)
  summarize <- function(d, role) data.table::data.table(
    model = model,
    role = role,
    opportunities = nrow(d),
    attempts = sum(d$policy_challenge),
    challenge_rate = mean(d$policy_challenge),
    successes = sum(d$policy_success),
    success_rate = ifelse(
      sum(d$policy_challenge), mean(d$policy_success[d$policy_challenge]), NA_real_
    ),
    captured_re = sum(d$captured_re),
    zero_inventory_rate = mean(d$inventory_before_policy == 0L)
  )
  out <- list(summarize(x, "league"))
  for (value in sort(unique(x$role))) {
    out[[length(out) + 1L]] <- summarize(x[role == value], value)
  }
  data.table::rbindlist(out)
}

run_joint_perception_mdp <- function(
  scored_ledger, re_model, p_col = "p_hat", prior_n = 30,
  tol = 1e-7, max_iter = 3000L
) {
  opportunities <- prepare_mdp_opportunities(scored_ledger, re_model, p_col = p_col)
  fit <- fit_challenge_mdp(
    opportunities, prior_n = prior_n, tol = tol, max_iter = max_iter
  )
  decisions <- replay_challenge_policy(opportunities, fit, "mdp", initial_inventory = 2L)
  list(
    opportunities = opportunities,
    fit = fit,
    decisions = decisions,
    evaluation = summarize_joint_mdp_replay(decisions)
  )
}
