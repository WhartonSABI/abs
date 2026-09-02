continuous_model_root <- function() {
  tryCatch(
    rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd()),
    error = function(error) getwd()
  )
}

continuous_stan_compile_dir <- function() {
  path <- file.path(
    continuous_model_root(), "data", "processed", "stan", "compiled"
  )
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

continuous_allowlist <- function(data, allowed, label) {
  x <- data.table::copy(data.table::as.data.table(data))
  retained <- intersect(allowed, names(x))
  if (!length(retained)) stop(label, " contains no recognized columns")
  x[, ..retained]
}

continuous_mode <- function(value) {
  value <- as.character(value)
  value <- value[!is.na(value) & nzchar(value)]
  if (!length(value)) return("unknown")
  counts <- sort(table(value), decreasing = TRUE)
  sort(names(counts)[counts == max(counts)])[[1L]]
}

fit_continuous_design <- function(data, categorical = character(),
                                  numeric = character()) {
  x <- data.table::as.data.table(data)
  stop_if_missing_columns(
    x, c(categorical, numeric), "continuous-model design input"
  )
  categorical_spec <- lapply(categorical, function(column) {
    value <- as.character(x[[column]])
    fallback <- continuous_mode(value)
    value[is.na(value) | !nzchar(value)] <- fallback
    levels <- sort(unique(value))
    list(
      column = column,
      levels = levels,
      reference = levels[[1L]],
      fallback = fallback
    )
  })
  names(categorical_spec) <- categorical
  numeric_spec <- lapply(numeric, function(column) {
    value <- as.numeric(x[[column]])
    center <- mean(value, na.rm = TRUE)
    scale <- stats::sd(value, na.rm = TRUE)
    if (!is.finite(center)) center <- 0
    if (!is.finite(scale) || scale < 1e-8) scale <- 1
    list(column = column, center = center, scale = scale)
  })
  names(numeric_spec) <- numeric
  specification <- list(
    categorical = categorical_spec,
    numeric = numeric_spec
  )
  list(
    matrix = score_continuous_design(x, specification),
    specification = specification
  )
}

score_continuous_design <- function(data, specification) {
  x <- data.table::as.data.table(data)
  pieces <- list()
  column_names <- character()
  for (entry in specification$categorical) {
    stop_if_missing_columns(x, entry$column, "continuous-model scoring input")
    value <- as.character(x[[entry$column]])
    value[
      is.na(value) | !nzchar(value) | !value %in% entry$levels
    ] <- entry$fallback
    nonreference <- setdiff(entry$levels, entry$reference)
    if (length(nonreference)) {
      block <- vapply(
        nonreference, function(level) as.numeric(value == level),
        numeric(nrow(x))
      )
      if (is.null(dim(block))) block <- matrix(block, ncol = 1L)
      names <- paste0(entry$column, "__", make.names(nonreference, unique = TRUE))
      colnames(block) <- names
      pieces[[length(pieces) + 1L]] <- block
      column_names <- c(column_names, names)
    }
  }
  for (entry in specification$numeric) {
    stop_if_missing_columns(x, entry$column, "continuous-model scoring input")
    value <- as.numeric(x[[entry$column]])
    value[!is.finite(value)] <- entry$center
    block <- matrix((value - entry$center) / entry$scale, ncol = 1L)
    name <- paste0(entry$column, "__scaled")
    colnames(block) <- name
    pieces[[length(pieces) + 1L]] <- block
    column_names <- c(column_names, name)
  }
  if (!length(pieces)) {
    result <- matrix(numeric(), nrow = nrow(x), ncol = 0L)
  } else {
    result <- do.call(cbind, pieces)
  }
  storage.mode(result) <- "double"
  colnames(result) <- column_names
  result
}

continuous_stan_fit <- function(
  stan_file, data, backend = c("cmdstanr", "rstan"), chains = 4L,
  parallel_chains = chains, iter_warmup = 1000L, iter_sampling = 1000L,
  seed = 42L, adapt_delta = 0.95, max_treedepth = 12L, refresh = 100L,
  init = NULL
) {
  backend <- match.arg(backend)
  if (!file.exists(stan_file)) stop("Stan file not found: ", stan_file)
  if (backend == "cmdstanr") {
    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      stop("The cmdstanr backend requires cmdstanr and a working CmdStan install")
    }
    model <- cmdstanr::cmdstan_model(
      stan_file, dir = continuous_stan_compile_dir()
    )
    sample_arguments <- list(
      data = data,
      seed = as.integer(seed),
      chains = as.integer(chains),
      parallel_chains = min(as.integer(parallel_chains), as.integer(chains)),
      iter_warmup = as.integer(iter_warmup),
      iter_sampling = as.integer(iter_sampling),
      refresh = as.integer(refresh),
      adapt_delta = as.numeric(adapt_delta),
      max_treedepth = as.integer(max_treedepth)
    )
    if (!is.null(init)) sample_arguments$init <- init
    fit <- do.call(model$sample, sample_arguments)
  } else {
    if (!requireNamespace("rstan", quietly = TRUE)) {
      stop("The rstan backend requires rstan")
    }
    model <- rstan::stan_model(file = stan_file, auto_write = FALSE)
    sampling_arguments <- list(
      object = model,
      data = data,
      chains = as.integer(chains),
      cores = min(as.integer(parallel_chains), as.integer(chains)),
      iter = as.integer(iter_warmup + iter_sampling),
      warmup = as.integer(iter_warmup),
      seed = as.integer(seed),
      refresh = as.integer(refresh),
      control = list(
        adapt_delta = as.numeric(adapt_delta),
        max_treedepth = as.integer(max_treedepth)
      )
    )
    if (!is.null(init)) sampling_arguments$init <- init
    fit <- do.call(rstan::sampling, sampling_arguments)
  }
  fit
}

continuous_draw_matrix <- function(stan_fit, variables = NULL) {
  if (inherits(stan_fit, "CmdStanMCMC")) {
    return(stan_fit$draws(variables = variables, format = "matrix"))
  }
  if (inherits(stan_fit, "stanfit")) {
    return(as.matrix(stan_fit, pars = variables))
  }
  if (is.matrix(stan_fit)) {
    if (is.null(variables)) return(stan_fit)
    keep <- unique(unlist(lapply(variables, function(variable) {
      grep(
        paste0("^", variable, "($|\\[)"),
        colnames(stan_fit), value = TRUE
      )
    })))
    return(stan_fit[, keep, drop = FALSE])
  }
  stop("Unsupported posterior fit object")
}

continuous_thin_draws <- function(draws, ndraws = NULL, seed = 42L) {
  draws <- as.matrix(draws)
  if (!is.null(ndraws) && nrow(draws) > as.integer(ndraws)) {
    set.seed(seed)
    draws <- draws[
      sort(sample.int(nrow(draws), as.integer(ndraws))), , drop = FALSE
    ]
  }
  attr(draws, "draw_id") <- seq_len(nrow(draws))
  draws
}

continuous_extract_scalar <- function(draws, parameter) {
  if (!parameter %in% colnames(draws)) {
    stop("Posterior is missing parameter ", parameter)
  }
  as.numeric(draws[, parameter])
}

continuous_extract_vector <- function(draws, parameter, length) {
  if (!length) return(matrix(numeric(), nrow(draws), 0L))
  result <- matrix(NA_real_, nrow(draws), length)
  for (index in seq_len(length)) {
    name <- paste0(parameter, "[", index, "]")
    if (!name %in% colnames(draws)) stop("Posterior is missing ", name)
    result[, index] <- draws[, name]
  }
  result
}

continuous_extract_matrix <- function(draws, parameter, nrow, ncol) {
  if (!nrow || !ncol) {
    return(array(numeric(), dim = c(nrow(draws), nrow, ncol)))
  }
  result <- array(NA_real_, dim = c(nrow(draws), nrow, ncol))
  for (row in seq_len(nrow)) {
    for (column in seq_len(ncol)) {
      name <- paste0(parameter, "[", row, ",", column, "]")
      if (!name %in% colnames(draws)) stop("Posterior is missing ", name)
      result[, row, column] <- draws[, name]
    }
  }
  result
}

continuous_probability_summary <- function(draw_matrix, prefix) {
  data.table::data.table(
    value_mean__ = rowMeans(draw_matrix),
    value_median__ = apply(draw_matrix, 1L, stats::median),
    value_lower__ = apply(
      draw_matrix, 1L, stats::quantile, probs = 0.025, names = FALSE
    ),
    value_upper__ = apply(
      draw_matrix, 1L, stats::quantile, probs = 0.975, names = FALSE
    )
  )[, data.table::setnames(
    .SD,
    c("value_mean__", "value_median__", "value_lower__", "value_upper__"),
    paste0(prefix, c("mean", "median", "lower_95", "upper_95"))
  )]
}

continuous_softmax <- function(eta) {
  eta <- as.numeric(eta)
  shifted <- eta - max(eta)
  weight <- exp(shifted)
  weight / sum(weight)
}
