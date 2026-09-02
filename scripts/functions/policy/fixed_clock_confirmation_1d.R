# Temporal confirmation, coordinated whole-game bootstrap, and reporting
# helpers for the fixed-opportunity one-dimensional challenge-policy estimand.
#
# The confirmation layer is intentionally model-agnostic. Expensive historical
# RE and development nuisance fits are supplied as callbacks, which makes every
# bootstrap replicate independently reproducible and safe to checkpoint.

fixed_clock_confirmation_cutoff_1d <- function() as.Date("2026-07-20")

fixed_clock_confirmation_snapshot_expectations_1d <- function() {
  data.table::data.table(
    partition = c("development", "confirmation"),
    first_date = as.Date(c("2026-03-25", "2026-07-20")),
    last_date = as.Date(c("2026-07-19", "2026-08-25")),
    games = c(1490L, 499L),
    called_pitches = c(219405L, 72976L),
    tracked_clock_pitches = c(219171L, 72956L)
  )
}

.fixed_clock_require_columns_1d <- function(x, required, label) {
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      label, " is missing required columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  invisible(x)
}

.fixed_clock_scalar_integer_1d <- function(
  value, label, minimum = 1L, maximum = .Machine$integer.max
) {
  numeric_value <- suppressWarnings(as.numeric(value))
  integer_value <- suppressWarnings(as.integer(numeric_value))
  if (length(numeric_value) != 1L || !is.finite(numeric_value) ||
      numeric_value != integer_value || integer_value < minimum ||
      integer_value > maximum) {
    stop(
      label, " must be one integer in [", minimum, ", ", maximum, "]",
      call. = FALSE
    )
  }
  integer_value
}

.fixed_clock_game_ids_1d <- function(value, label = "game IDs") {
  ids <- as.character(value)
  if (!length(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop(label, " must be unique, non-missing, non-empty values", call. = FALSE)
  }
  sort(ids)
}

.fixed_clock_canonical_1d <- function(value) {
  if (data.table::is.data.table(value)) {
    value <- data.table::copy(value)
    data.table::setDF(value)
  }
  if (is.data.frame(value)) {
    value <- value[, sort(names(value)), drop = FALSE]
    rownames(value) <- NULL
    return(value)
  }
  if (is.list(value)) {
    if (!is.null(names(value))) {
      value <- value[order(names(value))]
    }
    return(lapply(value, .fixed_clock_canonical_1d))
  }
  value
}

fixed_clock_hash_object_1d <- function(value) {
  digest::digest(
    .fixed_clock_canonical_1d(value),
    algo = "sha256", serialize = TRUE
  )
}

.fixed_clock_split_core_1d <- function(game_split, cutoff) {
  x <- data.table::copy(data.table::as.data.table(game_split))
  .fixed_clock_require_columns_1d(
    x, c("game_pk", "game_date", "partition"),
    "fixed-clock game split"
  )
  x[, `:=`(
    game_pk = as.character(game_pk),
    game_date = as.Date(game_date),
    partition = as.character(partition)
  )]
  data.table::setorder(x, game_pk)
  list(
    schema = "fixed_clock_temporal_split_v1",
    cutoff = format(as.Date(cutoff), "%Y-%m-%d"),
    games = x[]
  )
}

.fixed_clock_validate_split_hash_1d <- function(split) {
  if (!inherits(split, "fixed_clock_confirmation_split_1d")) {
    stop("split must be a fixed_clock_confirmation_split_1d", call. = FALSE)
  }
  expected <- fixed_clock_hash_object_1d(
    .fixed_clock_split_core_1d(split$game_split, split$cutoff)
  )
  if (!identical(as.character(split$split_sha256), expected)) {
    stop("The fixed-clock split integrity hash is invalid", call. = FALSE)
  }
  invisible(TRUE)
}

fixed_clock_confirmation_split_1d <- function(
  pitch_ledger,
  cutoff = fixed_clock_confirmation_cutoff_1d()
) {
  x <- data.table::copy(data.table::as.data.table(pitch_ledger))
  .fixed_clock_require_columns_1d(
    x, c("game_pk", "game_date"), "fixed-clock pitch ledger"
  )
  cutoff <- as.Date(cutoff)
  if (length(cutoff) != 1L || is.na(cutoff)) {
    stop("cutoff must be one non-missing date", call. = FALSE)
  }
  x[, `:=`(
    game_pk = as.character(game_pk),
    game_date = as.Date(game_date)
  )]
  if (!nrow(x) || anyNA(x[, .(game_pk, game_date)]) ||
      any(!nzchar(x$game_pk))) {
    stop("fixed-clock pitch ledger has invalid game keys or dates", call. = FALSE)
  }
  dates <- unique(x[, .(game_pk, game_date)])
  if (anyDuplicated(dates$game_pk)) {
    stop("A game is associated with more than one game date", call. = FALSE)
  }
  dates[, partition := ifelse(
    game_date < cutoff, "development", "confirmation"
  )]
  if (!setequal(dates$partition, c("development", "confirmation"))) {
    stop("The temporal split must contain both partitions", call. = FALSE)
  }
  data.table::setorder(dates, game_pk)
  development_games <- dates[partition == "development", game_pk]
  confirmation_games <- dates[partition == "confirmation", game_pk]
  development <- x[game_pk %in% development_games]
  confirmation <- x[game_pk %in% confirmation_games]
  if (nrow(development) + nrow(confirmation) != nrow(x) ||
      length(intersect(development_games, confirmation_games))) {
    stop("The temporal split failed to partition the pitch ledger", call. = FALSE)
  }
  summary <- dates[, .(
    first_date = min(game_date),
    last_date = max(game_date),
    games = .N
  ), by = partition]
  summary[partition == "development", rows := nrow(development)]
  summary[partition == "confirmation", rows := nrow(confirmation)]
  summary <- summary[match(c("development", "confirmation"), partition)]
  core <- .fixed_clock_split_core_1d(dates, cutoff)
  structure(
    list(
      development = development[],
      confirmation = confirmation[],
      game_split = dates[],
      summary = summary[],
      cutoff = cutoff,
      split_sha256 = fixed_clock_hash_object_1d(core)
    ),
    class = "fixed_clock_confirmation_split_1d"
  )
}

validate_fixed_clock_confirmation_split_1d <- function(
  split,
  expected_development_games = 1490L,
  expected_confirmation_games = 499L,
  expected_development_rows = NULL,
  expected_confirmation_rows = NULL
) {
  if (!inherits(split, "fixed_clock_confirmation_split_1d")) {
    stop("split must be a fixed_clock_confirmation_split_1d", call. = FALSE)
  }
  games <- data.table::copy(data.table::as.data.table(split$game_split))
  .fixed_clock_require_columns_1d(
    games, c("game_pk", "game_date", "partition"),
    "fixed-clock split game table"
  )
  cutoff <- as.Date(split$cutoff)
  games[, `:=`(
    game_pk = as.character(game_pk),
    game_date = as.Date(game_date),
    partition = as.character(partition)
  )]
  if (anyNA(games) || anyDuplicated(games$game_pk) ||
      any(!games$partition %in% c("development", "confirmation")) ||
      any(games[partition == "development", game_date >= cutoff]) ||
      any(games[partition == "confirmation", game_date < cutoff])) {
    stop("The fixed-clock temporal assignment is invalid", call. = FALSE)
  }
  development_ids <- sort(games[partition == "development", game_pk])
  confirmation_ids <- sort(games[partition == "confirmation", game_pk])
  if (length(intersect(development_ids, confirmation_ids))) {
    stop("Development and confirmation games overlap", call. = FALSE)
  }
  partition_ids <- c(development_ids, confirmation_ids)
  if (!setequal(unique(split$development$game_pk), development_ids) ||
      !setequal(unique(split$confirmation$game_pk), confirmation_ids) ||
      !setequal(partition_ids, games$game_pk)) {
    stop("Split ledgers and game assignment do not align", call. = FALSE)
  }
  expected_development_games <- .fixed_clock_scalar_integer_1d(
    expected_development_games, "expected_development_games", minimum = 1L
  )
  expected_confirmation_games <- .fixed_clock_scalar_integer_1d(
    expected_confirmation_games, "expected_confirmation_games", minimum = 1L
  )
  observed <- c(length(development_ids), length(confirmation_ids))
  expected <- c(expected_development_games, expected_confirmation_games)
  if (!identical(as.integer(observed), as.integer(expected))) {
    stop(sprintf(
      paste0(
        "Fixed-clock snapshot has %s development and %s confirmation games; ",
        "expected %s and %s"
      ), observed[[1L]], observed[[2L]], expected[[1L]], expected[[2L]]
    ), call. = FALSE)
  }
  if (!is.null(expected_development_rows) &&
      nrow(split$development) != as.integer(expected_development_rows)) {
    stop("Development row count does not match its expectation", call. = FALSE)
  }
  if (!is.null(expected_confirmation_rows) &&
      nrow(split$confirmation) != as.integer(expected_confirmation_rows)) {
    stop("Confirmation row count does not match its expectation", call. = FALSE)
  }
  .fixed_clock_validate_split_hash_1d(split)
  data.table::data.table(
    partition = c("development", "confirmation"),
    games = as.integer(observed),
    rows = c(nrow(split$development), nrow(split$confirmation)),
    first_date = c(min(split$development$game_date), min(split$confirmation$game_date)),
    last_date = c(max(split$development$game_date), max(split$confirmation$game_date)),
    cutoff = cutoff,
    split_sha256 = split$split_sha256
  )
}

validate_fixed_clock_confirmation_snapshot_1d <- function(pitch_ledger) {
  split <- fixed_clock_confirmation_split_1d(pitch_ledger)
  expectations <- fixed_clock_confirmation_snapshot_expectations_1d()
  validation <- validate_fixed_clock_confirmation_split_1d(
    split,
    expected_development_games = expectations[partition == "development", games],
    expected_confirmation_games = expectations[partition == "confirmation", games],
    expected_development_rows = expectations[
      partition == "development", called_pitches
    ],
    expected_confirmation_rows = expectations[
      partition == "confirmation", called_pitches
    ]
  )
  list(split = split, validation = validation)
}

fixed_clock_bootstrap_seed_1d <- function(
  seed, replicate_id, source = "__replicate__"
) {
  seed <- .fixed_clock_scalar_integer_1d(seed, "seed", minimum = 1L)
  replicate_id <- .fixed_clock_scalar_integer_1d(
    replicate_id, "replicate_id", minimum = 1L, maximum = 100000000L
  )
  source <- as.character(source)
  if (length(source) != 1L || is.na(source) || !nzchar(source)) {
    stop("source must be one non-empty string", call. = FALSE)
  }
  code <- utf8ToInt(enc2utf8(source))
  source_offset <- if (length(code)) {
    sum(as.numeric(code) * seq_along(code))
  } else {
    0
  }
  modulus <- 2147483646
  value <- (
    as.numeric(seed) + as.numeric(replicate_id) * 1000003 +
      source_offset * 9176
  ) %% modulus
  as.integer(value + 1)
}

.fixed_clock_with_seed_1d <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(code)
}

fixed_clock_source_resample_1d <- function(
  game_ids, source, replicate_id, seed = 20260826L
) {
  ids <- .fixed_clock_game_ids_1d(game_ids, paste(source, "game IDs"))
  source <- as.character(source)
  if (length(source) != 1L || is.na(source) || !nzchar(source)) {
    stop("source must be one non-empty string", call. = FALSE)
  }
  replicate_id <- .fixed_clock_scalar_integer_1d(
    replicate_id, "replicate_id", minimum = 1L, maximum = 100000000L
  )
  replicate_seed <- fixed_clock_bootstrap_seed_1d(
    seed, replicate_id, "__replicate__"
  )
  source_seed <- fixed_clock_bootstrap_seed_1d(seed, replicate_id, source)
  sampled <- .fixed_clock_with_seed_1d(
    source_seed, sample(ids, length(ids), replace = TRUE)
  )
  sampled_weights <- data.table::data.table(game_pk = sampled)[, .(
    bootstrap_weight = .N
  ), by = game_pk]
  weights <- data.table::data.table(game_pk = ids)
  weights[sampled_weights, on = "game_pk", bootstrap_weight := i.bootstrap_weight]
  weights[is.na(bootstrap_weight), bootstrap_weight := 0L]
  data.table::setorder(weights, game_pk)
  weights[, `:=`(
    replicate = replicate_id,
    source = source,
    base_seed = as.integer(seed),
    replicate_seed = replicate_seed,
    source_seed = source_seed,
    source_games = length(ids),
    sampled_games = length(sampled)
  )]
  data.table::setcolorder(weights, c(
    "replicate", "source", "base_seed", "replicate_seed", "source_seed",
    "game_pk", "bootstrap_weight", "source_games", "sampled_games"
  ))
  weights[]
}

fixed_clock_bootstrap_plan_1d <- function(
  source_game_ids,
  replicate_ids,
  seed = 20260826L
) {
  if (!is.list(source_game_ids) || is.null(names(source_game_ids)) ||
      anyNA(names(source_game_ids)) || any(!nzchar(names(source_game_ids))) ||
      anyDuplicated(names(source_game_ids))) {
    stop("source_game_ids must be a uniquely named list", call. = FALSE)
  }
  sources <- sort(names(source_game_ids))
  numeric_replicates <- suppressWarnings(as.numeric(replicate_ids))
  integer_replicates <- suppressWarnings(as.integer(numeric_replicates))
  if (!length(numeric_replicates) || anyNA(numeric_replicates) ||
      any(!is.finite(numeric_replicates)) ||
      any(numeric_replicates != integer_replicates) ||
      any(integer_replicates < 1L)) {
    stop("replicate_ids must contain positive integers", call. = FALSE)
  }
  replicate_ids <- sort(unique(integer_replicates))
  pieces <- lapply(replicate_ids, function(replicate_id) {
    data.table::rbindlist(lapply(sources, function(source) {
      fixed_clock_source_resample_1d(
        source_game_ids[[source]], source, replicate_id, seed
      )
    }), use.names = TRUE)
  })
  out <- data.table::rbindlist(pieces, use.names = TRUE)
  data.table::setorder(out, replicate, source, game_pk)
  out[]
}

.fixed_clock_bootstrap_sources_1d <- function(source_game_ids) {
  required <- c("historical_re", "development", "confirmation")
  if (!is.list(source_game_ids) || is.null(names(source_game_ids)) ||
      !all(required %in% names(source_game_ids))) {
    stop(
      "source_game_ids must include historical_re, development, and confirmation",
      call. = FALSE
    )
  }
  extras <- setdiff(names(source_game_ids), required)
  if (length(extras)) {
    stop(
      "Unexpected bootstrap sources: ", paste(extras, collapse = ", "),
      call. = FALSE
    )
  }
  out <- source_game_ids[required]
  lapply(names(out), function(source) {
    .fixed_clock_game_ids_1d(out[[source]], paste(source, "game IDs"))
  }) |>
    stats::setNames(required)
}

.fixed_clock_callback_1d <- function(callback, arguments, label, seed) {
  if (!is.function(callback)) stop(label, " must be a function", call. = FALSE)
  .fixed_clock_with_seed_1d(seed, do.call(callback, arguments))
}

.fixed_clock_score_table_1d <- function(value, replicate_id) {
  if (data.table::is.data.table(value) || is.data.frame(value)) {
    out <- data.table::copy(data.table::as.data.table(value))
  } else if (is.list(value) && !is.null(names(value)) &&
      all(lengths(value) == 1L)) {
    out <- data.table::as.data.table(value)
  } else {
    stop(
      "score_confirmation must return a data frame or named scalar list",
      call. = FALSE
    )
  }
  if (!nrow(out)) stop("score_confirmation returned no rows", call. = FALSE)
  reserved <- intersect(
    names(out),
    c(
      "replicate", "replicate_row", "base_seed", "replicate_seed",
      "historical_re_seed", "development_seed", "confirmation_seed",
      "replicate_fingerprint"
    )
  )
  if (length(reserved)) {
    stop(
      "score_confirmation returned reserved columns: ",
      paste(reserved, collapse = ", "), call. = FALSE
    )
  }
  out[, `:=`(
    replicate = as.integer(replicate_id),
    replicate_row = seq_len(.N)
  )]
  out[]
}

.fixed_clock_bootstrap_callback_signature_1d <- function(callback) {
  list(
    formals = formals(callback),
    body = paste(deparse(body(callback), width.cutoff = 500L), collapse = "\n")
  )
}

.fixed_clock_bootstrap_contract_fingerprint_1d <- function(
  sources, refit_historical_re, refit_development, score_confirmation, context
) {
  fixed_clock_hash_object_1d(list(
    sources = sources,
    callbacks = lapply(
      list(
        refit_historical_re = refit_historical_re,
        refit_development = refit_development,
        score_confirmation = score_confirmation
      ),
      .fixed_clock_bootstrap_callback_signature_1d
    ),
    context = context
  ))
}

.fixed_clock_bootstrap_checkpoint_path_1d <- function(
  checkpoint_dir, replicate_id
) {
  file.path(
    checkpoint_dir,
    sprintf("fixed_clock_bootstrap_%08d.rds", as.integer(replicate_id))
  )
}

.fixed_clock_write_rds_atomic_1d <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = paste0(basename(path), "."), tmpdir = dirname(path)
  )
  on.exit(unlink(temporary), add = TRUE)
  saveRDS(value, temporary, version = 3)
  if (!file.rename(temporary, path)) {
    stop("Could not atomically write bootstrap checkpoint: ", path, call. = FALSE)
  }
  invisible(normalizePath(path, mustWork = TRUE))
}

run_fixed_clock_confirmation_bootstrap_1d <- function(
  source_game_ids,
  replicate_ids,
  refit_historical_re,
  refit_development,
  score_confirmation,
  seed = 20260826L,
  context = list(),
  checkpoint_dir = NULL,
  checkpoint_key = "fixed_clock_confirmation_v1",
  resume = TRUE,
  .bootstrap_contract_fingerprint = NULL
) {
  sources <- .fixed_clock_bootstrap_sources_1d(source_game_ids)
  numeric_replicates <- suppressWarnings(as.numeric(replicate_ids))
  integer_replicates <- suppressWarnings(as.integer(numeric_replicates))
  if (!length(numeric_replicates) || anyNA(numeric_replicates) ||
      any(!is.finite(numeric_replicates)) ||
      any(numeric_replicates != integer_replicates) ||
      any(integer_replicates < 1L)) {
    stop("replicate_ids must contain positive integers", call. = FALSE)
  }
  replicate_ids <- sort(unique(integer_replicates))
  if (!is.function(refit_historical_re) ||
      !is.function(refit_development) ||
      !is.function(score_confirmation)) {
    stop("All three bootstrap callbacks must be functions", call. = FALSE)
  }
  if (!is.list(context)) stop("context must be a list", call. = FALSE)
  checkpoint_key <- as.character(checkpoint_key)
  if (length(checkpoint_key) != 1L || is.na(checkpoint_key) ||
      !nzchar(checkpoint_key)) {
    stop("checkpoint_key must be one non-empty string", call. = FALSE)
  }
  if (!is.null(checkpoint_dir)) {
    checkpoint_dir <- normalizePath(
      checkpoint_dir, mustWork = FALSE
    )
    dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (is.null(.bootstrap_contract_fingerprint)) {
    bootstrap_contract_fingerprint <-
      .fixed_clock_bootstrap_contract_fingerprint_1d(
        sources,
        refit_historical_re,
        refit_development,
        score_confirmation,
        context
      )
  } else {
    bootstrap_contract_fingerprint <- as.character(
      .bootstrap_contract_fingerprint
    )
    if (length(bootstrap_contract_fingerprint) != 1L ||
        is.na(bootstrap_contract_fingerprint) ||
        !grepl("^[[:xdigit:]]{64}$", bootstrap_contract_fingerprint)) {
      stop("bootstrap_contract_fingerprint must be one SHA-256 string",
        call. = FALSE
      )
    }
  }

  result_parts <- vector("list", length(replicate_ids))
  plan_parts <- vector("list", length(replicate_ids))
  timing_parts <- vector("list", length(replicate_ids))
  for (index in seq_along(replicate_ids)) {
    replicate_started <- proc.time()[["elapsed"]]
    replicate_id <- replicate_ids[[index]]
    plan <- fixed_clock_bootstrap_plan_1d(
      sources, replicate_ids = replicate_id, seed = seed
    )
    plan_fingerprint <- fixed_clock_hash_object_1d(list(
      checkpoint_key = checkpoint_key,
      bootstrap_contract_fingerprint = bootstrap_contract_fingerprint,
      replicate = replicate_id,
      weights = plan[, .(
        source, source_seed, game_pk, bootstrap_weight,
        source_games, sampled_games
      )]
    ))
    checkpoint_path <- if (is.null(checkpoint_dir)) NULL else {
      .fixed_clock_bootstrap_checkpoint_path_1d(checkpoint_dir, replicate_id)
    }
    if (!is.null(checkpoint_path) && isTRUE(resume) &&
        file.exists(checkpoint_path)) {
      checkpoint <- readRDS(checkpoint_path)
      if (!is.list(checkpoint) ||
          !identical(checkpoint$schema, "fixed_clock_bootstrap_checkpoint_v1") ||
          !identical(checkpoint$fingerprint, plan_fingerprint) ||
          !identical(
            checkpoint$result_sha256,
            fixed_clock_hash_object_1d(checkpoint$result)
          )) {
        stop(
          "Bootstrap checkpoint fingerprint mismatch for replicate ",
          replicate_id, call. = FALSE
        )
      }
      result_parts[[index]] <- data.table::as.data.table(checkpoint$result)
      checkpoint_timing <- checkpoint$timing
      if (!is.data.frame(checkpoint_timing) || nrow(checkpoint_timing) != 1L) {
        checkpoint_timing <- data.table::data.table(
          replicate = as.integer(replicate_id),
          historical_re_seconds = NA_real_,
          development_seconds = NA_real_,
          confirmation_seconds = NA_real_,
          total_seconds = NA_real_,
          resumed = TRUE
        )
      } else {
        checkpoint_timing <- data.table::as.data.table(checkpoint_timing)
        checkpoint_timing[, resumed := TRUE]
      }
      timing_parts[[index]] <- checkpoint_timing
      plan_parts[[index]] <- unique(plan[, .(
        replicate, source, base_seed, replicate_seed, source_seed,
        source_games, sampled_games
      )])
      next
    }

    weights_for <- function(source_value) {
      data.table::copy(plan[get("source") == source_value, .(
        game_pk, bootstrap_weight
      )])
    }
    historical_seed <- fixed_clock_bootstrap_seed_1d(
      seed, replicate_id, "historical_re__callback"
    )
    development_seed <- fixed_clock_bootstrap_seed_1d(
      seed, replicate_id, "development__callback"
    )
    confirmation_seed <- fixed_clock_bootstrap_seed_1d(
      seed, replicate_id, "confirmation__callback"
    )
    historical_started <- proc.time()[["elapsed"]]
    historical_fit <- .fixed_clock_callback_1d(
      refit_historical_re,
      list(
        weights = weights_for("historical_re"),
        replicate_id = replicate_id,
        seed = historical_seed,
        context = context
      ),
      "refit_historical_re", historical_seed
    )
    historical_seconds <- proc.time()[["elapsed"]] - historical_started
    development_started <- proc.time()[["elapsed"]]
    development_fit <- .fixed_clock_callback_1d(
      refit_development,
      list(
        weights = weights_for("development"),
        historical_re_fit = historical_fit,
        replicate_id = replicate_id,
        seed = development_seed,
        context = context
      ),
      "refit_development", development_seed
    )
    development_seconds <- proc.time()[["elapsed"]] - development_started
    confirmation_started <- proc.time()[["elapsed"]]
    scored <- .fixed_clock_callback_1d(
      score_confirmation,
      list(
        weights = weights_for("confirmation"),
        historical_re_fit = historical_fit,
        development_fit = development_fit,
        replicate_id = replicate_id,
        seed = confirmation_seed,
        context = context
      ),
      "score_confirmation", confirmation_seed
    )
    confirmation_seconds <- proc.time()[["elapsed"]] - confirmation_started
    result <- .fixed_clock_score_table_1d(scored, replicate_id)
    replicate_seed <- unique(plan$replicate_seed)[[1L]]
    result[, `:=`(
      base_seed = as.integer(seed),
      replicate_seed = replicate_seed,
      historical_re_seed = historical_seed,
      development_seed = development_seed,
      confirmation_seed = confirmation_seed,
      replicate_fingerprint = plan_fingerprint
    )]
    data.table::setcolorder(result, c(
      "replicate", "replicate_row", "base_seed", "replicate_seed",
      "historical_re_seed", "development_seed", "confirmation_seed",
      "replicate_fingerprint",
      setdiff(names(result), c(
        "replicate", "replicate_row", "base_seed", "replicate_seed",
        "historical_re_seed", "development_seed", "confirmation_seed",
        "replicate_fingerprint"
      ))
    ))
    timing <- data.table::data.table(
      replicate = as.integer(replicate_id),
      historical_re_seconds = historical_seconds,
      development_seconds = development_seconds,
      confirmation_seconds = confirmation_seconds,
      total_seconds = proc.time()[["elapsed"]] - replicate_started,
      resumed = FALSE
    )
    if (!is.null(checkpoint_path)) {
      .fixed_clock_write_rds_atomic_1d(
        list(
          schema = "fixed_clock_bootstrap_checkpoint_v1",
          fingerprint = plan_fingerprint,
          result = result,
          result_sha256 = fixed_clock_hash_object_1d(result),
          timing = timing
        ),
        checkpoint_path
      )
    }
    result_parts[[index]] <- result[]
    timing_parts[[index]] <- timing
    plan_parts[[index]] <- unique(plan[, .(
      replicate, source, base_seed, replicate_seed, source_seed,
      source_games, sampled_games
    )])
  }
  results <- data.table::rbindlist(result_parts, use.names = TRUE, fill = TRUE)
  plan_summary <- data.table::rbindlist(plan_parts, use.names = TRUE)
  timing_summary <- data.table::rbindlist(
    timing_parts, use.names = TRUE, fill = TRUE
  )
  data.table::setorder(results, replicate, replicate_row)
  data.table::setorder(plan_summary, replicate, source)
  data.table::setorder(timing_summary, replicate)
  structure(
    list(
      results = results[],
      plan = plan_summary[],
      timing = timing_summary[],
      replicate_ids = replicate_ids,
      seed = as.integer(seed),
      checkpoint_key = checkpoint_key
    ),
    class = "fixed_clock_confirmation_bootstrap_1d"
  )
}

summarize_fixed_clock_frozen_policy_1d <- function(
  scores,
  group_columns = c("policy", "scenario_id", "role"),
  captured_column = "captured_re",
  observed_column = "observed_re",
  oracle_column = "oracle_re",
  team_games_column = "team_games"
) {
  x <- data.table::copy(data.table::as.data.table(scores))
  group_columns <- as.character(group_columns)
  required <- c(
    group_columns, captured_column, observed_column, oracle_column
  )
  .fixed_clock_require_columns_1d(x, required, "fixed-clock policy scores")
  for (column in c(captured_column, observed_column, oracle_column)) {
    value <- as.numeric(x[[column]])
    if (anyNA(value) || any(!is.finite(value))) {
      stop("Policy score column ", column, " must be finite", call. = FALSE)
    }
    x[, (column) := value]
  }
  if (!team_games_column %in% names(x)) {
    if (all(c("game_pk", "team_id") %in% names(x))) {
      x[, (team_games_column) := as.integer(seq_len(.N) == 1L),
        by = c(group_columns, "game_pk", "team_id")]
    } else {
      stop(
        "Scores need team_games or game_pk and team_id columns",
        call. = FALSE
      )
    }
  }
  team_games <- as.numeric(x[[team_games_column]])
  if (anyNA(team_games) || any(!is.finite(team_games)) ||
      any(team_games < 0)) {
    stop("team_games must be finite and non-negative", call. = FALSE)
  }
  x[, (team_games_column) := team_games]
  optional_sum <- intersect(
    c("attempts", "successes", "failures"), names(x)
  )
  measure_columns <- c(
    captured_column, observed_column, oracle_column,
    team_games_column, optional_sum
  )
  out <- x[, lapply(.SD, sum), by = group_columns, .SDcols = measure_columns]
  data.table::setnames(
    out,
    c(captured_column, observed_column, oracle_column, team_games_column),
    c("captured_re", "observed_re", "oracle_re", "team_games")
  )
  out[, `:=`(
    gain_over_observed_re = captured_re - observed_re,
    captured_re_per_team_game = ifelse(
      team_games > 0, captured_re / team_games, NA_real_
    ),
    gain_over_observed_re_per_team_game = ifelse(
      team_games > 0, (captured_re - observed_re) / team_games, NA_real_
    ),
    share_of_oracle = ifelse(oracle_re > 0, captured_re / oracle_re, NA_real_)
  )]
  if (all(c("attempts", "successes") %in% names(out))) {
    out[, success_rate := ifelse(attempts > 0, successes / attempts, NA_real_)]
  }
  data.table::setorderv(out, group_columns)
  out[]
}

# Aggregate one bootstrap replicate without materializing the Cartesian copy of
# scenario-invariant public/comparator game rows. Direct and Bellman values are
# already scenario-specific; public, observed, fitted-human, and oracle values
# are weighted once and only their small aggregate tables are replicated.
summarize_fixed_clock_bootstrap_evaluations_1d <- function(
  direct_game_role,
  public_game_role,
  comparator_game_role,
  confirmation_weights,
  scenario_ids,
  bellman_game_role = NULL,
  procedure_game_role = NULL
) {
  scenario_ids <- sort(unique(as.character(scenario_ids)))
  if (!length(scenario_ids) || anyNA(scenario_ids) ||
      any(!nzchar(scenario_ids))) {
    stop("scenario_ids must be non-empty, unique strings", call. = FALSE)
  }
  weights <- data.table::copy(data.table::as.data.table(
    confirmation_weights
  ))
  .fixed_clock_require_columns_1d(
    weights, c("game_pk", "bootstrap_weight"),
    "confirmation bootstrap weights"
  )
  numeric_weight <- suppressWarnings(as.numeric(weights$bootstrap_weight))
  integer_weight <- suppressWarnings(as.integer(numeric_weight))
  if (anyNA(numeric_weight) || any(!is.finite(numeric_weight)) ||
      any(numeric_weight < 0) || any(numeric_weight != integer_weight)) {
    stop("confirmation bootstrap weights must be non-negative integers",
      call. = FALSE
    )
  }
  weights <- weights[, .(
    game_pk = as.character(game_pk),
    bootstrap_weight = integer_weight
  )]
  if (anyNA(weights$game_pk) || any(!nzchar(weights$game_pk)) ||
      anyDuplicated(weights$game_pk)) {
    stop("confirmation bootstrap game keys are invalid", call. = FALSE)
  }

  measures <- c(
    "captured_re", "attempts", "successes", "failures",
    "exhausted_opportunity_mass", "opportunity_exposure"
  )
  prepare <- function(value, label) {
    x <- data.table::copy(data.table::as.data.table(value))
    .fixed_clock_require_columns_1d(
      x,
      c("game_pk", "team_id", "policy", "policy_family", "role", measures),
      label
    )
    x[, `:=`(
      game_pk = as.character(game_pk),
      team_id = as.character(team_id),
      policy = as.character(policy),
      policy_family = as.character(policy_family),
      role = as.character(role)
    )]
    if (anyNA(x[, c(
      "game_pk", "team_id", "policy", "policy_family", "role", measures
    ), with = FALSE]) ||
        any(!nzchar(x$game_pk)) || any(!nzchar(x$team_id)) ||
        any(!nzchar(x$policy)) || any(!nzchar(x$policy_family)) ||
        any(x$role == "combined") ||
        any(!is.finite(as.matrix(x[, ..measures])))) {
      stop(label, " contains invalid values", call. = FALSE)
    }
    score_keys <- c("game_pk", "team_id", "policy", "policy_family", "role")
    if (anyDuplicated(x[, ..score_keys])) {
      stop(label, " contains duplicate game/team/policy/role rows",
        call. = FALSE
      )
    }
    if (!setequal(unique(x$game_pk), weights$game_pk)) {
      stop(label, " does not cover the confirmation bootstrap games exactly",
        call. = FALSE
      )
    }
    if (any(abs(x$attempts - x$successes - x$failures) > 1e-8)) {
      stop(label, " violates attempts = successes + failures", call. = FALSE)
    }
    combined <- x[, lapply(.SD, sum),
      by = .(game_pk, team_id, policy, policy_family),
      .SDcols = measures
    ]
    combined[, role := "combined"]
    data.table::rbindlist(list(x, combined), use.names = TRUE)
  }
  weighted <- function(value, label) {
    x <- prepare(value, label)
    x <- merge(x, weights, by = "game_pk", all.x = TRUE, sort = FALSE)
    if (anyNA(x$bootstrap_weight)) {
      stop(label, " has games absent from confirmation weights", call. = FALSE)
    }
    x[, (measures) := lapply(.SD, `*`, bootstrap_weight),
      .SDcols = measures
    ]
    x[, team_games := bootstrap_weight]
    x[, lapply(.SD, sum),
      by = .(policy, policy_family, role),
      .SDcols = c(measures, "team_games")
    ]
  }
  scenario_specific <- function(value, prefix, policy_label, label) {
    if (is.null(value)) return(data.table::data.table())
    x <- prepare(value, label)
    x[, scenario_id := sub(paste0("^", prefix), "", policy)]
    if (any(!x$scenario_id %in% scenario_ids)) {
      stop(label, " contains an unknown scenario", call. = FALSE)
    }
    if (!setequal(unique(x$scenario_id), scenario_ids)) {
      stop(label, " does not contain the complete scenario grid",
        call. = FALSE
      )
    }
    x <- merge(x, weights, by = "game_pk", all.x = TRUE, sort = FALSE)
    if (anyNA(x$bootstrap_weight)) {
      stop(label, " has games absent from confirmation weights", call. = FALSE)
    }
    x[, (measures) := lapply(.SD, `*`, bootstrap_weight),
      .SDcols = measures
    ]
    x[, `:=`(team_games = bootstrap_weight, policy = policy_label)]
    x[, lapply(.SD, sum),
      by = .(policy, policy_family, scenario_id, role),
      .SDcols = c(measures, "team_games")
    ]
  }
  replicate_scenarios <- function(value) {
    data.table::rbindlist(lapply(scenario_ids, function(id) {
      out <- data.table::copy(value)
      out[, scenario_id := id]
      out
    }), use.names = TRUE)
  }

  comparator <- weighted(
    comparator_game_role, "bootstrap comparator game-role scores"
  )
  observed <- comparator[policy == "observed", .(
    role, observed_re = captured_re
  )]
  oracle <- comparator[policy == "exact_location_oracle", .(
    role, oracle_re = captured_re
  )]
  if (!nrow(observed) || !nrow(oracle) || anyDuplicated(observed$role) ||
      anyDuplicated(oracle$role)) {
    stop("bootstrap comparator references are incomplete", call. = FALSE)
  }

  pieces <- list(
    scenario_specific(
      direct_game_role, "fixed_clock__", "robust_signal_assisted",
      "bootstrap direct-policy game-role scores"
    ),
    replicate_scenarios(weighted(
      public_game_role, "bootstrap public-policy game-role scores"
    )),
    replicate_scenarios(comparator)
  )
  if (!is.null(bellman_game_role)) {
    pieces[[length(pieces) + 1L]] <- scenario_specific(
      bellman_game_role, "bellman__", "bellman_structural",
      "bootstrap Bellman game-role scores"
    )
  }
  if (!is.null(procedure_game_role)) {
    pieces[[length(pieces) + 1L]] <- scenario_specific(
      procedure_game_role, "fixed_clock__", "direct_learning_procedure",
      "bootstrap learning-procedure game-role scores"
    )
  }
  scores <- data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
  scores <- merge(scores, observed, by = "role", all.x = TRUE, sort = FALSE)
  scores <- merge(scores, oracle, by = "role", all.x = TRUE, sort = FALSE)
  if (anyNA(scores[, .(observed_re, oracle_re)])) {
    stop("bootstrap observed/oracle references do not cover every role",
      call. = FALSE
    )
  }
  summarize_fixed_clock_frozen_policy_1d(scores)
}

fixed_clock_scenario_envelope_1d <- function(
  values,
  estimate_column = "gain_over_observed_re",
  scenario_column = "scenario_id",
  group_columns = c("policy", "role"),
  replicate_column = NULL,
  compatible_column = NULL,
  expected_scenario_ids = NULL
) {
  x <- data.table::copy(data.table::as.data.table(values))
  group_columns <- as.character(group_columns)
  if (!is.null(replicate_column)) {
    group_columns <- unique(c(as.character(replicate_column), group_columns))
  }
  required <- c(group_columns, scenario_column, estimate_column)
  if (!is.null(compatible_column)) required <- c(required, compatible_column)
  .fixed_clock_require_columns_1d(x, required, "fixed-clock scenario values")
  if (!is.null(compatible_column)) {
    compatible <- as.logical(x[[compatible_column]])
    if (anyNA(compatible)) {
      stop("Scenario compatibility must be non-missing", call. = FALSE)
    }
    x <- x[compatible]
  }
  estimate <- as.numeric(x[[estimate_column]])
  scenario <- as.character(x[[scenario_column]])
  if (!nrow(x) || anyNA(estimate) || any(!is.finite(estimate)) ||
      anyNA(scenario) || any(!nzchar(scenario))) {
    stop("Scenario envelope inputs are invalid", call. = FALSE)
  }
  x[, `:=`(
    estimate__ = estimate,
    scenario__ = scenario
  )]
  envelope_keys <- c(group_columns, scenario_column)
  if (anyDuplicated(x[, ..envelope_keys])) {
    stop(
      "Scenario values must have one row per group and scenario",
      call. = FALSE
    )
  }
  if (!is.null(expected_scenario_ids)) {
    expected_scenario_ids <- sort(unique(as.character(expected_scenario_ids)))
    if (!length(expected_scenario_ids) || anyNA(expected_scenario_ids) ||
        any(!nzchar(expected_scenario_ids))) {
      stop("expected_scenario_ids must be non-empty and non-missing",
        call. = FALSE
      )
    }
    scenario_check <- x[, .(
      scenario_grid_valid = identical(
        sort(unique(scenario__)), expected_scenario_ids
      )
    ), by = group_columns]
    if (any(!scenario_check$scenario_grid_valid)) {
      stop("Scenario envelope lacks the complete expected scenario grid",
        call. = FALSE
      )
    }
  }
  out <- x[, {
    low <- order(estimate__, scenario__)[[1L]]
    high <- order(-estimate__, scenario__)[[1L]]
    .(
      scenario_lower = estimate__[[low]],
      scenario_upper = estimate__[[high]],
      lower_scenario_id = scenario__[[low]],
      upper_scenario_id = scenario__[[high]],
      scenarios = data.table::uniqueN(scenario__),
      all_scenarios_positive = all(estimate__ > 0),
      scenario_grid_sha256 = if (is.null(expected_scenario_ids)) {
        fixed_clock_hash_object_1d(sort(unique(scenario__)))
      } else {
        fixed_clock_hash_object_1d(expected_scenario_ids)
      },
      scenario_grid_validated = !is.null(expected_scenario_ids)
    )
  }, by = group_columns]
  data.table::setorderv(out, group_columns)
  out[]
}

summarize_fixed_clock_scenario_envelope_intervals_1d <- function(
  envelope_draws,
  group_columns = c("policy", "role"),
  replicate_column = "replicate",
  probabilities = c(0.025, 0.975)
) {
  x <- data.table::copy(data.table::as.data.table(envelope_draws))
  .fixed_clock_require_columns_1d(
    x,
    c(group_columns, replicate_column, "scenario_lower", "scenario_upper"),
    "fixed-clock scenario envelope draws"
  )
  probabilities <- as.numeric(probabilities)
  if (length(probabilities) != 2L || anyNA(probabilities) ||
      any(!is.finite(probabilities)) || probabilities[[1L]] < 0 ||
      probabilities[[2L]] > 1 || probabilities[[1L]] >= probabilities[[2L]]) {
    stop("probabilities must be two increasing values in [0, 1]", call. = FALSE)
  }
  out <- x[, .(
    draws = data.table::uniqueN(get(replicate_column)),
    scenario_lower_median = stats::median(scenario_lower),
    scenario_upper_median = stats::median(scenario_upper),
    scenario_lower_bound = stats::quantile(
      scenario_lower, probabilities[[1L]], names = FALSE
    ),
    scenario_upper_bound = stats::quantile(
      scenario_upper, probabilities[[2L]], names = FALSE
    ),
    probability_worst_case_positive = mean(scenario_lower > 0)
  ), by = group_columns]
  data.table::setorderv(out, group_columns)
  out[]
}

fixed_clock_policy_forbidden_columns_1d <- function() {
  c(
    "edge_distance_inches", "role_margin_inches", "abs_call", "call_wrong",
    "actual_wrong", "geometry_wrong", "geometry_success", "official_success",
    "challenge_outcome", "official_outcome", "is_overturned", "overturned",
    "actual_re_gain", "actual_wpa_gain", "truth_source",
    "official_geometry_mismatch"
  )
}

.fixed_clock_recursive_names_1d <- function(value) {
  found <- character()
  recurse <- function(x) {
    if (is.data.frame(x)) found <<- c(found, names(x))
    if (is.list(x)) {
      if (!is.null(names(x))) found <<- c(found, names(x))
      invisible(lapply(x, recurse))
    }
    invisible(NULL)
  }
  recurse(value)
  unique(found)
}

.fixed_clock_embedded_training_games_1d <- function(policy) {
  candidates <- list(policy$training_games)
  if (is.list(policy$prior_training_games)) {
    candidates <- c(candidates, unname(policy$prior_training_games))
  }
  if (is.list(policy$prior_fits)) {
    candidates <- c(candidates, lapply(
      policy$prior_fits, function(value) value$training_games
    ))
  }
  games <- unlist(candidates, recursive = TRUE, use.names = FALSE)
  games <- as.character(games)
  sort(unique(games[!is.na(games) & nzchar(games)]))
}

freeze_fixed_clock_policy_1d <- function(
  policy,
  training_game_ids,
  cutoff = fixed_clock_confirmation_cutoff_1d(),
  metadata = list()
) {
  training_games <- .fixed_clock_game_ids_1d(
    training_game_ids, "frozen-policy training games"
  )
  cutoff <- as.Date(cutoff)
  if (length(cutoff) != 1L || is.na(cutoff)) {
    stop("cutoff must be one non-missing date", call. = FALSE)
  }
  if (!is.list(metadata)) stop("metadata must be a list", call. = FALSE)
  embedded_training_games <- .fixed_clock_embedded_training_games_1d(policy)
  undeclared_training_games <- setdiff(embedded_training_games, training_games)
  if (length(undeclared_training_games)) {
    stop(
      "Frozen-policy training_game_ids omit embedded policy/nuisance games",
      call. = FALSE
    )
  }
  leaked <- intersect(
    .fixed_clock_recursive_names_1d(policy),
    fixed_clock_policy_forbidden_columns_1d()
  )
  if (length(leaked)) {
    stop(
      "Frozen policy contains evaluation truth/outcome fields: ",
      paste(sort(leaked), collapse = ", "), call. = FALSE
    )
  }
  core <- list(
    schema = "fixed_clock_frozen_policy_v1",
    cutoff = format(cutoff, "%Y-%m-%d"),
    training_game_ids = training_games,
    training_game_sha256 = fixed_clock_hash_object_1d(training_games),
    policy = .fixed_clock_canonical_1d(policy),
    metadata = .fixed_clock_canonical_1d(metadata)
  )
  structure(
    c(core, list(policy_sha256 = fixed_clock_hash_object_1d(core))),
    class = "fixed_clock_frozen_policy_1d"
  )
}

validate_frozen_fixed_clock_policy_1d <- function(frozen_policy) {
  if (!inherits(frozen_policy, "fixed_clock_frozen_policy_1d")) {
    stop("frozen_policy must be a fixed_clock_frozen_policy_1d", call. = FALSE)
  }
  required <- c(
    "schema", "cutoff", "training_game_ids", "training_game_sha256",
    "policy", "metadata", "policy_sha256"
  )
  if (!all(required %in% names(frozen_policy)) ||
      !identical(frozen_policy$schema, "fixed_clock_frozen_policy_v1")) {
    stop("Frozen-policy schema is invalid", call. = FALSE)
  }
  training_games <- .fixed_clock_game_ids_1d(
    frozen_policy$training_game_ids, "frozen-policy training games"
  )
  embedded_training_games <- .fixed_clock_embedded_training_games_1d(
    frozen_policy$policy
  )
  if (length(setdiff(embedded_training_games, training_games))) {
    stop(
      "Frozen-policy training_game_ids omit embedded policy/nuisance games",
      call. = FALSE
    )
  }
  if (!identical(
    frozen_policy$training_game_sha256,
    fixed_clock_hash_object_1d(training_games)
  )) {
    stop("Frozen-policy training-game hash is invalid", call. = FALSE)
  }
  core <- frozen_policy[setdiff(names(frozen_policy), "policy_sha256")]
  class(core) <- NULL
  if (!identical(
    as.character(frozen_policy$policy_sha256),
    fixed_clock_hash_object_1d(core)
  )) {
    stop("Frozen-policy integrity hash is invalid", call. = FALSE)
  }
  leaked <- intersect(
    .fixed_clock_recursive_names_1d(frozen_policy$policy),
    fixed_clock_policy_forbidden_columns_1d()
  )
  if (length(leaked)) {
    stop("Frozen policy contains evaluation truth/outcome fields", call. = FALSE)
  }
  invisible(TRUE)
}

build_fixed_clock_confirmation_manifest_1d <- function(
  split,
  frozen_policy,
  scenario_ids,
  source_hashes,
  seed = 20260826L,
  metadata = list()
) {
  validate_frozen_fixed_clock_policy_1d(frozen_policy)
  .fixed_clock_validate_split_hash_1d(split)
  scenarios <- sort(unique(as.character(scenario_ids)))
  if (!length(scenarios) || anyNA(scenarios) || any(!nzchar(scenarios))) {
    stop("scenario_ids must contain non-empty values", call. = FALSE)
  }
  source_hash_names <- names(source_hashes)
  source_hashes <- as.character(source_hashes)
  names(source_hashes) <- source_hash_names
  if (!length(source_hashes) || is.null(names(source_hashes)) ||
      anyNA(names(source_hashes)) || any(!nzchar(names(source_hashes))) ||
      anyDuplicated(names(source_hashes)) || anyNA(source_hashes) ||
      any(!nzchar(source_hashes))) {
    stop("source_hashes must be a uniquely named character vector", call. = FALSE)
  }
  source_hashes <- source_hashes[order(names(source_hashes))]
  seed <- .fixed_clock_scalar_integer_1d(seed, "seed", minimum = 1L)
  games <- data.table::as.data.table(split$game_split)
  development <- sort(games[partition == "development", as.character(game_pk)])
  confirmation <- sort(games[partition == "confirmation", as.character(game_pk)])
  payload <- list(
    schema = "fixed_clock_confirmation_manifest_v1",
    cutoff = format(as.Date(split$cutoff), "%Y-%m-%d"),
    split_sha256 = as.character(split$split_sha256),
    development_games = length(development),
    confirmation_games = length(confirmation),
    development_game_sha256 = fixed_clock_hash_object_1d(development),
    confirmation_game_sha256 = fixed_clock_hash_object_1d(confirmation),
    frozen_policy_sha256 = as.character(frozen_policy$policy_sha256),
    scenario_ids = scenarios,
    source_hashes = source_hashes,
    seed = seed,
    metadata = .fixed_clock_canonical_1d(metadata)
  )
  structure(
    list(
      payload = payload,
      manifest_sha256 = fixed_clock_hash_object_1d(payload)
    ),
    class = "fixed_clock_confirmation_manifest_1d"
  )
}

validate_fixed_clock_confirmation_manifest_1d <- function(
  manifest, split = NULL, frozen_policy = NULL
) {
  if (!inherits(manifest, "fixed_clock_confirmation_manifest_1d") ||
      !is.list(manifest$payload) ||
      !identical(
        manifest$payload$schema,
        "fixed_clock_confirmation_manifest_v1"
      )) {
    stop("Confirmation manifest schema is invalid", call. = FALSE)
  }
  expected <- fixed_clock_hash_object_1d(manifest$payload)
  if (!identical(as.character(manifest$manifest_sha256), expected)) {
    stop("Confirmation manifest integrity hash is invalid", call. = FALSE)
  }
  if (!is.null(split) && !identical(
    as.character(manifest$payload$split_sha256),
    as.character(split$split_sha256)
  )) {
    stop("Confirmation manifest does not match the temporal split", call. = FALSE)
  }
  if (!is.null(split)) {
    .fixed_clock_validate_split_hash_1d(split)
    games <- data.table::as.data.table(split$game_split)
    development <- sort(
      games[partition == "development", as.character(game_pk)]
    )
    confirmation <- sort(
      games[partition == "confirmation", as.character(game_pk)]
    )
    aligned <- identical(
      as.integer(manifest$payload$development_games),
      as.integer(length(development))
    ) && identical(
      as.integer(manifest$payload$confirmation_games),
      as.integer(length(confirmation))
    ) && identical(
      as.character(manifest$payload$development_game_sha256),
      fixed_clock_hash_object_1d(development)
    ) && identical(
      as.character(manifest$payload$confirmation_game_sha256),
      fixed_clock_hash_object_1d(confirmation)
    )
    if (!aligned) {
      stop(
        "Confirmation manifest game inventories do not match the split",
        call. = FALSE
      )
    }
  }
  if (!is.null(frozen_policy)) {
    validate_frozen_fixed_clock_policy_1d(frozen_policy)
    if (!identical(
      as.character(manifest$payload$frozen_policy_sha256),
      as.character(frozen_policy$policy_sha256)
    )) {
      stop("Confirmation manifest does not match the frozen policy", call. = FALSE)
    }
  }
  invisible(TRUE)
}

.fixed_clock_audit_row_1d <- function(check, passed, observed, expected, detail) {
  data.table::data.table(
    check = as.character(check),
    passed = isTRUE(passed),
    observed = as.character(observed),
    expected = as.character(expected),
    detail = as.character(detail)
  )
}

audit_fixed_clock_confirmation_leakage_1d <- function(
  split,
  frozen_policy,
  manifest = NULL,
  confirmation_actions = NULL,
  confirmation_truth = NULL,
  fail = FALSE
) {
  .fixed_clock_validate_split_hash_1d(split)
  games <- data.table::as.data.table(split$game_split)
  development <- sort(games[partition == "development", as.character(game_pk)])
  confirmation <- sort(games[partition == "confirmation", as.character(game_pk)])
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- .fixed_clock_audit_row_1d(...)

  policy_valid <- tryCatch({
    validate_frozen_fixed_clock_policy_1d(frozen_policy)
    TRUE
  }, error = function(error) FALSE)
  add(
    "frozen_policy_integrity", policy_valid, policy_valid, TRUE,
    "Frozen policy must retain its content hash"
  )
  training <- if (policy_valid) {
    as.character(frozen_policy$training_game_ids)
  } else {
    character()
  }
  add(
    "training_games_development_only",
    policy_valid && length(training) > 0L && all(training %in% development),
    sum(training %in% development), length(training),
    "Every frozen-policy training game must precede the cutoff"
  )
  overlap <- intersect(training, confirmation)
  add(
    "training_confirmation_disjoint", !length(overlap), length(overlap), 0,
    "No confirmation game may train the policy"
  )
  leaked_policy <- if (policy_valid) intersect(
    .fixed_clock_recursive_names_1d(frozen_policy$policy),
    fixed_clock_policy_forbidden_columns_1d()
  ) else "unverifiable"
  add(
    "frozen_policy_outcome_free", policy_valid && !length(leaked_policy),
    paste(leaked_policy, collapse = ","), "",
    "Frozen action rules may not contain evaluation truth or outcomes"
  )

  audit_confirmation_table <- function(value, label, outcome_free) {
    if (is.null(value)) return(invisible(NULL))
    x <- data.table::as.data.table(value)
    .fixed_clock_require_columns_1d(x, "game_pk", label)
    ids <- unique(as.character(x$game_pk))
    add(
      paste0(label, "_games_confirmation_only"),
      length(ids) > 0L && all(ids %in% confirmation),
      sum(ids %in% confirmation), length(ids),
      paste0(label, " rows must belong only to confirmation games")
    )
    if (isTRUE(outcome_free)) {
      leaked <- intersect(names(x), fixed_clock_policy_forbidden_columns_1d())
      add(
        paste0(label, "_outcome_free"), !length(leaked),
        paste(leaked, collapse = ","), "",
        paste0(label, " must be frozen before truth is joined")
      )
    }
    invisible(NULL)
  }
  audit_confirmation_table(
    confirmation_actions, "confirmation_actions", outcome_free = TRUE
  )
  audit_confirmation_table(
    confirmation_truth, "confirmation_truth", outcome_free = FALSE
  )
  if (!is.null(confirmation_actions) && !is.null(confirmation_truth)) {
    action <- data.table::as.data.table(confirmation_actions)
    truth <- data.table::as.data.table(confirmation_truth)
    keys <- intersect(
      c("game_pk", "team_id", "pitch_order"), intersect(names(action), names(truth))
    )
    aligned <- length(keys) > 0L && setequal(
      do.call(paste, c(action[, ..keys], sep = "\r")),
      do.call(paste, c(truth[, ..keys], sep = "\r"))
    )
    add(
      "confirmation_action_truth_keys_align", aligned,
      if (aligned) nrow(action) else "mismatch", nrow(truth),
      "Frozen action rows and quarantined truth must cover the same keys"
    )
  }
  if (!is.null(manifest)) {
    manifest_valid <- tryCatch({
      validate_fixed_clock_confirmation_manifest_1d(
        manifest, split = split, frozen_policy = frozen_policy
      )
      TRUE
    }, error = function(error) FALSE)
    add(
      "manifest_integrity", manifest_valid, manifest_valid, TRUE,
      "Manifest must match both split and frozen policy hashes"
    )
  }
  out <- data.table::rbindlist(rows, use.names = TRUE)
  if (isTRUE(fail) && any(!out$passed)) {
    stop(
      "Fixed-clock leakage audit failed: ",
      paste(out[passed == FALSE, check], collapse = ", "), call. = FALSE
    )
  }
  out[]
}
