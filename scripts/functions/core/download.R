mlb_get_to_file <- function(url, path, config, query = list(), timeout_seconds = 120) {
  if (file.exists(path) && !isTRUE(config$refresh)) return(normalizePath(path))
  request <- httr2::request(url) |>
    httr2::req_user_agent(config$user_agent) |>
    httr2::req_retry(max_tries = 5L, backoff = function(i) min(2^(i - 1), 20)) |>
    httr2::req_timeout(timeout_seconds)
  if (length(query)) request <- do.call(httr2::req_url_query, c(list(request), query))
  atomic_write(path, function(tmp) {
    response <- httr2::req_perform(request, path = tmp)
    httr2::resp_check_status(response)
  })
}

statcast_cache_has_data <- function(path) {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) {
    return(FALSE)
  }
  connection <- file(path, open = "rt", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  lines <- readLines(connection, n = 2L, warn = FALSE)
  length(lines) == 2L && nzchar(lines[[2L]])
}

live_feed_cache_is_final <- function(path, header_bytes = 65536L) {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) {
    return(FALSE)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  header <- rawToChar(readBin(connection, what = "raw", n = header_bytes))
  grepl('"abstractGameState"\\s*:\\s*"Final"', header)
}

download_statcast_history_mirror <- function(config) {
  # This immutable revision contains complete 2023 and 2024 regular seasons.
  # The upstream mirror had not yet ingested 2025 at this revision, so 2025 is
  # sourced separately from cap-safe daily Baseball Savant exports below.
  commit <- "cee03a3d322545fdee0db0d71d353c4191572469"
  url <- sprintf(
    paste0(
      "https://huggingface.co/datasets/Jensen-holm/statcast-era-pitches/",
      "resolve/%s/data/statcast_era_pitches.parquet"
    ),
    commit
  )
  path <- file.path(config$raw_dir, "statcast-history",
    paste0("statcast_era_pitches_", substr(commit, 1L, 8L), ".parquet"))
  expected_sha256 <- "e9b897ae79e9d3d135aad9be485924825f953aea9f9252c6f04b560b727d6e41"
  if (file.exists(path) && !isTRUE(config$refresh)) {
    if (!identical(sha256_file(path), expected_sha256)) {
      stop("Pinned historical Statcast mirror failed its SHA-256 check")
    }
    return(normalizePath(path))
  }
  atomic_write(path, function(tmp) {
    handle <- curl::new_handle()
    curl::handle_setopt(handle,
      followlocation = TRUE, timeout = 1800, connecttimeout = 30
    )
    curl::curl_download(url, tmp, quiet = TRUE, handle = handle)
    if (!identical(sha256_file(tmp), expected_sha256)) {
      stop("Downloaded historical Statcast mirror failed its SHA-256 check")
    }
  })
}

statcast_history_day_path <- function(date, config) {
  file.path(
    config$raw_dir, "statcast-history", "daily",
    paste0("statcast_", format(as.Date(date)), ".csv")
  )
}

download_statcast_history_day <- function(date, config) {
  date <- as.Date(date)
  mlb_get_to_file(
    "https://baseballsavant.mlb.com/statcast_search/csv",
    statcast_history_day_path(date, config),
    config,
    list(
      all = "true",
      type = "pitcher",
      game_date_gt = format(date, "%Y-%m-%d"),
      game_date_lt = format(date, "%Y-%m-%d")
    )
  )
}

download_statcast_history_days <- function(dates, config, cores = 8L) {
  dates <- as.Date(dates)
  paths <- vapply(dates, statcast_history_day_path, character(1), config = config)
  dir.create(dirname(paths[[1L]]), recursive = TRUE, showWarnings = FALSE)
  needed <- which(isTRUE(config$refresh) | !file.exists(paths))
  if (!length(needed)) return(normalizePath(paths))

  make_url <- function(date) {
    request <- httr2::request(
      "https://baseballsavant.mlb.com/statcast_search/csv"
    ) |>
      httr2::req_url_query(
        all = "true",
        type = "pitcher",
        game_date_gt = format(date, "%Y-%m-%d"),
        game_date_lt = format(date, "%Y-%m-%d")
      )
    request$url
  }
  batches <- split(needed, ceiling(seq_along(needed) / max(1L, cores)))
  for (batch in batches) {
    destinations <- paste0(paths[batch], ".partial")
    result <- curl::multi_download(
      urls = vapply(dates[batch], make_url, character(1)),
      destfiles = destinations,
      resume = TRUE,
      progress = FALSE,
      multi_timeout = 600,
      multiplex = TRUE,
      httpheader = c(paste0("User-Agent: ", config$user_agent))
    )
    good <- result$success & result$status_code %in% c(200L, 206L) &
      file.exists(destinations) & file.info(destinations)$size > 0
    if (!all(good)) {
      stop("Daily historical Statcast download failed for: ",
        paste(format(dates[batch][!good]), collapse = ", "))
    }
    moved <- file.rename(destinations, paths[batch])
    if (!all(moved)) stop("Could not move daily Statcast downloads into cache")
  }
  normalizePath(paths)
}

parallel_range_download <- function(
  url, path, expected_bytes, expected_sha256, chunks = 8L
) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temp_root <- tempfile(pattern = paste0(basename(path), ".parts."),
    tmpdir = dirname(path))
  dir.create(temp_root)
  on.exit(unlink(temp_root, recursive = TRUE), add = TRUE)
  starts <- floor(seq(0, expected_bytes, length.out = chunks + 1L))
  ranges <- data.frame(
    start = starts[-length(starts)],
    end = starts[-1L] - 1,
    part = file.path(temp_root, sprintf("part-%03d", seq_len(chunks)))
  )
  ranges$end[[nrow(ranges)]] <- expected_bytes - 1
  download_part <- function(i) {
    handle <- curl::new_handle()
    curl::handle_setopt(handle,
      range = sprintf("%d-%d", ranges$start[[i]], ranges$end[[i]]),
      followlocation = TRUE, timeout = 900, connecttimeout = 30
    )
    curl::curl_download(url, ranges$part[[i]], quiet = TRUE, handle = handle)
    expected <- ranges$end[[i]] - ranges$start[[i]] + 1
    observed <- unname(file.info(ranges$part[[i]])$size)
    if (observed != expected) stop("Range download size mismatch for part ", i)
    ranges$part[[i]]
  }
  results <- if (.Platform$OS.type != "windows" && chunks > 1L) {
    parallel::mclapply(seq_len(chunks), download_part,
      mc.cores = chunks, mc.preschedule = FALSE)
  } else {
    lapply(seq_len(chunks), download_part)
  }
  failed <- vapply(results, inherits, logical(1), what = "try-error")
  if (any(failed)) {
    stop("Mirror range download failed: ",
      paste(vapply(results[failed], as.character, character(1)), collapse = "; "))
  }
  parts <- unlist(results, use.names = FALSE)
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  if (!file.create(tmp)) stop("Could not create assembled mirror file")
  for (part in parts) {
    if (!file.append(tmp, part)) stop("Could not append mirror range part")
  }
  if (unname(file.info(tmp)$size) != expected_bytes) stop("Assembled mirror size mismatch")
  if (!identical(sha256_file(tmp), expected_sha256)) stop("Mirror SHA-256 mismatch")
  if (!file.rename(tmp, path)) stop("Could not move assembled mirror into place")
  normalizePath(path, mustWork = TRUE)
}

schedule_path <- function(config, start = config$start_date, end = config$cutoff_date) {
  file.path(
    config$raw_dir, "schedule",
    sprintf("schedule_%s_%s.json", format(start), format(end))
  )
}

download_schedule <- function(config, start = config$start_date, end = config$cutoff_date) {
  ensure_directories(config)
  mlb_get_to_file(
    "https://statsapi.mlb.com/api/v1/schedule",
    schedule_path(config, start, end),
    config,
    list(
      sportId = 1,
      gameType = config$game_type,
      startDate = format(start, "%Y-%m-%d"),
      endDate = format(end, "%Y-%m-%d"),
      hydrate = "team,linescore"
    )
  )
}

statcast_path <- function(date, config) {
  file.path(config$raw_dir, "statcast", paste0("statcast_", format(date), ".csv"))
}

download_statcast_day <- function(date, config) {
  ensure_directories(config)
  path <- statcast_path(date, config)
  if (!isTRUE(config$refresh) && file.exists(path) &&
      !statcast_cache_has_data(path)) {
    config$refresh <- TRUE
  }
  mlb_get_to_file(
    "https://baseballsavant.mlb.com/statcast_search/csv",
    path,
    config,
    list(
      all = "true",
      type = "pitcher",
      game_date_gt = format(date, "%Y-%m-%d"),
      game_date_lt = format(date, "%Y-%m-%d")
    )
  )
}

statcast_history_path <- function(start, end, config) {
  file.path(config$raw_dir, "statcast-history",
    sprintf("statcast_%s_%s.csv", format(start), format(end)))
}

download_statcast_period <- function(period, config) {
  start <- as.Date(period$start[[1L]])
  end <- as.Date(period$end[[1L]])
  mlb_get_to_file(
    "https://baseballsavant.mlb.com/statcast_search/csv",
    statcast_history_path(start, end, config),
    config,
    list(
      all = "true",
      type = "pitcher",
      game_date_gt = format(start, "%Y-%m-%d"),
      game_date_lt = format(end, "%Y-%m-%d")
    )
  )
}

download_statcast_period_complete <- function(period, config, row_cap = 25000L) {
  start <- as.Date(period$start[[1L]])
  end <- as.Date(period$end[[1L]])
  path <- download_statcast_period(data.frame(start = start, end = end), config)
  rows <- nrow(data.table::fread(path, select = "game_pk", showProgress = FALSE))
  if (rows < row_cap || start >= end) return(path)
  span <- as.integer(end - start)
  left_end <- start + floor(span / 2)
  right_start <- left_end + 1L
  c(
    download_statcast_period_complete(
      data.frame(start = start, end = left_end), config, row_cap
    ),
    download_statcast_period_complete(
      data.frame(start = right_start, end = end), config, row_cap
    )
  )
}

live_feed_path <- function(game_pk, config) {
  file.path(config$raw_dir, "live", paste0("game_", game_pk, ".json"))
}

download_live_feed <- function(game_pk, config) {
  ensure_directories(config)
  path <- live_feed_path(game_pk, config)
  if (!isTRUE(config$refresh) && file.exists(path) &&
      !live_feed_cache_is_final(path)) {
    config$refresh <- TRUE
  }
  mlb_get_to_file(
    sprintf("https://statsapi.mlb.com/api/v1.1/game/%s/feed/live", game_pk),
    path,
    config
  )
}

savant_team_path <- function(team_id, config) {
  file.path(config$raw_dir, "savant", paste0("team_", team_id, "_", config$season, ".json"))
}

savant_dashboard_path <- function(config) {
  file.path(config$raw_dir, "savant", paste0("abs_dashboard_", config$season, ".html"))
}

download_savant_dashboard <- function(config) {
  ensure_directories(config)
  mlb_get_to_file(
    "https://baseballsavant.mlb.com/abs",
    savant_dashboard_path(config),
    config
  )
}

download_savant_team <- function(team_id, config) {
  ensure_directories(config)
  mlb_get_to_file(
    sprintf("https://baseballsavant.mlb.com/leaderboard/services/abs/%s", team_id),
    savant_team_path(team_id, config),
    config,
    list(
      gameType = "regular",
      year = config$season,
      challengeType = "team-summary",
      level = config$level,
      dataCount = "runs"
    )
  )
}

write_source_manifest <- function(paths, source, config) {
  manifest <- file_manifest(paths, source)
  path <- file.path(config$raw_dir, "manifests", paste0(source, ".csv"))
  write_csv_atomic(manifest, path)
}
