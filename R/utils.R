`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

scalar_chr <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L) return(default)
  as.character(x[[1L]])
}

scalar_int <- function(x, default = NA_integer_) {
  if (is.null(x) || length(x) == 0L) return(default)
  as.integer(x[[1L]])
}

scalar_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) return(default)
  as.numeric(x[[1L]])
}

scalar_lgl <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0L) return(default)
  as.logical(x[[1L]])
}

ensure_directories <- function(config) {
  paths <- c(
    config$raw_dir,
    file.path(config$raw_dir, "schedule"),
    file.path(config$raw_dir, "statcast"),
    file.path(config$raw_dir, "statcast-history"),
    file.path(config$raw_dir, "live"),
    file.path(config$raw_dir, "savant"),
    file.path(config$raw_dir, "manifests"),
    config$derived_dir,
    config$fixture_dir
  )
  invisible(lapply(paths, dir.create, recursive = TRUE, showWarnings = FALSE))
  paths
}

atomic_write <- function(path, writer) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  writer(tmp)
  if (!file.rename(tmp, path)) stop("Could not atomically move file into place: ", path)
  normalizePath(path, mustWork = TRUE)
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

file_manifest <- function(paths, source, retrieved_at = Sys.time()) {
  paths <- paths[file.exists(paths)]
  data.frame(
    source = source,
    path = normalizePath(paths, mustWork = TRUE),
    bytes = unname(file.info(paths)$size),
    sha256 = vapply(paths, sha256_file, character(1)),
    retrieved_at_utc = format(retrieved_at, tz = "UTC", usetz = TRUE),
    stringsAsFactors = FALSE
  )
}

write_parquet_atomic <- function(x, path) {
  atomic_write(path, function(tmp) arrow::write_parquet(x, tmp, compression = "zstd"))
}

write_csv_atomic <- function(x, path) {
  atomic_write(path, function(tmp) readr::write_csv(x, tmp, na = ""))
}

safe_rbindlist <- function(x) {
  x <- Filter(function(z) !is.null(z) && nrow(z) > 0L, x)
  if (!length(x)) return(data.table::data.table())
  data.table::rbindlist(x, fill = TRUE, use.names = TRUE)
}

opposite_call <- function(call) {
  out <- rep(NA_character_, length(call))
  out[call == "ball"] <- "called_strike"
  out[call == "called_strike"] <- "ball"
  out
}

score_bucket <- function(score_diff) {
  cut(
    score_diff,
    breaks = c(-Inf, -5, -3, -2, -1, 0, 1, 2, 4, Inf),
    labels = c("<=-5", "-4:-3", "-2", "-1", "0", "1", "2", "3:4", ">=5"),
    right = TRUE
  )
}

stop_if_missing_columns <- function(x, required, label = deparse(substitute(x))) {
  missing <- setdiff(required, names(x))
  if (length(missing)) stop(label, " is missing required columns: ", paste(missing, collapse = ", "))
  invisible(x)
}
