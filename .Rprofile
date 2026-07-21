local_library <- file.path(getwd(), ".Rlib")
if (dir.exists(local_library)) .libPaths(c(normalizePath(local_library), .libPaths()))

if (file.exists("renv/activate.R")) {
  if (requireNamespace("renv", quietly = TRUE)) {
    renv::load()
  } else {
    source("renv/activate.R")
  }
}

# Keep the lightweight local bootstrap library visible after renv activation.
if (dir.exists(local_library)) .libPaths(c(normalizePath(local_library), .libPaths()))

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  targets.progress = "summary"
)
