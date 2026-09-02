# Recursively discover and source the project's modular R implementation.

abs_function_files <- function(project_root = ".") {
  sort(list.files(
    file.path(project_root, "scripts", "functions"),
    pattern = "[.]R$",
    full.names = TRUE,
    recursive = TRUE
  ))
}

load_abs_functions <- function(project_root = ".", envir = globalenv()) {
  files <- abs_function_files(project_root)
  if (!length(files)) {
    stop("No R function files were found under scripts/functions")
  }
  invisible(lapply(files, sys.source, envir = envir))
}
