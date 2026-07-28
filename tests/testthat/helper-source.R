project_root <- rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd())
source(file.path(project_root, "config", "project.R"))
invisible(lapply(
  list.files(file.path(project_root, "scripts", "functions"), full.names = TRUE),
  source
))

fixture_path <- function(name) file.path(project_root, "data", "fixtures", name)
