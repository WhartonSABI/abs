project_root <- rprojroot::find_root(rprojroot::has_file("_targets.R"), path = getwd())
source(file.path(project_root, "config", "project.R"))
source(file.path(project_root, "scripts", "load_functions.R"))
load_abs_functions(project_root)

fixture_path <- function(name) file.path(project_root, "data", "fixtures", name)
