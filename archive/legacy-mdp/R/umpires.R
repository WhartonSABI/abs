# Home-plate umpire extraction from MLB live-feed JSONs.
# This is the provenance of data/processed/umpires_2026.rds -- day to day,
# just readRDS() that file. Pending: fold this into the targets pipeline.

get_home_plate_umpire <- function(path) {
  feed <- jsonlite::read_json(path)
  officials <- feed$liveData$boxscore$officials
  hp <- Filter(function(o) o$officialType == "Home Plate", officials)
  if (length(hp) == 0) {
    return(data.frame(game_pk = feed$gamePk,
                      umpire_id = NA_integer_, umpire_name = NA_character_))
  }
  data.frame(game_pk     = feed$gamePk,
             umpire_id   = hp[[1]]$official$id,
             umpire_name = hp[[1]]$official$fullName)
}

# Takes a few minutes over ~2,400 game files.
build_umpire_table <- function(dir = "data/raw/live") {
  paths <- list.files(dir, pattern = "json$", full.names = TRUE)
  do.call(rbind, lapply(paths, get_home_plate_umpire))
}

# Audit after building (verified 2026-08-12: 100% coverage, zero NAs):
#   umpires <- build_umpire_table()
#   nrow(umpires)                                      # one row per game
#   sum(is.na(umpires$umpire_id))                      # want 0
#   dplyr::n_distinct(umpires$umpire_name)             # ~70-100 MLB umpires
#   mean(unique(ledger$game_pk) %in% umpires$game_pk)  # want 1.0
#   saveRDS(umpires, "data/processed/umpires_2026.rds")
