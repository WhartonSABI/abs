#!/usr/bin/env Rscript

# Export the compact, versioned paper-results bundle from one completed
# fixed-clock run. The policy remains frozen; the reporting calendar removes
# games listed in data/reference/game_exclusions.csv.

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop(
    "Usage: Rscript scripts/export_paper_results.R <completed-full-run-dir>",
    call. = FALSE
  )
}

project_root <- rprojroot::find_root(
  rprojroot::has_file("_targets.R"), path = getwd()
)
run_dir <- normalizePath(args[[1L]], mustWork = TRUE)
results_dir <- file.path(project_root, "results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

required <- c(
  "compact_reporting_table.csv",
  "confirmation_game_values.parquet",
  "confirmation_direct_replay.parquet",
  "confirmation_direct_policy_actions.parquet",
  "coordinated_bootstrap_draws.parquet",
  "scenario_percentile_intervals.csv",
  "effective_widths.csv",
  "empirical_prior_alpha_selection.csv",
  "frozen_policy.rds",
  "split_games.csv",
  "run_manifest.json"
)
missing <- required[!file.exists(file.path(run_dir, required))]
if (length(missing)) {
  stop("Completed run is missing: ", paste(missing, collapse = ", "),
    call. = FALSE
  )
}

one <- function(x, label) {
  if (length(x) != 1L || is.na(x) || !nzchar(as.character(x))) {
    stop(label, " must resolve to exactly one non-missing value", call. = FALSE)
  }
  as.character(x)
}

frozen_policy <- readRDS(file.path(run_dir, "frozen_policy.rds"))
source_run_manifest <- fromJSON(file.path(run_dir, "run_manifest.json"))
source_development_games <- as.integer(source_run_manifest$development_games)
source_confirmation_games <- as.integer(source_run_manifest$confirmation_games)
source_frozen_policy_replicates <- as.integer(source_run_manifest$bootstrap_reps)
source_full_procedure_replicates <- as.integer(
  source_run_manifest$learning_procedure_bootstrap_reps
)
if (anyNA(c(
  source_development_games,
  source_confirmation_games,
  source_frozen_policy_replicates,
  source_full_procedure_replicates
))) {
  stop("The completed run manifest has incomplete provenance counts")
}
selected_candidate_id <- one(
  frozen_policy$policy$selected_candidate_id,
  "Selected candidate"
)
cross_evaluation <- as.data.table(frozen_policy$policy$cross_evaluation)
binding <- cross_evaluation[
  candidate_id == selected_candidate_id
][order(expected_re_per_team_game, evaluation_scenario_id)]
if (!nrow(binding)) stop("Selected candidate has no cross-evaluation rows")
reporting_scenario_id <- one(
  binding$evaluation_scenario_id[[1L]],
  "Binding evaluation scenario"
)

split_games <- fread(file.path(run_dir, "split_games.csv"))
split_games[, game_pk := as.character(game_pk)]
game_exclusions <- fread(file.path(
  project_root, "data", "reference", "game_exclusions.csv"
))
game_exclusions[, game_pk := as.character(game_pk)]
excluded_split <- merge(
  game_exclusions,
  split_games,
  by = "game_pk",
  all.x = TRUE,
  sort = FALSE
)
if (anyNA(excluded_split$partition)) {
  stop("A versioned game exclusion is absent from the completed-run split")
}
reporting_split <- split_games[!game_pk %chin% game_exclusions$game_pk]
expected_games <- c(development = 1488L, confirmation = 497L)
observed_games <- reporting_split[, .N, by = partition]
for (partition_name in names(expected_games)) {
  count <- observed_games[partition == partition_name, N]
  if (length(count) != 1L || count != expected_games[[partition_name]]) {
    stop("Unexpected ", partition_name, " reporting-game count: ", count)
  }
}
confirmation_exclusions <- excluded_split[
  partition == "confirmation", game_pk
]

team_values <- as.data.table(read_parquet(file.path(
  run_dir, "confirmation_game_values.parquet"
)))[scenario_id == reporting_scenario_id]
replay <- as.data.table(read_parquet(file.path(
  run_dir, "confirmation_direct_replay.parquet"
)))[evaluation_scenario_id == reporting_scenario_id]
policy_actions <- as.data.table(read_parquet(file.path(
  run_dir, "confirmation_direct_policy_actions.parquet"
)))[evaluation_scenario_id == reporting_scenario_id]

for (object_name in c("team_values", "replay", "policy_actions")) {
  object <- get(object_name)
  object[, game_pk := as.character(game_pk)]
  object <- object[!game_pk %chin% confirmation_exclusions]
  assign(object_name, object)
}
if (uniqueN(team_values$game_pk) != expected_games[["confirmation"]] ||
    uniqueN(replay$game_pk) != expected_games[["confirmation"]] ||
    uniqueN(policy_actions$game_pk) != expected_games[["confirmation"]]) {
  stop("Corrected applied files do not contain exactly 497 games")
}

summary_data <- team_values[, .(
  captured_re = sum(captured_re),
  observed_re = sum(observed_re),
  oracle_re = sum(oracle_re),
  team_games = sum(team_games),
  attempts = sum(attempts),
  successes = sum(successes),
  failures = sum(failures)
), by = .(policy, scenario_id, role)]
summary_data[, `:=`(
  gain_over_observed_re = captured_re - observed_re,
  captured_re_per_team_game = captured_re / team_games,
  gain_over_observed_re_per_team_game =
    (captured_re - observed_re) / team_games,
  share_of_oracle = captured_re / oracle_re,
  success_rate = successes / attempts,
  games = expected_games[["confirmation"]],
  captured_re_per_game = captured_re / expected_games[["confirmation"]],
  gain_over_observed_re_per_game =
    (captured_re - observed_re) / expected_games[["confirmation"]],
  fraction_observed_to_oracle_gap_closed =
    (captured_re - observed_re) / (oracle_re - observed_re)
)]
setorder(summary_data, policy, role)

# The two excluded confirmation games had no observed challenges. Their
# removal changes the inning denominators and learned expected attempts, but
# not the observed challenge numerators.
source_inning_path <- file.path(results_dir, "inning_policy_summary.csv")
if (!file.exists(source_inning_path)) {
  stop(
    "results/inning_policy_summary.csv is needed to retain the audited ",
    "observed inning numerators", call. = FALSE
  )
}
source_inning <- fread(source_inning_path)
source_inning[, inning := as.character(inning)]
replay[, inning_group := fifelse(inning >= 10L, "10+", as.character(inning))]
inning_summary <- replay[, .(
  clock_opportunities = .N,
  learned_expected_challenges = sum(expected_challenges)
), by = .(role, inning = inning_group)]
inning_summary <- merge(
  inning_summary,
  source_inning[, .(role, inning, observed_challenges)],
  by = c("role", "inning"),
  all.x = TRUE
)
if (anyNA(inning_summary$observed_challenges) ||
    sum(inning_summary$observed_challenges) != 2275L) {
  stop("Observed inning counts do not reconcile to 2,275 challenges")
}
inning_summary[, `:=`(
  observed_challenge_rate = observed_challenges / clock_opportunities,
  learned_challenge_rate =
    learned_expected_challenges / clock_opportunities
)]
inning_summary[, inning_index := fifelse(
  inning == "10+", 10L, suppressWarnings(as.integer(inning))
)]
setorder(inning_summary, role, inning_index)
inning_summary[, inning_index := NULL]

pitch_ledger <- tryCatch(
  as.data.table(targets::tar_read(pitch_ledger)),
  error = function(error) {
    public_path <- file.path(
      project_root, "data", "analysis", "pitch_ledger.parquet"
    )
    if (!file.exists(public_path)) {
      stop(
        "Neither the pitch_ledger target nor public analysis input exists: ",
        conditionMessage(error), call. = FALSE
      )
    }
    as.data.table(read_parquet(public_path))
  }
)
savant_challenges <- tryCatch(
  as.data.table(targets::tar_read(savant)),
  error = function(error) {
    public_path <- file.path(
      project_root, "data", "analysis", "savant_challenges.parquet"
    )
    if (!file.exists(public_path)) {
      stop(
        "Neither the savant target nor public analysis input exists: ",
        conditionMessage(error), call. = FALSE
      )
    }
    as.data.table(read_parquet(public_path))
  }
)
pitch_ledger[, `:=`(
  game_pk = as.character(game_pk),
  partition = fifelse(
    as.Date(game_date) < as.Date("2026-07-20"),
    "development", "confirmation"
  )
)]
reporting_ledger <- pitch_ledger[
  !game_pk %chin% game_exclusions$game_pk
]
sample_counts <- reporting_ledger[, .(
  games = uniqueN(game_pk),
  called_pitches = .N,
  tracked_pitches = sum(tracking_available),
  observed_challenges = sum(challenge_occurred)
), by = partition]
if (!identical(
  sample_counts[order(partition), games],
  as.integer(expected_games[sort(names(expected_games))])
)) {
  stop("Pitch-ledger reporting counts do not match the corrected split")
}
confirmation_ledger <- reporting_ledger[partition == "confirmation"]
sample_summary <- data.table(
  metric = c(
    "raw_games", "raw_called_pitches", "raw_tracked_pitches",
    "development_games", "development_called_pitches",
    "development_tracked_pitches", "development_observed_challenges",
    "confirmation_games", "confirmation_called_pitches",
    "confirmation_tracked_pitches", "confirmation_observed_challenges",
    "confirmation_overturned", "confirmation_upheld",
    "tracked_official_challenge_rulings",
    "confirmation_geometry_correctable_calls",
    "confirmation_positive_value_oracle_actions",
    "confirmation_zero_value_correctable_calls"
  ),
  value = c(
    uniqueN(pitch_ledger$game_pk),
    nrow(pitch_ledger),
    sum(pitch_ledger$tracking_available),
    sample_counts[partition == "development", games],
    sample_counts[partition == "development", called_pitches],
    sample_counts[partition == "development", tracked_pitches],
    sample_counts[partition == "development", observed_challenges],
    sample_counts[partition == "confirmation", games],
    sample_counts[partition == "confirmation", called_pitches],
    sample_counts[partition == "confirmation", tracked_pitches],
    sample_counts[partition == "confirmation", observed_challenges],
    confirmation_ledger[challenge_occurred == TRUE, sum(is_overturned)],
    confirmation_ledger[challenge_occurred == TRUE, sum(!is_overturned)],
    nrow(savant_challenges),
    sum(replay$geometry_success),
    summary_data[
      policy == "exact_location_oracle" & role == "combined", attempts
    ],
    sum(replay$geometry_success) - summary_data[
      policy == "exact_location_oracle" & role == "combined", attempts
    ]
  )
)

original_bootstrap_draws <- as.data.table(read_parquet(file.path(
  run_dir, "coordinated_bootstrap_draws.parquet"
)))[scenario_id == reporting_scenario_id]
original_bootstrap_intervals <- fread(file.path(
  run_dir, "scenario_percentile_intervals.csv"
))[scenario_id == reporting_scenario_id]

# The saved full-learning draws are aggregate-only, so they cannot be
# reweighted after deleting games. Compute a corrected whole-game interval for
# the already-frozen policies and retain the original full-learning interval as
# a separately labeled sensitivity artifact.
bootstrap_replicates <- 5000L
bootstrap_seed <- 20260904L
game_ids <- sort(unique(team_values$game_pk))
set.seed(bootstrap_seed)
game_weights <- rmultinom(
  n = bootstrap_replicates,
  size = length(game_ids),
  prob = rep(1 / length(game_ids), length(game_ids))
)
game_values <- team_values[, .(
  captured_re = sum(captured_re),
  observed_re = sum(observed_re),
  oracle_re = sum(oracle_re),
  team_games = sum(team_games),
  attempts = sum(attempts),
  successes = sum(successes),
  failures = sum(failures)
), by = .(game_pk, policy, scenario_id, role)]

bootstrap_parts <- lapply(
  split(game_values, by = c("policy", "scenario_id", "role"), keep.by = TRUE),
  function(group) {
    group <- group[match(game_ids, game_pk)]
    if (anyNA(group$game_pk)) {
      stop("A policy-role group is missing a corrected reporting game")
    }
    totals <- lapply(
      c(
        "captured_re", "observed_re", "oracle_re", "team_games",
        "attempts", "successes", "failures"
      ),
      function(column) as.numeric(crossprod(game_weights, group[[column]]))
    )
    names(totals) <- c(
      "captured_re", "observed_re", "oracle_re", "team_games",
      "attempts", "successes", "failures"
    )
    out <- as.data.table(totals)
    out[, `:=`(
      replicate = seq_len(bootstrap_replicates),
      policy = group$policy[[1L]],
      scenario_id = group$scenario_id[[1L]],
      role = group$role[[1L]]
    )]
    out[, `:=`(
      gain_over_observed_re = captured_re - observed_re,
      captured_re_per_team_game = captured_re / team_games,
      gain_over_observed_re_per_team_game =
        (captured_re - observed_re) / team_games,
      share_of_oracle = captured_re / oracle_re,
      success_rate = successes / attempts,
      games = expected_games[["confirmation"]],
      captured_re_per_game =
        captured_re / expected_games[["confirmation"]],
      gain_over_observed_re_per_game =
        (captured_re - observed_re) / expected_games[["confirmation"]]
    )]
    out[]
  }
)
bootstrap_draws <- rbindlist(bootstrap_parts, use.names = TRUE)
setcolorder(bootstrap_draws, c(
  "replicate", "policy", "scenario_id", "role",
  setdiff(names(bootstrap_draws), c(
    "replicate", "policy", "scenario_id", "role"
  ))
))
setorder(bootstrap_draws, replicate, policy, role)
bootstrap_intervals <- bootstrap_draws[, .(
  bootstrap_replicates = uniqueN(replicate),
  captured_re_lower_95 = quantile(captured_re, 0.025),
  captured_re_upper_95 = quantile(captured_re, 0.975),
  gain_lower_95 = quantile(gain_over_observed_re, 0.025),
  gain_upper_95 = quantile(gain_over_observed_re, 0.975),
  oracle_share_lower_95 = quantile(share_of_oracle, 0.025),
  oracle_share_upper_95 = quantile(share_of_oracle, 0.975)
), by = .(policy, scenario_id, role)]

write_parquet(team_values, file.path(results_dir, "team_game_values.parquet"))
write_parquet(replay, file.path(results_dir, "opportunity_replay.parquet"))
write_parquet(
  policy_actions, file.path(results_dir, "policy_actions.parquet")
)
write_parquet(
  bootstrap_draws, file.path(results_dir, "bootstrap_draws.parquet")
)
write_parquet(
  original_bootstrap_draws,
  file.path(results_dir, "full_procedure_bootstrap_original_499.parquet")
)
fwrite(summary_data, file.path(results_dir, "summary.csv"))
fwrite(bootstrap_intervals, file.path(results_dir, "bootstrap_intervals.csv"))
fwrite(
  original_bootstrap_intervals,
  file.path(results_dir, "full_procedure_intervals_original_499.csv")
)
fwrite(inning_summary, file.path(results_dir, "inning_policy_summary.csv"))
fwrite(sample_summary, file.path(results_dir, "sample_summary.csv"))
fwrite(reporting_split, file.path(results_dir, "split_games.csv"))
fwrite(excluded_split, file.path(results_dir, "reporting_game_exclusions.csv"))

for (filename in c(
  "effective_widths.csv",
  "empirical_prior_alpha_selection.csv",
  "frozen_policy.rds",
  "run_manifest.json"
)) {
  copied <- file.copy(
    file.path(run_dir, filename),
    file.path(results_dir, filename),
    overwrite = TRUE
  )
  if (!isTRUE(copied)) stop("Could not copy ", filename)
}

reporting_manifest <- list(
  schema = "paper_results_bundle_v1",
  source_run_directory = basename(run_dir),
  selected_candidate_id = selected_candidate_id,
  binding_evaluation_scenario_id = reporting_scenario_id,
  reporting_games = as.list(expected_games),
  source_run_games = list(
    development = source_development_games,
    confirmation = source_confirmation_games
  ),
  excluded_games = split(
    excluded_split$game_pk, excluded_split$partition
  ),
  point_estimate_scope = paste(
    "frozen policy rescored by deleting games without ABS infrastructure"
  ),
  bootstrap_scope = paste(
    bootstrap_replicates,
    "whole-game resamples of the corrected",
    paste0(expected_games[["confirmation"]], "-game evaluation set, with"),
    "the learned policy held fixed"
  ),
  bootstrap_seed = bootstrap_seed,
  sensitivity_scope = paste(
    "the original completed-run artifacts retain",
    source_full_procedure_replicates, "full-learning and",
    source_frozen_policy_replicates,
    "frozen-policy replicates on the uncorrected",
    paste0(source_confirmation_games, "-game calendar and are"),
    "stored separately rather than relabeled as corrected uncertainty"
  )
)
write_json(
  reporting_manifest,
  file.path(results_dir, "reporting_manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

message(
  "Exported corrected ", expected_games[["confirmation"]],
  "-game paper bundle for ", reporting_scenario_id,
  " from ", basename(run_dir)
)
