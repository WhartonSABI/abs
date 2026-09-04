#!/usr/bin/env Rscript

# Build the paper's publication figures and table fragments from the compact,
# committed applied-results bundle. Run from the repository root with:
#
#   Rscript paper/build_figures.R

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(ggplot2)
  library(ltc)
  library(scales)
})

args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(script_arg)) {
  normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
} else {
  normalizePath("paper/build_figures.R", mustWork = TRUE)
}
project_root <- dirname(dirname(script_path))
results_dir <- file.path(project_root, "results")
output_dir <- file.path(project_root, "paper", "figures")
supplement_dir <- file.path(project_root, "paper", "supplement")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(supplement_dir, recursive = TRUE, showWarnings = FALSE)

required_inputs <- c(
  "summary.csv",
  "bootstrap_intervals.csv",
  "inning_policy_summary.csv",
  "sample_summary.csv",
  "effective_widths.csv",
  "empirical_prior_alpha_selection.csv",
  "reporting_manifest.json",
  "run_manifest.json",
  "frozen_policy.rds",
  "team_game_values.parquet",
  "opportunity_replay.parquet",
  "policy_actions.parquet"
)
missing_inputs <- required_inputs[
  !file.exists(file.path(results_dir, required_inputs))
]
if (length(missing_inputs)) {
  stop(
    "Missing committed results inputs: ",
    paste(missing_inputs, collapse = ", "),
    call. = FALSE
  )
}

summary_data <- fread(file.path(results_dir, "summary.csv"))
interval_data <- fread(file.path(results_dir, "bootstrap_intervals.csv"))
inning_policy_summary <- fread(file.path(
  results_dir, "inning_policy_summary.csv"
))
sample_summary <- fread(file.path(results_dir, "sample_summary.csv"))
effective_widths <- fread(file.path(results_dir, "effective_widths.csv"))
alpha_selection <- fread(file.path(
  results_dir, "empirical_prior_alpha_selection.csv"
))
reporting_manifest <- jsonlite::fromJSON(file.path(
  results_dir, "reporting_manifest.json"
))
run_manifest <- jsonlite::fromJSON(file.path(
  results_dir, "run_manifest.json"
))
frozen_policy <- readRDS(file.path(results_dir, "frozen_policy.rds"))
team_game_values <- as.data.table(read_parquet(
  file.path(results_dir, "team_game_values.parquet")
))
opportunity_replay <- as.data.table(read_parquet(
  file.path(results_dir, "opportunity_replay.parquet")
))
policy_actions <- as.data.table(read_parquet(
  file.path(results_dir, "policy_actions.parquet")
))

final_scenario_id <- reporting_manifest$binding_evaluation_scenario_id
if (length(final_scenario_id) != 1L || is.na(final_scenario_id) ||
    !nzchar(final_scenario_id)) {
  stop("The reporting manifest has no binding evaluation scenario")
}
development_training_games <- as.integer(run_manifest$development_games)
if (is.na(development_training_games) || development_training_games <= 0L) {
  stop("The run manifest has no valid development-game count")
}

one_row <- function(out, label) {
  if (nrow(out) != 1L) {
    stop(label, " must resolve to exactly one row; found ", nrow(out),
      call. = FALSE
    )
  }
  out
}

sample_value <- function(metric_name) {
  row <- one_row(
    sample_summary[metric == metric_name],
    paste("Sample metric", metric_name)
  )
  as.numeric(row$value)
}

width_for <- function(role_name) {
  row <- one_row(
    effective_widths[role == role_name],
    paste(role_name, "effective width")
  )
  as.numeric(row$sigma_inches)
}

selected_alpha <- alpha_selection[selected == TRUE]
if (nrow(selected_alpha) != 2L ||
    !setequal(selected_alpha$role, c("offense", "defense")) ||
    uniqueN(selected_alpha$alpha) != 1L) {
  stop("Empirical-prior alpha selection is incomplete or role-inconsistent")
}
selected_alpha_value <- as.numeric(selected_alpha$alpha[[1L]])

observed <- one_row(
  summary_data[
    policy == "observed" & role == "combined" &
      scenario_id == final_scenario_id
  ],
  "Observed summary"
)
learned <- one_row(
  summary_data[
    policy == "direct_learning_procedure" & role == "combined" &
      scenario_id == final_scenario_id
  ],
  "Learned-policy summary"
)
bellman <- one_row(
  summary_data[
    policy == "bellman_structural" & role == "combined" &
      scenario_id == final_scenario_id
  ],
  "Bellman summary"
)
oracle <- one_row(
  summary_data[
    policy == "exact_location_oracle" & role == "combined" &
      scenario_id == final_scenario_id
  ],
  "Oracle summary"
)
learned_offense <- one_row(
  summary_data[
    policy == "direct_learning_procedure" & role == "offense" &
      scenario_id == final_scenario_id
  ],
  "Learned offense summary"
)
learned_defense <- one_row(
  summary_data[
    policy == "direct_learning_procedure" & role == "defense" &
      scenario_id == final_scenario_id
  ],
  "Learned defense summary"
)
oracle_offense <- one_row(
  summary_data[
    policy == "exact_location_oracle" & role == "offense" &
      scenario_id == final_scenario_id
  ],
  "Oracle offense summary"
)
oracle_defense <- one_row(
  summary_data[
    policy == "exact_location_oracle" & role == "defense" &
      scenario_id == final_scenario_id
  ],
  "Oracle defense summary"
)
public_information <- one_row(
  summary_data[
    policy == "public_information_only" & role == "combined" &
      scenario_id == final_scenario_id
  ],
  "Public-information summary"
)
fitted_human <- one_row(
  summary_data[
    policy == "fitted_human_selection" & role == "combined" &
      scenario_id == final_scenario_id
  ],
  "Fitted-human summary"
)

interval_for <- function(policy_name, role_name = "combined") {
  one_row(
    interval_data[
      policy == policy_name & role == role_name &
        scenario_id == final_scenario_id
    ],
    paste(policy_name, role_name, "bootstrap interval")
  )
}

learned_interval <- interval_for("direct_learning_procedure")
bellman_interval <- interval_for("bellman_structural")
oracle_interval <- interval_for("exact_location_oracle")

if (sample_value("development_games") != 1488L ||
    sample_value("confirmation_games") != 497L ||
    reporting_manifest$reporting_games$development != 1488L ||
    reporting_manifest$reporting_games$confirmation != 497L) {
  stop("The compact bundle is not the 1,488/497 analysis sample")
}
if (reporting_manifest$source_run_games$development !=
    development_training_games) {
  stop("The reporting and run manifests disagree on the estimation sample")
}

if (uniqueN(opportunity_replay$evaluation_scenario_id) != 1L ||
    unique(opportunity_replay$evaluation_scenario_id) != final_scenario_id ||
    uniqueN(policy_actions$evaluation_scenario_id) != 1L ||
    unique(policy_actions$evaluation_scenario_id) != final_scenario_id) {
  stop("The applied opportunity files do not contain only the final scenario",
    call. = FALSE
  )
}

# The policy object contains the development-period 25-by-25 cross-evaluation
# used for maximin candidate selection.
scenario_grid <- as.data.table(frozen_policy$metadata$scenario_grid)
policy_cross_evaluation <- as.data.table(
  frozen_policy$policy$cross_evaluation
)
candidate_summary <- as.data.table(frozen_policy$policy$candidate_summary)
selected_candidate_id <- frozen_policy$policy$selected_candidate_id

if (nrow(scenario_grid) != 25L ||
    uniqueN(scenario_grid$scenario_id) != 25L ||
    nrow(policy_cross_evaluation) != 625L ||
    uniqueN(policy_cross_evaluation$candidate_id) != 25L ||
    uniqueN(policy_cross_evaluation$evaluation_scenario_id) != 25L ||
    nrow(candidate_summary) != 25L ||
    sum(candidate_summary$maximin_selected) != 1L ||
    candidate_summary[maximin_selected == TRUE, candidate_id] !=
      selected_candidate_id) {
  stop("Frozen-policy cross-evaluation is incomplete or inconsistent",
    call. = FALSE
  )
}

selected_cross_evaluation <- policy_cross_evaluation[
  candidate_id == selected_candidate_id
]
selected_cross_evaluation <- merge(
  selected_cross_evaluation,
  scenario_grid[, .(
    evaluation_scenario_id = scenario_id,
    offense_kappa,
    defense_kappa
  )],
  by = "evaluation_scenario_id",
  all.x = TRUE
)
setorder(selected_cross_evaluation, offense_kappa, defense_kappa)
if (nrow(selected_cross_evaluation) != 25L ||
    anyNA(selected_cross_evaluation[, .(offense_kappa, defense_kappa)])) {
  stop("Selected policy does not have all 25 development evaluations",
    call. = FALSE
  )
}

selected_candidate_summary <- one_row(
  candidate_summary[candidate_id == selected_candidate_id],
  "Selected candidate robustness summary"
)
candidate_parameters <- copy(scenario_grid[, .(
  candidate_id = scenario_id,
  candidate_offense_kappa = offense_kappa,
  candidate_defense_kappa = defense_kappa
)])
selected_candidate_parameters <- one_row(
  candidate_parameters[candidate_id == selected_candidate_id],
  "Selected candidate parameters"
)
binding_evaluation_parameters <- one_row(
  selected_cross_evaluation[
    expected_re_per_team_game == min(expected_re_per_team_game)
  ],
  "Binding evaluation parameters"
)
if (binding_evaluation_parameters$evaluation_scenario_id != final_scenario_id) {
  stop("The reporting scenario is not the selected policy's binding scenario")
}
candidate_summary_output <- merge(
  copy(candidate_summary),
  candidate_parameters,
  by = "candidate_id",
  all.x = TRUE
)
setcolorder(candidate_summary_output, c(
  "candidate_id", "candidate_offense_kappa", "candidate_defense_kappa",
  "worst_case_expected_re_per_team_game",
  "mean_expected_re_per_team_game",
  "best_case_expected_re_per_team_game", "maximin_selected"
))
setorder(
  candidate_summary_output,
  -worst_case_expected_re_per_team_game,
  -mean_expected_re_per_team_game
)
fwrite(
  selected_cross_evaluation[, .(
    candidate_id,
    candidate_offense_kappa =
      selected_candidate_parameters$candidate_offense_kappa,
    candidate_defense_kappa =
      selected_candidate_parameters$candidate_defense_kappa,
    evaluation_scenario_id,
    evaluation_offense_kappa = offense_kappa,
    evaluation_defense_kappa = defense_kappa,
    expected_re_per_team_game,
    total_expected_re
  )],
  file.path(output_dir, "direct_policy_scenario_cross_evaluation.csv")
)
fwrite(
  candidate_summary_output,
  file.path(output_dir, "direct_policy_candidate_summary.csv")
)

# -------------------------------------------------------------------------
# Figure 1: mechanism by inning
# -------------------------------------------------------------------------

inning_levels <- c(as.character(1:9), "10+")
opportunity_replay[, inning_group := fifelse(
  inning >= 10L, "10+", as.character(inning)
)]
policy_actions[, inning_group := fifelse(
  inning >= 10L, "10+", as.character(inning)
)]

expected_innings <- CJ(
  role = c("defense", "offense"),
  inning = inning_levels,
  unique = TRUE
)
required_inning_columns <- c(
  "role", "inning", "clock_opportunities", "observed_challenges",
  "observed_challenge_rate", "learned_expected_challenges",
  "learned_challenge_rate"
)
if (!all(required_inning_columns %chin% names(inning_policy_summary)) ||
    nrow(inning_policy_summary) != 20L ||
    uniqueN(inning_policy_summary, by = c("role", "inning")) != 20L ||
    nrow(inning_policy_summary[expected_innings,
      on = .(role, inning), nomatch = 0L
    ]) != 20L ||
    anyNA(inning_policy_summary[, ..required_inning_columns]) ||
    sum(inning_policy_summary$clock_opportunities) != nrow(opportunity_replay) ||
    sum(inning_policy_summary$observed_challenges) != observed$attempts ||
    abs(sum(inning_policy_summary$learned_expected_challenges) -
      learned$attempts) > 1e-8 ||
    any(abs(
      inning_policy_summary$observed_challenge_rate -
        inning_policy_summary$observed_challenges /
          inning_policy_summary$clock_opportunities
    ) > 1e-12) ||
    any(abs(
      inning_policy_summary$learned_challenge_rate -
        inning_policy_summary$learned_expected_challenges /
          inning_policy_summary$clock_opportunities
    ) > 1e-12)) {
  stop("The committed inning summary does not reconcile to the final run",
    call. = FALSE
  )
}
observed_role_totals <- inning_policy_summary[, .(
  attempts = sum(observed_challenges)
), by = role]
observed_role_reference <- summary_data[
  policy == "observed" & role %chin% c("offense", "defense") &
    scenario_id == final_scenario_id,
  .(role, attempts)
]
observed_role_check <- merge(
  observed_role_totals,
  observed_role_reference,
  by = "role",
  suffixes = c("_summary", "_reference")
)
if (nrow(observed_role_check) != 2L ||
    any(observed_role_check$attempts_summary !=
      observed_role_check$attempts_reference)) {
  stop("The inning summary does not reproduce observed attempts by role",
    call. = FALSE
  )
}
inning_policy_summary[, inning_index := match(inning, inning_levels)]
setorder(inning_policy_summary, role, inning_index)

replay_check <- opportunity_replay[, .(
  clock_opportunities = .N,
  learned_expected_challenges = sum(expected_challenges)
), by = .(role, inning = inning_group)]
summary_check <- merge(
  inning_policy_summary[, .(
    role, inning, clock_opportunities, learned_expected_challenges
  )],
  replay_check,
  by = c("role", "inning"),
  suffixes = c("_summary", "_replay")
)
if (nrow(summary_check) != 20L ||
    any(summary_check$clock_opportunities_summary !=
      summary_check$clock_opportunities_replay) ||
    any(abs(summary_check$learned_expected_challenges_summary -
      summary_check$learned_expected_challenges_replay) > 1e-8)) {
  stop("The inning summary is not aligned with the committed replay",
    call. = FALSE
  )
}

challenge_rates <- melt(
  inning_policy_summary,
  id.vars = c("role", "inning", "inning_index", "clock_opportunities"),
  measure.vars = c("observed_challenge_rate", "learned_challenge_rate"),
  variable.name = "policy",
  value.name = "challenge_rate"
)
challenge_rates[, policy := factor(
  policy,
  levels = c("observed_challenge_rate", "learned_challenge_rate"),
  labels = c("Observed", "Learned policy")
)]

inventory_values <- policy_actions[, .(
  C1 = mean(inventory_loss_k1),
  C2 = mean(inventory_loss_k1 + inventory_loss_k2),
  opportunities = .N
), by = inning_group]
inventory_values[, inning_index := match(inning_group, inning_levels)]
setorder(inventory_values, inning_index)
l2_example <- one_row(
  inventory_values[inning_index == 5L],
  "Figure 1 inventory-loss example"
)
inventory_long <- melt(
  inventory_values,
  id.vars = c("inning_group", "inning_index", "opportunities"),
  measure.vars = c("C1", "C2"),
  variable.name = "inventory",
  value.name = "continuation_value"
)
inventory_long[, inventory := factor(
  inventory,
  levels = c("C1", "C2"),
  labels = c("One challenge", "Two challenges")
)]

fwrite(
  inning_policy_summary[, .(
    role, inning, clock_opportunities, observed_challenges,
    observed_challenge_rate, learned_expected_challenges,
    learned_challenge_rate
  )],
  file.path(output_dir, "mechanism_challenge_rates.csv")
)
fwrite(
  inventory_values[, .(
    inning = inning_group,
    opportunities,
    one_challenge_value = C1,
    two_challenge_value = C2
  )],
  file.path(output_dir, "mechanism_inventory_values.csv")
)

role_opportunities <- opportunity_replay[, .(
  fixed_clock_opportunities = .N,
  geometry_correctable_calls = sum(geometry_success),
  geometry_correctable_rate = mean(geometry_success),
  mean_potential_gain_per_opportunity = mean(stake_G)
), by = role]
oracle_by_role <- rbindlist(list(oracle_offense, oracle_defense))[, .(
  role,
  positive_value_correctable_calls = attempts,
  exact_location_oracle_re = captured_re,
  exact_location_oracle_re_per_game = captured_re_per_game
)]
learned_gain_by_role <- rbindlist(list(
  learned_offense,
  learned_defense
))[, .(
  role,
  learned_gain_re = gain_over_observed_re,
  learned_gain_re_per_game = gain_over_observed_re_per_game
)]
role_opportunity_summary <- merge(
  role_opportunities,
  oracle_by_role,
  by = "role"
)
role_opportunity_summary <- merge(
  role_opportunity_summary,
  learned_gain_by_role,
  by = "role"
)
role_opportunity_summary[, positive_value_correctable_rate :=
  positive_value_correctable_calls / fixed_clock_opportunities]
role_opportunity_summary[, mean_re_per_positive_value_correctable_call :=
  exact_location_oracle_re / positive_value_correctable_calls]
setcolorder(role_opportunity_summary, c(
  "role",
  "fixed_clock_opportunities",
  "geometry_correctable_calls",
  "geometry_correctable_rate",
  "positive_value_correctable_calls",
  "positive_value_correctable_rate",
  "mean_potential_gain_per_opportunity",
  "mean_re_per_positive_value_correctable_call",
  "exact_location_oracle_re",
  "exact_location_oracle_re_per_game",
  "learned_gain_re",
  "learned_gain_re_per_game"
))
setorder(role_opportunity_summary, role)
fwrite(
  role_opportunity_summary,
  file.path(output_dir, "role_opportunity_summary.csv")
)

reading_palette <- unname(ltc::ltc("reading"))
if (length(reading_palette) != 8L) {
  stop("The ltc Reading palette must contain eight colors", call. = FALSE)
}
ltc_colors <- c(
  gold = reading_palette[[1L]],
  sage = reading_palette[[2L]],
  blush = reading_palette[[3L]],
  slate = reading_palette[[4L]],
  teal = reading_palette[[5L]],
  mint = reading_palette[[6L]],
  bluegray = reading_palette[[7L]],
  mist = reading_palette[[8L]]
)
secondary_color <- colorspace::darken(ltc_colors[["blush"]], amount = 0.18)
l2_color <- colorspace::darken(ltc_colors[["blush"]], amount = 0.28)

role_colors <- c(
  offense = ltc_colors[["slate"]],
  defense = secondary_color
)
policy_linetypes <- c("Observed" = "22", "Learned policy" = "solid")
policy_shapes <- c("Observed" = 1, "Learned policy" = 16)
inventory_colors <- c(
  "One challenge" = ltc_colors[["teal"]],
  "Two challenges" = ltc_colors[["sage"]]
)
inventory_shapes <- c("One challenge" = 16, "Two challenges" = 17)

paper_theme <- theme_minimal(base_size = 9, base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      face = "bold", size = 9.5, color = "black"
    ),
    plot.subtitle = element_text(size = 8, color = "black"),
    axis.title = element_text(size = 8.5, color = "black"),
    axis.text = element_text(size = 8, color = "black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, color = "black"),
    legend.position = "bottom",
    legend.margin = margin(t = -2, unit = "pt"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = scales::alpha(ltc_colors[["mist"]], 0.72),
      linewidth = 0.3
    ),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(4, 6, 3, 4, unit = "pt")
  )

rate_plot <- ggplot(
  challenge_rates,
  aes(
    x = inning_index,
    y = challenge_rate,
    color = role,
    linetype = policy,
    shape = policy,
    group = interaction(role, policy)
  )
) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.8, stroke = 0.45) +
  scale_color_manual(
    values = role_colors,
    labels = c(offense = "Offense", defense = "Defense")
  ) +
  scale_linetype_manual(values = policy_linetypes) +
  scale_shape_manual(values = policy_shapes) +
  scale_x_continuous(
    breaks = seq_along(inning_levels),
    labels = inning_levels,
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.21),
    breaks = seq(0, 0.20, 0.05),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "A. The policy becomes more aggressive late",
    subtitle = "Same held-out tracked called-pitch opportunity clock",
    x = "Inning",
    y = "Challenge rate"
  ) +
  paper_theme +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  theme(
    legend.box = "vertical",
    legend.box.just = "left",
    legend.spacing.y = unit(-3, "pt")
  )

inventory_plot <- ggplot(
  inventory_long,
  aes(
    x = inning_index,
    y = continuation_value,
    color = inventory,
    shape = inventory,
    group = inventory
  )
) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 2.0, stroke = 0.3) +
  annotate(
    "segment",
    x = 5, xend = 5,
    y = l2_example$C1, yend = l2_example$C2,
    color = l2_color, linewidth = 0.85
  ) +
  annotate(
    "segment",
    x = 4.88, xend = 5.12,
    y = l2_example$C1, yend = l2_example$C1,
    color = l2_color, linewidth = 0.85
  ) +
  annotate(
    "segment",
    x = 4.88, xend = 5.12,
    y = l2_example$C2, yend = l2_example$C2,
    color = l2_color, linewidth = 0.85
  ) +
  annotate(
    "text",
    x = 5.22,
    y = (l2_example$C1 + l2_example$C2) / 2,
    label = "L[2]", parse = TRUE,
    hjust = 0, size = 3.1, color = l2_color
  ) +
  scale_color_manual(values = inventory_colors) +
  scale_shape_manual(values = inventory_shapes) +
  scale_x_continuous(
    breaks = seq_along(inning_levels),
    labels = inning_levels,
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 0.01),
    limits = c(0, 0.24),
    breaks = seq(0, 0.24, 0.06),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "B. Inventory becomes cheaper to risk",
    subtitle = "Mean continuation value on held-out opportunity states",
    x = "Inning",
    y = "Inventory continuation value (RE)"
  ) +
  paper_theme

mechanism_pdf <- file.path(output_dir, "mechanism_by_inning.pdf")
grDevices::pdf(
  mechanism_pdf,
  width = 7.1,
  height = 3.35,
  family = "Helvetica",
  useDingbats = FALSE
)
grid::grid.newpage()
layout <- grid::grid.layout(
  nrow = 1,
  ncol = 2,
  widths = grid::unit(c(1.03, 1), "null")
)
grid::pushViewport(grid::viewport(layout = layout))
print(rate_plot, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(
  inventory_plot,
  vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2)
)
grid::popViewport()
grDevices::dev.off()

# -------------------------------------------------------------------------
# Online-supplement figures: the same Reading palette and visual grammar
# -------------------------------------------------------------------------

supplement_theme <- theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      face = "bold", size = 14, color = "black"
    ),
    plot.subtitle = element_text(size = 10.5, color = "black"),
    axis.title = element_text(size = 11, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    strip.text = element_text(
      face = "bold", size = 11, color = "black"
    ),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = "black"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = scales::alpha(ltc_colors[["mist"]], 0.68),
      linewidth = 0.35
    ),
    plot.margin = margin(8, 10, 7, 8, unit = "pt")
  )

observed_inning_plot <- ggplot(
  challenge_rates[policy == "Observed"],
  aes(
    x = inning_index,
    y = challenge_rate,
    color = role,
    shape = role,
    group = role
  )
) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.7, stroke = 0.4) +
  scale_color_manual(
    values = role_colors,
    labels = c(offense = "Offense", defense = "Defense")
  ) +
  scale_shape_manual(
    values = c(offense = 16, defense = 17),
    labels = c(offense = "Offense", defense = "Defense")
  ) +
  scale_x_continuous(
    breaks = seq_along(inning_levels),
    labels = inning_levels,
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, 0.21),
    breaks = seq(0, 0.20, 0.05),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title = "Observed challenge rate by inning",
    subtitle = "Held-out tracked called-pitch opportunity clock",
    x = "Inning",
    y = "Challenge rate"
  ) +
  supplement_theme +
  theme(panel.grid.major.x = element_blank())

ggsave(
  filename = file.path(supplement_dir, "abs_challenge_rate_by_inning.png"),
  plot = observed_inning_plot,
  width = 8.5,
  height = 5.5,
  units = "in",
  dpi = 240,
  bg = "white"
)

challenge_events_path <- file.path(
  project_root, "data", "analysis", "challenge_events.parquet"
)
if (file.exists(challenge_events_path)) {
  challenge_events <- as.data.table(read_parquet(challenge_events_path))
  challenge_locations <- challenge_events[, .(
    role = factor(
      challenger_role,
      levels = c("batter", "catcher", "pitcher"),
      labels = c("Batter", "Catcher", "Pitcher")
    ),
    outcome = factor(
      fifelse(is_overturned, "Overturned", "Upheld"),
      levels = c("Overturned", "Upheld")
    ),
    x_inches = fifelse(!is.na(sc_plate_x), sc_plate_x, feed_plate_x) * 12,
    z_normalized = (
      fifelse(!is.na(sc_plate_z), sc_plate_z, feed_plate_z) -
        fifelse(!is.na(sc_sz_bot), sc_sz_bot, feed_sz_bot)
    ) / (
      fifelse(!is.na(sc_sz_top), sc_sz_top, feed_sz_top) -
        fifelse(!is.na(sc_sz_bot), sc_sz_bot, feed_sz_bot)
    ) * 20
  )]
  challenge_locations <- challenge_locations[
    !is.na(role) & is.finite(x_inches) & is.finite(z_normalized)
  ]

  location_plot <- ggplot(
    challenge_locations,
    aes(x = x_inches, y = z_normalized, color = outcome, shape = outcome)
  ) +
    annotate(
      "rect",
      xmin = -8.5, xmax = 8.5, ymin = 0, ymax = 20,
      fill = NA, color = ltc_colors[["slate"]], linewidth = 0.7
    ) +
    geom_point(alpha = 0.42, size = 1.25, stroke = 0.35) +
    facet_wrap(~role, nrow = 1) +
    scale_color_manual(values = c(
      Overturned = ltc_colors[["slate"]],
      Upheld = secondary_color
    )) +
    scale_shape_manual(values = c(Overturned = 1, Upheld = 2)) +
    coord_cartesian(xlim = c(-18, 18), ylim = c(-8, 29)) +
    scale_x_continuous(breaks = seq(-15, 15, 5)) +
    scale_y_continuous(breaks = seq(-5, 25, 5)) +
    labs(
      title = "Observed challenge locations by role",
      subtitle = "Strike-zone rectangle shown in role-oriented normalized coordinates",
      x = "Horizontal location (inches from plate center)",
      y = "Normalized vertical location"
    ) +
    supplement_theme

  ggsave(
    filename = file.path(
      supplement_dir, "abs_challenge_locations_by_role.png"
    ),
    plot = location_plot,
    width = 12,
    height = 5.6,
    units = "in",
    dpi = 240,
    bg = "white"
  )
}

# -------------------------------------------------------------------------
# Figure 2: all-team paired gain with whole-game cluster intervals
# -------------------------------------------------------------------------

team_data <- team_game_values[
  policy == "direct_learning_procedure" & role == "combined"
]
if (nrow(team_data) != 2L * learned$games ||
    uniqueN(team_data$game_pk) != learned$games ||
    uniqueN(team_data$team_id) != 30L) {
  stop("Unexpected team-game layout for the learned policy", call. = FALSE)
}
team_data[, gain_re := captured_re - observed_re]

game_ids <- sort(unique(team_data$game_pk))
team_ids <- sort(unique(team_data$team_id))
gain_matrix <- matrix(
  0,
  nrow = length(game_ids),
  ncol = length(team_ids),
  dimnames = list(as.character(game_ids), as.character(team_ids))
)
game_matrix <- gain_matrix
matrix_index <- cbind(
  match(team_data$game_pk, game_ids),
  match(team_data$team_id, team_ids)
)
gain_matrix[matrix_index] <- team_data$gain_re
game_matrix[matrix_index] <- team_data$team_games

bootstrap_replicates <- 5000L
set.seed(20260904L)
game_weights <- rmultinom(
  n = bootstrap_replicates,
  size = length(game_ids),
  prob = rep(1 / length(game_ids), length(game_ids))
)
bootstrap_numerator <- crossprod(game_weights, gain_matrix)
bootstrap_denominator <- crossprod(game_weights, game_matrix)
bootstrap_estimates <- bootstrap_numerator / bootstrap_denominator

team_intervals <- data.table(
  team_id = team_ids,
  games = as.numeric(colSums(game_matrix)),
  gain_re_per_team_game = as.numeric(
    colSums(gain_matrix) / colSums(game_matrix)
  ),
  lower_95 = apply(
    bootstrap_estimates,
    2L,
    quantile,
    probs = 0.025,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  ),
  upper_95 = apply(
    bootstrap_estimates,
    2L,
    quantile,
    probs = 0.975,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
)

team_abbreviations <- c(
  `108` = "LAA", `109` = "ARI", `110` = "BAL", `111` = "BOS",
  `112` = "CHC", `113` = "CIN", `114` = "CLE", `115` = "COL",
  `116` = "DET", `117` = "HOU", `118` = "KC",  `119` = "LAD",
  `120` = "WSH", `121` = "NYM", `133` = "ATH", `134` = "PIT",
  `135` = "SD",  `136` = "SEA", `137` = "SF",  `138` = "STL",
  `139` = "TB",  `140` = "TEX", `141` = "TOR", `142` = "MIN",
  `143` = "PHI", `144` = "ATL", `145` = "CWS", `146` = "MIA",
  `147` = "NYY", `158` = "MIL"
)
team_intervals[, team := unname(team_abbreviations[as.character(team_id)])]
if (anyNA(team_intervals$team)) {
  stop("Team abbreviation lookup is incomplete", call. = FALSE)
}
setorder(team_intervals, gain_re_per_team_game)
team_intervals[, team := factor(team, levels = team)]
team_intervals[, estimate_sign := fifelse(
  gain_re_per_team_game >= 0, "Nonnegative estimate", "Negative estimate"
)]
team_full_names <- c(
  LAA = "Los Angeles Angels", ARI = "Arizona", BAL = "Baltimore",
  BOS = "Boston", CHC = "Chicago Cubs", CIN = "Cincinnati",
  CLE = "Cleveland", COL = "Colorado", DET = "Detroit", HOU = "Houston",
  KC = "Kansas City", LAD = "Los Angeles Dodgers", WSH = "Washington",
  NYM = "New York Mets", ATH = "Athletics", PIT = "Pittsburgh",
  SD = "San Diego", SEA = "Seattle", SF = "San Francisco",
  STL = "St. Louis", TB = "Tampa Bay", TEX = "Texas", TOR = "Toronto",
  MIN = "Minnesota", PHI = "Philadelphia", ATL = "Atlanta",
  CWS = "Chicago White Sox", MIA = "Miami", NYY = "New York Yankees",
  MIL = "Milwaukee"
)
top_team <- team_intervals[which.max(gain_re_per_team_game)]
bottom_team <- team_intervals[which.min(gain_re_per_team_game)]
top_team_name <- unname(team_full_names[as.character(top_team$team)])
bottom_team_name <- unname(team_full_names[as.character(bottom_team$team)])
if (is.na(top_team_name) || is.na(bottom_team_name)) {
  stop("Full team-name lookup is incomplete")
}

fwrite(
  team_intervals[, .(
    team_id,
    team = as.character(team),
    games,
    gain_re_per_team_game,
    lower_95,
    upper_95,
    bootstrap_replicates = bootstrap_replicates,
    bootstrap_seed = 20260904L,
    bootstrap_unit = "whole_game",
    policy_refit = FALSE,
    interval_note = paste0(
      format(bootstrap_replicates, big.mark = ","),
      " whole-game resamples; learned policy held fixed"
    )
  )][order(-gain_re_per_team_game)],
  file.path(output_dir, "team_policy_gain.csv")
)

team_plot <- ggplot(team_intervals, aes(y = team)) +
  geom_vline(
    xintercept = 0,
    color = ltc_colors[["slate"]],
    linewidth = 0.5
  ) +
  geom_segment(
    aes(x = lower_95, xend = upper_95, yend = team),
    color = scales::alpha(ltc_colors[["bluegray"]], 0.72),
    linewidth = 0.55
  ) +
  geom_point(
    aes(
      x = gain_re_per_team_game,
      color = estimate_sign,
      shape = estimate_sign
    ),
    size = 2.0
  ) +
  scale_color_manual(
    values = c(
      "Nonnegative estimate" = ltc_colors[["slate"]],
      "Negative estimate" = secondary_color
    ),
    guide = "none"
  ) +
  scale_shape_manual(
    values = c(
      "Nonnegative estimate" = 16,
      "Negative estimate" = 17
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = label_number(accuracy = 0.05),
    breaks = seq(-0.10, 0.20, 0.05),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  labs(
    title = "Estimated policy gains vary across teams",
    x = "Gain over observed decisions (RE per team-game)",
    y = NULL
  ) +
  paper_theme +
  theme(
    legend.position = "none",
    axis.text.y = element_text(
      size = 8, color = "black", face = "bold"
    ),
    panel.grid.major.x = element_line(
      color = scales::alpha(ltc_colors[["mist"]], 0.72),
      linewidth = 0.3
    ),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(5, 8, 5, 5, unit = "pt")
  )

ggsave(
  filename = file.path(output_dir, "team_policy_gain.pdf"),
  plot = team_plot,
  device = grDevices::pdf,
  width = 6.8,
  height = 5.6,
  units = "in",
  family = "Helvetica",
  useDingbats = FALSE
)

# -------------------------------------------------------------------------
# Generated results table and headline macros
# -------------------------------------------------------------------------

results_numeric <- data.table(
  policy = c(
    "Observed decisions",
    "Learned policy",
    "Bellman check",
    "Exact-location oracle"
  ),
  captured_re_per_game = c(
    observed$captured_re_per_game,
    learned$captured_re_per_game,
    bellman$captured_re_per_game,
    oracle$captured_re_per_game
  ),
  gain_re_per_game = c(
    observed$gain_over_observed_re_per_game,
    learned$gain_over_observed_re_per_game,
    bellman$gain_over_observed_re_per_game,
    oracle$gain_over_observed_re_per_game
  ),
  gain_lower_95_per_game = c(
    NA_real_,
    learned_interval$gain_lower_95 / learned$games,
    bellman_interval$gain_lower_95 / bellman$games,
    oracle_interval$gain_lower_95 / oracle$games
  ),
  gain_upper_95_per_game = c(
    NA_real_,
    learned_interval$gain_upper_95 / learned$games,
    bellman_interval$gain_upper_95 / bellman$games,
    oracle_interval$gain_upper_95 / oracle$games
  ),
  success_rate = c(
    observed$success_rate,
    learned$success_rate,
    bellman$success_rate,
    oracle$success_rate
  ),
  oracle_share = c(
    observed$share_of_oracle,
    learned$share_of_oracle,
    bellman$share_of_oracle,
    oracle$share_of_oracle
  ),
  bootstrap_replicates = c(
    NA_integer_,
    learned_interval$bootstrap_replicates,
    bellman_interval$bootstrap_replicates,
    oracle_interval$bootstrap_replicates
  )
)
fwrite(results_numeric, file.path(output_dir, "results_table.csv"))

fmt3 <- function(x) formatC(x, format = "f", digits = 3)
fmt1_pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "\\%")
fmt_int <- function(x) formatC(
  round(x), format = "f", digits = 0, big.mark = "{,}"
)
gain_cell <- function(point, lower, upper) {
  if (is.na(lower) || is.na(upper)) return("--")
  paste0(fmt3(point), " [", fmt3(lower), ", ", fmt3(upper), "]")
}

results_tex_rows <- vapply(seq_len(nrow(results_numeric)), function(index) {
  row <- results_numeric[index]
  paste0(
    row$policy,
    " & ", fmt3(row$captured_re_per_game),
    " & ", gain_cell(
      row$gain_re_per_game,
      row$gain_lower_95_per_game,
      row$gain_upper_95_per_game
    ),
    " & ", fmt1_pct(row$success_rate),
    " & ", fmt1_pct(row$oracle_share),
    " \\\\"
  )
}, character(1L))

results_table_tex <- c(
  "% Generated by paper/build_figures.R; do not edit by hand.",
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Held-out performance on the factual opportunity clock}",
  "\\label{tab:main-results}",
  "\\small",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  paste0(
    "Policy & RE/game & Gain vs. observed [95\\% CI] & ",
    "\\shortstack{Model-implied\\\\overturn rate} & Oracle share \\\\"
  ),
  "\\midrule",
  results_tex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\vspace{0.4em}",
  "\\begin{minipage}{0.96\\linewidth}",
  paste0(
    "\\footnotesize\\itshape Notes: Values are modeled run expectancy, not realized runs. ",
    "Brackets are paired intervals from ",
    format(learned_interval$bootstrap_replicates, big.mark = "{,}"),
    " whole-game resamples of the held-out evaluation set, holding fitted ",
    "policies fixed."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(results_table_tex, file.path(output_dir, "results_table.tex"))

relative_gain <- learned$gain_over_observed_re / observed$captured_re
attempt_increase <- (learned$attempts - observed$attempts) / observed$attempts
offense_gain_share <- learned_offense$gain_over_observed_re /
  learned$gain_over_observed_re
role_offense <- one_row(
  role_opportunity_summary[role == "offense"],
  "Offense opportunity summary"
)
role_defense <- one_row(
  role_opportunity_summary[role == "defense"],
  "Defense opportunity summary"
)

macro_lines <- c(
  "% Generated by paper/build_figures.R; do not edit by hand.",
  sprintf("\\newcommand{\\ObservedREPerGame}{%s}", fmt3(observed$captured_re_per_game)),
  sprintf("\\newcommand{\\LearnedREPerGame}{%s}", fmt3(learned$captured_re_per_game)),
  sprintf("\\newcommand{\\GainREPerGame}{%s}", fmt3(learned$gain_over_observed_re_per_game)),
  sprintf("\\newcommand{\\GainLowerPerGame}{%s}", fmt3(learned_interval$gain_lower_95 / learned$games)),
  sprintf("\\newcommand{\\GainUpperPerGame}{%s}", fmt3(learned_interval$gain_upper_95 / learned$games)),
  sprintf("\\newcommand{\\ObservedSuccessRate}{%s}", fmt1_pct(observed$success_rate)),
  sprintf("\\newcommand{\\LearnedSuccessRate}{%s}", fmt1_pct(learned$success_rate)),
  sprintf("\\newcommand{\\RelativeGainPercent}{%s}", fmt1_pct(relative_gain)),
  sprintf("\\newcommand{\\ObservedAttempts}{%s}", format(observed$attempts, big.mark = "{,}", scientific = FALSE)),
  sprintf("\\newcommand{\\LearnedAttempts}{%s}", formatC(learned$attempts, format = "f", digits = 1, big.mark = "{,}")),
  sprintf("\\newcommand{\\LearnedExpectedSuccesses}{%s}", fmt_int(learned$successes)),
  sprintf("\\newcommand{\\AttemptIncreasePercent}{%s}", fmt1_pct(attempt_increase)),
  sprintf("\\newcommand{\\OffenseGainPerGame}{%s}", fmt3(learned_offense$gain_over_observed_re_per_game)),
  sprintf("\\newcommand{\\DefenseGainPerGame}{%s}", fmt3(learned_defense$gain_over_observed_re_per_game)),
  sprintf("\\newcommand{\\OffenseGainShare}{%s}", fmt1_pct(offense_gain_share)),
  sprintf("\\newcommand{\\BellmanREPerGame}{%s}", fmt3(bellman$captured_re_per_game)),
  sprintf("\\newcommand{\\OracleREPerGame}{%s}", fmt3(oracle$captured_re_per_game)),
  sprintf("\\newcommand{\\PolicyOracleShare}{%s}", fmt1_pct(learned$share_of_oracle)),
  sprintf("\\newcommand{\\PublicInformationREPerGame}{%s}", fmt3(public_information$captured_re_per_game)),
  sprintf("\\newcommand{\\FittedHumanREPerGame}{%s}", fmt3(fitted_human$captured_re_per_game)),
  sprintf("\\newcommand{\\ObservedCapturedRETotal}{%s}", fmt3(observed$captured_re)),
  sprintf("\\newcommand{\\RawGames}{%s}", fmt_int(sample_value("raw_games"))),
  sprintf("\\newcommand{\\RawCalledPitches}{%s}", fmt_int(sample_value("raw_called_pitches"))),
  sprintf("\\newcommand{\\RawTrackedPitches}{%s}", fmt_int(sample_value("raw_tracked_pitches"))),
  sprintf("\\newcommand{\\DevelopmentTrainingGames}{%s}", fmt_int(development_training_games)),
  sprintf("\\newcommand{\\DevelopmentGames}{%s}", fmt_int(sample_value("development_games"))),
  sprintf("\\newcommand{\\DevelopmentCalledPitches}{%s}", fmt_int(sample_value("development_called_pitches"))),
  sprintf("\\newcommand{\\DevelopmentTrackedPitches}{%s}", fmt_int(sample_value("development_tracked_pitches"))),
  sprintf("\\newcommand{\\DevelopmentChallenges}{%s}", fmt_int(sample_value("development_observed_challenges"))),
  sprintf("\\newcommand{\\ConfirmationGames}{%s}", fmt_int(sample_value("confirmation_games"))),
  sprintf("\\newcommand{\\ConfirmationCalledPitches}{%s}", fmt_int(sample_value("confirmation_called_pitches"))),
  sprintf("\\newcommand{\\ConfirmationTrackedPitches}{%s}", fmt_int(sample_value("confirmation_tracked_pitches"))),
  sprintf("\\newcommand{\\ConfirmationChallenges}{%s}", fmt_int(sample_value("confirmation_observed_challenges"))),
  sprintf("\\newcommand{\\ConfirmationOverturned}{%s}", fmt_int(sample_value("confirmation_overturned"))),
  sprintf("\\newcommand{\\ConfirmationUpheld}{%s}", fmt_int(sample_value("confirmation_upheld"))),
  sprintf("\\newcommand{\\TrackedOfficialRulings}{%s}", fmt_int(sample_value("tracked_official_challenge_rulings"))),
  sprintf("\\newcommand{\\GeometryCorrectableCalls}{%s}", fmt_int(sample_value("confirmation_geometry_correctable_calls"))),
  sprintf("\\newcommand{\\PositiveValueOracleActions}{%s}", fmt_int(sample_value("confirmation_positive_value_oracle_actions"))),
  sprintf("\\newcommand{\\ZeroValueCorrectableCalls}{%s}", fmt_int(sample_value("confirmation_zero_value_correctable_calls"))),
  sprintf("\\newcommand{\\OffenseEffectiveWidth}{%s}", fmt3(width_for("offense"))),
  sprintf("\\newcommand{\\DefenseEffectiveWidth}{%s}", fmt3(width_for("defense"))),
  sprintf("\\newcommand{\\EmpiricalPriorAlpha}{%s}", fmt_int(selected_alpha_value)),
  sprintf("\\newcommand{\\ConditionalBootstrapReplicates}{%s}", fmt_int(learned_interval$bootstrap_replicates)),
  sprintf("\\newcommand{\\TopTeamName}{%s}", top_team_name),
  sprintf("\\newcommand{\\BottomTeamName}{%s}", bottom_team_name),
  sprintf("\\newcommand{\\TeamMinimumGames}{%s}", fmt_int(min(team_intervals$games))),
  sprintf("\\newcommand{\\TeamMaximumGames}{%s}", fmt_int(max(team_intervals$games))),
  sprintf("\\newcommand{\\DevelopmentScenarioCount}{%d}", nrow(selected_cross_evaluation)),
  sprintf(
    "\\newcommand{\\DevelopmentWorstCaseREPerTeamGame}{%s}",
    fmt3(selected_candidate_summary$worst_case_expected_re_per_team_game)
  ),
  sprintf(
    "\\newcommand{\\DevelopmentBestCaseREPerTeamGame}{%s}",
    fmt3(selected_candidate_summary$best_case_expected_re_per_team_game)
  ),
  sprintf(
    "\\newcommand{\\DevelopmentMeanREPerTeamGame}{%s}",
    fmt3(selected_candidate_summary$mean_expected_re_per_team_game)
  ),
  sprintf(
    "\\newcommand{\\SelectedOffenseKappa}{%.2f}",
    selected_candidate_parameters$candidate_offense_kappa
  ),
  sprintf(
    "\\newcommand{\\SelectedDefenseKappa}{%.2f}",
    selected_candidate_parameters$candidate_defense_kappa
  ),
  sprintf(
    "\\newcommand{\\BindingOffenseKappa}{%.2f}",
    binding_evaluation_parameters$offense_kappa
  ),
  sprintf(
    "\\newcommand{\\BindingDefenseKappa}{%.2f}",
    binding_evaluation_parameters$defense_kappa
  ),
  sprintf(
    "\\newcommand{\\OffenseClockOpportunities}{%s}",
    format(role_offense$fixed_clock_opportunities,
      big.mark = "{,}", scientific = FALSE
    )
  ),
  sprintf(
    "\\newcommand{\\DefenseClockOpportunities}{%s}",
    format(role_defense$fixed_clock_opportunities,
      big.mark = "{,}", scientific = FALSE
    )
  ),
  sprintf(
    "\\newcommand{\\OffensePositiveCorrectableCalls}{%s}",
    format(role_offense$positive_value_correctable_calls,
      big.mark = "{,}", scientific = FALSE
    )
  ),
  sprintf(
    "\\newcommand{\\DefensePositiveCorrectableCalls}{%s}",
    format(role_defense$positive_value_correctable_calls,
      big.mark = "{,}", scientific = FALSE
    )
  ),
  sprintf(
    "\\newcommand{\\OffensePositiveCorrectableRate}{%s}",
    fmt1_pct(role_offense$positive_value_correctable_rate)
  ),
  sprintf(
    "\\newcommand{\\DefensePositiveCorrectableRate}{%s}",
    fmt1_pct(role_defense$positive_value_correctable_rate)
  ),
  sprintf(
    "\\newcommand{\\OffenseMeanCorrectableStake}{%s}",
    fmt3(role_offense$mean_re_per_positive_value_correctable_call)
  ),
  sprintf(
    "\\newcommand{\\DefenseMeanCorrectableStake}{%s}",
    fmt3(role_defense$mean_re_per_positive_value_correctable_call)
  ),
  sprintf(
    "\\newcommand{\\OffenseOracleREPerGame}{%s}",
    fmt3(role_offense$exact_location_oracle_re_per_game)
  ),
  sprintf(
    "\\newcommand{\\DefenseOracleREPerGame}{%s}",
    fmt3(role_defense$exact_location_oracle_re_per_game)
  ),
  sprintf(
    "\\newcommand{\\TeamBootstrapReplicates}{%s}",
    format(bootstrap_replicates, big.mark = "{,}", scientific = FALSE)
  ),
  paste0(
    "\\newcommand{\\TeamCIMethod}{",
    format(bootstrap_replicates, big.mark = "{,}", scientific = FALSE),
    " whole-game resamples with the learned policy held fixed}"
  )
)
writeLines(macro_lines, file.path(output_dir, "headline_macros.tex"))

headline_numeric <- data.table(
  metric = c(
    "observed_re_per_game", "learned_re_per_game", "gain_re_per_game",
    "gain_lower_95_per_game", "gain_upper_95_per_game",
    "observed_success_rate", "learned_success_rate", "relative_gain",
    "observed_attempts", "learned_expected_attempts", "attempt_increase",
    "offense_gain_per_game", "defense_gain_per_game", "offense_gain_share",
    "bellman_re_per_game", "oracle_re_per_game", "policy_oracle_share",
    "development_scenario_count",
    "development_worst_case_re_per_team_game",
    "development_best_case_re_per_team_game",
    "development_mean_re_per_team_game",
    "selected_offense_kappa", "selected_defense_kappa",
    "offense_clock_opportunities", "defense_clock_opportunities",
    "offense_positive_correctable_calls",
    "defense_positive_correctable_calls",
    "offense_positive_correctable_rate",
    "defense_positive_correctable_rate",
    "offense_mean_correctable_stake", "defense_mean_correctable_stake",
    "offense_oracle_re_per_game", "defense_oracle_re_per_game",
    "team_bootstrap_replicates"
  ),
  value = c(
    observed$captured_re_per_game,
    learned$captured_re_per_game,
    learned$gain_over_observed_re_per_game,
    learned_interval$gain_lower_95 / learned$games,
    learned_interval$gain_upper_95 / learned$games,
    observed$success_rate,
    learned$success_rate,
    relative_gain,
    observed$attempts,
    learned$attempts,
    attempt_increase,
    learned_offense$gain_over_observed_re_per_game,
    learned_defense$gain_over_observed_re_per_game,
    offense_gain_share,
    bellman$captured_re_per_game,
    oracle$captured_re_per_game,
    learned$share_of_oracle,
    nrow(selected_cross_evaluation),
    selected_candidate_summary$worst_case_expected_re_per_team_game,
    selected_candidate_summary$best_case_expected_re_per_team_game,
    selected_candidate_summary$mean_expected_re_per_team_game,
    selected_candidate_parameters$candidate_offense_kappa,
    selected_candidate_parameters$candidate_defense_kappa,
    role_offense$fixed_clock_opportunities,
    role_defense$fixed_clock_opportunities,
    role_offense$positive_value_correctable_calls,
    role_defense$positive_value_correctable_calls,
    role_offense$positive_value_correctable_rate,
    role_defense$positive_value_correctable_rate,
    role_offense$mean_re_per_positive_value_correctable_call,
    role_defense$mean_re_per_positive_value_correctable_call,
    role_offense$exact_location_oracle_re_per_game,
    role_defense$exact_location_oracle_re_per_game,
    bootstrap_replicates
  )
)
headline_numeric <- rbind(
  headline_numeric,
  data.table(
    metric = c(
      "public_information_re_per_game",
      "fitted_human_re_per_game",
      "observed_captured_re_total",
      "learned_expected_successes",
      "raw_games",
      "raw_called_pitches",
      "raw_tracked_pitches",
      "reporting_development_games",
      "reporting_development_called_pitches",
      "reporting_development_tracked_pitches",
      "reporting_development_challenges",
      "reporting_confirmation_games",
      "reporting_confirmation_called_pitches",
      "reporting_confirmation_tracked_pitches",
      "reporting_confirmation_challenges",
      "reporting_confirmation_overturned",
      "reporting_confirmation_upheld",
      "tracked_official_challenge_rulings",
      "confirmation_geometry_correctable_calls",
      "confirmation_positive_value_oracle_actions",
      "confirmation_zero_value_correctable_calls",
      "offense_effective_width_inches",
      "defense_effective_width_inches",
      "empirical_prior_alpha",
      "conditional_bootstrap_replicates",
      "development_training_games",
      "team_minimum_games",
      "team_maximum_games",
      "binding_offense_kappa",
      "binding_defense_kappa"
    ),
    value = c(
      public_information$captured_re_per_game,
      fitted_human$captured_re_per_game,
      observed$captured_re,
      learned$successes,
      sample_value("raw_games"),
      sample_value("raw_called_pitches"),
      sample_value("raw_tracked_pitches"),
      sample_value("development_games"),
      sample_value("development_called_pitches"),
      sample_value("development_tracked_pitches"),
      sample_value("development_observed_challenges"),
      sample_value("confirmation_games"),
      sample_value("confirmation_called_pitches"),
      sample_value("confirmation_tracked_pitches"),
      sample_value("confirmation_observed_challenges"),
      sample_value("confirmation_overturned"),
      sample_value("confirmation_upheld"),
      sample_value("tracked_official_challenge_rulings"),
      sample_value("confirmation_geometry_correctable_calls"),
      sample_value("confirmation_positive_value_oracle_actions"),
      sample_value("confirmation_zero_value_correctable_calls"),
      width_for("offense"),
      width_for("defense"),
      selected_alpha_value,
      learned_interval$bootstrap_replicates,
      development_training_games,
      min(team_intervals$games),
      max(team_intervals$games),
      binding_evaluation_parameters$offense_kappa,
      binding_evaluation_parameters$defense_kappa
    )
  )
)
fwrite(headline_numeric, file.path(output_dir, "headline_values.csv"))

# -------------------------------------------------------------------------
# Generated case-study table. Identifiers choose the two examples discussed
# in the manuscript; every numerical entry comes from the committed bundle.
# -------------------------------------------------------------------------

case_keys <- data.table(
  game_pk = c("823916", "824489"),
  team_id = c("118", "113"),
  pitch_order = c(10L, 9L),
  case = c(
    "Challenge (KC at LAD)",
    "Wait (CLE at CIN)"
  )
)
case_actions <- merge(
  case_keys,
  policy_actions,
  by = c("game_pk", "team_id", "pitch_order"),
  all.x = TRUE
)
case_values <- merge(
  case_actions,
  opportunity_replay[, .(
    game_pk,
    team_id,
    pitch_order,
    role_margin_inches,
    probability_k2
  )],
  by = c("game_pk", "team_id", "pitch_order"),
  all.x = TRUE
)
if (nrow(case_values) != 2L ||
    anyNA(case_values[, .(
      inning,
      stage,
      role,
      count_state,
      stake_G,
      inventory_loss_k2,
      q_star_k2,
      signal_threshold_k2_inches,
      role_margin_inches,
      probability_k2
    )])) {
  stop("Case-study identifiers did not resolve to two complete rows",
    call. = FALSE
  )
}
case_values[, case_order := match(game_pk, case_keys$game_pk)]
setorder(case_values, case_order)
case_values[, `:=`(
  half_label = fifelse(stage %% 6L >= 3L, "B", "T"),
  outs_before = stage %% 3L
)]
case_values[, state := sprintf(
  "%s%d, %d out%s, %s",
  half_label,
  inning,
  outs_before,
  fifelse(outs_before == 1L, "", "s"),
  count_state
)]

case_macro_lines <- c(
  "% Generated by paper/build_figures.R; do not edit by hand.",
  sprintf(
    "\\newcommand{\\ChallengeCasePosteriorThreshold}{%s}",
    fmt1_pct(case_values[case_order == 1L, q_star_k2])
  ),
  sprintf(
    "\\newcommand{\\WaitCasePosteriorThreshold}{%s}",
    fmt1_pct(case_values[case_order == 2L, q_star_k2])
  ),
  sprintf(
    "\\newcommand{\\ChallengeCaseActionProbability}{%s}",
    fmt1_pct(case_values[case_order == 1L, probability_k2])
  ),
  sprintf(
    "\\newcommand{\\WaitCaseActionProbability}{%s}",
    fmt1_pct(case_values[case_order == 2L, probability_k2])
  )
)
writeLines(case_macro_lines, file.path(output_dir, "case_macros.tex"))

fwrite(
  case_values[, .(
    case,
    game_pk,
    pitch_order,
    state,
    role,
    margin_inches = role_margin_inches,
    gain_re = stake_G,
    loss_if_second_inventory_is_spent = inventory_loss_k2,
    posterior_threshold = q_star_k2,
    signal_threshold_inches = signal_threshold_k2_inches,
    conditional_action_probability = probability_k2
  )],
  file.path(output_dir, "case_studies_table.csv")
)

case_tex_rows <- vapply(seq_len(nrow(case_values)), function(index) {
  row <- case_values[index]
  paste0(
    row$case,
    " & ", row$state,
    " & ", tools::toTitleCase(row$role),
    " & ", fmt3(row$stake_G),
    " & ", fmt3(row$inventory_loss_k2),
    " & ", fmt3(row$q_star_k2),
    " & ", fmt3(row$probability_k2),
    " \\\\"
  )
}, character(1L))

case_table_tex <- c(
  "% Generated by paper/build_figures.R; do not edit by hand.",
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Two held-out challenge opportunities}",
  "\\label{tab:cases}",
  "\\small",
  "{\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabular}{lllrrrr}",
  "\\toprule",
  "Case & State & Role & $G$ & $L_2$ & $q_2^*$ & $P(A\\mid M)$ \\\\",
  "\\midrule",
  case_tex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "}",
  "\\par\\vspace{0.4em}",
  "\\begin{minipage}{0.96\\linewidth}",
  "\\footnotesize\\itshape Notes: $P(A=1\\mid M=m,k=2)$ is the ex post action probability obtained by integrating over the latent private signal conditional on realized geometry. The live decision statistic is $q=P(M>0\\mid S,r,c)$.",
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(case_table_tex, file.path(output_dir, "case_studies_table.tex"))

generated <- c(
  "direct_policy_scenario_cross_evaluation.csv",
  "direct_policy_candidate_summary.csv",
  "mechanism_by_inning.pdf",
  "mechanism_challenge_rates.csv",
  "mechanism_inventory_values.csv",
  "role_opportunity_summary.csv",
  "team_policy_gain.pdf",
  "team_policy_gain.csv",
  "results_table.tex",
  "results_table.csv",
  "headline_macros.tex",
  "headline_values.csv",
  "case_studies_table.tex",
  "case_studies_table.csv",
  "case_macros.tex"
)
missing_outputs <- generated[!file.exists(file.path(output_dir, generated))]
if (length(missing_outputs)) {
  stop("Figure build did not create: ", paste(missing_outputs, collapse = ", "),
    call. = FALSE
  )
}

message(
  "Generated ", length(generated), " publication artifacts in ", output_dir
)
