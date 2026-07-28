#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(ggplot2)
  library(jsonlite)
})

project_dir <- normalizePath(getwd(), mustWork = TRUE)
if (!file.exists(file.path(project_dir, "data", "processed", "pitch_ledger.parquet"))) {
  stop("Run this script from the ABS project root after `make pipeline`.")
}

args <- commandArgs(trailingOnly = TRUE)
json_path <- if (length(args)) args[[1L]] else
  file.path(project_dir, "output", "dashboard", "abs-dashboard-data.json")
if (!grepl("^/", json_path)) json_path <- file.path(project_dir, json_path)
figure_dir <- file.path(project_dir, "output", "figures", "dashboard")
dir.create(dirname(json_path), recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

read_derived <- function(name) {
  as.data.table(read_parquet(file.path(project_dir, "data", "processed", name)))
}

round_numeric <- function(x, digits = 6L) {
  columns <- names(x)[vapply(x, is.numeric, logical(1))]
  for (column in columns) set(x, j = column, value = round(x[[column]], digits))
  x
}

ledger <- read_derived("pitch_ledger.parquet")
challenges <- read_derived("challenge_events.parquet")
teams <- read_derived("team_summaries.parquet")
players <- read_derived("player_summaries.parquet")
team_intervals <- read_derived("team_intervals.parquet")
adjusted_intervals <- read_derived("adjusted_intervals.parquet")

team_lookup <- unique(rbind(
  ledger[, .(team_id = home_team_id, team = home_team)],
  ledger[, .(team_id = away_team_id, team = away_team)]
))
team_lookup <- team_lookup[!is.na(team_id) & !is.na(team)]
teams <- merge(teams, team_lookup, by = "team_id", all.x = TRUE)
players <- merge(players, team_lookup, by = "team_id", all.x = TRUE)

net_ci <- team_intervals[metric == "net_re", .(
  team_id, net_re_lower = lower, net_re_upper = upper
)]
missed_ci <- team_intervals[metric == "missed_re", .(
  team_id, missed_re_lower = lower, missed_re_upper = upper
)]
out_ci <- team_intervals[metric == "out_re", .(
  team_id, out_re_lower = lower, out_re_upper = upper
)]
residual_ci <- adjusted_intervals[metric == "challenge_residual", .(
  team_id, residual_lower = lower, residual_upper = upper
)]
teams <- Reduce(
  function(left, right) merge(left, right, by = "team_id", all.x = TRUE),
  list(teams, net_ci, missed_ci, out_ci, residual_ci)
)
teams[, `:=`(
  total_forgone_re = missed_available_re + out_of_challenges_re,
  missed_re_per_game = missed_available_re / games,
  out_re_per_game = out_of_challenges_re / games
)]

players[, net_re := gross_re - inventory_cost_re]
setorder(players, -net_re, -attempts, player_name)

published <- challenges[publication_eligible == TRUE]
opportunities <- ledger[correctable_opportunity == TRUE]
opportunities[, loss_type := fcase(
  opportunity_status == "challenged", "challenged",
  opportunity_status == "missed_available", "missed",
  opportunity_status == "out_of_challenges", "out",
  default = "other"
)]

league <- list(
  snapshot = "2026-07-19",
  games = uniqueN(ledger$game_pk),
  calledPitches = nrow(ledger),
  geometryCovered = sum(ledger$statcast_identity_matched & ledger$tracking_available),
  officialAttempts = nrow(published),
  overturns = sum(published$challenge_outcome == "overturned"),
  upheld = sum(published$challenge_outcome == "upheld"),
  successRate = mean(published$challenge_outcome == "overturned"),
  correctable = nrow(opportunities),
  challengedCorrectable = opportunities[loss_type == "challenged", .N],
  missedCount = opportunities[loss_type == "missed", .N],
  outCount = opportunities[loss_type == "out", .N],
  actualGrossRe = sum(published$actual_re_gain, na.rm = TRUE),
  failedInventoryRe = sum(published$failed_inventory_cost_re, na.rm = TRUE),
  actualNetRe = sum(published$actual_re_gain - published$failed_inventory_cost_re,
    na.rm = TRUE),
  actualGrossWpa = sum(published$actual_wpa_gain, na.rm = TRUE),
  missedRe = opportunities[loss_type == "missed",
    sum(potential_challenger_re, na.rm = TRUE)],
  missedWpa = opportunities[loss_type == "missed",
    sum(potential_challenger_wpa, na.rm = TRUE)],
  outRe = opportunities[loss_type == "out",
    sum(potential_challenger_re, na.rm = TRUE)],
  outWpa = opportunities[loss_type == "out",
    sum(potential_challenger_wpa, na.rm = TRUE)]
)
league$availableOpportunityChallengeRate <- league$challengedCorrectable /
  (league$challengedCorrectable + league$missedCount)
league$outOpportunityShare <- league$outCount / league$correctable
league$outForgoneReShare <- league$outRe / (league$missedRe + league$outRe)

team_payload <- teams[, .(
  id = team_id,
  team,
  games,
  attempts,
  overturns,
  upheld,
  successRate = success_rate,
  grossRe = gross_re,
  netRe = net_re,
  netReLower = net_re_lower,
  netReUpper = net_re_upper,
  grossWpa = gross_wpa,
  netWpa = net_wpa,
  failedInventoryRe = failed_inventory_cost_re,
  missedCount = missed_available,
  missedRe = missed_available_re,
  missedReLower = missed_re_lower,
  missedReUpper = missed_re_upper,
  outCount = out_of_challenges,
  outRe = out_of_challenges_re,
  outReLower = out_re_lower,
  outReUpper = out_re_upper,
  totalForgoneRe = total_forgone_re,
  missedRePerGame = missed_re_per_game,
  outRePerGame = out_re_per_game,
  expectedCorrectableStart = expected_correctable_remaining,
  expectedReStart = expected_re_remaining,
  firstUnitRe = marginal_re_first_unit,
  secondUnitRe = marginal_re_second_unit,
  observedAttempts = observed_attempts,
  expectedAttempts = expected_attempts,
  challengeResidual = challenge_residual,
  residualLower = residual_lower,
  residualUpper = residual_upper
)]
setorder(team_payload, -totalForgoneRe)

player_payload <- players[, .(
  id = player_id,
  name = player_name,
  role,
  team,
  teamId = team_id,
  attempts,
  overturns,
  upheld,
  successRate = success_rate,
  grossRe = gross_re,
  inventoryCostRe = inventory_cost_re,
  netRe = net_re,
  grossWpa = gross_wpa
)]

role_payload <- players[, .(
  players = .N,
  attempts = sum(attempts),
  overturns = sum(overturns),
  successRate = sum(overturns) / sum(attempts),
  grossRe = sum(gross_re),
  inventoryCostRe = sum(inventory_cost_re),
  netRe = sum(net_re)
), by = role][order(-attempts)]

inning_payload <- opportunities[, .(
  opportunities = .N,
  re = sum(potential_challenger_re, na.rm = TRUE),
  wpa = sum(potential_challenger_wpa, na.rm = TRUE)
), by = .(
  inning = fifelse(inning >= 10L, "10+", as.character(inning)),
  type = loss_type
)]
inning_payload[, inningOrder := match(inning, c(as.character(1:9), "10+"))]
setorder(inning_payload, inningOrder, type)

count_payload <- opportunities[, .(
  opportunities = .N,
  re = sum(potential_challenger_re, na.rm = TRUE),
  wpa = sum(potential_challenger_wpa, na.rm = TRUE)
), by = .(balls = balls_before, strikes = strikes_before, type = loss_type)]
setorder(count_payload, balls, strikes, type)

outs_payload <- opportunities[, .(
  opportunities = .N,
  re = sum(potential_challenger_re, na.rm = TRUE),
  wpa = sum(potential_challenger_wpa, na.rm = TRUE)
), by = .(outs = outs_before, type = loss_type)]
setorder(outs_payload, outs, type)

role_situation_payload <- opportunities[, .(
  opportunities = .N,
  re = sum(potential_challenger_re, na.rm = TRUE),
  wpa = sum(potential_challenger_wpa, na.rm = TRUE)
), by = .(side = adverse_role, type = loss_type)]
setorder(role_situation_payload, side, type)

opportunities[, affected_score_diff := fifelse(
  adverse_role == "offense", bat_score - fld_score, fld_score - bat_score
)]
opportunities[, score_situation := fcase(
  affected_score_diff <= -3, "Down 3+",
  affected_score_diff == -2, "Down 2",
  affected_score_diff == -1, "Down 1",
  affected_score_diff == 0, "Tied",
  affected_score_diff == 1, "Up 1",
  affected_score_diff == 2, "Up 2",
  affected_score_diff >= 3, "Up 3+",
  default = "Unknown"
)]
score_order <- c("Down 3+", "Down 2", "Down 1", "Tied", "Up 1", "Up 2", "Up 3+")
score_payload <- opportunities[score_situation != "Unknown", .(
  opportunities = .N,
  re = sum(potential_challenger_re, na.rm = TRUE),
  wpa = sum(potential_challenger_wpa, na.rm = TRUE)
), by = .(situation = score_situation, type = loss_type)]
score_payload[, order := match(situation, score_order)]
setorder(score_payload, order, type)

opportunities[, base_situation := fcase(
  !is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b), "Bases loaded",
  !is.na(on_2b) | !is.na(on_3b), "RISP",
  !is.na(on_1b), "First only",
  default = "Empty"
)]
base_order <- c("Empty", "First only", "RISP", "Bases loaded")
base_payload <- opportunities[, .(
  opportunities = .N,
  re = sum(potential_challenger_re, na.rm = TRUE),
  wpa = sum(potential_challenger_wpa, na.rm = TRUE)
), by = .(situation = base_situation, type = loss_type)]
base_payload[, order := match(situation, base_order)]
setorder(base_payload, order, type)

zone <- opportunities[loss_type %in% c("missed", "out") &
  is.finite(plate_x) & is.finite(plate_z) & is.finite(sz_top) & is.finite(sz_bot) &
  sz_top > sz_bot]
zone[, zone_z := (plate_z - sz_bot) / (sz_top - sz_bot)]
x_breaks <- seq(-1.35, 1.35, length.out = 14L)
z_breaks <- seq(-0.35, 1.35, length.out = 14L)
zone[, `:=`(
  x_bin = cut(plate_x, x_breaks, labels = FALSE, include.lowest = TRUE),
  z_bin = cut(zone_z, z_breaks, labels = FALSE, include.lowest = TRUE)
)]
zone_payload <- zone[!is.na(x_bin) & !is.na(z_bin), .(
  opportunities = .N,
  re = sum(potential_challenger_re, na.rm = TRUE)
), by = .(type = loss_type, x_bin, z_bin)]
zone_payload[, `:=`(
  x = (x_breaks[x_bin] + x_breaks[x_bin + 1L]) / 2,
  z = (z_breaks[z_bin] + z_breaks[z_bin + 1L]) / 2,
  width = diff(x_breaks)[1L],
  height = diff(z_breaks)[1L]
)]
setorder(zone_payload, type, z_bin, x_bin)

for (object_name in c(
  "team_payload", "player_payload", "role_payload", "inning_payload",
  "count_payload", "outs_payload", "role_situation_payload", "score_payload",
  "base_payload", "zone_payload"
)) {
  assign(object_name, round_numeric(get(object_name)))
}

payload <- list(
  meta = c(league, list(
    methodology = "Official challenge outcomes; validated public geometry for unchallenged calls",
    boundarySensitivityInches = 0.25,
    publicationSet = "Baseball Savant official ABS analytical set"
  )),
  teams = team_payload,
  players = player_payload,
  playerRoles = role_payload,
  situations = list(
    inning = inning_payload,
    count = count_payload,
    outs = outs_payload,
    side = role_situation_payload,
    score = score_payload,
    bases = base_payload,
    zone = zone_payload
  )
)

writeLines(
  toJSON(payload, dataframe = "rows", auto_unbox = TRUE, digits = 8,
    pretty = FALSE, na = "null"),
  json_path,
  useBytes = TRUE
)

penn_blue <- "#011F5B"
penn_red <- "#990000"
blue_light <- "#4F6DA8"
red_light <- "#C95F5F"
gold <- "#B57C00"
warm <- "#F5F2EB"
base_theme <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "sans", colour = penn_blue),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(colour = "#5C564C"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    plot.background = element_rect(fill = warm, colour = NA),
    panel.background = element_rect(fill = warm, colour = NA)
  )

save_plot <- function(plot, name, width, height) {
  ggsave(file.path(figure_dir, paste0(name, ".png")), plot,
    width = width, height = height, dpi = 220, bg = warm)
}

league_values <- data.table(
  category = factor(c("Corrected by challenge", "Missed with inventory", "Lost at zero"),
    levels = rev(c("Corrected by challenge", "Missed with inventory", "Lost at zero"))),
  re = c(league$actualGrossRe, league$missedRe, league$outRe),
  type = c("Realized gain", "Potential loss", "Potential loss")
)
p <- ggplot(league_values, aes(re, category, fill = type)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = sprintf("%.1f RE", re)), hjust = -0.08, fontface = "bold") +
  scale_fill_manual(values = c("Realized gain" = penn_blue, "Potential loss" = penn_red)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "The largest cost was passing with a challenge available",
    subtitle = "Immediate run value through July 19, 2026",
    x = "Runs", y = NULL, fill = NULL
  ) + base_theme
save_plot(p, "league-value-comparison", 9.5, 4.8)

team_long <- melt(teams[, .(team, missed_available_re, out_of_challenges_re)],
  id.vars = "team", variable.name = "type", value.name = "re")
team_long[, type := factor(type,
  levels = c("missed_available_re", "out_of_challenges_re"),
  labels = c("Missed with inventory", "Out of challenges"))]
team_order <- teams[order(total_forgone_re), team]
team_long[, team := factor(team, levels = team_order)]
p <- ggplot(team_long, aes(re, team, fill = type)) +
  geom_col(width = 0.68) +
  scale_fill_manual(values = c("Missed with inventory" = blue_light,
    "Out of challenges" = penn_red)) +
  labs(
    title = "Potential run value left uncorrected by team",
    subtitle = "Wrong calls not challenged, separated by whether inventory existed",
    x = "Potential RE", y = NULL, fill = NULL
  ) + base_theme
save_plot(p, "team-forgone-run-value", 9.5, 9.5)

p <- ggplot(teams, aes(net_re, total_forgone_re)) +
  geom_hline(yintercept = mean(teams$total_forgone_re), colour = "#D9D4C9") +
  geom_vline(xintercept = mean(teams$net_re), colour = "#D9D4C9") +
  geom_point(aes(size = attempts, colour = success_rate), alpha = 0.85) +
  geom_text(aes(label = team), size = 3, fontface = "bold", colour = penn_blue) +
  scale_colour_gradient(low = red_light, high = penn_blue,
    labels = scales::label_percent(accuracy = 1)) +
  scale_size(range = c(6, 12), guide = "none") +
  labs(
    title = "Challenge returns and forgone opportunity are different skills",
    subtitle = "Point size is attempt volume; color is official success rate",
    x = "Net run value gained from challenges",
    y = "Potential RE not corrected",
    colour = "Success"
  ) + base_theme + theme(panel.grid.major.y = element_line(colour = "#E8E4DC"))
save_plot(p, "team-decision-map", 9, 7)

top_players <- players[order(-net_re)][1:min(20L, .N)]
top_players[, label := paste0(player_name, " (", team, ")")]
top_players[, label := factor(label, levels = rev(label))]
p <- ggplot(top_players, aes(net_re, label, fill = role)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.1f", net_re)), hjust = -0.12, size = 3.4) +
  scale_fill_manual(values = c(batter = penn_blue, catcher = penn_red, pitcher = gold)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Player leaders in net challenge run value",
    subtitle = "Official Savant publication set",
    x = "Net RE", y = NULL, fill = "Role"
  ) + base_theme
save_plot(p, "player-net-run-value", 9.5, 7.5)

count_plot <- count_payload[type %in% c("missed", "out")]
count_plot[, type := factor(type, levels = c("missed", "out"),
  labels = c("Missed with inventory", "Out of challenges"))]
p <- ggplot(count_plot, aes(factor(balls), factor(strikes), fill = re)) +
  geom_tile(colour = warm, linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.1f", re)), fontface = "bold") +
  facet_wrap(~type, scales = "free") +
  scale_fill_gradient(low = "#E8EEF8", high = penn_red) +
  labs(
    title = "Full counts make each uncorrected call more expensive",
    subtitle = "Cell labels are potential RE lost",
    x = "Balls before pitch", y = "Strikes before pitch", fill = "RE"
  ) + base_theme + theme(panel.grid = element_blank())
save_plot(p, "count-situation-heatmap", 9.5, 4.8)

inning_plot <- inning_payload[type %in% c("missed", "out")]
inning_plot[, type := factor(type, levels = c("missed", "out"),
  labels = c("Missed with inventory", "Out of challenges"))]
p <- ggplot(inning_plot, aes(factor(inning, levels = c(as.character(1:9), "10+")), re,
  fill = type)) +
  geom_col(position = position_dodge(width = 0.76), width = 0.7) +
  scale_fill_manual(values = c("Missed with inventory" = blue_light,
    "Out of challenges" = penn_red)) +
  labs(
    title = "Zero-inventory losses arrive late",
    subtitle = "Potential RE by inning; extra innings grouped as 10+",
    x = "Inning", y = "Potential RE", fill = NULL
  ) + base_theme
save_plot(p, "inning-run-value-loss", 9.5, 5.2)

role_long <- melt(role_payload,
  id.vars = "role", measure.vars = c("attempts", "successRate", "netRe"),
  variable.name = "metric", value.name = "value")
role_long[, metric := factor(metric,
  levels = c("attempts", "successRate", "netRe"),
  labels = c("Attempts", "Success rate", "Net RE"))]
p <- ggplot(role_long, aes(value, reorder(role, value), fill = role)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  facet_wrap(~metric, scales = "free_x") +
  scale_fill_manual(values = c(batter = penn_blue, catcher = penn_red, pitcher = gold)) +
  labs(
    title = "Catchers drove the challenge game",
    subtitle = "Player role recorded by the MLB review feed",
    x = NULL, y = NULL
  ) + base_theme
save_plot(p, "player-role-summary", 10, 4.5)

zone_plot <- copy(zone_payload)
zone_plot[, type := factor(type, levels = c("missed", "out"),
  labels = c("Missed with inventory", "Out of challenges"))]
p <- ggplot(zone_plot, aes(x, z, fill = opportunities)) +
  geom_tile(aes(width = width, height = height)) +
  annotate("rect", xmin = -17 / 24, xmax = 17 / 24, ymin = 0, ymax = 1,
    fill = NA, colour = penn_blue, linewidth = 0.8) +
  facet_wrap(~type) +
  scale_fill_gradient(low = "#F5F2EB", high = penn_red) +
  coord_cartesian(xlim = c(-1.35, 1.35), ylim = c(-0.35, 1.35), expand = FALSE) +
  labs(
    title = "Uncorrected calls cluster around the ABS edge",
    subtitle = "Pitch height is normalized to each batter's zone",
    x = "Horizontal location (feet)", y = "Normalized zone height", fill = "Pitches"
  ) + base_theme + theme(panel.grid = element_blank())
save_plot(p, "zone-opportunity-density", 9.5, 5.2)

message("Wrote dashboard data to ", json_path)
message("Wrote 8 figures to ", figure_dir)
