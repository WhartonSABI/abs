library(arrow)
library(dplyr)

p4 <- read_parquet(
  "data/processed/joint_perception/pilot_probabilities_bin4_sdcar0p5.parquet"
)
borderline_pitches <- p4 %>%
  filter(abs(edge_distance_inches) <= 0.5)

borderline_pitches <- p4 %>%
  filter(abs(edge_distance_inches) <= 2) %>%
  mutate(
    probability_group = case_when(
      p_hat < 0.01 ~ "Below 1%",
      p_hat > 0.99 ~ "Above 99%",
      TRUE ~ "Between 1% and 99%"
    )
  )

borderline_summary <- borderline_pitches %>%
  summarise(
    pitches = n(),
    average_predicted = mean(p_hat),
    actual_wrong_rate = mean(call_wrong),
    below_1_percent = mean(p_hat < 0.01),
    above_99_percent = mean(p_hat > 0.99),
    total_extreme_rate = mean(p_hat < 0.01 | p_hat > 0.99)
  )

borderline_summary
borderline_pitches %>%
  select(
    any_of(c(
      "game_pk",
      "pitch_order",
      "inning",
      "role",
      "initial_call",
      "edge_distance_inches",
      "p_hat",
      "call_wrong",
      "plate_x",
      "plate_z"
    ))
  ) %>%
  arrange(edge_distance_inches) %>%
  View


distance_summary <- p4 %>%
  mutate(
    distance_band = case_when(
      abs(edge_distance_inches) <= 1 ~ "Within 1 inch",
      abs(edge_distance_inches) <= 2 ~ "1–2 inches",
      abs(edge_distance_inches) <= 3 ~ "2–3 inches",
      TRUE ~ "More than 3 inches"
    ),
    distance_band = factor(
      distance_band,
      levels = c(
        "Within 1 inch",
        "1–2 inches",
        "2–3 inches",
        "More than 3 inches"
      )
    )
  ) %>%
  group_by(distance_band) %>%
  summarise(
    pitches = n(),
    predicted_wrong_rate = mean(p_hat),
    actual_wrong_rate = mean(call_wrong),
    below_1_percent = mean(p_hat < 0.01),
    above_99_percent = mean(p_hat > 0.99),
    extreme_rate = mean(p_hat < 0.01 | p_hat > 0.99),
    .groups = "drop"
  )

distance_summary


borderline_extreme_check <- borderline_pitches %>%
  group_by(probability_group) %>%
  summarise(
    pitches = n(),
    average_prediction = mean(p_hat),
    actual_wrong_rate = mean(call_wrong),
    .groups = "drop"
  )

borderline_extreme_check


wrong_borderline <- borderline_pitches %>%
  filter(call_wrong == TRUE)

wrong_borderline %>%
  select(
    edge_distance_inches,
    initial_call,
    abs_call,
    call_wrong,
    p_hat
  ) %>%
  arrange(p_hat) %>%
  View()

wrong_borderline %>%
  summarise(
    pitches = n(),
    average_p_hat = mean(p_hat),
    median_p_hat = median(p_hat),
    below_1_percent = mean(p_hat < 0.01),
    above_99_percent = mean(p_hat > 0.99)
  )


library(dplyr)
library(ggplot2)
library(scales)

border_width <- 0.5

borderline_heat <- p4 %>%
  filter(abs(edge_distance_inches) <= border_width) %>%
  mutate(
    umpire_call = recode(
      as.character(initial_call),
      ball = "Umpire called ball",
      called_strike = "Umpire called strike"
    ),

    # Convert P(wrong) into P(ABS strike)
    p_strike = if_else(
      initial_call == "ball",
      p_hat,
      1 - p_hat
    )
  ) %>%
  group_by(umpire_call, ix, iz) %>%
  summarise(
    x_cell = first(ix) * 4,
    z_cell = first(iz) * 4,
    posterior_p_strike = mean(p_strike),
    pitches = n(),
    .groups = "drop"
  )

borderline_strike_heatmap <- ggplot(
  borderline_heat,
  aes(x = x_cell, y = z_cell, fill = posterior_p_strike)
) +
  geom_tile(
    width = 4,
    height = 4,
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = pitches),
    size = 3,
    color = "black"
  ) +
  facet_wrap(~umpire_call, nrow = 1) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0.5,
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    name = "Posterior\nP(strike)"
  ) +
  coord_fixed() +
  labs(
    title = "Posterior Strike Probability Near the ABS Boundary",
    subtitle = paste0(
      "Pitches with the ball edge within ",
      border_width,
      " inch of the boundary; numbers show pitches per cell"
    ),
    x = "Horizontal location (inches from plate center)",
    y = "Normalized vertical location\n(0 = zone bottom, 20 = zone top)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

borderline_strike_heatmap

library(dplyr)
library(ggplot2)
library(scales)

border_width <- 0.5

success_heat <- p4 %>%
  filter(abs(edge_distance_inches) <= border_width) %>%
  mutate(
    challenge_role = case_when(
      initial_call == "ball" ~ "Defense challenge: umpire called ball",
      initial_call == "called_strike" ~
        "Offense challenge: umpire called strike"
    )
  ) %>%
  group_by(challenge_role, ix, iz) %>%
  summarise(
    x_cell = first(ix) * 4,
    z_cell = first(iz) * 4,
    posterior_success = mean(p_hat),
    pitches = n(),
    .groups = "drop"
  )

borderline_success_heatmap <- ggplot(
  success_heat,
  aes(
    x = x_cell,
    y = z_cell,
    fill = posterior_success
  )
) +
  geom_tile(
    width = 4,
    height = 4,
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = paste0(
        percent(posterior_success, accuracy = 1),
        "\n(n=", pitches, ")"
      )
    ),
    size = 3,
    color = "black"
  ) +
  facet_wrap(~challenge_role, nrow = 1) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0.5,
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    name = "Posterior\nP(success)"
  ) +
  coord_fixed() +
  labs(
    title = "Posterior Challenge-Success Probability Near the ABS Boundary",
    subtitle = paste0(
      "Ball edge within ",
      border_width,
      " inch of the boundary"
    ),
    x = "Horizontal location (inches from plate center)",
    y = "Normalized vertical location\n(0 = zone bottom, 20 = zone top)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

borderline_success_heatmap
library(dplyr); library(ggplot2); library(brms)

ledger <- targets::tar_read(pitch_ledger)
umps   <- readRDS("data/processed/umpires_2026.rds")
ledger <- left_join(ledger, umps, by = "game_pk")

fit_strikes <- readRDS("data/processed/fit_car_strikes_final.rds")
fit_balls   <- readRDS("data/processed/fit_car_balls_final.rds")

score_fit <- function(fit) {
  d <- fit$data
  d$p_hat <- fitted(fit, ndraws = 500)[, "Estimate"] / d$n
  d %>% mutate(across(c(cell, umpire, catcher), as.character)) %>%
    select(cell, umpire, catcher, p_hat) %>% distinct()
}
p_strikes <- score_fit(fit_strikes) %>% mutate(initial_call = "called_strike")
p_balls   <- score_fit(fit_balls)   %>% mutate(initial_call = "ball")

scored <- ledger %>%
  filter(tracking_available, abs(edge_distance_inches) <= 12) %>%
  mutate(x_in = plate_x * 12,
         z_in = (plate_z - sz_bot) / (sz_top - sz_bot) * 20,
         cell = paste(round(x_in / 1.5), round(z_in / 1.5), sep = "_"),
         umpire  = as.character(umpire_name),
         catcher = as.character(fielder_2)) %>%
  left_join(bind_rows(p_strikes, p_balls),
            by = c("cell", "umpire", "catcher", "initial_call"))

mean(scored$p_hat, na.rm = TRUE)   # sanity: ~0.0767

# the real called-strike wrongness map (your fitted model, same as always)
strike_map <- scored %>%
  filter(initial_call == "called_strike", !is.na(p_hat)) %>%
  mutate(x_bin = round(x_in / 1.5) * 1.5, z_bin = round(z_in / 1.5) * 1.5) %>%
  group_by(x_bin, z_bin) %>%
  summarise(p = mean(p_hat), n = n(), .groups = "drop") %>% filter(n >= 15)

# one real-ish pitch: called strike, ball-edge ~1.5" off the outside corner
px <- 10; pz <- 10
sigma_batter <- 2   # batter blur, inches (illustrative until fitted)

circle <- function(r) data.frame(x = px + r * cos(seq(0, 2*pi, length = 100)),
                                 z = pz + r * sin(seq(0, 2*pi, length = 100)))

ggplot(strike_map, aes(x_bin, z_bin, fill = p)) +
  geom_tile() +
  annotate("rect", xmin = -8.5, xmax = 8.5, ymin = 0, ymax = 20,
           color = "black", fill = NA, linewidth = 0.7) +
  geom_path(data = circle(sigma_batter), aes(x, z), inherit.aes = FALSE,
            color = "purple", linewidth = 0.9) +
  geom_path(data = circle(2 * sigma_batter), aes(x, z), inherit.aes = FALSE,
            color = "purple", linetype = "dashed") +
  annotate("point", x = px, y = pz, size = 2.5, color = "purple4") +
  scale_fill_gradient(low = "grey95", high = "red3", labels = scales::percent,
                      name = "P(called strike\nwas wrong)") +
  coord_equal(xlim = c(-14, 22), ylim = c(-5, 25)) + theme_minimal(base_size = 13) +
  labs(x = "inches from plate center", y = "zone-relative height (in)",
       title = "One pitch, seen by the machine vs. the batter",
       subtitle = "dot = where the ball truly was; purple rings = where the batter thinks it might have been")

# what each observer concludes about THIS pitch:
w <- exp(-((strike_map$x_bin - px)^2 + (strike_map$z_bin - pz)^2) / (2 * sigma_batter^2))
machine_p <- strike_map$p[which.max(-((strike_map$x_bin - px)^2 + (strike_map$z_bin - pz)^2))]
batter_p  <- sum(w * strike_map$p * strike_map$n) / sum(w * strike_map$n)
c(machine_sees = machine_p, batter_perceives = batter_p)
