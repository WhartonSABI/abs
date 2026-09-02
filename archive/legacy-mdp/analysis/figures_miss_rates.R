# Descriptive figures: ring heatmap (raw baseline), challenged-only overturn
# curve (the selection-bias exhibit), and the lefty-strike horizontal check.

library(dplyr)
library(ggplot2)

ledger <- targets::tar_read(pitch_ledger)

# ---- ring heatmap: where umpires miss, 1.5-inch bins -----------------------

heat <- ledger %>%
  filter(tracking_available) %>%
  mutate(
    call_wrong = initial_call != abs_call,
    x_in = plate_x * 12,                  # feet -> inches
    z_in = (plate_z - sz_bot) * 12        # height above zone bottom, inches
  ) %>%
  mutate(
    x_bin = floor(x_in / 1.5) * 1.5 + 0.75,
    z_bin = floor(z_in / 1.5) * 1.5 + 0.75
  ) %>%
  group_by(x_bin, z_bin) %>%
  summarise(n = n(), miss_rate = mean(call_wrong), .groups = "drop")

avg_zone_top <- with(ledger, mean((sz_top - sz_bot) * 12, na.rm = TRUE))

ggplot(filter(heat, n >= 15),
       aes(x_bin, z_bin, fill = miss_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red3",
                      labels = scales::percent, name = "Percent missed") +
  annotate("rect", xmin = -8.5, xmax = 8.5, ymin = 0, ymax = avg_zone_top,
           color = "black", fill = NA, linewidth = 0.7) +
  coord_equal() +
  labs(x = "Horizontal pitch-center location (inches)",
       y = "Vertical location above zone bottom (inches)",
       title = "Where umpires miss ball-strike calls — 2026",
       subtitle = "1.5-inch bins; bins with fewer than 15 calls hidden") +
  theme_minimal()

# ---- challenged-only overturn curve: the selection-bias exhibit ------------
# Overturn rate is flat (~55%) across location because players only challenge
# close calls. This is why the model target is call_wrong on ALL pitches.

challenged <- ledger %>%
  filter(challenge_occurred, tracking_available)

raw_curve <- challenged %>%
  mutate(edge_bin = cut(edge_distance_inches,
                        breaks = seq(-6, 6, by = 1))) %>%
  group_by(edge_bin) %>%
  summarise(n = n(),
            overturn_rate = mean(is_overturned),
            .groups = "drop")

raw_curve

ggplot(raw_curve, aes(edge_bin, overturn_rate)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.4, size = 3) +
  labs(x = "distance from zone edge (inches)",
       y = "raw overturn rate",
       title = "Raw p by pitch location — 2026 challenges")

# ---- lefty strike check: horizontal called-zone edges by batter side -------
# 2026 result: nearly symmetric; the classic LHB outside extension is gone.

horiz <- ledger %>%
  filter(tracking_available,
         plate_z > sz_bot, plate_z < sz_top) %>%   # vertical middle only
  mutate(x_in = plate_x * 12,
         called_strike = initial_call == "called_strike",
         x_bin = round(x_in / 1.5) * 1.5) %>%
  group_by(stand, x_bin) %>%
  summarise(n = n(), strike_rate = mean(called_strike), .groups = "drop") %>%
  filter(n >= 30)

ggplot(horiz, aes(x_bin, strike_rate, color = stand)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = c(-8.5, 8.5), linetype = "dashed") +
  labs(x = "horizontal location (inches, catcher's view)",
       y = "called-strike rate",
       title = "The called zone's horizontal edges, by batter side — 2026",
       subtitle = "Dashed lines = the rulebook plate edges") +
  theme_minimal()
