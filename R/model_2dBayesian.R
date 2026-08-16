# 2D model workhorse -- MRF-smoothed bins (teammate-aligned), catcher reinstated
# as shrinkage demonstration. Core: call_wrong ~ mrf(cell, by = initial_call)
#   + initial_call + (1 | umpire) + (1 | catcher)
# History: pitch type OUT (2D collapse), stand OUT (~1% decision flips),
# t2() superseded by MRF (te/t2 underfit the boundary ridge). See CLAUDEABS.md.

library(dplyr)
library(tidyr)
library(ggplot2)
library(brms)
library(mgcv)

# ---- A. data: ledger + umpires + pitch type + catcher ----------------------

# ---- A. data: ledger + umpires + pitch type (fielder_2 already in ledger) --

ledger <- targets::tar_read(pitch_ledger)
umps   <- readRDS("data/processed/umpires_2026.rds")
ledger <- left_join(ledger, umps, by = "game_pk")

sc_files <- list.files("data/raw/statcast", pattern = "csv$", full.names = TRUE)
sc_cols <- purrr::map_dfr(sc_files, function(f)
  readr::read_csv(f,
                  col_select = c(game_pk, at_bat_number, pitch_number, pitch_type),
                  show_col_types = FALSE))
ledger <- ledger %>%
  left_join(sc_cols, by = c("game_pk", "at_bat_number",
                            "statcast_pitch_number" = "pitch_number"))

mean(is.na(ledger$pitch_type))   # audit: want near 0
mean(is.na(ledger$fielder_2))    # ledger's own catcher column: want near 0
n_distinct(ledger$fielder_2)     # ~100-ish catchers

# ---- B. model data + cells + neighbor graph (1.5-inch bins) ----------------

model_data_2d <- ledger %>%
  filter(tracking_available) %>%
  transmute(
    call_wrong = as.integer(initial_call != abs_call),
    edge_dist  = edge_distance_inches,
    x_in  = plate_x * 12,                                 # inches from center
    z_in  = (plate_z - sz_bot) / (sz_top - sz_bot) * 20,  # zone-relative inches
    initial_call = factor(initial_call),
    umpire  = factor(umpire_name),
    catcher = factor(fielder_2)
  ) %>%
  filter(abs(edge_dist) <= 12) %>%
  mutate(ix = round(x_in / 1.5), iz = round(z_in / 1.5),
         cell = paste(ix, iz, sep = "_"))

cell_tab <- model_data_2d %>% distinct(ix, iz, cell) %>% arrange(ix, iz)

# rook adjacency; loop because dropping an orphan can orphan its ex-neighbor
repeat {
  nb <- lapply(seq_len(nrow(cell_tab)), function(i) {
    which((abs(cell_tab$ix - cell_tab$ix[i]) == 1 & cell_tab$iz == cell_tab$iz[i]) |
            (cell_tab$ix == cell_tab$ix[i] & abs(cell_tab$iz - cell_tab$iz[i]) == 1))
  })
  names(nb) <- cell_tab$cell
  orphans <- names(nb)[lengths(nb) == 0]
  if (!length(orphans)) break
  cell_tab <- cell_tab %>% filter(!cell %in% orphans)
}
model_data_2d <- model_data_2d %>%
  filter(cell %in% cell_tab$cell) %>%
  mutate(cell = factor(cell, levels = cell_tab$cell))

nrow(model_data_2d)                 # ~192k
mean(model_data_2d$call_wrong)      # ~0.075
colSums(is.na(model_data_2d))       # all zeros
nrow(cell_tab)                      # ~800-1,500 cells

# ---- C. screens (concluded 2026-08-14; re-running reproduces the verdicts:
#         pitch_group OUT, stand flat OUT, stand surfaces real but ~1% flips,
#         nearest_edge vs te() significant -> motivated the MRF switch) ------

close2d <- ledger %>%
  filter(tracking_available, abs(edge_distance_inches) <= 3) %>%
  transmute(
    call_wrong = as.integer(initial_call != abs_call),
    x_in  = plate_x * 12,
    z_rel = (plate_z - sz_bot) / (sz_top - sz_bot),
    initial_call = factor(initial_call),
    umpire = factor(umpire_name),
    stand  = factor(stand),
    pitch_group = factor(case_when(
      pitch_type %in% c("FF", "SI", "FC")             ~ "fastball",
      pitch_type %in% c("SL", "CU", "KC", "ST", "SV") ~ "breaking",
      pitch_type %in% c("CH", "FS", "FO", "KN")       ~ "offspeed",
      TRUE ~ NA_character_)),
    d_side = abs(abs(plate_x) - 17/24),
    d_top  = abs(plate_z - sz_top),
    d_bot  = abs(plate_z - sz_bot),
    nearest_edge = factor(case_when(
      d_side <= d_top & d_side <= d_bot ~ "side",
      d_bot  <= d_top                   ~ "bottom",
      TRUE                              ~ "top"))
  ) %>% select(-d_side, -d_top, -d_bot) %>% droplevels()

nrow(close2d)   # ~71,039

screen_candidate_2d <- function(candidate, data = close2d) {
  stopifnot(candidate %in% names(data))
  data <- data[!is.na(data[[candidate]]), , drop = FALSE] |> droplevels()
  f0 <- call_wrong ~ te(x_in, z_rel, by = initial_call, k = c(8, 8)) +
    initial_call + umpire
  f1 <- update(f0, paste("~ . +", candidate))
  b0 <- bam(f0, data = data, method = "fREML", discrete = TRUE)
  b1 <- bam(f1, data = data, method = "fREML", discrete = TRUE)
  a  <- anova(b0, b1, test = "F")
  coefs <- coef(b1)[grepl(paste0("^", candidate), names(coef(b1)))]
  list(verdict = data.frame(candidate = candidate, n = nrow(data),
                            F_stat = round(a$F[2], 2),
                            p_value = signif(a$`Pr(>F)`[2], 3)),
       effect_sizes_pct_points = round(100 * coefs, 2))
}

screen_candidate_2d("nearest_edge")   # recorded: F=4.82, p=0.008 (te underfits ridge)
screen_candidate_2d("pitch_group")    # recorded: F=0.31, p=0.73  (OUT)
screen_candidate_2d("stand")          # recorded: F=0.48, p=0.49  (flat shift OUT)

b_flat  <- bam(call_wrong ~ te(x_in, z_rel, by = initial_call) + initial_call
               + umpire + stand, data = close2d, method = "fREML", discrete = TRUE)
b_shift <- bam(call_wrong ~ te(x_in, z_rel, by = interaction(initial_call, stand))
               + initial_call + umpire + stand,
               data = close2d, method = "fREML", discrete = TRUE)
anova(b_flat, b_shift, test = "F")    # recorded: F=2.18, p=4.2e-06

gL <- close2d %>% mutate(stand = factor("L", levels = levels(close2d$stand)))
gR <- close2d %>% mutate(stand = factor("R", levels = levels(close2d$stand)))
pL <- predict(b_shift, newdata = gL)
pR <- predict(b_shift, newdata = gR)
d_pts <- 100 * (pR - pL)

summary(d_pts)                          # recorded: median |d| ~0.9 pts
quantile(abs(d_pts), c(0.5, 0.9, 0.99))
mean((pL > 0.09) != (pR > 0.09))        # recorded: 0.013
mean((pL > 0.30) != (pR > 0.30))        # recorded: 0.010

tail_df <- close2d %>% mutate(d = d_pts) %>% filter(abs(d) >= 3)
nrow(tail_df)
table(tail_df$initial_call, sign(tail_df$d))

ggplot(tail_df, aes(x_in, z_rel, color = d)) +
  geom_point(alpha = 0.3, size = 0.8) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        name = "RHB-LHB (pts)") +
  annotate("rect", xmin = -8.5, xmax = 8.5, ymin = 0, ymax = 1,
           color = "black", fill = NA) +
  facet_wrap(~initial_call) + coord_fixed(20) +
  labs(title = "Close pitches where batter side moves p by 3+ points")

# ---- D. aggregated MRF population fit + validation (tens of minutes) -------

cells_agg <- model_data_2d %>%
  group_by(cell, initial_call) %>%
  summarise(n = n(), wrong = sum(call_wrong), .groups = "drop")

sum(cells_agg$n) == nrow(model_data_2d)   # conservation check, must be TRUE

t0 <- Sys.time()
fit_mrf_pop <- brm(
  wrong | trials(n) ~ s(cell, bs = "mrf", xt = list(nb = nb), by = initial_call)
  + initial_call,
  data = cells_agg, family = binomial(),
  chains = 4, cores = 4, seed = 42,
  file = "data/processed/fit_mrf_pop"
)
Sys.time() - t0
summary(fit_mrf_pop)   # Rhat <= 1.01, ESS > 400, no divergence warnings

pred <- fitted(fit_mrf_pop)[, "Estimate"] / cells_agg$n

plot_df <- cells_agg %>%
  left_join(cell_tab, by = "cell") %>%
  mutate(raw = wrong / n, model = pred) %>%
  filter(n >= 15) %>%
  pivot_longer(c(raw, model), names_to = "source", values_to = "p_wrong")

ggplot(plot_df, aes(ix * 1.5, iz * 1.5, fill = p_wrong)) +
  geom_tile() +
  facet_grid(initial_call ~ source) +
  annotate("rect", xmin = -8.5, xmax = 8.5, ymin = 0, ymax = 20,
           color = "black", fill = NA, linewidth = 0.6) +
  scale_fill_gradient(low = "white", high = "red3",
                      labels = scales::percent, name = "P(call wrong)") +
  coord_equal() + theme_minimal() +
  labs(x = "inches from plate center", y = "zone-relative height (in)",
       title = "MRF surface vs raw bins, by call direction",
       subtitle = "cells with n < 15 hidden")

# ---- E. THE FULL FIT -- runs LAST, overnight -------------------------------
# Post-fit checklist: Rhat <= 1.01, zero divergences, surface matches raw ring,
# sd(umpire) ~0.05-0.12; PREDICTION: sd(catcher) < half sd(umpire), interval
# hugging zero (the in-model confirmation of the catcher screen).

t0 <- Sys.time()
fit_mrf_full <- brm(
  call_wrong ~ s(cell, bs = "mrf", xt = list(nb = nb), by = initial_call)
  + initial_call + (1 | umpire) + (1 | catcher),
  data = model_data_2d, family = bernoulli(),
  chains = 4, cores = 4, seed = 42,
  file = "data/processed/fit_mrf_full"
)
Sys.time() - t0




library(Matrix)   # ships with base R

# Build per-call-direction data + adjacency. ICAR needs a connected graph,
# so this keeps the largest connected component and reports what it dropped.
build_car_input <- function(call_type) {
  d <- model_data_2d %>% filter(initial_call == call_type)
  ct <- d %>% distinct(ix, iz) %>% arrange(ix, iz) %>%
    mutate(cell = paste(ix, iz, sep = "_"))
  n <- nrow(ct)
  nbl <- lapply(seq_len(n), function(i)
    which((abs(ct$ix - ct$ix[i]) == 1 & ct$iz == ct$iz[i]) |
            (ct$ix == ct$ix[i] & abs(ct$iz - ct$iz[i]) == 1)))
  comp <- integer(n); cur <- 0
  for (s in seq_len(n)) if (comp[s] == 0) {
    cur <- cur + 1; q <- s; comp[s] <- cur
    while (length(q)) {
      v <- q[1]; q <- q[-1]
      new <- nbl[[v]][comp[nbl[[v]]] == 0]
      comp[new] <- cur; q <- c(q, new)
    }
  }
  keep <- comp == which.max(tabulate(comp))
  message(call_type, ": keeping ", sum(keep), " of ", n, " cells (largest component)")
  ct <- ct[keep, ]; n <- nrow(ct)
  nbl <- lapply(seq_len(n), function(i)
    which((abs(ct$ix - ct$ix[i]) == 1 & ct$iz == ct$iz[i]) |
            (ct$ix == ct$ix[i] & abs(ct$iz - ct$iz[i]) == 1)))
  edges <- do.call(rbind, lapply(seq_len(n), function(i) cbind(i, nbl[[i]])))
  M <- sparseMatrix(i = edges[, 1], j = edges[, 2], x = 1, dims = c(n, n),
                    dimnames = list(ct$cell, ct$cell))
  dat <- d %>% filter(cell %in% ct$cell) %>%
    mutate(cell = factor(as.character(cell), levels = ct$cell)) %>%
    group_by(cell, umpire, catcher) %>%
    summarise(n = n(), wrong = sum(call_wrong), .groups = "drop")
  message(call_type, ": ", nrow(dat), " rows after aggregation")
  list(data = dat, M = M)
}

strikes_in <- build_car_input("called_strike")   # the smaller half: pilot here

# ---- PILOT: one chain, 300 iterations, progress every 10 -------------------
fit_pilot <- brm(
  wrong | trials(n) ~ car(M, gr = cell, type = "esicar")
  + (1 | umpire) + (1 | catcher),
  data = strikes_in$data, data2 = list(M = strikes_in$M),
  family = binomial(),
  prior = prior(student_t(3, 0, 2), class = sdcar) +
    prior(student_t(3, 0, 2), class = sd),
  chains = 1, iter = 300, warmup = 150, refresh = 10,
  init = 0, seed = 42
)


fit_car_strikes <- brm(
  wrong | trials(n) ~ car(M, gr = cell, type = "esicar")
  + (1 | umpire) + (1 | catcher),
  data = strikes_in$data, data2 = list(M = strikes_in$M),
  family = binomial(),
  prior = prior(student_t(3, 0, 2), class = sdcar) +
    prior(student_t(3, 0, 2), class = sd),
  chains = 4, cores = 4, iter = 2000, init = 0, seed = 42,
  file = "data/processed/fit_car_strikes"
)

balls_in <- build_car_input("ball")

fit_car_balls <- brm(
  wrong | trials(n) ~ car(M, gr = cell, type = "esicar")
  + (1 | umpire) + (1 | catcher),
  data = balls_in$data, data2 = list(M = balls_in$M),
  family = binomial(),
  prior = prior(student_t(3, 0, 2), class = sdcar) +
    prior(student_t(3, 0, 2), class = sd),
  chains = 4, cores = 4, iter = 2000, init = 0, seed = 42,
  file = "data/processed/fit_car_balls"
)


fit_car_strikes <- brm(
  wrong | trials(n) ~ car(M, gr = cell, type = "esicar")
  + (1 | umpire) + (1 | catcher),
  data = strikes_in$data, data2 = list(M = strikes_in$M),
  family = binomial(),
  prior = prior(student_t(3, 0, 2), class = sdcar) +
    prior(student_t(3, 0, 2), class = sd),
  chains = 4, cores = 4, iter = 4000, init = 0, seed = 42,
  file = "data/processed/fit_car_strikes_4k"
)

fit_car_balls <- brm(
  wrong | trials(n) ~ car(M, gr = cell, type = "esicar")
  + (1 | umpire) + (1 | catcher),
  data = balls_in$data, data2 = list(M = balls_in$M),
  family = binomial(),
  prior = prior(student_t(3, 0, 2), class = sdcar) +
    prior(student_t(3, 0, 2), class = sd),
  chains = 4, cores = 4, iter = 4000, init = 0, seed = 42,
  file = "data/processed/fit_car_balls_4k"
)
