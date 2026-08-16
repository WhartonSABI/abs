# The live workhorse: rebuild model data, run pending screens, launch full fit.
# Model core (settled): call_wrong ~ s(edge_dist) + initial_call + (1 | umpire)

library(dplyr)
library(brms)

# ---- 1. data ---------------------------------------------------------------

ledger <- targets::tar_read(pitch_ledger)
umps   <- readRDS("data/processed/umpires_2026.rds")   # provenance: R/umpires.R
ledger <- left_join(ledger, umps, by = "game_pk")

model_data <- ledger %>%
  filter(tracking_available) %>%
  transmute(
    call_wrong  = as.integer(initial_call != abs_call),
    edge_dist   = edge_distance_inches,
    plate_x_in  = plate_x * 12,
    plate_z_rel = (plate_z - sz_bot) * 12,
    initial_call,
    umpire      = factor(umpire_name),
    d_side = abs(abs(plate_x) - 17/24),   # unsigned distance to nearer side edge (ft)
    d_top  = abs(plate_z - sz_top),
    d_bot  = abs(plate_z - sz_bot),
    nearest_edge = factor(case_when(
      d_side <= d_top & d_side <= d_bot ~ "side",
      d_bot  <= d_top                   ~ "bottom",
      TRUE                              ~ "top"
    ))
  ) %>%
  select(-d_side, -d_top, -d_bot)

# ---- 2. audit -- always, before anything runs on it ------------------------

nrow(model_data)              # ~219k, must match previous build
mean(model_data$call_wrong)   # must print 0.0674
colSums(is.na(model_data))    # all zeros (umpire NAs = broken join)

close <- model_data %>% filter(abs(edge_dist) <= 3) %>% droplevels()
nrow(close)                   # ~71,039
table(close$nearest_edge)     # three groups, none tiny

# ---- 3. the screening machine ----------------------------------------------
# Give it a column name (in quotes) that exists in `close`.
# Fits baseline vs baseline + candidate, reports the verdict.

screen_candidate <- function(candidate, data = close) {
  stopifnot(candidate %in% names(data))
  data <- data[!is.na(data[[candidate]]), , drop = FALSE] |> droplevels()
  base_formula <- call_wrong ~ poly(edge_dist, 3) + initial_call + umpire
  chal_formula <- update(base_formula, paste("~ . +", candidate))
  m0 <- lm(base_formula, data = data)
  m1 <- lm(chal_formula, data = data)
  a  <- anova(m0, m1)
  coefs <- coef(m1)[grepl(paste0("^", candidate), names(coef(m1)))]
  list(
    verdict = data.frame(
      candidate = candidate, n = nrow(data), df_added = a$Df[2],
      F_stat = round(a$F[2], 2), p_value = signif(a$`Pr(>F)`[2], 3)
    ),
    effect_sizes_pct_points = round(100 * coefs, 2)
  )
}

# ---- 4. edge-symmetry screen: the empirical case for/against 2D ------------
# Coefficients are relative to "bottom" (alphabetically first level).

screen_candidate("nearest_edge")

# ---- 5. THE FULL FIT -- launch and walk away -------------------------------
# Keep the laptop plugged in and awake (caffeinate -i in a Terminal tab).
# Post-fit checklist: Rhat <= 1.01, zero divergences, initial_call ~ +1 logit,
# sd(umpire) ~0.05-0.12 with tighter interval than pilot, smooth tracks the
# raw ring, mean fitted ~ observed rate.

t0 <- Sys.time()
fit_full <- brm(
  call_wrong ~ s(edge_dist) + initial_call + (1 | umpire),
  data   = model_data %>% filter(abs(edge_dist) <= 12),
  family = bernoulli(),
  chains = 4, cores = 4,
  seed   = 42,
  file   = "data/processed/fit_full_1d"   # auto-saves the moment sampling ends
)
Sys.time() - t0
