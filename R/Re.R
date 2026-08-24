
ledger %>%
  filter(tracking_available) %>%
  summarise(n = n(),
            na_re = mean(is.na(potential_challenger_re)),
            mean_re = mean(potential_challenger_re, na.rm = TRUE),
            q = list(quantile(potential_challenger_re, c(.01,.5,.99), na.rm = TRUE)))

ledger %>%
  filter(tracking_available) %>%
  group_by(balls_before, strikes_before) %>%
  summarise(mean_stake = mean(potential_challenger_re, na.rm = TRUE), .groups="drop") %>%
  arrange(desc(mean_stake))   # 3-2 and 0-2/3-0 should top the table

source("scripts/functions/valuation.R")
source("scripts/functions/utils.R")
source("scripts/functions/modeling.R")
source("scripts/functions/valuation.R")

library(dplyr); library(tidyr); library(brms); library(data.table)

# the fits (load, don't refit) and the run-expectancy table
fit_strikes <- readRDS("data/processed/fit_car_strikes_final.rds")
fit_balls   <- readRDS("data/processed/fit_car_balls_final.rds")
re_model    <- targets::tar_read(re_model)

# the function files the stakes calc needs
invisible(lapply(list.files("scripts/functions", full.names = TRUE), source))
re_model <- targets::tar_read(re_model)

add_decision_stakes <- function(ledger, re_model) {
  x <- data.table::as.data.table(ledger)
  ok <- which(x$initial_call %in% c("ball", "called_strike") &
                !is.na(x$balls_before) & !is.na(x$strikes_before) &
                !is.na(x$outs_before))
  rows <- x[ok]
  flipped_call <- ifelse(rows$initial_call == "ball", "called_strike", "ball")
  stands  <- vectorized_call_branch(rows, rows$initial_call)
  flipped <- vectorized_call_branch(rows, flipped_call)
  batting_gain <- vectorized_branch_re(re_model$table, flipped) -
    vectorized_branch_re(re_model$table, stands)
  x[, stake_G := NA_real_]
  x$stake_G[ok] <- ifelse(rows$adverse_team_id == rows$bat_team_id,
                          batting_gain, -batting_gain)
  x[]
}

ledger <- add_decision_stakes(ledger, re_model)

# ---- audits, with predictions on the record ----
mean(is.na(ledger$stake_G[ledger$tracking_available]))
summary(ledger$stake_G)
# consistency: on wrong calls, flipping == correcting, so stake_G must
# reproduce the pipeline's potential_challenger_re EXACTLY
chk <- ledger %>% filter(correctable_opportunity, !is.na(potential_challenger_re))
max(abs(chk$stake_G - chk$potential_challenger_re))      # want exactly 0



# ---- block 1: score every pitch with p-hat from the matching final fit ----
score_fit <- function(fit) {
  d <- fit$data
  d$p_hat <- fitted(fit, ndraws = 500)[, "Estimate"] / d$n
  d %>%
    mutate(across(c(cell, umpire, catcher), as.character)) %>%
    select(cell, umpire, catcher, p_hat) %>% distinct()
}
p_strikes <- score_fit(fit_strikes)  %>% mutate(initial_call = "called_strike")
p_balls   <- score_fit(fit_balls)    %>% mutate(initial_call = "ball")

scored <- ledger %>%
  filter(tracking_available, abs(edge_distance_inches) <= 12) %>%
  mutate(x_in = plate_x * 12,
         z_in = (plate_z - sz_bot) / (sz_top - sz_bot) * 20,
         cell = paste(round(x_in / 1.5), round(z_in / 1.5), sep = "_"),
         umpire  = as.character(umpire_name),
         catcher = as.character(fielder_2)) %>%
  left_join(bind_rows(mutate(p_strikes, initial_call = "called_strike"),
                      mutate(p_balls,   initial_call = "ball")),
            by = c("cell", "umpire", "catcher", "initial_call"))

# audits, predictions on record:
mean(is.na(scored$p_hat))
mean(scored$p_hat, na.rm = TRUE)
cor(scored$p_hat,
    as.integer(scored$initial_call != scored$abs_call),
    use = "complete.obs")
set.seed(42)
samp <- scored %>%
  filter(!is.na(p_hat)) %>%
  group_by(initial_call) %>% slice_sample(n = 25000) %>% ungroup() %>%
  mutate(call = ifelse(initial_call == "ball", "called ball", "called strike"))

ggplot(samp, aes(x_in, z_in, color = p_hat)) +
  geom_point(size = 0.35, alpha = 0.55) +
  annotate("rect", xmin = -8.5, xmax = 8.5, ymin = 0, ymax = 20,
           color = "black", fill = NA, linewidth = 0.6) +
  scale_color_gradientn(colors = c("grey85", "gold", "orangered", "red4"),
                        values = c(0, 0.15, 0.5, 1),
                        labels = scales::percent, name = "p-hat\nP(call wrong)") +
  facet_wrap(~call) +
  coord_equal() + theme_minimal(base_size = 13) +
  labs(x = "inches from plate center", y = "zone-relative height (in)",
       title = "Every pitch scored: model probability the call was wrong",
       subtitle = "25,000 sampled pitches per panel; black box = rulebook zone")

set.seed(42)
samp <- scored %>%
  filter(!is.na(p_hat),
         abs(x_in) <= 13, z_in >= -4, z_in <= 24) %>%   # keep only near-zone pitches
  group_by(initial_call) %>% slice_sample(n = 25000) %>% ungroup() %>%
  mutate(call = ifelse(initial_call == "ball", "called ball", "called strike"))

ggplot(samp, aes(x_in, z_in, color = p_hat)) +
  geom_point(size = 0.5, alpha = 0.6) +
  annotate("rect", xmin = -8.5, xmax = 8.5, ymin = 0, ymax = 20,
           color = "black", fill = NA, linewidth = 0.6) +
  scale_color_gradientn(colors = c("grey85", "gold", "orangered", "red4"),
                        values = c(0, 0.15, 0.5, 1),
                        labels = scales::percent, name = "p-hat\nP(call wrong)") +
  facet_wrap(~call) +
  coord_equal(xlim = c(-13, 13), ylim = c(-4, 24)) +
  theme_minimal(base_size = 13) +
  labs(x = "inches from plate center", y = "zone-relative height (in)",
       title = "Model probability the call was wrong — near-zone view",
       subtitle = "black box = rulebook zone (ball-center); colors shift ~1.4\" outside it = ball radius")

truth <- scored %>%
  mutate(true_strike = as.integer(abs_call == "called_strike"),
         x_bin = round(x_in / 1.5) * 1.5,
         z_bin = round(z_in / 1.5) * 1.5) %>%
  group_by(x_bin, z_bin) %>%
  summarise(n = n(), p_strike = mean(true_strike), .groups = "drop") %>%
  filter(n >= 15)

ggplot(truth, aes(x_bin, z_bin, fill = p_strike)) +
  geom_tile(color = "black", linewidth = 0.15,
            width = 1.5, height = 1.5) +
  annotate("rect", xmin = -8.5, xmax = 8.5, ymin = 0, ymax = 20,
           color = "black", fill = NA, linewidth = 0.8) +
  scale_fill_gradient(low = "grey95", high = "dodgerblue4",
                      labels = scales::percent, name = "share that are\ntrue strikes (ABS)") +
  coord_equal(xlim = c(-13, 13), ylim = c(-4, 24)) +
  theme_minimal(base_size = 13) +
  labs(x = "inches from plate center", y = "zone-relative height (in)",
       title = "Proportion of called pitches that are true strikes, by location",
       subtitle = "each outlined square = one 1.5\" model cell; n < 15 hidden")


opps <- scored %>%
  filter(!is.na(p_hat), !is.na(stake_G), !is.na(adverse_team_id)) %>%
  transmute(
    game_pk,
    team = adverse_team_id,
    inning, is_top, outs_before,
    outs_elapsed = pmin((inning - 1L) * 6L +
                          ifelse(is_top, 0L, 3L) + outs_before, 53L),
    p_hat, stake_G,
    ev0 = p_hat * stake_G,
    challenge_occurred, challenge_outcome
  ) %>%
  arrange(game_pk, team, outs_elapsed)

opps %>% count(game_pk, team) %>%
  summarise(mean_opps = mean(n), median_opps = median(n))

summary(opps$ev0)
mean(opps$p_hat > 0.5)
mean(opps$ev0 > 0.05)


opps %>% group_by(game_pk, team) %>%
  summarise(best_ev0 = max(ev0), n_good = sum(p_hat > 0.5), .groups = "drop") %>%
  summarise(mean_best = mean(best_ev0), mean_good = mean(n_good),median_good = median(n_good))



opps %>% group_by(game_pk, team) %>%
  summarise(n_strong = sum(p_hat > 0.8),
            n_valuable = sum(ev0 > 0.05),
            n_both = sum(p_hat > 0.8 & ev0 > 0.05), .groups = "drop") %>%
  summarise(across(everything(), mean))

opps %>% count(game_pk, team) %>%
  summarise(mean_pitches = mean(n), median_pitches = median(n))




# one ordered opportunity stream per team-game
streams <- opps %>%
  group_by(game_pk, team) %>%
  group_split() %>%
  lapply(function(d) d[order(d$outs_elapsed),
                       c("p_hat", "stake_G", "ev0", "outs_elapsed")])

length(streams)

# play ONE stream: challenge every opportunity above threshold tau,
# until you run out of challenges. Returns runs captured.
play_stream <- function(stream, tau, k) {
  remaining <- k
  captured  <- 0
  for (i in seq_len(nrow(stream))) {
    if (remaining > 0 && stream$ev0[i] > tau) {      # worth challenging & have one
      if (runif(1) < stream$p_hat[i]) {
        captured <- captured + stream$stake_G[i]      # WIN: bank the prize, keep challenge
      } else {
        remaining <- remaining - 1                    # LOSE: burn a challenge
      }
    }
  }
  captured
}

# quick test on one stream
set.seed(42)
play_stream(streams[[1]], tau = 0.03, k = 2)
V_tail <- function(o, k, tau = 0.05, reps = 15) {
  tails <- lapply(streams, function(s) s[s$outs_elapsed >= o, , drop = FALSE])
  vals  <- replicate(reps, mean(vapply(tails, play_stream, numeric(1), tau = tau, k = k)))
  mean(vals)
}

set.seed(42)
outs_grid <- seq(0, 48, by = 3)
decay <- data.frame(out = outs_grid)
decay$V1 <- sapply(outs_grid, V_tail, k = 1)
decay$V2 <- sapply(outs_grid, V_tail, k = 2)
decay$L_last  <- decay$V1
decay$L_extra <- decay$V2 - decay$V1
decay$outs_remaining <- 54 - decay$out

decay



# average runs captured across ALL streams, for a given (tau, k)
V_of <- function(tau, k, reps = 20) {
  vals <- replicate(reps,
                    mean(vapply(streams, play_stream, numeric(1), tau = tau, k = k)))
  mean(vals)
}

set.seed(42)
grid <- expand.grid(tau = seq(0, 0.20, by = 0.01), k = c(1, 2))
grid$V <- mapply(function(t, k) V_of(t, k), grid$tau, grid$k)

wide <- grid %>%
  pivot_wider(names_from = k, values_from = V, names_prefix = "V")
wide$L_second <- wide$V2 - wide$V1     # value of the SECOND challenge
wide



library(ggplot2)

sweep_long <- wide %>%
  select(tau, V1, V2) %>%
  pivot_longer(c(V1, V2), names_to = "held", values_to = "value") %>%
  mutate(held = ifelse(held == "V1", "1 challenge", "2 challenges"))

ggplot(sweep_long, aes(tau, value, color = held)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.6) +
  labs(x = "challenge threshold \u03c4  (min expected runs to bother challenging)",
       y = "runs captured per game",
       title = "How picky should a team be?",
       subtitle = "Value peaks at a modest bar; challenging everything (\u03c4 = 0) captures almost nothing",
       color = NULL) +
  theme_minimal(base_size = 13)


library(ggplot2)
decay_long <- decay %>%
  select(outs_remaining, L_last, L_extra) %>%
  pivot_longer(c(L_last, L_extra), names_to = "situation", values_to = "L") %>%
  mutate(situation = ifelse(situation == "L_last",
                            "spending your LAST challenge",
                            "spending one of TWO"))

ggplot(decay_long, aes(outs_remaining, L, color = situation)) +
  geom_line(linewidth = 1.1) + geom_point(size = 1.6) +
  scale_x_reverse() +                     # game flows left (start) -> right (end)
  labs(x = "outs remaining in the game",
       y = "L  =  value of the challenge you'd spend (runs)",
       title = "Use it or lose it: a challenge is worth less as the game ends",
       subtitle = "L falls toward zero \u2014 so the optimal challenge bar should drop late",
       color = NULL) +
  theme_minimal(base_size = 13)


stakes <- c(low = 0.10, medium = 0.30, high = 0.60)   # representative prize sizes (runs)

thr <- do.call(rbind, lapply(names(stakes), function(nm) {
  G <- stakes[[nm]]
  data.frame(outs_remaining = decay$outs_remaining,
             p_star = decay$L_last / (G + decay$L_last),
             stakes = sprintf("%s stakes (G = %.1f)", nm, G))
}))

ggplot(thr, aes(outs_remaining, 100 * p_star, color = stakes)) +
  geom_line(linewidth = 1.1) + geom_point(size = 1.5) +
  scale_x_reverse() +
  labs(x = "outs remaining in the game",
       y = "challenge bar: minimum % chance the call is wrong",
       title = "The optimal challenge bar drops as the game ends",
       subtitle = "Down to your last challenge: picky early, loose late \u2014 and always looser when stakes are high",
       color = NULL) +
  theme_minimal(base_size = 13)




source("analysis/run_fixed_probability_mdp.R")
