# Step 1-2 experiments: EV vs confidence, threshold sweeps, jackpot timing,
# oracle comparison. All inputs here are placeholders (gain = 0.2, loss = 0.02);
# real stakes come from RE288 at step 5.
# Key results: naive breakeven ~9%; scarcity raises the optimal bar to ~30%;
# the mechanism is unpredictable timing of big opportunities; tuned threshold
# captures ~34% of oracle vs ~20% naive.

library(ggplot2)
source("R/decision_engine.R")

# ---- single-challenge EV: sim converges to the algebra ----------------------

set.seed(42)
simulate_single_challenge(p = 0.5, gain = 0.2, loss = 0.02, n_sims = 100000)

set.seed(42)
sweep <- sweep_challenge_probability(
  p_grid = seq(0, 1, by = 0.02),
  gain = 0.2, loss = 0.02, n_sims = 10000
)

ggplot(sweep, aes(x = p)) +
  geom_line(aes(y = ev_exact), linewidth = 1) +
  geom_point(aes(y = ev_sim), color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = break_even_probability(0.2, 0.02),
             color = "red", linetype = "dotted") +
  labs(x = "p — probability the call was wrong",
       y = "EV of challenging (runs)",
       title = "Challenge EV vs confidence, sim (dots) vs algebra (line)")

# ---- one game, limited challenges ------------------------------------------

set.seed(42)
p_values <- rbeta(15, shape1 = 1, shape2 = 4)   # mostly small, occasionally big
round(p_values, 2)

set.seed(42)
simulate_challenge_game(p_values, threshold = 0.0909,
                        gain = 0.2, loss = 0.02)

# ---- where should the challenge bar sit? -----------------------------------

set.seed(42)
threshold_sweep <- data.frame(threshold = seq(0, 0.60, by = 0.02))
threshold_sweep$avg_runs <- sapply(
  threshold_sweep$threshold, average_game_value,
  gain = 0.2, loss = 0.02, n_games = 2000
)

ggplot(threshold_sweep, aes(threshold, avg_runs)) +
  geom_line() +
  geom_vline(xintercept = 0.0909, color = "red", linetype = "dotted") +
  labs(x = "challenge threshold on p",
       y = "average runs per game (2000 games)",
       title = "Where should a team set its challenge bar?")

# ---- scarcity: the jackpot experiments -------------------------------------

set.seed(42)
jackpot_sweep <- data.frame(threshold = seq(0, 0.60, by = 0.02))
jackpot_sweep$avg_runs <- sapply(
  jackpot_sweep$threshold, average_game_value_jackpot,
  gain = 0.2, loss = 0.02, n_games = 2000
)

threshold_sweep$world <- "normal"
jackpot_sweep$world   <- "jackpot"
both <- rbind(threshold_sweep, jackpot_sweep)

ggplot(both, aes(threshold, avg_runs, color = world)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0.0909, linetype = "dotted") +
  labs(x = "challenge threshold on p",
       y = "average runs per game",
       title = "Scarcity moves the optimal challenge bar")

jackpot_sweep[which.max(jackpot_sweep$avg_runs), ]

# Timing isolation: jackpot always first, so no risk of having spent challenges
set.seed(42)
first_sweep <- data.frame(threshold = seq(0, 0.60, by = 0.02))
first_sweep$avg_runs <- sapply(
  first_sweep$threshold, average_game_value_jackpot_first,
  gain = 0.2, loss = 0.02, n_games = 2000
)
first_sweep$world <- "jackpot_first"

all_three <- rbind(threshold_sweep, jackpot_sweep, first_sweep)

ggplot(all_three, aes(threshold, avg_runs, color = world)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0.0909, linetype = "dotted") +
  labs(title = "Timing of the jackpot moves the bar",
       x = "challenge threshold on p", y = "average runs per game")

# ---- how much of perfect play does a threshold capture? --------------------

set.seed(42)
compare_to_oracle(threshold = 0.25, gain = 0.2, loss = 0.02, n_games = 5000)

set.seed(42)
oracle_grid <- do.call(rbind, lapply(
  seq(0, 0.60, by = 0.05),
  compare_to_oracle, gain = 0.2, loss = 0.02, n_games = 5000
))
oracle_grid
