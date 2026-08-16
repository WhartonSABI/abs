# Decision engine: challenge EV, breakeven, limited-challenge simulation.
# Functions only -- the runs and plots live in analysis/decision_engine_sims.R.
# Every Monte Carlo result here was validated against its closed-form answer.

break_even_probability <- function(gain, loss) {
  stopifnot(gain >= 0, loss >= 0, gain + loss > 0)
  loss / (gain + loss)
}

simulate_single_challenge <- function(p, gain, loss, n_sims) {
  flips   <- rbinom(n_sims, size = 1, prob = p)
  payoffs <- ifelse(flips == 1, gain, -loss)
  mean(payoffs)
}

sweep_challenge_probability <- function(p_grid, gain, loss, n_sims) {
  data.frame(
    p        = p_grid,
    ev_sim   = sapply(p_grid, simulate_single_challenge,
                      gain = gain, loss = loss, n_sims = n_sims),
    ev_exact = p_grid * gain - (1 - p_grid) * loss
  )
}

# One game with limited challenges: win = keep the challenge, loss = burn it.
simulate_challenge_game <- function(p_values, threshold, gain, loss,
                                    n_challenges = 2) {
  remaining <- n_challenges
  total_runs <- 0
  n_used <- 0

  for (p in p_values) {
    if (remaining > 0 && p > threshold) {
      n_used <- n_used + 1
      success <- rbinom(1, size = 1, prob = p) == 1
      if (success) {
        total_runs <- total_runs + gain
      } else {
        total_runs <- total_runs - loss
        remaining <- remaining - 1
      }
    }
  }

  data.frame(total_runs = total_runs,
             challenges_used = n_used,
             challenges_left = remaining)
}

average_game_value <- function(threshold, gain, loss,
                               n_games, n_opps = 15) {
  per_game <- replicate(n_games, {
    p_values <- rbeta(n_opps, shape1 = 1, shape2 = 4)
    simulate_challenge_game(p_values, threshold, gain, loss)$total_runs
  })
  mean(per_game)
}

# One guaranteed high-p opportunity at a random spot in the game
average_game_value_jackpot <- function(threshold, gain, loss,
                                       n_games, n_opps = 15) {
  per_game <- replicate(n_games, {
    p_values <- rbeta(n_opps, shape1 = 1, shape2 = 4)
    p_values[sample(n_opps, 1)] <- 0.9
    simulate_challenge_game(p_values, threshold, gain, loss)$total_runs
  })
  mean(per_game)
}

# Same jackpot but always first -- isolates the timing mechanism
average_game_value_jackpot_first <- function(threshold, gain, loss,
                                             n_games, n_opps = 15) {
  per_game <- replicate(n_games, {
    p_values <- rbeta(n_opps, shape1 = 1, shape2 = 4)
    p_values[1] <- 0.9
    simulate_challenge_game(p_values, threshold, gain, loss)$total_runs
  })
  mean(per_game)
}

# Outcomes flipped once, shared between policy and oracle -- fair comparison
play_game_with_outcomes <- function(p_values, outcomes, threshold,
                                    gain, loss, n_challenges = 2) {
  remaining <- n_challenges
  total <- 0
  for (i in seq_along(p_values)) {
    if (remaining > 0 && p_values[i] > threshold) {
      if (outcomes[i] == 1) {
        total <- total + gain
      } else {
        total <- total - loss
        remaining <- remaining - 1
      }
    }
  }
  total
}

compare_to_oracle <- function(threshold, gain, loss,
                              n_games, n_opps = 15) {
  policy_runs <- numeric(n_games)
  oracle_runs <- numeric(n_games)
  for (g in seq_len(n_games)) {
    p_values <- rbeta(n_opps, shape1 = 1, shape2 = 4)
    outcomes <- rbinom(n_opps, size = 1, prob = p_values)
    policy_runs[g] <- play_game_with_outcomes(p_values, outcomes,
                                              threshold, gain, loss)
    oracle_runs[g] <- gain * sum(outcomes)
  }
  data.frame(threshold      = threshold,
             policy_avg     = mean(policy_runs),
             oracle_avg     = mean(oracle_runs),
             pct_of_oracle  = mean(policy_runs) / mean(oracle_runs))
}
