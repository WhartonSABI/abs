library(data.table)
library(ggplot2)
library(scales)

# Make sure the results are a data.table
decisions <- as.data.table(mdp_pitch_decisions)
state_values <- as.data.table(mdp_fit$state_values)



inning_summary <- decisions[
  ,
  .(
    challenge_rate = mean(policy_challenge),
    success_rate = mean(policy_success[policy_challenge]),
    zero_inventory_rate =
      mean(inventory_before_policy == 0L)
  ),
  by = .(
    inning_group = pmin(inning, 10L),
    role
  )
]

inning_summary[
  ,
  inning_label := factor(
    ifelse(inning_group == 10L, "10+", inning_group),
    levels = c(as.character(1:9), "10+")
  )
]

challenge_rate_plot <- ggplot(
  inning_summary,
  aes(
    x = inning_label,
    y = challenge_rate,
    color = role,
    group = role
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "MDP Challenge Rate by Inning",
    subtitle = "Percentage of opportunities where the policy recommends a challenge",
    x = "Inning",
    y = "Challenge rate",
    color = "Team role"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

challenge_rate_plot



zero_inventory_plot <- ggplot(
  inning_summary,
  aes(
    x = inning_label,
    y = zero_inventory_rate,
    color = role,
    group = role
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "How Often the Policy Has No Challenges Left",
    subtitle = "A higher value means the team used both challenges earlier",
    x = "Inning",
    y = "Opportunities with zero inventory",
    color = "Team role"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

zero_inventory_plot



real_rate <- mdp_opportunities %>%
  mutate(
    inning_grp = ifelse(
      inning >= 10,
      "10+",
      as.character(inning)
    ),
    inning_grp = factor(
      inning_grp,
      levels = c(as.character(1:9), "10+")
    ),
    observed_challenge =
      coalesce(challenge_occurred, FALSE) &
      (
        is.na(challenger_team_id) |
          challenger_team_id == team_id
      )
  ) %>%
  group_by(inning_grp, role) %>%
  summarise(
    rate = mean(observed_challenge),
    n = n(),
    .groups = "drop"
  )


ggplot(
  real_rate,
  aes(
    x = inning_grp,
    y = rate,
    color = role,
    group = role
  )
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Actual Challenge Rate by Inning",
    subtitle = "Percentage of opportunities where teams actually challenged",
    x = "Inning",
    y = "Actual challenge rate",
    color = "Team role"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


comparison_rate <- mdp_pitch_decisions %>%
  mutate(
    inning_grp = ifelse(
      inning >= 10,
      "10+",
      as.character(inning)
    ),
    inning_grp = factor(
      inning_grp,
      levels = c(as.character(1:9), "10+")
    ),

    # What teams actually did
    actual = coalesce(challenge_occurred, FALSE) &
      (
        is.na(challenger_team_id) |
          challenger_team_id == team_id
      ),

    # What our MDP recommends
    mdp = policy_challenge
  ) %>%
  group_by(inning_grp, role) %>%
  summarise(
    Actual = mean(actual),
    MDP = mean(mdp),
    opportunities = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Actual, MDP),
    names_to = "strategy",
    values_to = "challenge_rate"
  )

actual_vs_mdp_plot <- ggplot(
    comparison_rate,
    aes(
      x = inning_grp,
      y = challenge_rate,
      color = role,
      linetype = strategy,
      shape = strategy,
      group = interaction(role, strategy)
    )
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = percent_format()) +
  scale_linetype_manual(
    values = c(
      "Actual" = "dashed",
      "MDP" = "solid"
    )
  ) +
  labs(
    title = "Actual vs. MDP Challenge Rate",
    subtitle = "Solid lines are MDP recommendations; dashed lines are actual team decisions",
    x = "Inning",
    y = "Challenge rate",
    color = "Team role",
    linetype = "Decision source",
    shape = "Decision source"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

actual_vs_mdp_plot

decision_comparison <- mdp_pitch_decisions %>%
  mutate(
    actual_challenge =
      coalesce(challenge_occurred, FALSE) &
      (
        is.na(challenger_team_id) |
          challenger_team_id == team_id
      ),

    decision_group = case_when(
      actual_challenge & policy_challenge
      ~ "Both challenge",

      !actual_challenge & !policy_challenge
      ~ "Both wait",

      !actual_challenge & policy_challenge
      ~ "MDP only",

      actual_challenge & !policy_challenge
      ~ "Actual team only"
    )
  )


decision_group_summary <- decision_comparison %>%
  group_by(decision_group) %>%
  summarise(
    pitches = n(),
    wrong_call_rate = mean(actual_wrong),
    total_potential_re = sum(
      ifelse(actual_wrong, stake_G, 0),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

decision_group_summary



ggplot(
  decision_group_summary,
  aes(
    x = decision_group,
    y = wrong_call_rate,
    fill = decision_group
  )
) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = scales::percent(wrong_call_rate, accuracy = 0.1)),
    vjust = -0.4
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "How Often Was the Call Actually Wrong?",
    subtitle = "Comparing actual team decisions with MDP recommendations",
    x = NULL,
    y = "Wrong-call rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(face = "bold")
  )

value_comparison <- decision_comparison %>%
  summarise(
    actual_attempts = sum(actual_challenge),
    mdp_attempts = sum(policy_challenge),

    actual_successes =
      sum(actual_challenge & actual_wrong),

    mdp_successes =
      sum(policy_challenge & actual_wrong),

    actual_captured_re = sum(
      ifelse(
        actual_challenge & actual_wrong,
        stake_G,
        0
      ),
      na.rm = TRUE
    ),

    mdp_captured_re = sum(
      captured_re,
      na.rm = TRUE
    )
  ) %>%
  mutate(
    actual_re_per_attempt =
      actual_captured_re / actual_attempts,

    mdp_re_per_attempt =
      mdp_captured_re / mdp_attempts,

    mdp_re_advantage =
      mdp_captured_re - actual_captured_re
  )

value_comparison


missed_value <- decision_comparison %>%
  filter(
    policy_challenge,
    !actual_challenge,
    actual_wrong
  ) %>%
  summarise(
    missed_successful_challenges = n(),
    potential_re_missed = sum(stake_G),
    average_re_per_pitch = mean(stake_G)
  )

missed_value


calibration <- mdp_pitch_decisions %>%
  mutate(
    probability_group = ntile(p_hat, 10)
  ) %>%
  group_by(probability_group) %>%
  summarise(
    predicted_probability = mean(p_hat),
    actual_wrong_rate = mean(actual_wrong),
    pitches = n(),
    .groups = "drop"
  )

ggplot(
  calibration,
  aes(
    x = predicted_probability,
    y = actual_wrong_rate
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "gray40"
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Are the Bayesian Probabilities Accurate?",
    subtitle = "Points near the dashed line indicate accurate probabilities",
    x = "Average predicted probability",
    y = "Actual wrong-call rate"
  ) +
  theme_minimal(base_size = 13)



