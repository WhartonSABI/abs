# Fixed-clock applied results

This directory contains the compact applied-results bundle from production run
`full_97d9d9f3281fc735580d`. It is restricted to the final perception scenario,
`offense_kappa_1.00__defense_kappa_1.00`, and excludes checkpoints,
intermediate fits, and alternate scenarios.

The headline `robust_signal_assisted` result on the 499-game held-out
confirmation sample is 0.5580286 modeled RE captured per game with a 45.2206%
success rate. These are modeled run-expectancy quantities, not realized runs
caused by challenges.

| File | Unit | Purpose |
|---|---|---|
| `summary.csv` | Policy and role | Headline estimates for the final scenario |
| `team_game_values.parquet` | Game, team, role, and policy | Team summaries, comparisons, and case-study rankings |
| `opportunity_replay.parquet` | ABS-eligible adverse-call opportunity | Final-policy inventory path, action probability, outcome, and captured RE |
| `policy_actions.parquet` | ABS-eligible adverse-call opportunity | Learned inventory losses, thresholds, and action probabilities |
| `bootstrap_draws.parquet` | Bootstrap replicate, policy, and role | Cluster-bootstrap uncertainty draws |
| `bootstrap_intervals.csv` | Policy and role | Pointwise 95% bootstrap intervals |
| `effective_widths.csv` | Role | Estimated perception-error widths used by the scenario |
| `frozen_policy.rds` | Model object | Frozen direct-policy coefficients and nuisance models |
| `split_games.csv` | Game | Development/confirmation assignment |
| `run_manifest.json` | Run | Configuration, hashes, sample sizes, and provenance |

For team plots, start with `team_game_values.parquet`. For pitch-level case
studies or inventory-state plots, use `opportunity_replay.parquet`. The policy
to report as the primary result is `robust_signal_assisted`.
