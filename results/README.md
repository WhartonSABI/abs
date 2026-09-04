# Fixed-clock applied results

This directory contains the compact applied-results bundle for the empirical
fixed-clock policy. The models and policy are fit on the 1,490-game schedule
through July 19, which contains 1,488 ABS-enabled games, and evaluated on 497
ABS-enabled games from July 20 through August 25. The bundle is restricted to
the final perception scenario,
`offense_kappa_1.00__defense_kappa_1.00`, and excludes checkpoints,
intermediate fits, and alternate-scenario replays.

The headline `direct_learning_procedure` policy captures 0.559503 modeled RE/game
on the held-out confirmation set, versus 0.459483 for observed decisions: a
gain of 0.100020 RE/game (95% CI [0.073751, 0.128128]). Its model-implied
success rate is 45.2716%. The interval comes from 5,000 whole-game resamples of
the 497-game confirmation set with the fitted policies held fixed. These are
modeled run-expectancy quantities, not realized runs caused by challenges.

| File | Unit | Purpose |
|---|---|---|
| `summary.csv` | Policy and role | Headline estimates for the final scenario |
| `sample_summary.csv` | Reporting metric | Sample counts used in the paper |
| `inning_policy_summary.csv` | Role and inning (10+ pooled) | Observed and learned rates on the same tracked called-pitch opportunity clock |
| `team_game_values.parquet` | Game, team, role, and policy | Team summaries, comparisons, and case-study rankings |
| `opportunity_replay.parquet` | Tracked adverse-call opportunity on the fixed clock | Final-policy inventory path, action probability, outcome, and captured RE |
| `policy_actions.parquet` | Tracked adverse-call opportunity on the fixed clock | Learned inventory losses, thresholds, and action probabilities |
| `bootstrap_draws.parquet` | Bootstrap replicate, policy, and role | Held-out fixed-policy whole-game draws |
| `bootstrap_intervals.csv` | Policy and role | Held-out fixed-policy 95% intervals |
| `effective_widths.csv` | Role | Estimated effective action-curve widths used by the scenario |
| `empirical_prior_alpha_selection.csv` | Role and candidate alpha | Cross-fitted empirical-prior shrinkage selection |
| `frozen_policy.rds` | Model object | Frozen direct-policy coefficients, nuisance models, and 25-by-25 development cross-evaluation |
| `split_games.csv` | Game | Development/confirmation assignment |
| `reporting_game_exclusions.csv` | Excluded game | Versioned reason for each non-ABS exclusion |
| `reporting_manifest.json` | Reporting bundle | Analysis scope, selected scenario, uncertainty, and provenance |
| `run_manifest.json` | Estimation run | Fitted-policy configuration, hashes, and sample provenance |

For team plots, start with `team_game_values.parquet`. For pitch-level case
studies or inventory-state plots, use `opportunity_replay.parquet`. The policy
to report as the primary result is `direct_learning_procedure`.

From the repository root, `make paper-figures` regenerates all manuscript
exhibits from this compact bundle without rerunning the learning pipeline.
