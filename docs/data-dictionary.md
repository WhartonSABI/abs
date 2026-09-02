# Data dictionary

## `pitch_ledger.parquet`

One row per called pitch, keyed by `game_pk`, `at_bat_number`, and
`pitch_number`. Important fields include:

- `play_id`: MLB live-feed pitch identifier.
- `initial_call`, `final_call`, `abs_call`: on-field original, post-review, and
  geometry-implied calls.
- `challenge_occurred`, `challenge_outcome`: challenge flag and explicit
  `overturned`/`upheld` result.
- `linkage_source`: `pitch_event` or `plate_appearance_terminal`; composite
  reviews nested under `additionalReviews` are also recovered.
- `statcast_identity_matched`: Statcast batter/pitcher identities agree with
  the live-feed pitch key; mismatches remain in the coverage denominator but
  are not used to infer geometry.
- `*_challenges_before/after`: sequentially reconstructed inventory.
- `edge_distance_inches`: signed ball-edge distance; non-positive is a strike.
- `correctable_opportunity`, `opportunity_status`: wrong adverse call and its
  challenged/missed/out-of-challenges classification.
- `potential_challenger_re`, `potential_challenger_wpa`: value of correction to
  the adversely affected team.

## `challenge_events.parquet`

One row per resolved challenge, including role, original/final call, outcome,
immediate value, inventory before/after, marginal failed-challenge cost, and
Savant benchmark fields.
`publication_eligible` is true only for the 8,537 challenges in the official
Savant analytical set. One live-feed-only ruling remains in sequential
inventory reconstruction and is explicitly quarantined from published rates.

## `remaining_opportunities.parquet`

Two rows per called pitch (one per team). Contains realized and leave-one-game-
out expected future correctable counts/RE/WPA, offense/defense splits, modeled
challenge propensity, reachable value at inventories 0/1/2, and marginal
2-to-1 and 1-to-0 inventory value.

## Fixed-clock confirmation outputs

The production workflow writes immutable run directories under
`data/processed/perception/fixed_clock_confirmation/runs/`. Each completed run
contains frozen direct and Bellman policies, confirmation-set value summaries,
selection and empirical-margin diagnostics, coordinated bootstrap intervals,
and manifests that record the data split, code hashes, settings, and scenario
grid. Intermediate checkpoints support exact restart of long cluster runs.

## Validation and interval artifacts

- `validation_checks.csv`, `coverage.csv`, and `history_coverage.csv` contain
  the publication gates.
- `model_validation.csv` contains the 2025 holdout diagnostics and the
  model-to-Savant overturn comparison.
- `team_intervals.parquet` and `adjusted_intervals.parquet` contain 500-
  replicate game-cluster 95% intervals, including start-of-game inventory
  quantities and opportunity-adjusted challenge residuals.
- `data/reference/manual_audit.csv` records the 50-event stratified review set.
