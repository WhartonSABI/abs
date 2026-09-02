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

## Continuous-perception source tables

The normal pipeline writes five purpose-specific tables for the separate
`_targets_perception.R` workflow. Their schemas enforce the model's information
boundaries rather than exposing the full pitch ledger to every fit.

### `continuous_location_features.parquet`

One row per tracked pitch with physical location in inches relative to the
batter-specific zone center, exact zone dimensions, count, pitch family,
handedness matchup, and pitcher identity. It supplies the contextual continuous
pitch-location prior. It contains no initial call, challenge action, `abs_call`,
or outcome.

### `continuous_swing_features.parquet`

One row per eligible swing/take observation near the zone boundary. It contains
the pre-call swing indicator, exact signed edge distance, physical location,
batter and pitcher identities, count, pitch family, and handedness context. It
estimates batter-specific localization width from swing/take behavior only;
challenge data cannot update that width.

### `continuous_call_features.parquet`

One row per eligible taken pitch with the initial ball/strike call, continuous
location and signed-distance features, count and matchup context, and
umpire/catcher identities. It fits the public initial-call cue. It excludes
`abs_call`, challenge actions, final calls, and overturn outcomes.

### `continuous_decision_features.parquet`

One row per eligible batter decision after an initial called strike, with the
challenge/pass action, tracked location used only to integrate over possible
private signals, the hypothetical strike-to-ball run-value gain computed for
every eligible call without consulting ABS truth, and a failed-challenge
inventory cost derived from a Poisson opportunity-value approximation using
leave-one-game-out future correctable counts and run value. It also retains
timing/leverage context. This is the first table allowed to expose a challenge
action. It contains no official or geometry-implied outcome.

### `continuous_challenge_labels.parquet`

One row per observed batter challenge with official overturn success and the
geometry-implied success indicator. This quarantined leaf table is joined only
after all perception and decision fits are complete, for held-out evaluation of
the choice-conditioned subjective probability.

## Continuous-perception outputs

The heavy workflow writes the following under `data/processed/perception/`:

- `batter_perception_parameters.parquet`: batter exposure, posterior summaries
  of localization width in inches, the shared-anisotropy gate result, and
  partial-pooling fallback status.
- `human_decision_posteriors.parquet`: game-cross-fitted private-signal,
  call-updated, and choice-conditioned beliefs; predicted challenge
  probabilities and intervals; trust variant; quadrature diagnostics; and
  official outcomes attached only for final held-out evaluation.
- `validation_gates.csv`: fail-closed numerical, recovery, held-out prediction,
  identifiability, five-fold, and outcome-calibration gates.
- `posterior_diagnostics.csv`: compact sampler diagnostics by fold and model.
- `model_manifest.json`: priors and model version, mixture selection, folds,
  preserved draw alignment, call-trust and anisotropy gates, seeds, software
  versions, Git SHA, and the outcome-quarantine declaration.

These probabilities are model-implied beliefs under an unobserved private
location signal. They are not tracking-error estimates or direct measurements
of eyesight. Results are cross-validated and exploratory because no untouched
temporal confirmation set is reserved. A failed outcome-calibration gate blocks
the phrase "calibrated player probability" but not reporting the model-implied
subjective belief.

## Validation and interval artifacts

- `validation_checks.csv`, `coverage.csv`, and `history_coverage.csv` contain
  the publication gates.
- `model_validation.csv` contains the 2025 holdout diagnostics and the
  model-to-Savant overturn comparison.
- `team_intervals.parquet` and `adjusted_intervals.parquet` contain 500-
  replicate game-cluster 95% intervals, including start-of-game inventory
  quantities and opportunity-adjusted challenge residuals.
- `data/reference/manual_audit.csv` records the 50-event stratified review set.
