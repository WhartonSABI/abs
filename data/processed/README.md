# Derived data

This directory is populated by `make pipeline`. The Parquet and CSV files are
analysis-scale, reproducible products and are ignored by Git.

The canonical table is `pitch_ledger.parquet`, with one row per called pitch.
See [`../../docs/data-dictionary.md`](../../docs/data-dictionary.md) for the complete
contracts for the pitch ledger, challenge events, remaining opportunities,
summaries, intervals, and validation artifacts.

The normal pipeline also writes five information-safe inputs for the separate
continuous-perception workflow:

- `continuous_location_features.parquet`
- `continuous_swing_features.parquet`
- `continuous_call_features.parquet`
- `continuous_decision_features.parquet`
- `continuous_challenge_labels.parquet`

The first three are challenge-free model inputs. Challenge/pass actions first
appear in the decision table; official and geometry-implied success remain in
the quarantined labels table and enter only final held-out evaluation.

`make perception-pilot` or `make perception-full` writes the primary research
artifacts under `perception/`: `batter_perception_parameters.parquet`,
`human_decision_posteriors.parquet`, `validation_gates.csv`,
`posterior_diagnostics.csv`, and `model_manifest.json`. The pilot runs one of
the five game folds for diagnostics; only the full profile completes and scores
all five folds.

The current revealed-selection analysis is a separate, snapshot-locked
workflow. `make revealed-policy-pilot` checks one joint perception scenario;
`make revealed-policy-full` scores the full 25-scenario identified set and runs
2,000 game-cluster bootstrap replicates. It writes, without overwriting the
older continuous-model artifacts, under `perception/revealed_policy/`:

- `challenge_selection_oof.parquet`
- `challenge_margin_distributions.parquet`
- `perception_profile.parquet`
- `subjective_belief_envelope.parquet`
- `policy_thresholds.parquet`
- `policy_value_summary.csv`
- `model_manifest.json`

All seven carry the same current-snapshot identifier. Priors cannot receive
challenge actions or results, selection fits cannot receive results, and
official overturns are joined only for final validation after the policies are
frozen.

Do not hand-edit these files. Change the pipeline or reviewed configuration and
rebuild them.
