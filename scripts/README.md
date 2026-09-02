# Scripts

Reusable code is grouped by workflow:

- `functions/core/` contains ingestion, parsing, geometry, inventory,
  modeling, valuation, summaries, and validation.
- `functions/perception/` contains the continuous-perception, common-width,
  and signal-integration models.
- `functions/policy/` contains revealed-selection, empirical-prior, and
  fixed-clock confirmation code.
- `load_functions.R` is the single recursive loader used by targets, runners,
  and tests.
- `stan/continuous_*.stan` contains the corresponding Stan programs. Compiled
  executables and raw draws are generated locally and ignored by Git.
- `build_dashboard_assets.R` reads `data/processed/` and writes the compact
  website payload and static figures under `output/`.

`_targets.R` remains at the repository root so the `targets` package and common
editor integrations discover the pipeline automatically.

The computationally heavy model graph is intentionally separate at
`_targets_perception.R`. Run `make perception-setup` once to configure the
pinned CmdStan toolchain, `make perception-pilot` for a one-fold diagnostic
build, and `make perception-full` for all five game-separated folds and primary
artifacts. `make perception-report` renders `report/perception.qmd` from the
completed full artifacts.

The workflow never gives tracked true location directly to the modeled decision
rule. It integrates the nonlinear action probability over a batter's possible
private signals, after conditioning on take behavior and optionally the public
initial-call cue. No rectangular spatial grid is used: Gaussian-mixture algebra
and adaptive Gauss--Hermite/Legendre quadrature operate on the exact rounded ABS
region. Official outcomes are evaluation-only leaf inputs. Dashboard assets and
the existing inventory MDP are deliberately outside v1.

The current replacement workflow is
`analysis/policy/run_revealed_challenge_policy_1d.R`, with modular functions in
`functions/policy/`. It models all inventory-available offense and defense
challenge/pass choices, uses a local common-probit width only as an effective
behavioral width, profiles sensory versus action noise instead of claiming to
identify either one, and values accepted scenarios with one chronological
shared team inventory. Run `make revealed-policy-pilot`, then
`make revealed-policy-full`; render the separate analysis with
`make revealed-policy-report`. This workflow does not write dashboard assets.

The production confirmation entry point is
`analysis/policy/run_fixed_clock_confirmation_1d.R`. Use
`make fixed-clock-smoke` for a bounded local check and `make fixed-clock-full`
for the complete analysis. Cluster launchers are under `hpc/fixed-clock/`.
