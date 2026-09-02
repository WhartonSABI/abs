# Scripts

Reusable code is grouped by responsibility:

- `functions/core/` contains ingestion, parsing, geometry, inventory,
  modeling, valuation, summaries, and validation.
- `functions/perception/` contains the challenge-discrimination,
  empirical-margin, signal-integration, and fixed-clock context helpers.
- `functions/policy/` contains challenge selection, empirical priors, Bellman
  evaluation, confirmation analysis, and coordinated bootstrap code.
- `load_functions.R` recursively loads those three function groups for
  targets, production runners, and tests.
- `stan/hierarchical_challenge_discrimination_1d.stan` is the
  hierarchical discrimination model. Compiled executables and draws are local
  generated artifacts and are ignored by Git.
- `build_dashboard_assets.R` reads `data/processed/` and writes the compact
  website payload and static figures under `output/`.

`_targets.R` remains at the repository root so `targets` and editor integrations
discover the acquisition and valuation pipeline automatically.

The production policy entry point is
`analysis/policy/run_fixed_clock_confirmation_1d.R`. Use
`make fixed-clock-smoke` for a bounded local check and `make fixed-clock-full`
for the complete empirical fixed-clock analysis. The corresponding production
cluster launcher is under `hpc/fixed-clock/`.
