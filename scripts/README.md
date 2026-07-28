# Scripts

All analysis code lives in this directory.

- `functions/` contains the modular ingestion, parsing, geometry, inventory,
  modeling, valuation, summary, and validation functions sourced by
  `_targets.R`.
- `build_dashboard_assets.R` reads `data/processed/` and writes the compact
  website payload and static figures under `output/`.

`_targets.R` remains at the repository root so the `targets` package and common
editor integrations discover the pipeline automatically.
