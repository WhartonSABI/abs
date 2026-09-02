# MLB ABS Challenge Run Value

This project measures the value created or lost by MLB automated ball-strike
(ABS) challenge decisions. It reconstructs every team's challenge inventory,
identifies calls that public ABS geometry says were correctable, and values the
actual and counterfactual decisions in run-expectancy and win-probability units.

The default research snapshot contains completed 2026 MLB regular-season games
through **August 25, 2026**, the latest fully published game day as of August 26.

## Current snapshot

| Quantity | Count |
|---|---:|
| Completed games | 1,989 |
| Called pitches | 292,381 |
| Parsed challenges with explicit outcomes | 8,538 |
| Challenges in the official Savant publication set | 8,537 |
| Geometry-inferred correctable calls | 19,757 |
| Correctable calls challenged | 4,578 |
| Correctable calls missed with inventory | 13,751 |
| Correctable calls encountered at zero inventory | 1,428 |

The pitch ledger is the canonical analysis table. It retains unsuccessful
challenges as explicit `upheld` outcomes; a JSON `false` outcome is never
treated as missing. Unchallenged outcomes are inferred from validated public
geometry, not official rulings.

## Repository map

```text
analysis/core/             Core descriptive and legacy MDP entry points
analysis/perception/       Perception-model experiments and diagnostics
analysis/policy/           Revealed-policy and fixed-clock production runners
scripts/functions/core/    Acquisition, geometry, inventory, and valuation
scripts/functions/perception/ Perception and signal-integration models
scripts/functions/policy/  Revealed-policy and fixed-clock implementation
scripts/stan/              Continuous-perception Stan programs
scripts/                   Standalone build scripts and script documentation
hpc/fixed-clock/           Portable Slurm launchers and environment file
config/                    Frozen executable project configuration
data/raw/                  Downloaded MLB/Savant inputs; ignored by Git
data/processed/            Generated analysis tables; ignored by Git
data/reference/            Reviewed exclusions, quarantine, and manual audit
data/fixtures/             Compact, versioned audit fixtures
docs/                      Acquisition, methodology, and data contracts
output/                    Versioned dashboard data, figures, and PDF
report/                    Quarto report source
tests/testthat/            Unit tests
_targets.R                 Reproducible pipeline graph
_targets_perception.R      Heavy continuous human-perception model graph
renv.lock                  Frozen R dependency versions
abs.Rproj                  RStudio project entry point
```

Additional directory-specific notes live in
[`config/README.md`](config/README.md),
[`data/README.md`](data/README.md),
[`scripts/README.md`](scripts/README.md), and
[`output/README.md`](output/README.md).

## Setup

Requirements:

- R 4.3 or newer
- Quarto, including a TeX installation if the PDF report is needed
- `make`

Clone the repository, create the local environment file, and restore the locked
R dependencies:

```sh
git clone https://github.com/WhartonSABI/abs.git
cd abs
cp .Renviron.example .Renviron
make install
```

`.Renviron` is intentionally ignored because it is machine-specific and may
eventually contain credentials. The committed example configures the local
`renv` library and cache used by this project.

## Main workflows

```sh
make test       # Run the unit suite
make pipeline   # Build or update the targets pipeline
make report     # Render the Quarto HTML and PDF report
make dashboard  # Build dashboard JSON and eight static figures
make outputs    # Pipeline, report, and dashboard
```

The continuous human-perception model is intentionally separate from the core
publication build:

```sh
make perception-setup   # Install/configure the pinned CmdStan toolchain
make perception-pilot   # One-fold diagnostic profile
make perception-full    # All five game-separated folds
make perception-report  # Render the exploratory perception report
```

The perception-aware challenge policy has two increasingly expensive entry
points:

```sh
make revealed-policy-pilot  # Fast cross-fitted workflow check
make fixed-clock-smoke      # Bounded end-to-end confirmation check
make fixed-clock-full       # Full fixed-clock policy and bootstrap analysis
```

The full fixed-clock run is designed for parallel compute. Portable Slurm
launchers live under `hpc/fixed-clock/`; submit them from the project root or
set `ABS_PROJECT_DIR` and, optionally, `ABS_ENVIRONMENT_DIR`.

It models a batter acting on one noisy private location signal, not on tracked
true location. The nonlinear challenge rule is evaluated signal by signal and
then integrated continuously over the exact rounded ABS geometry. Challenge
outcomes are quarantined until final held-out evaluation. V1 does not modify the
dashboard or feed its beliefs into the inventory MDP.

The normal frozen build uses cached inputs. To refresh or move the endpoint:

```sh
ABS_REFRESH=true make pipeline
ABS_CUTOFF=2026-08-01 ABS_REFRESH=true make pipeline
```

Available environment parameters are:

| Variable | Default | Purpose |
|---|---|---|
| `ABS_START` | `2026-03-25` | First game date |
| `ABS_CUTOFF` | `2026-08-25` | Last included game date |
| `ABS_REFRESH` | `false` | Re-download cached inputs |
| `ABS_BOOTSTRAP_REPS` | `500` | Game-cluster bootstrap replicates |

## Data products

Running `make pipeline` writes the following local artifacts under
`data/processed/`:

| File | Unit | Purpose |
|---|---|---|
| `pitch_ledger.parquet` | One row per called pitch | Calls, challenge result, geometry, inventory, and pitch value |
| `challenge_events.parquet` | One row per resolved challenge | Official outcome, immediate value, and failed-inventory cost |
| `remaining_opportunities.parquet` | One row per team and called pitch | Future opportunity counts/value and marginal inventory value |
| `team_summaries.parquet` | One row per team | Team decision and inventory summaries |
| `player_summaries.parquet` | One row per player-role | Player challenge summaries |
| `*_intervals.parquet` | Team/model estimates | Cluster-bootstrap uncertainty intervals |
| `validation_checks.csv` | Publication checks | Acceptance-test results |

The core pipeline also writes five purpose-specific continuous-perception
source tables. Challenge actions appear only in
`continuous_decision_features.parquet`; official and geometry-implied outcomes
are isolated in `continuous_challenge_labels.parquet`. The heavy workflow writes
`batter_perception_parameters.parquet`, `human_decision_posteriors.parquet`,
validation gates, posterior diagnostics, and a reproducibility manifest under
`data/processed/perception/`. See the [data dictionary](docs/data-dictionary.md)
for their contracts and leakage boundary.

These analysis-scale files are reproducible and therefore ignored by Git. The
compact dashboard JSON, figures, and report PDF under `output/` are versioned as
presentation artifacts.

## Validation gates

The frozen snapshot currently passes all publication gates:

- Every challenge resolves explicitly to `overturned` or `upheld`.
- Parsed successful and failed totals reconcile to every MLB game feed.
- All 8,537 official Savant rows match on play, game, challenger, original call,
  and outcome.
- Public geometry agrees with all tracked official rulings.
- League candidate-pitch coverage is 99.913%; every club exceeds 99%.
- The 50-event stratified manual audit passes, including upheld challenges and
  zero-inventory correctable calls.
- The unit suite passes.

## Interpretation

`out_of_challenges` is the realized value of correctable calls encountered when
inventory was zero. It does not claim the team would have challenged every one
of those pitches. Likewise, `missed_available` measures recoverable value that
was available, not a causal estimate of player decision-making skill.

## Documentation

- [Data acquisition walkthrough](docs/data-acquisition-walkthrough.md)
- [Methodology](docs/methodology.md)
- [Data dictionary](docs/data-dictionary.md)

The WSABI website consumes `output/dashboard/abs-dashboard-data.json` and hosts
the interactive dashboard at `/seminar/projects/abs`.

## Contribution checklist

Before opening a pull request:

1. Do not commit `data/raw/`, local libraries, or generated files under
   `data/processed/`.
2. Run `make test`.
3. If analysis logic changed, run `make pipeline` and inspect the validation
   artifacts.
4. Update the relevant data contract or methodology documentation.
5. Rebuild versioned presentation artifacts with `make dashboard` and, when
   appropriate, `make report`.

Released under the [MIT License](LICENSE).
