# MLB ABS Challenge Run Value

This project measures the value created or lost by MLB automated ball-strike
(ABS) challenge decisions. It reconstructs every team's challenge inventory,
identifies calls that public ABS geometry says were correctable, and values the
actual and counterfactual decisions in run-expectancy and win-probability units.

The default research snapshot contains completed 2026 MLB regular-season games
through **August 25, 2026**, the latest fully published game day as of August 26.

## Research snapshot

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
.
├── analysis/
│   └── policy/                 # Empirical fixed-clock production runner
├── scripts/
│   ├── functions/
│   │   ├── core/               # Acquisition, geometry, inventory, and valuation
│   │   ├── perception/         # Signal and margin models
│   │   └── policy/             # Fixed-clock policy implementation
│   ├── stan/                   # Hierarchical discrimination model
│   └── load_functions.R        # Shared recursive function loader
├── config/                     # Frozen executable project configuration
├── data/
│   ├── raw/                    # Downloaded MLB and Savant inputs; ignored by Git
│   ├── processed/              # Generated analysis tables; ignored by Git
│   ├── reference/              # Reviewed exclusions, quarantine, and manual audit
│   └── fixtures/               # Compact, versioned audit fixtures
├── output/                     # Versioned dashboard data, figures, and PDF
├── report/                     # Quarto report source
├── tests/
│   └── testthat/               # Unit and workflow tests
├── _targets.R                  # Main reproducible pipeline graph
└── renv.lock                   # Frozen R dependency versions
```

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

The empirical fixed-clock policy has two entry points:

```sh
make fixed-clock-smoke  # Bounded end-to-end confirmation check
make fixed-clock-full   # Full fixed-clock policy and bootstrap analysis
```

The production model cross-fits challenge selection and empirical margin
distributions on the development sample, conditions those distributions by
role and count with shrinkage, and combines them with the estimated challenge
signal and Bellman inventory value. Challenge outcomes remain quarantined until
the frozen policies are evaluated on the July 20 confirmation split.

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

The fixed-clock workflow writes its run directory, frozen policies, bootstrap
checkpoints, diagnostics, and manifests under
`data/processed/perception/fixed_clock_confirmation/`.

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

The WSABI website consumes `output/dashboard/abs-dashboard-data.json` and hosts
the interactive dashboard at `/seminar/projects/abs`.

Released under the [MIT License](LICENSE).
