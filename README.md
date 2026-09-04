# Challenge or Pass?

This repository contains the data, code, applied results, and manuscript for
the MLB automated ball-strike (ABS) challenge-policy study. The central result
is that a good challenge policy maximizes expected run value, not its success
rate.

On 497 temporally held-out games, the learned policy captures 0.5595 modeled
run expectancy (RE) per game, compared with 0.4595 for observed decisions: a
gain of 0.1000 RE/game (95% CI [0.0738, 0.1281]). Its model-implied success rate
is lower, 45.3% versus 54.2%, because it challenges more uncertain calls when
the potential run value is high.

## Repository layout

```text
.
├── config/                     # Frozen project and sample configuration
├── data/
│   ├── analysis/               # Compressed inputs for the final analysis
│   ├── fixtures/               # Small raw-data samples used by tests
│   └── reference/              # Reviewed exclusions and validation records
├── results/                    # Compact final estimates, replays, and policy
├── scripts/
│   ├── functions/
│   │   ├── core/               # Geometry, inventory, and run valuation
│   │   ├── perception/         # Effective widths, priors, and posteriors
│   │   └── policy/             # Direct/maximin and Bellman policy methods
│   ├── stan/                   # Hierarchical discrimination model
│   ├── export_paper_results.R  # Export applied results from a completed run
│   ├── export_public_data.R    # Export the compressed analysis inputs
│   └── run_fixed_clock_confirmation_1d.R
├── paper/
│   ├── main.tex                # Submission manuscript
│   ├── references.bib          # Verified bibliography
│   ├── build_figures.R         # Tables and figures generated from results/
│   └── figures/                # Versioned manuscript exhibits
├── output/pdf/                 # Submission-ready manuscript PDF
├── tests/testthat/             # Unit and workflow tests
├── _targets.R                  # Upstream data-construction pipeline
├── DESCRIPTION                 # R dependency declaration
├── renv.lock                   # Frozen R dependency versions
└── Makefile                    # Reproducible entry points
```

Local downloads, regenerated intermediates, caches, logs, checkpoints, and
working notes are excluded from Git. Superseded materials live locally under
the ignored `archive/`; non-public operational material lives under the ignored
`internal/`.

## Reproduce the study

Requirements are R 4.3 or newer, GNU Make, and a TeX installation for the
manuscript PDF.

```sh
git clone https://github.com/WhartonSABI/abs.git
cd abs
cp .Renviron.example .Renviron
make install
make test
```

The versioned `data/analysis/` bundle is the frozen input boundary for the
final policy. It includes the called-pitch ledger, minimal 2023--2025 RE288
inputs, the frozen run-expectancy model, challenge events, and the official
Savant challenge table. The bundle is 47 MB after Zstandard/xz compression;
no file exceeds GitHub's ordinary 100 MB per-file limit. Verify it with:

```sh
cd data/analysis
shasum -a 256 -c SHA256SUMS
cd ../..
```

Run a bounded integration check or the complete final analysis with:

```sh
make fixed-clock-smoke
make fixed-clock-full
```

The full analysis writes regenerated run directories and checkpoints beneath
the ignored `data/processed/` tree. It does not overwrite the committed
`results/` bundle unless results are explicitly exported from a completed run.

## Results and manuscript

`results/` contains the exact applied bundle used for every numerical table,
figure, team comparison, and case study in the paper. The primary result is
`direct_learning_procedure`; `bellman_structural` is the independent structural
check. These are modeled RE quantities on the held-out factual opportunity
clock, not actual runs caused or a regenerated-game counterfactual.

Regenerate the manuscript exhibits from the committed results or build and
validate the submission PDF with:

```sh
make paper-figures
make paper-check
```

The finished manuscript is
[`output/pdf/abs-challenge-policy-review.pdf`](output/pdf/abs-challenge-policy-review.pdf).

## Data provenance

The 2026 pitch ledger covers completed MLB regular-season games through August
25, 2026. The reporting sample contains 1,488 ABS-enabled development games and
497 later ABS-enabled confirmation games. The repository redistributes the
compact derived fields required for analysis; MLB Stats API and Baseball
Savant remain the authoritative upstream sources.

See [`data/analysis/README.md`](data/analysis/README.md) for the public input
schema and [`results/README.md`](results/README.md) for the applied-results
schema.

Released under the [MIT License](LICENSE).
