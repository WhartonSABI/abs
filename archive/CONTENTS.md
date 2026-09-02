# Archive

This directory preserves superseded research code for provenance. Nothing
under `archive/` is loaded by `_targets.R`, sourced by the production runner, or
included in the active test suite. Historical scripts retain their original
paths internally and may require path adjustments before they can be rerun.

```text
archive/
├── legacy-mdp/                  # Original notebooks and fixed-probability MDP
│   ├── R/                       # Early interactive analyses and helper code
│   └── analysis/                # Legacy simulations, plots, and model runners
├── perception-experiments/      # Superseded perception-model families
│   ├── analysis/                # EDA and standalone experiment runners
│   ├── functions/               # Gaussian, continuous, distance, and swing/take code
│   ├── stan/                    # Stan programs used only by those experiments
│   ├── tests/                   # Their historical unit tests
│   └── _targets_perception.R    # Retired heavy continuous-perception graph
├── revealed-policy/             # Pre-confirmation revealed-policy workflow
│   ├── analysis/                # Superseded production runner
│   └── report/                  # Exploratory revealed-policy report
└── hpc/fixed-clock/             # Superseded GMM and initial empirical launchers
```

The active replacement is the empirical fixed-clock workflow rooted at
`analysis/policy/run_fixed_clock_confirmation_1d.R`.
