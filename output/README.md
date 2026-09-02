# Presentation outputs

This directory contains compact, versioned artifacts intended for readers and
the WSABI website:

- `dashboard/abs-dashboard-data.json` is the browser-ready dashboard payload.
- `figures/dashboard/` contains eight publication-ready PNG figures.
- `pdf/mlb-abs-challenge-run-value.pdf` is the compact report export.

Rebuild the dashboard payload and figures with:

```sh
make dashboard
```

Generated analysis tables belong in `data/processed/`, not here.

The continuous human-perception analysis is a separate research workflow. Its
tables, validation gates, diagnostics, and manifest are generated under
`data/processed/perception/`; its source is `report/perception.qmd`, rendered
with `make perception-report`. V1 intentionally does not change the dashboard
payload, dashboard figures, or the inventory MDP.
