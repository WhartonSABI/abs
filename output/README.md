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
