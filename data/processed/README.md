# Derived data

This directory is populated by `make pipeline`. The Parquet and CSV files are
analysis-scale, reproducible products and are ignored by Git.

The canonical table is `pitch_ledger.parquet`, with one row per called pitch.
See [`../../docs/data-dictionary.md`](../../docs/data-dictionary.md) for the complete
contracts for the pitch ledger, challenge events, remaining opportunities,
summaries, intervals, and validation artifacts.

Do not hand-edit these files. Change the pipeline or reviewed configuration and
rebuild them.
