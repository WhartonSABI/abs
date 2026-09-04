# Data

The repository includes the compact data required to reproduce the final
analysis and manuscript.

```text
data/
├── analysis/   # Compressed final-analysis inputs and checksums
├── fixtures/   # Small source-data samples used by tests
└── reference/  # Reviewed exclusions and manual validation records
```

The much larger acquisition cache and regenerated intermediate tables are not
part of the repository. The pipeline writes those ignored files to `data/raw/`
and `data/processed/` when source acquisition or a full rebuild is requested.
