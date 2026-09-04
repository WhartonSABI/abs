# Public analysis inputs

These compressed files are the frozen input boundary for the final fixed-clock
analysis. They make the reported model reproducible without committing the
multi-gigabyte download cache or thousands of temporary checkpoints.

| File | Rows | Columns | Contents |
|---|---:|---:|---|
| `pitch_ledger.parquet` | 292,381 | 118 | 2026 called-pitch ledger, geometry, challenge state, and values |
| `history_re_inputs.parquet` | 2,139,983 | 22 | Minimal 2023--2025 pitch fields used to estimate RE288 |
| `re288_model.rds` | -- | -- | Frozen RE288/RE24 run-expectancy model |
| `challenge_events.parquet` | 8,538 | 134 | Parsed challenges with outcomes and game state |
| `savant_challenges.parquet` | 8,537 | 65 | Official Baseball Savant challenge publication set |

Parquet files use Zstandard level-19 compression; the model uses xz-compressed
R serialization. Verify the committed bytes from this directory with:

```sh
shasum -a 256 -c SHA256SUMS
```

`make fixed-clock-smoke` and `make fixed-clock-full` read these files directly.
After rebuilding the upstream targets pipeline, `make public-data` regenerates
the bundle.
