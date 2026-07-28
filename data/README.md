# Data

The data tree follows the standard WSABI raw/processed/reference/fixtures
separation.

| Directory | Contents | Versioned? |
|---|---|---|
| `raw/` | MLB schedules, Statcast exports, live feeds, Savant responses, checksums, and manifests | No |
| `processed/` | Pitch ledger, challenge events, summaries, model outputs, and validation tables | README only |
| `reference/` | Reviewed exclusions, feed-only quarantine, and 50-event manual audit | Yes |
| `fixtures/` | Compact test and audit inputs | Yes |

Game `823724` is retained as a fixture because it exercises seven challenges,
upheld rulings, and plate-appearance-level terminal linkage.

Refresh the cache with:

```sh
ABS_REFRESH=true make pipeline
```

Never edit raw or processed data manually. Record reviewed corrections or
exclusions under `reference/` and rebuild the pipeline.
