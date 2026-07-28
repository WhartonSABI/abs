# Data acquisition and reconciliation walkthrough

This document explains how the project turns several incomplete public data
sources into a validated pitch and challenge ledger. It also records the
problems encountered during acquisition and the distinction between facts
reported by MLB, fields reported by Statcast, and quantities inferred by the
analysis.

## The central data problem

There is no single public file containing all of the following:

- the exact pitch and game state;
- the original umpire call and the post-review call;
- whether a challenge occurred and whether it was overturned or upheld;
- the team's challenge inventory at that point;
- the pitch location and batter-specific strike-zone limits;
- the counterfactual run and win value of the other call.

The project therefore uses four public inputs, each for a narrow purpose.

| Source | What it contributes | What it does not establish |
|---|---|---|
| MLB schedule API | The completed regular-season game universe | Pitch or challenge details |
| MLB live game feed | Ordered pitch events, timestamps, game context, the `MJ` review tree, explicit review outcome, challenger, and final team challenge totals | A publishable analytical table or complete pitch geometry |
| Daily Baseball Savant Statcast CSV | Pitch location, strike-zone measurements, count/base/score state, player IDs, and benchmark run/WP fields | The complete ABS review tree, especially upheld challenges and their linkage |
| Savant ABS team-detail service | The official analytical challenge set, original call, outcome, challenger, edge distance, and Savant value fields | Every called pitch or the full sequential game timeline |

The 2026 snapshot is frozen through July 19. Its canonical raw manifest covers
114 daily Statcast files, 1,490 unique live game feeds, and 31 Savant files (30
team-detail responses plus the dashboard). Every cached source has a SHA-256
checksum.

## Why an R Statcast scrape was not enough

Packages such as `baseballr` and `pybaseball` make Statcast pitch data easier to
request, but the ordinary pitch export is not an ABS event ledger. It supplies
the location and game state, but it does not expose the full live-feed review
tree needed to identify all attempted challenges and preserve an explicit
`false` outcome.

We did not bypass a protected dataset or discover a hidden pre/post flag.
Instead, we called the same public sources directly and joined them:

1. Statcast supplies pitch measurements and the pre-pitch state.
2. The MLB live feed says exactly when an `MJ` challenge occurred and whether
   `reviewDetails.isOverturned` was `true` or `false`.
3. Savant independently supplies its official ABS row for validation.
4. The analysis constructs the two post-call counterfactual states from the
   shared pre-pitch state.

This distinction matters. A challenge is classified as correct or incorrect
from the explicit review result, not from our geometry. Geometry is used to
classify unchallenged calls and to verify that the public measurements reproduce
official challenged-pitch rulings.

## Acquisition problems and solutions

### Broad Savant exports truncate

The Savant CSV endpoint truncates broad requests at 25,000 rows. A season or
large monthly request can therefore return a valid-looking CSV that is
incomplete. Treating a successful HTTP response as proof of completeness would
have biased the RE288 training data.

For the current 2026 ledger, requests are made one game date at a time. For the
2023--2025 model history, the final reproducible solution is:

- a commit-pinned, checksum-verified Parquet mirror produced from Statcast by
  `pybaseball` for the complete 2023 and 2024 seasons; and
- 184 one-day official Savant exports for the 2025 game dates.

Daily responses remain well below the row cap. The resulting historical layer
contains 2,139,983 pitches from all 7,289 expected completed regular-season
games. The pipeline refuses to fit the models below 99.5% scheduled-game
coverage; the frozen data has 100% coverage.

### Large downloads and macOS process forking were fragile

The raw inputs are large: approximately 1.20 GB of 2026 live feeds, 303 MB of
2026 Statcast CSVs, 22 MB of Savant ABS responses, and 1.07 GB in the canonical
historical manifest. Long monolithic transfers timed out during bootstrap.
Forking R workers while they initialized macOS networking also produced an
Objective-C `NSCharacterSet initialize` crash.

The stable path was to use small resumable daily requests and libcurl's
non-forking multi-download interface, write to `.partial` destinations, check
HTTP status and nonzero size, and only then rename each response into the
cache. Requests use retry/backoff and timeouts. Raw sources are not committed,
but their path, byte size, modification time, and SHA-256 digest are written to
manifests.

### Schedule rows are not automatically an analysis universe

Schedule responses can include duplicate references around suspended games and
rows that later become postponed or cancelled. The universe is therefore the
set of unique `game_pk` values whose final status is completed; cancelled and
postponed rows are explicitly excluded. Statcast rows are filtered back to
those game IDs.

## Recovering every challenge and its outcome

An ABS challenge is an MLB review with `reviewType = "MJ"`. The parser searches
three locations because MLB's JSON is not structurally uniform:

1. a review directly attached to a pitch event;
2. a review attached to the plate appearance, which is linked to its terminal
   called pitch; and
3. an `MJ` review nested in `additionalReviews` under another primary review
   type.

The linkage source is retained on every event. A plate appearance can contain
an earlier pitch-level challenge and another terminal challenge, so the parser
deduplicates only a review that is already attached to that same terminal
pitch; it does not discard all but one review per plate appearance.

Game `823724` is the regression fixture for this logic. It contains seven ABS
challenges, including a terminal plate-appearance review, and resolves to five
overturned and two upheld calls.

The outcome parser deliberately distinguishes three states:

```text
isOverturned = true   -> overturned
isOverturned = false  -> upheld
missing/null          -> unresolved
```

In particular, JSON `false` is never allowed to disappear through a truthiness
filter or become an R missing value. An unresolved outcome stops inventory
reconstruction and cannot enter a published success rate. Success rate is
therefore `overturned / (overturned + upheld)`.

The live feed normally displays the final call after review. We recover the
original call as follows:

```text
upheld:      initial call = final call
overturned:  initial call = opposite of final call
```

Savant's independent `original_isStrike_ump` field validates this recovery.
Every challenge also retains the event start/end time, inning, half, count,
outs, bases, score, pitch order, `play_id`, challenger team and player, and
batter/catcher/pitcher role.

## Joining the live feed to Statcast

The intended key is `game_pk + at_bat_number + pitch_number`, with `play_id`
retained as a separate event identifier. One subtle mismatch had to be fixed:
Statcast's `pitch_number` sequence includes automatic balls and automatic
strikes, while the live feed's physical-pitch sequence does not. The project
recomputes a physical pitch number within each plate appearance by cumulatively
counting non-automatic pitches, removes the automatic events, and then joins.

A matching numeric key is not accepted blindly. Batter and pitcher IDs must
also agree between sources. If they do not, the row remains in the coverage
denominator but its Statcast coordinates are not used. In the frozen ledger,
all 219,405 called pitches find a numeric Statcast key, 61 have an identity
mismatch, and another 173 identity-matched pitches lack complete tracking.
This leaves 219,171 geometry-covered pitches, or 99.893%; the lowest team
coverage is 99.312%, above the 99% publication gate.

## Determining whether a pitch was actually an ABS strike

The project does not use the generic Statcast `zone` bucket and does not use
the umpire's call to decide geometric correctness. It reconstructs the ABS
boundary from the pitch center and the batter-specific zone:

- plate width: 17 inches, so horizontal half-width is `17/24` feet;
- pitch center: Statcast `plate_x` and `plate_z` used by Savant;
- vertical limits: the official/live-feed strike-zone top and bottom, with a
  Statcast fallback, rounded to three decimals;
- baseball radius: 1.45 inches; and
- corner distance: Euclidean rather than a square extension of the zone.

For center `(x, z)`, top `t`, and bottom `b`, the signed ball-edge distance is:

```text
qx = abs(x) - 17/24
qz = max(b - z, z - t)
d_center = sqrt(max(qx, 0)^2 + max(qz, 0)^2) + min(max(qx, qz), 0)
d_edge_inches = 12 * d_center - 1.45
```

`d_edge_inches <= 0` is an ABS strike: some part of the ball intersects the
zone. A positive distance is an ABS ball. This formula reproduces Savant's
`edge_dist_calc` to floating-point tolerance and agrees with all 6,261 tracked
official Savant challenge rulings in the frozen publication set. Results are
also repeated after excluding pitches within 0.25 inches of the boundary.

The opportunity definitions then become mechanical:

- initial called strike plus geometric ABS ball: a correctable opportunity for
  the batting team; and
- initial ball plus geometric ABS strike: a correctable opportunity for the
  fielding team.

For challenged pitches, however, the official overturned/upheld field remains
the source of truth for success-rate accounting. For unchallenged pitches,
correctness is an inference from validated public geometry, not an official ABS
ruling.

## What "pre" and "post" mean in the value calculation

The pre-pitch count, outs, runners, inning/half, and score are observed fields.
The two post-call states are not scraped as a ready-made pair. They are built
by applying the initial call and the corrected ABS call to the same pre-pitch
state:

```text
observed pre-pitch state
          |-- apply initial umpire call --> initial-call state
          `-- apply corrected ABS call ---> corrected-call state
```

The state transition code handles ordinary balls and strikes, full-count
walks, strike three, forced advancement and runs on a walk, and the end of a
half-inning. Runner movement concurrent with the pitch, such as a steal, is
preserved in both branches so that only the ball/strike ruling changes. RE288
and the home-win model are evaluated on both states; their difference is then
oriented to the adversely affected team's perspective.

An overturned challenge receives that immediate difference. An upheld
challenge receives zero immediate gain and may lose an inventory unit. The
future cost of that unit is modeled separately rather than being folded into
the immediate pitch value.

## Reconstructing challenge inventory

Inventory is reconstructed sequentially within each game, not inferred from a
season total:

- both teams begin with two challenges;
- an overturned challenge retains the unit;
- an upheld challenge decrements inventory by one; and
- a team at zero is restored to one when entering each extra inning.

The ledger records inventory immediately before and after every pitch. A wrong
adverse call with inventory available and no challenge is `missed_available`;
the same event at zero inventory is `out_of_challenges`.

The live feed's final `usedSuccessful` and `usedFailed` totals are reconciled
for both teams in every game. Its final `remaining` field is also reconciled
for regulation games. That field does not expose an unused extra-inning grant,
so extra-inning inventory is validated event by event, including the invariant
that no team challenges at reconstructed zero.

## Reconciliation and quarantine

Validation is deliberately redundant:

1. Every parsed challenge must be explicitly overturned or upheld.
2. Per-game, per-team parsed totals must equal the live feed's final successful
   and failed totals.
3. Savant rows must match on `play_id`, game, challenger, original call, and
   outcome.
4. Geometry must agree with every tracked official challenge ruling.
5. League and per-team pitch coverage must clear their publication gates.
6. A 50-event stratified manual audit must continue to pass.

The live feeds contain 6,263 rule-counted challenges: 3,344 overturned and
2,919 upheld. Savant's official analytical set contains 6,261: 3,343
overturned and 2,918 upheld, for a 53.394% success rate. Two live-feed events
are absent from Savant's analytical service. They remain in the sequential
inventory ledger so game totals stay correct, but are explicitly quarantined
from published success rates and challenge-value summaries. Their identifiers
and reasons are recorded in `data/reference/feed_only_challenges.csv`.

This is why the project can say both that all live-feed challenge accounting
reconciles and that published results use the official Savant population. The
two claims refer to documented, testable sets rather than an unexplained row
count difference.

## Reproducibility map

- Acquisition and cache logic: `scripts/functions/download.R`
- Live-feed review parsing: `scripts/functions/live_feed.R`
- Statcast physical-pitch join: `scripts/functions/statcast.R`
- ABS geometry: `scripts/functions/geometry.R`
- Sequential inventory: `scripts/functions/inventory.R`
- Counterfactual states and value: `scripts/functions/valuation.R`
- Acceptance gates: `scripts/functions/validation.R`
- Pipeline graph: `_targets.R`
- Field definitions: `docs/data-dictionary.md`
- Compact method specification: `docs/methodology.md`

The pipeline can be refreshed with `ABS_REFRESH=true`; the default remains the
frozen July 19 snapshot so later source corrections cannot silently change the
published analysis.
