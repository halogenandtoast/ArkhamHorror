# What `arkham_steps` actually costs

`arkham_steps` is the database — 94 of 98 GB. This document is the result of
measuring *why*, in October 2026, and what the levers are. It exists because the
obvious lever (a better TOAST compression codec) turns out to be the weakest one
available, and the two strong levers are not compression at all.

Nothing here has been implemented. The reports are findings plus a design;
`docs/game-archiving.md` covers the one piece of this that is already built.

## The size breakdown

```
heap   9899 MB
toast    83 GB
indexes 1498 MB
```

TOAST is 88% of the table, so the payloads govern essentially the whole
problem. `reltuples` ≈ 14.9M rows (the 7.6M in
`migrations/deploy/add_primary_key_to_arkham_steps.sql` is stale).

Measured on prod, 3,210 rows via `TABLESAMPLE SYSTEM (0.02)`:

| column | raw B/row | stored B/row | pglz ratio | share of stored |
|---|---|---|---|---|
| `choice` | 9,619 | 3,252 | 2.96x | 61.3% |
| `action_diff` | 7,339 | 2,057 | 3.57x | **38.7%** |
| total | 16,958 | 5,309 | 3.19x | |

## Lever 1 — `action_diff` is written and never read (−38.7%)

**This is the largest single lever and it needs no migration.**

`gameActionDiff` is a serialized field of `Game`
(`backend/arkham-api/library/Arkham/Game/Base.hs:151`, encoded at
`Arkham/Game/Json.hs:79,155,242`), so it already persists inside
`arkham_games.current_data`. The engine reads it from the loaded `Game` —
`Arkham/Game/Runner.hs:721` (the `UndoAction` fold) and `:4596-4607`
(`handleActionDiff`) — and never from the step row.

`arkham_steps.action_diff` is therefore a second copy of a value only ever read
from the game row. Every reference to it is a write or a pass-through:

- Writes: `Api/Handler/Arkham/Games/Shared.hs:598`, `:601` (main save path),
  `Shared.hs:1137`, `Api/Handler/Arkham/Decks.hs:242`,
  `Api/Handler/Arkham/PendingGames.hs:107`, `:189`.
  (`Games.hs:250` and `Events.hs:690` already write `ActionDiff []`.)
- Pass-through only: full export (`Api/Arkham/Archive.hs:361,372-377`), archive
  restore (`Archive.hs:740-743`), debug re-import (`Game/Debug.hs:249`).
- Zero read sites in `arkham-api/library`, `app-replay`, or the frontend.
  `app-replay` parses only `choiceMessages`/`choicePatchDown`.

It is **not** redundant with `choice`: across 17,344 real step rows, zero had an
`action_diff` op identical to a `patchDown` op, and only three were a strict
subset. Byte-identical overlapping ops are 3.2% of raw. It duplicates the *game
row*, not the choice.

It is non-empty on ~38% of rows (`handleActionDiff` populates it only
`when gameInAction`), carrying ~9 KB raw on those — and mid-action saves are the
common case: every target choice, every test commitment.

### Time Warp (2) is not affected — it reads the other copy

This is the first objection anyone raises, so: **Time Warp (2) (03311,
`Arkham/Event/Events/TimeWarp2.hs`) consumes `gameActionDiff`, the `Game` field,
not `arkham_steps.action_diff`.** The card pushes `UndoAction` (`:25`), whose
handler folds `gameActionDiff g` off the in-memory game
(`Arkham/Game/Runner.hs:721`). That field is serialized into
`arkham_games.current_data` (`Arkham/Game/Json.hs:79,155`, a *required* key at
`:242`), and `handleActionDiff` (`Runner.hs:4592-4607`) rebuilds the runtime
snapshot after a mid-action load by folding those same patches out of the loaded
`Game`. The step column is never consulted.

The structural proof is better than the textual one: the test suite has no
`arkham_steps` table at all, and the card works. `tests/Arkham/Event/Events/
TimeWarp2Spec.hs` covers the revert, the mid-action save/load round-trip, and
asserts the field is present in the serialized `Game` — so a change that moves
this data out of the `Game` fails there rather than silently in production.

**The adjacent bug that justifies the retention:** `Game/Diff.hs:27` excludes
`gameActionDiff` from the step diff, so a patch never touches it. After a
*database* undo (`PUT /undo`, which replays `patchDown` into `current_data`),
`gameActionDiff` therefore still holds the **newer** step's value, and a Time
Warp played after a DB undo can revert the wrong action. That bug exists today,
independent of this proposal, masked by `unsafePatch`'s `Error _ -> g` fallback.
`arkham_steps[n-1].action_diff` is the only sound source for the correct value —
which is what the two-row retention below preserves, at ~0.3% of rows.

### Why the column exists, and the shape that keeps the option open

`Api/Handler/Arkham/Undo.hs:157` carries `-- TODO: We need to add back the
gameActionDiff`. `Arkham.Game.Diff.diff` deliberately excludes `gameActionDiff`
(`Arkham/Game/Diff.hs:27`), so after an undo `current_data` retains the *newer*
step's revert diff; the only sound source for the correct one is
`arkham_steps[n-1].action_diff`. The column is data for an unimplemented fix,
currently masked by `unsafePatch`'s `Error _ -> g` fallback.

So this is a decision, not a cleanup. The shape that captures the win without
foreclosing the fix: write `ActionDiff []` at the six sites, and in the main
save path clear the row two steps back —

```sql
UPDATE arkham_steps SET action_diff = '[]'
 WHERE arkham_game_id = ? AND step = ?   -- currentStep - 2
```

one narrow row, folded into the existing transaction beside the `deleteWhere`.
That retains the newest two rows per game, the only ones an undo can need, at
~0.3% of rows.

**Decode safety:** the column and the Haskell field stay, `[]` is a valid
`ActionDiff`, all existing rows decode unchanged, and `arkham-replay` never
touches it. Fully reversible — stop clearing and the data resumes.

## Lever 2 — the payload content is close to irreducible

Composition of the raw JSON, measured over 17,344 real step rows extracted from
debug exports (mean 12,890 B/row; p50 10.6 KB, p90 26.5 KB, p99 43.8 KB, max
183 KB):

| component | B/row | share |
|---|---|---|
| `choicePatchDown` | 6,824 | 52.9% |
| `action_diff` | 3,453 | 26.8% |
| `choiceMessages` | 2,614 | 20.3% |

Inside `choicePatchDown`, one thing dominates: **`/gameQuestion` ops are 3,882
B/row, 30% of the entire row**, of which 2,066 B/row is depth-4
`add /gameQuestion/<playerId>/choices/<n>` — whole choice objects added back,
each carrying its embedded `messages` list with full card records. The tail:
`/gameMode/That/chaosBag` 481 B/row, `/gameActiveAbilities` and `/gameSkillTest`
~2% each, per-investigator entity ops <1.5% each.

Inside `choiceMessages`: `CheckWindows` 39.7% (n=23,770, ~756 B each) and
`MoveWithSkillTest` 26.4% (n=11,515, **~1,038 B each**) are two-thirds of the
queue.

### Rejected, with the evidence

- **Narrowing the `gameQuestion` diff, or storing the question verbatim.**
  Reconstructing state by replaying 6,954 patches backwards from `currentData`
  (4 op failures total): the question map at the reverted-to state averages
  4,913 B, and the positional diff producing it costs 4,352 B — **0.89x**.
  aeson-diff already beats whole-value storage. Consecutive prompts differ
  almost entirely and undo must restore the exact prompt the player answered.
  Irreducible.
- **Sum-type re-encoding.** `"tag"`/`"contents"` scaffolding is 26.6% of *raw*
  bytes and **3.3% of stored** bytes — pglz has already deduplicated it. Not
  worth a format break across 15M rows.
- **Chaos-bag diffing.** 481 B/row against an 11.7 KB bag; aeson-diff is already
  doing well on reshuffles. Stabilizing bag order would touch every chaos-token
  interaction for ~3% of raw.
- **`choiceMessages`.** All of it is live: the current step's queue resumes play
  (`Shared.hs:442`), older steps' queues resume after an undo, and
  `app-replay/Main.hs:233,283` pushes them. `MoveWithSkillTest` at ~1 KB per
  instance is the one place worth an *engine* look, but that is a message-shape
  question, not a storage one.

### The one encoding change worth making

`omitNothingFields = True`, per-type. Null/false/empty fields are 16.7% of raw
bytes; worth ~4 points of stored bytes after levers 1 and 3. Start at
`Arkham/Window.hs:603` (`deriveJSON defaultOptions ''Window`) — `windowBatchId`
and `windowConditionTick` alone are 3.0% of raw bytes and 29% of all null-field
bytes, and `Window` is the most-repeated record in the queue. Then the card
record (`facedown`/`mutated`/`chained`/`errata`/`meta`/`tabooList`/
`customizations`) and `Ability`.

Safe because aeson's TH and generic `FromJSON` parse a `Maybe` field with the
maybe-field parser, which accepts **absent or null** — so existing rows keep
decoding *and* a code rollback still decodes rows written by the new encoder.

Do **not** chase the further `false`/`[]` omissions (another ~3 points). That
needs aeson 2.2 `omittedField` instances per type and, once written, a rollback
cannot decode the new rows. One-way door for three points.

Do this per-type, not by changing `aesonOptions` in `library/Json.hs`. A global
flip also re-encodes `arkham_games.current_data` and every websocket payload (a
bandwidth bonus), but it would cross hand-written `FromJSON` instances using
required `.:` — `Arkham/Game/Json.hs:242` does exactly that for
`gameActionDiff`.

## Lever 3 — the storage encoding (−55%)

Both columns are `jsonb`, which is the worst of the available options, and TOAST
codecs cannot fix it.

Measured per-value on real payloads (prod sample for the codecs, 1,477 rows;
local materialization for the pglz variants, 1,200 rows):

| variant | ratio | vs today |
|---|---|---|
| pglz + `jsonb` (today) | 2.96x | — |
| TOAST `lz4` + `jsonb` | — | **+4.3%** (worse) |
| pglz + `json` / `text` | 3.50x | −27.8% |
| app-level gzip-6 + `bytea` | 7.15x | ~−55% |
| app-level zstd-3 + `bytea` | 7.12x | ~−55% |
| app-level zstd-12 + `bytea` | 7.86x | ~−58% |

Three results worth keeping:

1. **TOAST `lz4` is worse than `pglz` here** — measured at 1.043x on real
   payloads, projecting 83 GB → 86 GB. Postgres has only `pglz` and `lz4` as
   TOAST methods (there is no `lzf`, and `zstd` is not a TOAST method in any
   release), so the in-database codec choice is exhausted. `lz4` is a
   speed-for-ratio trade by design; it was added in PG14 because pglz
   *decompression* was a CPU bottleneck, not to save space.
2. **`jsonb` costs 40% over identical content in `json` or `text`.** `json` and
   `text` measured byte-identical. The cause is `jsonb`'s per-element JEntry
   headers plus its key reordering (shortest-first), which destroys the key
   locality pglz exploits. This is why the baseline looked like a mediocre
   2.7–3.0x: pglz was not doing badly, `jsonb` was sabotaging it.
3. **gzip-6 ties zstd-3, and `zlib` is already a dependency**
   (`backend/arkham-api/package.yaml:139`). The zstd dependency buys zstd-12's
   extra ~10% of ratio and much faster decompression — relevant to
   `GET /replay/#`, which selects every step of a game — not the headline win.

Nothing depends on `jsonb` semantics: there is not one `->`/`->>`/`@>` against
`arkham_steps` anywhere in the codebase. The only SQL touching the table is the
two triggers in `setup.sql:36-99` (which read `step` and `arkham_game_id` only)
and the archive's plain `SELECT choice, step, action_diff`.
`Entity/Arkham/Step.hs:33,47` already declare `sqlType _ = SqlString`.

### Combined effect

| stage | stored B/row | vs today |
|---|---|---|
| today | 5,309 | — |
| drop `action_diff` | 3,252 | −38.7% |
| + compressed `bytea` on `choice` | ~1,480 | **~−72%** |

~94 GB of equivalent content becomes ~26 GB. The ~1,480 is the one extrapolated
figure: the 7.12x prod measurement compressed `choice` and `action_diff`
*together in one frame*, and `choice` alone does slightly worse (~6.5x
estimated, from a local choice-only measurement of 5.57x scaled by prod's higher
compressibility). The gap between together and separate is ~8% of compressed
size, consistent with shared vocabulary rather than duplicated content. Measure
it exactly with the `choice`-only script below before relying on the number.

### Drift check

Recent rows (last 40,000 heap pages) are slightly *smaller* (13,504 B/row raw
vs 15,773) and compress slightly *worse* (6.45x vs 7.12x), netting ~2,094 B/row
against ~2,214 for the whole table. No structural drift, so whole-table numbers
are sound for reasoning about future growth — which is what matters, since only
new rows are ever affected.

## The migration design for lever 3

An in-place `ALTER COLUMN ... TYPE` **rewrites the relation** and needs ~94 GB
of free space that does not exist, plus `ACCESS EXCLUSIVE` for hours. Ruled out
by the same constraint as `VACUUM FULL`. A type change is always a rewrite.

So: **new nullable columns alongside, with a read fallback.** `ADD COLUMN` with
no default is metadata-only on PG 11+; sub-second on 15M rows. This pattern
serves either encoding (`json`, or compressed `bytea`) — and since the migration
shape is identical, prefer `bytea` and take the extra 27 points.

Properties:

- **No backfill, ever.** An `UPDATE` of 15M rows means 15M new row versions plus
  their TOAST in MVCC — temporarily doubling 94 GB. Also ruled out. Legacy rows
  stay as they are and are read by the fallback until archiving deletes them.
- The archive **restore** path keeps writing the legacy encoding, so
  `jsonb_to_recordset` at `Archive.hs:721-724,741-743` needs no change.
- Mixed encodings coexist within one game's history indefinitely. A multi-step
  undo folding `choicePatchDown` across both is identical — same values,
  different storage.

### Rolling-deploy requirement

Once a pod writes `choice = NULL`, a pod that does not know about the new column
gets `NULL` for a non-`Maybe` persistent field and fails to decode the row — a
broken save or broken undo on a live game. **The deploy that can read the new
encoding must land strictly before the deploy that writes it.** Use a settings
flag rather than two images:

```yaml
# config/settings.yml, next to the archive block
step-compression: "_env:STEP_COMPRESSION:false"
```

plus `appStepCompression <- o .:? "step-compression" .!= False` in
`Settings.hs`. Per `project_envrc_not_env_reaches_the_backend`, it reaches the
pod through `app_secrets`/`env_from` in terraform (and locally through `.envrc`,
not `.env`), and `env_from` resolves at container start — so flipping it needs
`terraform apply` + `kubectl rollout restart`. The restart is a feature: it
guarantees every serving pod is on the new image before any of them writes a
compressed row.

The rollback for the write flip is then a config change plus a restart, and it
is always safe because the read path is unconditional. Rows written while it was
on stay readable with it off.

**Point of no return:** once `STEP_COMPRESSION=true` has served one request, the
last pre-feature image is no longer a valid rollback target. The rollback target
is the same image with the flag off.

### Haskell side

`zstd-0.1.3.0` is in the `nightly-2026-08-09` snapshot, so no `extra-deps`. Its
`standalone` flag defaults to `True` and vendors the C library, so there is **no
`libzstd-dev` to add to the Dockerfile**. Add the dep to
`backend/arkham-api/package.yaml` (~line 139, next to `zlib`) —
`arkham-api.cabal` is hpack-generated. `Codec.Compression.Zstd` gives
`compress :: Int -> ByteString -> ByteString` and
`decompress :: ByteString -> Decompress` where
`data Decompress = Skip | Error String | Decompress ByteString`.

Prefix the payload with a one-byte version so the next codec change is a
read-side switch rather than magic-number sniffing (15 MB over 15M rows). Make
**every** failure a `Left` — `Skip` is not `mempty`, an unknown version byte is
not a guess. A corrupt payload must surface as a 500 on one game, never as a
silently empty `Choice`, because an empty `Choice` is an undo that quietly lands
the game somewhere it has never been. zstd frames carry an xxhash checksum, so
corruption surfaces as `Zstd.Error` rather than plausible garbage — that is the
property that makes this acceptable for data you cannot regenerate.

No trained dictionary: it buys a few percent on 15 KB payloads (zstd's window
already catches intra-document key repetition) and the dictionary file becomes
part of the on-disk format forever.

Two traps in the entity:

- `aesonOptions` (`library/Json.hs`) has `omitNothingFields = False`, so leaving
  the generic `ToJSON` in place would emit `"choiceZ":null` into **every export
  and archive object**, changing the export contract and breaking `app-replay`'s
  assumptions. Write explicit `ToJSON`/`FromJSON` that hide the storage shape;
  parsing an export should yield the plain representation so imports and the
  restore path need no change.
- `Games/Shared.hs:600` is an `upsertBy` update list. Post-flip it must set
  **all four** columns, or an upsert over a pre-existing legacy row leaves stale
  `jsonb` beside the new `bytea`. The accessor prefers the new one, so the row
  is merely twice the size and the CHECK constraint cannot see the problem.

Read sites to swap to the fallback accessor: `Undo.hs:156`, `:522`,
`Replay.hs:29`, `PendingGames.hs:76`, `:117`, `Decks.hs:177`,
`Games/Shared.hs:442`, `:1107`.

Unaffected: `app-replay/Main.hs` (reads an export *file*, never the DB —
`eitherDecodeFileStrict'` at `:170,177`), `Game/Debug.hs:249` (`insertMany_` of
steps parsed from an export, so plain), `Admin/Archives.hs` (counters on
`arkham_game_archives`), `setup.sql` (`BASELINE_THROUGH` is
`add_step_constraint`, so a fresh install applies this on top).

Runtime cost: zstd-3 on 15 KB is ~20–40 µs against a save path already around
50 ms. Decompress ~5–10 µs/row; the worst reader is `GET /replay/#` at 12k steps
≈ 0.1–0.2 s, replacing a 12k-row `jsonb` parse, so probably net faster. The
directional win on managed Postgres: fewer bytes detoasted server-side and fewer
on the wire, in exchange for app-side CPU — moving work off the expensive
machine.

### Migration SQL

```sql
-- compress_arkham_steps
-- Catalog-only: ADD COLUMN with no default is metadata-only on PG 11+, and
-- SET STORAGE / DROP NOT NULL / ADD CONSTRAINT NOT VALID never read a row.
-- But it takes ACCESS EXCLUSIVE on the busiest table in the application, so it
-- must not queue behind a long transaction -- a waiting DDL lock blocks every
-- game save behind it. lock_timeout fails fast instead; just run it again.
SET lock_timeout = '3s';

BEGIN;

ALTER TABLE arkham_steps ADD COLUMN IF NOT EXISTS choice_z      bytea;
ALTER TABLE arkham_steps ADD COLUMN IF NOT EXISTS action_diff_z bytea;

-- Already compressed. Skip pglz's futile attempt on every write.
ALTER TABLE arkham_steps ALTER COLUMN choice_z      SET STORAGE EXTERNAL;
ALTER TABLE arkham_steps ALTER COLUMN action_diff_z SET STORAGE EXTERNAL;

ALTER TABLE arkham_steps ALTER COLUMN choice      DROP NOT NULL;
ALTER TABLE arkham_steps ALTER COLUMN action_diff DROP NOT NULL;

-- Exactly one encoding present, always. NOT VALID applies to new rows only,
-- skipping the 15M-row scan -- and existing rows already satisfy it.
ALTER TABLE arkham_steps
  ADD CONSTRAINT arkham_steps_choice_present
  CHECK (choice IS NOT NULL OR choice_z IS NOT NULL) NOT VALID;
ALTER TABLE arkham_steps
  ADD CONSTRAINT arkham_steps_action_diff_present
  CHECK (action_diff IS NOT NULL OR action_diff_z IS NOT NULL) NOT VALID;

COMMIT;
```

The CHECKs make "a step row with no payload" impossible at the storage layer —
the one bug in this design that would be silent and unrecoverable.

The revert must **refuse** once any compressed row exists, because dropping the
column would destroy undo history that exists nowhere else. Do not re-add
`NOT NULL` on revert: that scans 15M rows. If it is ever wanted back, add a
validated `CHECK (choice IS NOT NULL)` first, then `SET NOT NULL`, which PG 12+
satisfies from the constraint without a second scan.

Note the migration mechanism is **not** sqitch despite the layout: `migrate.sh`
reads `migrations/sqitch.plan` for *ordering* and tracks applied names in
`arkham_schema_migrations`. Prod is applied by hand.

## What none of this does

**Nothing here returns a byte to the volume.** Postgres never shrinks a relation
on `DELETE`; `VACUUM` returns pages to the relation's free space map for reuse,
and can truncate only a trailing run of entirely-empty pages. `arkham_steps` is
append-mostly, so the oldest TOAST chunks are at the *front* of the file and
truncation finds nothing. `VACUUM FULL` and `pg_repack` both need roughly the
table's size free — 4 GB against a 94 GB table, and waiting does not change
that.

So levers 1 and 3 are **growth-rate changes**: the table stops growing and
absorbs the next tens of GB of writes inside the space already paid for.
Archiving (`docs/game-archiving.md`) is the only lever that converts used space
into reusable free space at the scale of the existing 83 GB.

**How the bill actually comes down:** only a rewrite onto *different hardware* —
a logical-replication cutover, or `pg_dump`/restore under maintenance mode (the
switch exists; `/health` must keep returning 200) — onto a fresh smaller
cluster, after archiving has drained. Do **not** scale the DO volume up to make
room for `pg_repack`: DO disks grow with the plan and cannot shrink, so you
would buy the bigger plan permanently to do a one-time compaction.

Also expect heap to grow as TOAST shrinks: at ~1,500–2,000 B stored, many rows
fall under the TOAST threshold and stay inline. Judge by
`pg_total_relation_size`, not by the TOAST number alone.

## Recommended order

1. **Stop writing `action_diff`** — −38.7%, one line, no migration, reversible.
   Needs a decision on the two-row retention for the `Undo.hs:157` TODO.
2. **Finish the archiving drain** — the only lever on the existing 83 GB.
3. **Dual-column compressed `bytea`** — ~−55% more on new rows, metadata-only
   migration. Choose zstd (faster reads, +10% ratio) or the already-present
   `zlib`.
4. **`omitNothingFields`** on `Window` and the card record — ~4 points.
5. **Logical cutover** to a smaller cluster — the only step that changes the
   invoice.

## Measurement traps

Anyone re-running these numbers will hit all four:

- **`pg_column_size()` on an out-of-line TOAST value returns the 18-byte
  pointer**, not the payload. For whole-table comparisons use
  `pg_total_relation_size` on materialized tables. Applied *directly to a table
  column* it does report true stored size, which is what the breakdown query
  below relies on.
- **A value is only compressed once stored.** `pg_column_size(x::jsonb)` on an
  expression returns the uncompressed datum, and a CTE detoasts before
  `pg_column_size` sees it. Materialize every variant into a real table.
- **`INSERT ... SELECT` can carry an already-compressed datum across
  unchanged**, so a naive pglz-vs-lz4 comparison measures identical sizes in
  both arms. Force a fresh datum with `::text::jsonb` and verify with
  `pg_column_compression()`.
- **`TABLESAMPLE SYSTEM` selects whole pages and `LIMIT` stops at the first N
  rows**, so the sample clusters in the physically earliest pages — the *oldest*
  games, which is the wrong population when the question is about new rows. Use
  `BERNOULLI` (independent per-row, costs a heap seq scan) and derive the rate
  from `reltuples` instead of using `LIMIT`.

## Queries

Composition, per column, true stored sizes (read-only):

```sql
SELECT count(*) AS sampled_rows,
       sum(octet_length(choice::text))      AS choice_raw,
       sum(pg_column_size(choice))          AS choice_stored,
       sum(octet_length(action_diff::text)) AS diff_raw,
       sum(pg_column_size(action_diff))     AS diff_stored,
       round(100.0 * sum(pg_column_size(action_diff))
             / nullif(sum(pg_column_size(choice))
                      + sum(pg_column_size(action_diff)), 0), 1) AS diff_pct_of_stored
FROM arkham_steps TABLESAMPLE SYSTEM (0.02);
```

Size breakdown, to re-baseline:

```sql
SELECT pg_size_pretty(pg_relation_size('arkham_steps'))      AS heap,
       pg_size_pretty(pg_total_relation_size(reltoastrelid)) AS toast,
       pg_size_pretty(pg_indexes_size('arkham_steps'))       AS indexes,
       pg_size_pretty(pg_total_relation_size('arkham_steps')) AS total
FROM pg_class WHERE oid = 'arkham_steps'::regclass;
```

New-row volume, which bounds the whole payoff:

```sql
SELECT n_tup_ins, n_live_tup, now() - stats_reset AS window,
       round(n_tup_ins / greatest(extract(epoch from (now() - stats_reset))/86400, 1))
         AS inserts_per_day
FROM pg_stat_all_tables WHERE relname = 'arkham_steps';
```

`jsonb` vs `json` on prod rows, materialized so compression actually happens:

```sql
BEGIN;
CREATE TEMP TABLE _probe AS
  SELECT choice AS as_jsonb, choice::text::json AS as_json
  FROM arkham_steps TABLESAMPLE SYSTEM (0.02);
SELECT round(avg(pg_column_size(as_jsonb))) AS jsonb_bytes,
       round(avg(pg_column_size(as_json)))  AS json_bytes
FROM _probe;
ROLLBACK;
```

After a write flip, the encoding mix (`both_bug > 0` means the
`Games/Shared.hs:600` upsert update-list fix was missed):

```sql
SELECT count(*) FILTER (WHERE choice_z IS NOT NULL) AS compressed,
       count(*) FILTER (WHERE choice   IS NOT NULL) AS legacy,
       count(*) FILTER (WHERE choice IS NOT NULL AND choice_z IS NOT NULL) AS both_bug
FROM arkham_steps TABLESAMPLE SYSTEM (0.02);
```

### Codec comparison script

Compresses each value **independently**, matching how per-row storage works — a
single concatenated stream lets the codec exploit cross-row redundancy and
badly overstates the ratio. Pass `choice` as the fourth argument to measure that
column alone, which is what gets stored once `action_diff` is dropped.

```sh
#!/usr/bin/env bash
# Usage: ./codec-test.sh "<conn>" [rows] [all|recent] [both|choice]
set -euo pipefail
CONN="${1:?connection string required}"
ROWS="${2:-1500}"; SCOPE="${3:-all}"; COLS="${4:-both}"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT

PCT=$(psql "$CONN" -At -c "SELECT round(least(100::numeric, greatest(0.0005::numeric,
        $ROWS::numeric / nullif(reltuples::numeric,0) * 100))::numeric, 4)
      FROM pg_class WHERE oid='arkham_steps'::regclass")

WINDOW_PAGES=40000
if [ "$SCOPE" = recent ]; then
  # Newest rows are in the physically last pages. PCT came from the FULL row
  # count, so scale it up by the window fraction or the sample is ~30x short.
  IFS='|' read -r WHERE PCT <<<"$(psql "$CONN" -At -F'|' -c "
    WITH p AS (SELECT (pg_relation_size('arkham_steps')/8192)::bigint AS total)
    SELECT format('WHERE ctid > ''(%s,0)''::tid', greatest(0, total - $WINDOW_PAGES)),
           round(least(100::numeric,
                 $PCT::numeric * total / least($WINDOW_PAGES, greatest(total,1))), 4)
    FROM p")"
else WHERE=""; fi

psql "$CONN" -At -c "COPY (
  SELECT CASE WHEN '$COLS' = 'choice' THEN choice::text
              ELSE choice::text || E'\t' || action_diff::text END
  FROM arkham_steps TABLESAMPLE BERNOULLI ($PCT) $WHERE
) TO STDOUT" > "$WORK/sample.tsv"
[ -s "$WORK/sample.tsv" ] || { echo "empty sample; ANALYZE arkham_steps?" >&2; exit 1; }

mkdir -p "$WORK/vals"
awk -v d="$WORK/vals" '{ f = d "/" NR ".json"; print > f; close(f) }' "$WORK/sample.tsv"
raw=$(cat "$WORK/vals"/*.json | wc -c | tr -d ' ')
sum_codec() { local t=0; for f in "$WORK/vals"/*.json; do
  t=$(( t + $("$@" -c "$f" 2>/dev/null | wc -c) )); done; echo "$t"; }

printf 'rows %s, raw %s\n' "$(ls "$WORK/vals" | wc -l | tr -d ' ')" "$raw"
for spec in "zstd-3:zstd -3 -q" "zstd-12:zstd -12 -q" "lz4:lz4 -1 -q" "gzip-6:gzip -6"; do
  v=$(sum_codec ${spec#*:})
  printf '%-10s %12s %7sx\n' "${spec%%:*}" "$v" \
    "$(awk -v r="$raw" -v c="$v" 'BEGIN{printf "%.2f", r/c}')"
done
```

## Provenance

Prod measurements were run by the repository owner; no agent connected to the
production database. The composition and `jsonb`-vs-`json` figures come from 352
debug/full game exports in the owner's `~/Downloads` (a full export emits step
rows verbatim — `Api/Handler/Arkham/Game/Debug.hs:71-96`), loaded into temp
tables in the local dev Postgres. Representativeness: those samples measured
2.53x and 2.92x pglz against prod's 2.96x on `choice`.
