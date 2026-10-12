---
title: project_jsonb_columns_cost_40_percent_over_json
description: "A jsonb column stores identical content ~40% larger than json or text, because key reordering destroys the locality pglz compresses on; measuring it needs materialized values, since pg_column_size on an expression or a TOAST pointer reports the wrong thing"
---

`jsonb` is the wrong type for an opaque blob column, and the cost is large.
Measured on real `arkham_steps.choice` payloads (1,200 rows, materialized into
local Postgres 16, `pg_total_relation_size`):

| stored as | bytes | ratio vs raw |
|---|---|---|
| `jsonb` | 4,603,904 | 2.53x |
| `json` | 3,325,952 | 3.50x |
| `text` | 3,325,952 | 3.50x |

`json` and `text` come out **byte-identical**; `jsonb` costs **+38%**. Two
causes: `jsonb`'s per-element JEntry headers, and its key reordering
(shortest-first), which destroys the key locality pglz's window exploits. The
same effect was independently measured at +40% on a 3,000-row sample.

This is why `arkham_steps` appeared to compress at only ~2.7–3.0x. pglz was not
doing badly — the column type was sabotaging it. Prod `choice` measures 2.96x as
`jsonb`; app-level gzip-6 or zstd-3 on the same content reaches 7.1x.

Nothing in this codebase needs `jsonb` semantics for these columns: there is not
one `->`/`->>`/`@>` against `arkham_steps` anywhere, and
`Entity/Arkham/Step.hs:33,47` already declare `sqlType _ = SqlString`. Note
`persistent` does not care either — `toPersistValue` emits
`PersistLiteralEscaped` (an untyped literal PG coerces into a `json` column) and
`fromPersistValueJsonB` accepts both `PersistText` and `PersistByteString`.

**The catch:** `ALTER COLUMN ... TYPE` rewrites the relation and needs headroom
equal to the table. On `arkham_steps` (94 GB, 4 GB free) that is impossible, so
the fix is a new nullable column plus a read fallback, never a type change. See
`docs/step-storage.md`.

**How to apply:** when adding a column that stores a serialized blob the
database will never query into, declare it `json` (keeps validity checking and
the operators for ad-hoc queries, at `text`'s storage cost) or `bytea` with
app-level compression — not `jsonb`. Reserve `jsonb` for columns something
actually indexes or traverses.

**Measuring any of this** has four traps, each of which silently reports a
wrong-but-plausible number:

- `pg_column_size()` on an **out-of-line TOAST value returns the 18-byte
  pointer**, not the payload. Use `pg_total_relation_size` on materialized
  tables for whole-table comparisons. Applied directly to a table column it does
  report true stored size.
- **A value is only compressed once stored.** `pg_column_size(x::jsonb)` on an
  expression returns the uncompressed datum, and a CTE detoasts before
  `pg_column_size` sees it. Materialize every variant into a real table.
- **`INSERT ... SELECT` can carry an already-compressed datum across
  unchanged**, so a pglz-vs-lz4 comparison measures identical sizes in both
  arms and looks like "no difference." Force a fresh datum with `::text::jsonb`
  and verify with `pg_column_compression()`.
- **`TABLESAMPLE SYSTEM` selects whole pages and `LIMIT` stops at the first N
  rows**, clustering the sample in the physically earliest pages — the oldest
  rows, the wrong population whenever the question is about new writes. Use
  `BERNOULLI` and derive the rate from `reltuples` instead of using `LIMIT`.

Also: Postgres TOAST compression offers exactly `pglz` and `lz4` (there is no
`lzf`, and `zstd` is not a TOAST method in any release). `lz4` measured **4.3%
worse** than `pglz` on this data — it is a speed-for-ratio trade by design,
added in PG14 because pglz *decompression* was a CPU bottleneck. The in-database
codec is not a space lever.

Related: [[project_arkham_steps_is_the_whole_database]],
[[project_game_archiving_to_s3]].
