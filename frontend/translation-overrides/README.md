# Card translation overrides

Corrections applied on top of the upstream translations in
[`Kamalisk/arkhamdb-json-data`](https://github.com/Kamalisk/arkhamdb-json-data) when generating
`public/cards_<lang>.json`.

`scripts/import-card-translations.cjs` loads `<source-locale>.json` from this directory and
applies it **after** the upstream merge, so a bad upstream entry no longer comes back the next
time someone runs `npm run import-zh-cn-cards`.

Each file is a JSON array of partial card records. `code` is required; every other key must be
one of the importer's `TRANSLATED_FIELDS` (`name`, `text`, `flavor`, `traits`, `back_text`, …).
Only list the fields that are actually wrong — everything else falls through to upstream.

```json
[
  { "code": "09676b", "name": "怜悯同情之痛", "text": "…" }
]
```

Overrides are matched by exact card code and are deliberately excluded from the importer's
reprint-reuse pass, so an override never leaks onto a different printing.

**These entries are temporary.** When upstream fixes the underlying translation, delete the
entry here and re-import.

## Current entries

### `zh-cn.json`

- `09675b` / `09675d` (Buried Miner: *A Lost Memento* / *Exhume the Bones*) and
  `09676b` / `09676d` (Slain Foreman: *Sympathy Pain* / *Familial Pain*) have their `name` and
  `text` swapped between the two story sides in
  `translations/zh-cn/pack/tsk/tskc.json`. Each card therefore quoted the wrong campaign-log
  key — Sympathy Pain read "换到了一只小猫" (*traded for a kitten*) instead of
  "分担了深沉的痛苦" (*shared a deep pain*), so players could not tell why the engine flipped
  the enemy back instead of adding it to the victory display. See
  [#5364](https://github.com/halogenandtoast/ArkhamHorror/issues/5364).

  The `flavor` on those entries is already correct and is left to upstream.
