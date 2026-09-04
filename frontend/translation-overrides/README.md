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

The importer also consults this file when two upstream pack files disagree about a card: a code
listed here silences the conflict, since the override wins anyway. Use that only for genuine
upstream data bugs, and say which copy you picked.

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

### `fr.json`

Two upstream pack files disagree, which made `--source-locale fr` throw and left
`public/cards_fr.json` frozen at June 2025 with none of the 2026 core set:

- `06168` and `06189` are duplicated between `translations/fr/pack/tcu/tsh_encounter.json` and
  `tde/tsh_encounter.json`. The `tcu/` copies are untranslated English; the `tde/` copies are the
  real translation. Per-field merging resolves `06168` on its own, but `06189`'s English `flavor`
  carries an en-dash where `cards_en.json` has a hyphen, so it is not recognised as a placeholder
  and is pinned here to the `tde/` value.
- `09057` (Fingerprint Kit) appears in both `tsk/tskp.json` and `tdc/tskp.json`, both French,
  differing on one word: the `tsk/` copy calls the card *Kit d'Empreintes Digitales* mid-text while
  naming it *Nécessaire à Empreintes Digitales*. Pinned to the self-consistent `tdc/` copy.

The remaining 171 entries are the **2026 core set (chapter 2)**, contributed in
[#5259](https://github.com/halogenandtoast/ArkhamHorror/issues/5259) and transcribed from the
printed VF cards. These are a bridge, not a correction of a one-off mistake:
`translations/fr/pack/core/core_2026_encounter.json` upstream is a *verbatim English placeholder*
for all 92 encounter cards, and `core_2026.json` covers 96 of the 104 player cards with real typos
(`pas round` for `par round`, `Mélédiction` for `Malédiction`, `monter` for `montrer`). Without
these entries 89 of the 196 chapter-2 cards render entirely in English.

Delete this block once the data lands upstream in `Kamalisk/arkhamdb-json-data` and re-import.
Apostrophes and ellipses are normalized to `'` and `...` to match the rest of the fr corpus.
