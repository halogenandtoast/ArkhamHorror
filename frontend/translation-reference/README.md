# Translation provenance

Where third-party-sourced translations in `src/locales` and `../homebrew/*/locales` came from.

`arkham-cards-data-zh-cn.json` is the record for the 172 `zh-cn` messages filled in
[#5952](https://github.com/halogenandtoast/ArkhamHorror/pull/5952) from
[`zzorba/arkham-cards-data`](https://github.com/zzorba/arkham-cards-data) at
`8b283b08e290d982ee83ec03960e0c4a1e293587`, across 44 locale files.

**It holds no translation text** — only, per message, the locale file and key path it landed at,
the upstream `.po` file and line it came from, and `sha256` of both the English source and the
Chinese result. Nothing imports it at runtime.

It is not only an audit trail, though: `tests/zhCnLocaleLoading.test.mjs` asserts every one of
the 172 entries, both hashes included. **So these 172 strings are hash-locked in both
languages.** Editing one of them — fixing a translation, or just reworking the English copy —
fails that test until you update the matching `englishSha256` / `translationSha256` here.

That is worth knowing before you reach for it. The English-side check is useful (it flags a
translation gone stale against reworded English), but the translation-side check mainly forbids
improving a translation, which is not what we want to make hard. If it starts costing more than
it catches, drop the `translationSha256` assertion from that test and keep the rest.

Two entries are worth a second look if either reads oddly, because they were not whole-string
matches from the matching campaign:

- `agesUnwound.aWorldTornDown.rules.haunted.body` was assembled from fragments of
  `i18n/zh-cn/campaigns/side/film_fatale.po:14` (`matchKind: aligned-fragment`; every other
  message matched a whole `msgid`).
- `agesUnwound.unstuck.eagerSphinx.label.gainThreeResources` is the one entry converted
  Traditional→Simplified (OpenCC `t2s`).

Matching is by English string, so a generic label can legitimately source from an unrelated
campaign's catalogue — `gainThreeResources` above came from a Hemlock Vale prologue file.

## Refreshing

The one-off Python importer that produced this manifest was not kept: `frontend/` is otherwise
Node-only, so it could not run under `npm test` and its dependencies (`polib`, `opencc`) had no
other use here. For the next bulk import, extend the existing Node path —
`scripts/import-card-translations.cjs`, `npm run import-zh-cn-cards` and
`../translation-overrides/` — rather than reviving it. Recover it from #5952 if the PO-parsing
and zh→zh-cn fallback logic is useful as a reference.

Note that `zh` is **Traditional** Chinese and `zh-cn` is **Simplified** (see
`src/locales/language.ts`, which maps the `hant` subtag to `zh`). They are separate catalogues,
not duplicates.
