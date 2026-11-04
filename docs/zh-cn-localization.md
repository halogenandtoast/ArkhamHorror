# Simplified Chinese localization

The Simplified Chinese UI locale is `zh-cn`. It is independent of `zh`.

`frontend/src/locales/zh-cn.ts` assembles the active messages. A translated JSON
file must also be imported at the same namespace as its English counterpart.
In particular, Hemlock Vale scenarios are named properties of their campaign,
and both Heart of the Elders parts have their own namespaces.

Shared interface text lives in `locales/{en,zh-cn}/interface.json`. Display
tables use `ui.enums` so that, for example, the `ui.keywords` heading cannot be
overwritten by a table of keyword names. Deck-specific messages live in
`gameBoard/deck_ui.json`. Other locales fall back to the English catalog.

`scenarioNames.json` and `campaignNames.json` provide display names keyed by
stable engine IDs. Default English names in old saves are translated when they
match the corresponding metadata exactly; player-authored names remain intact.
Card codes, filter values, weakness-pool tokens, and serialized game messages
retain their original values. Damage/horror assignment labels are translated at
display time, after their counts have been parsed.

Campaign translations were checked against
[arkham-cards-data](https://github.com/zzorba/arkham-cards-data/tree/8b283b0/i18n),
preferring `zh-cn` and using `zh` as a secondary reference. Interface text and
unavailable community display titles were translated from the English source;
card names and keyword terminology also use the project's Simplified Chinese
card data. Community campaign story text still depends on the translations
provided by its authors and can fall back to English.

Run from `frontend`:

```sh
npm run check:zh-cn
npm run tc
npm run build
```

The first command audits visible Vue literals, including component labels,
tooltips, and interpolated text. It preserves trademarks, language names, and
code examples. The integration tests load the real locale modules, check core
UI and official campaign coverage, catch message/group collisions, render
representative controls, and check interpolations and stable identifiers.

For a report of missing or entirely English campaign files, run from the root:

```sh
python3 scripts/audit-campaign-locales.py --output /tmp/campaign-locales-zh-cn.json
```

The report separates community campaigns. It supplements the runtime checks;
file coverage alone cannot detect a missing import or a wrong message namespace.
