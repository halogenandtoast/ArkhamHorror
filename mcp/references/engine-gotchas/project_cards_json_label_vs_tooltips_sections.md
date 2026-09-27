---
name: project_cards_json_label_vs_tooltips_sections
description: cards.json has two sibling sections — `label` (choice labels via labeled') and `tooltips` (ability tooltips via withI18nTooltip); the i18n migration scattered choice strings into `tooltips` under keys the engine never looks up, so 17 cards rendered raw key paths (#5390)
metadata:
  type: project
---

`frontend/src/locales/<locale>/cards.json` has exactly two top-level sections, and they
are read by **different** backend helpers:

| section | written by | key the engine emits |
|---|---|---|
| `label` | `labeled'` / `labeledValidate'` inside `cardI18n $ scope "<card>"` | `cards.label.<card>.<key>` |
| `tooltips` | `withI18nTooltip "<card>.<key>"` (`Arkham/Ability.hs`) | `cards.tooltips.<card>.<key>` |

`labelKey` (`Arkham/I18n.hs`) is what makes the first row work: for scope `["cards", "<card>"]`
it splices `label` in after `cards` rather than appending it, producing
`cards.label.<card>.<key>`. `frontend/src/locales/en.ts` then exposes the same map twice —
`cards` (whole file) and `label.cards` (just the `label` sub-object) — so both
`cards.label.X.Y` and `label.cards.X.Y` resolve.

**The trap.** A card's choice strings can sit in `tooltips` under exactly the right key
names and still render as raw `cards.label.<card>.<key>` paths, because nothing reads
`cards.tooltips` unless the Haskell calls `withI18nTooltip`. The 2026 backend i18n migration
did this to 17 cards: an early commit dumped choice strings into what became `tooltips`, a
later commit added a `label` block for the same card with *machine-derived* key names
(`ability`/`move`/`discover`) that never matched the `labeled'` calls. Task Force (10027) was
the reported symptom (#5390); Aetheric Current (Yuggoth), Devil (Friend or Foe) (2),
Hand-Crank Flashlight, Local Map, Sledgehammer (3), Trusty Bullwhip (Advanced),
Broken Bottle, Call for Backup (2), On the Trail (1) and Throw the Book at Them were the rest.

**Two things that look broken but aren't.** Both reset the scope, so their keys live in the
*global* `label.json`, not in `cards.json`:

- `withI18n` / `unscoped` around a `labeled'`
- `labeledI` (always `withI18n $ labelKey`)

Any audit over `labeled'` call sites must check the global `label.json` too, or it will
false-positive on Blood Rite, Eye of Chaos, Kukri, Nautical Prowess and Enervation.

**Plural strings need `countVar`.** Global entries like
`discardRandomCardsFromHand: "Discard 1 random card… | Discard {count} random cards…"` are
vue-i18n plural messages; without a `countVar n` wrapper the raw pipe-separated string is
rendered. Alien Whispers shipped without it (fixed alongside #5390); Wild Compulsion,
Nile River, Well of Souls and the Mirror Nests all wrap correctly.

## Audit script

Run this after touching card i18n — it should print nothing:

```python
import json, re, os
lab = json.load(open('frontend/src/locales/en/cards.json'))['label']
glob = json.load(open('frontend/src/locales/en/label.json'))
for dp, _, fs in os.walk('backend/arkham-api/library'):
    for fn in (f for f in fs if f.endswith('.hs')):
        p = os.path.join(dp, fn); txt = open(p, errors='replace').read()
        if 'cardI18n' not in txt: continue
        sc = {s for s in re.findall(r'scope "([a-zA-Z0-9_]+)"', txt)} - {'cards', 'tooltips'}
        if len(sc) != 1: continue
        s = sc.pop(); have = set(lab.get(s) or {})
        for k in set(re.findall(r"labeled(?:Validate)?'\s+(?:\w+\s+)?\"([a-zA-Z0-9_]+)\"", txt)):
            if k not in have and k not in glob:
                print(f'{s}.{k}  ({p})')
```

Before deleting a `tooltips.<card>` block, confirm nothing references it:
`grep -rE '[Tt]ooltip[^"]*"<card>\.' backend/arkham-api/library` must be empty.

Related: [[project_i18n_lazy_loading]]
