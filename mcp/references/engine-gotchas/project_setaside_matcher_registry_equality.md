---
title: project_setaside_matcher_registry_equality
description: getSetAsideCardsMatching compares against the card registry by Eq, so a card you set aside as an entity's own toCard value is invisible to it — read ScenarioSetAsideCards directly
---

`getSetAsideCardsMatching` (`Arkham.Helpers.Query`) is `select . SetAsideCardMatch`, and
`SetAsideCardMatch` (`Arkham/Game.hs`) filters the **card registry** (`gameCards`) down to the
cards that are `elem` `ScenarioSetAsideCards`. That `elem` is structural `Eq Card`, not an id
comparison.

So when a card enters the set-aside pile as an entity's *rebuilt* card value —
`push $ SetCardAside (toCard attrs)`, where `toCard` on the attrs is `defaultToCard` — it never
compares equal to the registry instance for the same card id, and `getSetAsideCardsMatching`
returns nothing for it. The pile really does contain the card; the matcher just cannot see it.

**How to apply:** when you set aside a card you built from an entity's attrs and later need it
back, read the pile directly instead of matching:

```haskell
setAsideAgendas <-
  filter ((== AgendaType) . toCardType) <$> scenarioField ScenarioSetAsideCards
```

Found implementing Lost Quantum's The Quantum Maelstrom (Dark Matter), whose advance sets the
current agenda aside and, once nothing is below it, shuffles it back together with every set aside
agenda to form a new agenda deck. `SetCurrentAgendaDeck` then pulls those cards back out of the
set-aside pile for you (`setAsideCardsL %~ filter (notElem stack)`).

Related: [[project_alternate_printings_distinct_carddefs]].
