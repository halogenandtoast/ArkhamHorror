---
title: project_cardcode_loose_eq_list_membership
description: "`Eq CardCode` cross-matches a/b (and c/d) sides, so `elem`/`notElem` over a card-code list silently catches the sibling side — adding \"08648b\" to duplicatedScenarios also dropped 08648a. Ord is exact, so Map/Set keys are safe; use CardCodeExact for membership"
---

`Eq CardCode` (Card/CardCode.hs) is **not** structural. Two codes are equal when they share a base
and their side suffixes complement each other:

```haskell
(CardCode a) == (CardCode b) = a == b || (toBase a == toBase b && complements (sideOf a) (sideOf b))
  -- 'a' <-> 'b', 'c' <-> 'd'
```

So `"05178a" == "05178b"` is **True**. Unsuffixed does not cross-match (`"06169" == "06169a"` is
False — `sideOf` is `Nothing`, and `complements Nothing _ = False`), and the two escape hatches
`exceptionCardCodes` / `distinctPrintingCardCodes` force exact comparison for the codes listed there
(The Stranger; Written in Rock's rail tunnels).

**`Ord` is derived newtype-style from `Text`, so it is exact.** That split means:

- `Map CardCode v` / `Set CardCode` are keyed by `compare`, so `a` and `b` sides are **distinct**
  keys and `Map.lookup` / `Map.member` never cross-match. Safe.
- Anything going through `==` — `elem`, `notElem`, `lookup`, `filter (== code)`, `nub` — **does**
  cross-match, and there is no type error to warn you.

Real bug: adding `"08648b"` to `duplicatedScenarios` (Scenario.hs) to drop The Heart of Madness
part 2 from `allScenarioCards` also dropped `"08648a"`, because that list is consumed by
`filter ((`notElem` duplicatedScenarios) . fst)`. The list already contains both `"04205a"` and
`"04205b"` — redundant under loose `Eq`, which is the tell that the author expected exact matching.

Use `CardCodeExact` (`exactCardCode`, `cardCodeExactEq`) whenever you mean "this exact printed
side". `browsableCardDefs` (Api/Handler/Arkham/Cards.hs) pairs a card with its `cdOtherSide` over a
`Set CardCodeExact` for precisely this reason: with plain `CardCode` the nine `89010a`..`89010i`
Replicating Aberrations would fold into each other.

See also [[project_enemyis_loose_ab_crossmatch]].
