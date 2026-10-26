---
title: project_setasideevery_cannot_set_aside_a_weakness
description: "`setAsideEvery (cardIs someWeakness)` sets aside NOTHING — gatherEncounterSet drops every card with a cdCardSubType, so the gathered pile a guide's 'set the copies aside' line draws from never held it; use setAsideEveryCopyOf"
---

```haskell
gather Set.Unstuck
setAsideEvery $ cardIs Treacheries.timeRunsBackwards   -- sets aside ZERO cards
```

`setAsideEvery` is `fromGathered matcher`, and the gathered pile comes from
`gatherEncounterSet` (`Helpers/EncounterSet.hs`), whose def filter includes

```haskell
isNothing . cdCardSubType  -- weaknesses are never shuffled into the encounter deck
```

A weakness def is `weakness cardCode name = baseTreachery … (Just Weakness)`
(`Treachery/CardDefs/Base.hs:52`), so `cdCardSubType` is `Just Weakness` and
`gather` never produces it — no matter that the def carries
`cdEncounterSet = Just theSet` and `cdEncounterSetQuantity = Just 4`. That
filter is correct; the weakness must stay out of the encounter deck. But
`setAsideEvery` then matches an empty pile, adds nothing, and **reports
nothing**: an empty matcher result is legitimate for a conditional set, so
there is no assert to trip and no warning to read.

The damage shows up at the far end, in whatever reads the pool back:

```haskell
copies <- getSetAsideCardsMatching $ cardIs Treacheries.timeRunsBackwards  -- []
for_ (zip defeated copies) \(iid, card) -> addCampaignCardToDeck iid DoNotShuffleIn card
```

`zip xs [] = []`, so the loop body never runs. The flavor text around it still
prints, which makes the symptom "the resolution read Investigator Defeat and
nobody got the weakness."

**How to apply:** use `setAsideEveryCopyOf :: CardDef -> ScenarioBuilderT m ()`
(`Scenario/Setup.hs`), which mints `cdEncounterSetQuantity` copies straight into
`scenarioSetAsideCards` instead of taking them from the gather. Keeping them in
the pool is what makes the distinct-copy pairing in
[[project_addcampaigncardtodeck_reuses_the_set_aside_copy]] both necessary and
correct, and it keeps the printed copy count as a real cap.

The alternative shape, for a weakness no guide line says to set aside, is the
Forgotten Age one: never pool it, hand out `genCard`-minted copies by def and
count the handed-out ones against the printed total — `getSetAsidePoisonedCount`
(`Campaigns/TheForgottenAge/Helpers.hs:165`) is literally
`4 - selectCount (InDeckOf Anyone <> basic (cardIs Treacheries.poisoned))`.

Found in Ages Unwound: Unstuck's Investigator Defeat (Time Runs Backwards) and
A Year to Plan's Resolution 1 (Curse of a Thousand Winters) were both dead. The
other `setAsideEvery` call sites in that campaign — Stirring Titan, Hired Thugs,
Time Spirit — are ordinary encounter cards with no subtype and gather fine.
