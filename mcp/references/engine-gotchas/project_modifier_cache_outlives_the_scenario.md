---
title: project_modifier_cache_outlives_the_scenario
description: "`preloadModifiers` is `pure g` in campaign-only mode, so `gameModifiers` is NEVER rebuilt between scenarios — the finished scenario's rows (dead assets, the agenda's HandSize, an AsIfInHandFor into a card the deck upgrade deregisters) are what the whole interlude reads"
---

```haskell
preloadModifiers g = case gameMode g of
  This _ -> pure g   -- Game.hs:7393
  _ -> …rebuild…
```

`ForTarget GameTarget (EndOfScenario _)` (`Game/Runner.hs:786`) switches the mode
to `This c` and wipes the entities — locations, enemies, acts, agendas,
treacheries, events, skills, stories, every asset without `Persist`, every
effect that is not `effectIsForResolution`/`effectIsForNextGame`. The modifier
map is derived from exactly those entities, and the line above means nothing
rebuilds it until the next scenario is current. So whatever the map held when
the last scenario ended is what the entire between-scenarios phase reads:
interludes, `SufferTrauma`, `ReloadDecks`, `RemoveInvestigatorFromCampaign`, the
upgrade handler.

A TFA save after The City of Archives, parked on `ChooseUpgradeDeck` with zero
entities in the game, still had 17 targets and 41 rows:

```
InvestigatorTarget c08010 <- AssetSource 5bfed775… AsIfInHandFor   -- Backpack, holding a 2nd Backpack
InvestigatorTarget c08010 <- AgendaSource c04239   HandSize        -- Lost Memories, -2, during the upgrade screen
LocationTarget ea679eaf… <- TreacherySource …      CannotInvestigate
AssetTarget 2e351945…    <- InvestigatorSource …   AddTrait        -- source alive, target gone
```

That is also how it turns into a 500. `FinishedUpgradingDecks` regenerates each
deck with fresh card ids and `removeCard`s the old ones, so the surviving
`AsIfInHandFor ForPlay <old id>` named a card no longer in `gameCards`;
`getPlayProhibitions` (`Helpers/Playable.hs:308`) calls `getAsIfInHandCards` and
`withInvestigatorConnectionData` (`Game.hs:679`) runs it for every investigator
on every serialization, so `getCard`'s `error "Unregistered card id"` fired on
every read of the game. Before that caller existed (2026-10-23) the same state
only crashed on the next action, through `preloadEntities` →
`getAsIfInHandEffectCards`. Both now resolve with `getCardMaybe`: a card the
registry no longer holds is in nobody's hand.

**Prune, do not clear.** The modifiers that are meant to cross the boundary are
republished by what survives — a `Persist` asset, a setup-window effect, the
campaign (`campaignModifiersForAll`), `activeUltimatumsAndBoons` — but in `This`
mode nothing republishes anything, so clearing the map drops them for the whole
interlude. `keepCardCache = setupModifier GameSource GameTarget Persist`
(`Message/Lifted/Card.hs:151`) is the concrete casualty: it is a
`WindowModifierEffect` with `EffectSetupWindow`, kept by the teardown and read
back out of the map by `ForTarget GameTarget ResetGame` (`Game/Runner.hs:830`)
at the next scenario. The teardown therefore drops rows whose source or target
names an entity it just removed, keeping anything a surviving effect published
(`modifierEffect`), and `ScarletKeyTarget` is deliberately absent from both
lists — keys are campaign state and the teardown does not touch them.

`ForTarget GameTarget ResetGame` has the same omission and is NOT pruned: it is
the handler that reads `Persist` out of the map, and at a scenario start the
rebuild runs anyway.

The guard itself is deliberate (`Game.hs:7387`): "prevent scenario specific
modifiers from causing an exception. For instance when we need to call
`getVengeanceInVictoryDisplay`" — `getVictoryDisplay` is
`scenarioField ScenarioVictoryDisplay` → `selectJust TheScenario`, which throws
with no scenario. Added in `e2b954f3a9` (2022-11-01), when `preloadModifiers`
built the map over `map CardTarget cards <> map (CardIdTarget . toCardId) cards`
for `cards = allCards g` — every card in every deck, so scenario card code
really did run in campaign mode. Today it walks entities only, every vengeance
caller is a location/act/enemy/treachery that the teardown removes, and no
`HasModifiersFor` under `Investigator/`, `Campaign/`, `Effect/` or `Asset/Cards/`
calls a scenario-requiring helper. Flipping the guard is therefore plausible and
is the real fix for the frozen cache, but a modifier instance only has to call
`select` to reach one of the ~10 `scenarioField` sites inside the query
evaluator, so it needs its own test pass, not a rider.
