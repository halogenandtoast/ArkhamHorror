---
title: takecontrol-no-placeasset
description: "TakeControlOfSetAsideAsset never pushes PlaceAsset — assets initializing meta in PlaceAsset must also handle TakeControlOfAsset (FHV day-cost residents, issue"
---

`TakeControlOfSetAsideAsset` (Game/Runner.hs ~2963) creates the asset and pushes only `TakeControlOfAsset` — no `PlaceAsset`. Any card that initializes its meta in a `PlaceAsset` handler silently keeps default meta when it enters play via `takeControlOfSetAsideAsset` (e.g. `putAllyIntoPlay` in acts).

**Why:** FHV residents Theo Peters / Judith Park store the campaign day number in meta for their parley cost (`toResultDefault 1 a.meta` in HasAbilities); entering via the Lost Self act left meta null → cost 1 on Day 3 (issue #5092).

**How to apply:** duplicate the meta init under `TakeControlOfAsset _ aid | aid == toId attrs` alongside `PlaceAsset`. Keep the `PlaceAsset` case — preludes create these assets at a location before anyone controls them and the ability is usable uncontrolled. `StartScenario` handling is useless (entities are recreated per scenario; asset doesn't exist yet). Use `dayNumber <$> getCampaignDay` for FHV days.
