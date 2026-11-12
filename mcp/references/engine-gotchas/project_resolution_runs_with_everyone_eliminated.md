---
name: project_resolution_runs_with_everyone_eliminated
description: a scenario resolution runs with every investigator eliminated, so any query keyed on InvestigatorWithId answers nothing — SealedOnInvestigator counted 0 sealed blood and no one got The Blood Blight (#5986)
metadata:
  type: project
---

By the time `ScenarioResolution` is processed, **every** investigator satisfies
`isEliminated` (`Helpers/Investigator.hs:657` — `resigned || defeated ||
eliminated`), not just the ones who resigned or were defeated. In the #5986
export both seats were eliminated: one `resigned`, the other with the bare
`eliminated` flag from `Investigator/Runner/Damage.hs:317`.

`getInvestigatorsMatching` (`Game.hs:1103-1120`) pre-filters the candidate list
to uneliminated investigators unless the **top-level** matcher satisfies
`includeEliminated` (`Game.hs:1142-1156`). `Anyone` is on that list;
`InvestigatorWithId` is **not**. So at a resolution

```haskell
selectCount $ SealedOnInvestigator (InvestigatorWithId iid) #blood
```

answered `0` for everyone, while the sibling helper `getSealedChaosTokens`
(`Helpers/ChaosBag.hs:57`, which uses `Anyone`) found all six tokens — the same
handler released seals it had just claimed nobody held. New Horizons resolution
5, River of Blood resolution 4 and Blood Money's shared `DoStep 1` all read that
way, so no Children of Blood investigator ever gained The Blood Blight, and the
*Friends in Low Places* `noBloodBlight` check then passed and handed out the
Charlie Kane reward.

Nothing errors and nothing warns: `when (sealed >= 2) $ addCampaignCardToDeck …`
simply never pushes. The tell in an export is a resolution step whose queue holds
the unseals and the `AskMap` but **no** `AddCampaignCardToDeck`.

Fixed inside the matcher — `SealedOnInvestigator` now wraps its investigator
matcher in `IncludeEliminated` (`Game.hs:5757`). Where a token physically sits is
not a question about whether its holder can still act, and that keeps it
consistent with `getSealedChaosTokens`. Deliberately narrower than the
`includeEliminated` change rejected in
[[project_affectsothers_drops_include_eliminated]]: that one would have recursed
into `InvestigatorIfThen` and flipped ~22 `affectsOthers Anyone` targeting call
sites, whereas every other `SealedOnInvestigator` user runs mid-scenario against
uneliminated investigators and is unchanged.

**How to apply:** at a resolution, an interlude, or anything after
`EndOfScenario`, treat `InvestigatorWithId`/`You`-keyed selects as answering
nothing. Loop with `allInvestigators` (it is `select Anyone`, so eliminated seats
are included) or `campaignInvestigators`, and wrap any inner per-id matcher in
`IncludeEliminated`.

**Verifying:** `arkham-replay --undo 2` on such an export is already past the
handler — the seal count was computed synchronously when `ScenarioResolution`
ran, so the rewound state still shows the tokens and proves nothing. Undo far
enough that the resolution **message** is re-processed (`--undo 3` here, landing
on the act's `AllUndefeatedInvestigatorsResigned` Objective), then walk forward
with `--answers`. `questionVersion` is optional in a `{"tag":"Answer"}` entry
(`Entity/Answer.hs:125`), so a chain of `{"choice":N}` answers works without
knowing each step's version; the `ContinueCampaign` prompt at the end needs a
`CampaignStepAnswer` instead.
