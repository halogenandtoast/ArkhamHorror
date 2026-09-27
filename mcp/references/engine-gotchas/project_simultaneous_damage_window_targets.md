---
title: project_simultaneous_damage_window_targets
description: Simultaneous damage (Dark Pact / assignDamage) batches into one window; the investigator TakeDamage window covers self + assets you control (FAQ 2.12)
---

When an investigator is dealt damage via the `DamageAny` assign path (`assignDamage` / `InvestigatorAssignDamage`, e.g. Dark Pact) and splits it among self + Ally assets, the points are **deferred and land in one combined `CheckWindows` batch** (`Arkham/Investigator/Runner/Damage.hs`, the `finalizeDeferredDamageAssignment` / `finalizeDamageAssignment` window builders ~line 434/496). That batch contains `TakeDamage(investigator, n)` PLUS one `DealtDamage(target, amount)` per distinct recipient — so an investigator who took some of the damage themselves appears in **both** a `TakeDamage` and a `DealtDamage` window. Enemy attacks use a different per-point path (`DamageEvenly`) with `ComponentLabel` prompts that do NOT batch the same way.

`runWindow` (`Arkham/Investigator/Runner.hs:397-399`) attaches to a chosen reaction only the windows its own matcher matched — but for a card like Bandages whose matcher is `oneOf [AssetDealtDamage …, DealtDamage …]`, *both* window kinds match, so a `getHealTargets`-style extractor reading raw window targets still gets **duplicates** (the same investigator twice). `nub` them and filter to currently-healable before counting.

Consequence for "react once per damaged entity" cards (e.g. healing like Bandages 08073/12073): reaction abilities default to `PlayerLimit PerWindow 1` (`Arkham/Ability.hs:526`), and the `PerWindow` check treats the whole simultaneous batch as one bucket — so the engine offers the reaction only once for the batch. To heal/affect each distinct entity, resolve them all inside the single activation, and `nub` the window targets and filter to currently-healable before counting (heals are per damaged *entity*, not per point of damage). Fixed in `Arkham/Asset/Assets/Bandages.hs` (issue #4881).

## The investigator's TakeDamage amount is self + your own assets (#5394, then #5411)

`Window.TakeDamage source _ (InvestigatorTarget iid) n` / `Window.TakeHorror` originally reported `length damageTargets` — the **whole assignment**, including points assigned to *other* investigators and *their* assets. #5394 swung to the opposite extreme, `count (== toTarget iid) damageTargets`, which combined with the `| totalDamage > 0` guard suppressed the window **entirely** when an Ally soaked everything. That broke every "after you take damage/horror" reaction on a full soak — Jim Culver (4) / Haunted Musician were the report (#5411).

The rule is **FAQ (2.12)**: an ability reacting to "you" taking or being dealt damage/horror **also covers assets you control**; only points assigned to another investigator or their assets fall outside it. Both finalizers now count with a predicate over `select $ assetControlledBy iid`:

```haskell
InvestigatorTarget iid' -> iid' == iid
AssetTarget aid         -> aid `elem` ownAssets
```

So the window fires (and carries the soaked points in `n`) whenever anything landed on you or your stuff, and is suppressed only when the whole assignment went elsewhere. `Helpers/Window.hs` already documented this above `Matcher.DealtDamage`; #5394 had silently contradicted it.

**Reading targets out of the batch.** The aggregate `TakeDamage`/`TakeHorror` window is not a statement about *which card* took the points — use the per-recipient `DealtDamage`/`DealtHorror` windows for that. Bandages (08073/12073) is the worked example: its `getHealTargets` reads **only** `Window.DealtDamage`, so an investigator who soaked everything onto an Ally is not offered as a heal target (the original #5394 complaint). But that only works if the per-recipient window is *attached* — `runWindow` attaches only the windows the ability's own matcher matched, and **`InvestigatorTakeDamage` does not match `Window.DealtDamage (InvestigatorTarget …)`**. Bandages therefore triggers on `Matcher.DealtDamage`, which matches the aggregate *and* the per-recipient window. If you narrow a window extractor, check the matcher still attaches what the extractor reads.

Caveat: on the `SingleTarget` strategy each recipient is appended to `damageTargets` only **once** regardless of how many points they took, so the reported *amount* is 1 there. That predates all of this and affects the `DealtDamage` windows identically; only the presence/absence of the window is reliable on that path.
