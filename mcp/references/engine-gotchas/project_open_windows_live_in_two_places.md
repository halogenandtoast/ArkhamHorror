---
title: project-open-windows-live-in-two-places
description: "Rewriting an open window must reach ResolveWindowInitiations too, not just CheckWindows — the PerWindow limit intersects the pending set with usedAbilityWindows, so a one-sided rewrite makes the limit vacuous (Diving Suit looped, #5769)"
---

`ReassignDamage`/`ReassignHorror` (`Game/Runner.hs`) move a point of damage off the
investigator and onto an asset, then keep two copies of the *same* window list in sync so
the forced ability that did it cannot fire again:

1. `replaceWindowMany` (`Helpers/Window.hs`) rewrites the live windows
   (`PlacedToken src (InvestigatorTarget iid) Damage n` → `… (AssetTarget aid) …`, splitting
   when `m > n`);
2. `rewriteUsedAbilityWindows` (`Game/Runner.hs`) applies the identical rewrite to every
   investigator's recorded `usedAbilityWindows`.

Both halves are load-bearing, because `GroupLimit PerWindow` / `PlayerLimit PerWindow`
(`Helpers/Ability.hs`) is an **intersection**, not a counter:

```haskell
countInWs u = length (filter (`elem` ws) (usedAbilityWindows u))
```

Rewrite one side only and the intersection is empty — the limit reports "unused" forever.

**The trap:** since #5743 the live window list is no longer only in `CheckWindows`. The
materialised forced-initiation set is carried as **data** in
`ResolveWindowInitiations InvestigatorId [Window] [(Ability, [Window], [Message])]`, and by
the time the chosen ability actually resolves, that marker is queued behind a
`MoveWithSkillTest` — which `instance QueueWrapper Message` deliberately neither strips
(`stripQueueWrappers`) nor exposes as a group (`queueGroup`). `replaceWindowMany` used
`replaceAllMessagesMatching` on `CheckWindows`/`Do (CheckWindows)`, so it saw neither the
constructor nor the wrapper and the pending set stayed stale.

Result (#5769): Diving Suit's `forced $ PlacedCounter #when You AnySource #damage (atLeast 1)`
reassigned the same 2-damage Slitherer in Darkness attack onto itself over and over. The
export showed the desync exactly — pending initiation still
`PlacedToken … (InvestigatorTarget c07002) Damage 2`, every `usedAbilityWindows` entry
`PlacedToken … (AssetTarget 6ba41873…) Damage 1`. `handleReassignDamage` floors the
investigator's assigned damage at `max 0`, so the investigator stopped changing while
`PlaceTokens … #damage 1` kept landing: 14 damage on a 3-health asset, prompt still open.

`replaceWindowMany` is now a `mapQueue` that rewrites `CheckWindows` **and**
`ResolveWindowInitiations` (its own `[Window]` and each pending entry's), recursing through
`Do`, `MoveWithSkillTest`, `Priority`, `Retain`, `Run` and `Simultaneously`. Deliberately
*not* fixed by widening `queueGroup`/`stripQueueWrappers` for `MoveWithSkillTest`:
`Arkham/Message.hs` warns that rewrites ~150 call sites, and `wrapMessagesMatchingNested`
(which `handleDoUseAbility` uses to glue pending window effects behind a nested test) would
start double-wrapping.

**How to apply:** any new primitive that rewrites, splits or retargets an *open* window has
to reach every carrier of that window — `CheckWindows`, `Do (CheckWindows)`,
`ResolveWindowInitiations`, and the recorded `usedAbilityWindows` — or the window-scoped
limits silently stop working. `WindowAsk ws pid q` is a fourth carrier; it holds its
initiations inside `UI Message` choices and is already consumed on this path, but a future
rewrite that runs while another seat's ask is queued would need it too. Related:
[[project_window_effect_suspension]] (the #5743 materialised set),
[[project_forced_ability_defaults_are_group_limits]] (why Forced is a GroupLimit at all),
[[project_queue_scan_must_see_through_would_batches]] (same class: a flat queue scan missing
a nested carrier).
