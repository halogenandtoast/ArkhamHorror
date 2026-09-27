---
name: project_reaction_default_limit_is_per_player
description: "Shared-card abilities need GroupLimit PerWindow to dedupe across seats: reactions default to PlayerLimit (add groupLimit), and an explicit noLimit strips the correct forced default (drop it)."
metadata: 
  node_type: memory
  type: project
  originSessionId: e14b6a49-51b7-4a2f-899c-5c906dec5fa2
  modified: 2026-09-06T13:29:57.446Z
---

`defaultAbilityLimit` (`Arkham/Ability.hs`) maps `ReactionAbility _ _ _ -> PlayerLimit PerWindow 1`
(forced abilities get `GroupLimit PerWindow 1` — the asymmetry is the trap). The `PlayerLimit
PerWindow` branch of `getCanAffordUseWith` (`Arkham/Helpers/Ability.hs`) only inspects the
*asking* investigator's `usedAbilities`; only `GroupLimit PerWindow` scans every investigator.

So any `[reaction]` on a **shared** card — agenda, act, location, enemy, treachery — that prints
no limit is offered again to each other eligible seat for the *same* window occurrence.

**Why:** RR "Ability" → Triggered Abilities: *"Each [reaction] ability may be triggered only once
each time the specified condition on the ability is met."* One card = one ability instance = one
use per timing point, regardless of who uses it. `PlayerLimit` is only right for player cards,
where each copy is a distinct source and therefore a distinct ability anyway.

**How to apply:** wrap shared-card reactions in `groupLimit PerWindow` (`Arkham/Ability.hs`), the
established convention — 14 agenda and 192 location files already do, e.g.
`Location/Cards/TheForgottenAge/TheBoundaryBeyond/SacredWoods_184.hs`. This does **not** over-block:
both branches count via window intersection, so a later occurrence (a different `Window` value)
re-arms the ability.

Found #5621 (One Last Job agendas 11502/11503, `DiscoveringLastClue`), then **again** in #5632
(Chamber of the Tablet Unsealed, location 11604, also `DiscoveringLastClue`) — the second seat's
trigger crashed on `getSetAsideCard` because the first seat had already taken the Tidal Tablet.
Pair `groupLimit PerWindow` with an `exists (SetAsideCardMatch ...)` criterion whenever the handler
calls a partial `getSetAsideCard`. `Eq Ability` compares only source+index+cardCode, **not** the
limit, so switching a card to `GroupLimit` does re-gate already-persisted `PlayerLimit` use records
— a game whose *queue* is replayed is fixed retroactively (only a persisted *question* needs
`--undo`). Enforcement rides the
`Do (CheckWindows ws)` re-check behind a multi-seat ask — see
[[project_windowask_stale_seat_reask]]. Records already written keep their old limit stamp, so a
live game parked on the bad ask is not retroactively fixed —
[[used-ability-record-keeps-original-limit]]. The engine-wide default was left alone deliberately
(regression surface); revisit if this recurs.

## The mirror case: `noLimit` on a shared-card forced ability

A `forcedAbility` already gets the right `GroupLimit PerWindow 1` by default — but wrapping it in
`noLimit` throws that dedupe away and reintroduces exactly the bug above. `GroupLimit PerWindow` is
**the** cross-seat guard for forced abilities, enforced at
`Investigator/Runner.hs` `ResolveWindowInitiations`:

```haskell
remaining <- filterM (\(ability, ws, _) -> getCanAffordAbility iid ability ws) pending
```

`Do (CheckWindows ws)` fans out to *every* investigator, so each seat queues its own
`ResolveWindowInitiations`. The first seat's press records a `UsedAbility` against that window;
the second seat's `ResolveWindowInitiations` re-filters and drops it. With `noLimit`,
`getCanAffordAbility` is unconditionally `True` and the second seat gets the same button.

The Chariot VII (c05285) ability 2 had `noLimit`, so a breach placed on a 3-breach location
prompted **both** players and resolved two incursions — 2 doom instead of 1, and a doubled breach
on every connecting location (#5756). Fix was deleting the `noLimit $` wrapper.

**Why the `noLimit` was there:** added in `c426d23761` ("Fix chaining breaches") alongside the real
chaining fix (`EqualTo 3` → `atLeast 3`). It was never needed for chaining — `wouldWindows`
(`Helpers/Window.hs`) mints a **fresh `batchId` per placement** and `Window` equality is
`(timing, windowType, batchId)` (`Window.hs`), so a `PerWindow` bucket is scoped to one
placement event. A chained incursion is always a different window and re-arms normally.

**How to apply:** treat `noLimit` on a window-triggered `forcedAbility` as a smell — it is almost
never what you want on a shared card. If a forced ability must fire more than once, rely on the
windows being distinct rather than removing the limit. Only one other site does this
(`Enemy/Cards/CurseOfTheRougarou/TheRougarou.hs`), and it self-limits via a `damagePerPhase`
criterion.
