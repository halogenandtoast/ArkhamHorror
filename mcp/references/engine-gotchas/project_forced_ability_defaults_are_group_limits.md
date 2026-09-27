---
name: project_forced_ability_defaults_are_group_limits
description: "Every ForcedAbility default limit is a GroupLimit — it IS the cross-seat dedupe for the CheckWindows fan-out; a PlayerLimit on a shared source offers the trigger once per seat, and if the source then leaves play the unclaimed UseAbility loops the initiation queue forever (#5761)"
metadata:
  node_type: memory
  type: project
  originSessionId: 1b02d173-d8c2-4340-b651-337eda23b077
  modified: 2026-09-23T10:14:52.338Z
---

`Do (CheckWindows ws)` fans out to **every** investigator and each one queues its own
`ResolveWindowInitiations` (`Investigator/Runner.hs`). Nothing dedupes that at window-open
time — the dedupe is the **`GroupLimit` default** on `ForcedAbility`: the re-filter at
`ResolveWindowInitiations` drops the later seats' initiations once the first seat's use is
recorded. `7713e59ed4` (#5756, Chariot VII incursion resolving twice) is the same bug from
the other direction, via an explicit `noLimit`.

`defaultAbilityLimit` (`Arkham/Ability.hs`) used to hand out **player** limits for
`SkillTestResult`, `Moves` and `Enters`. That is a no-op for a `You` window (only one seat
matches), but on a **shared source with an `Anyone` window** it offers the Forced trigger
once per seat. All Forced defaults are now `GroupLimit`, keeping the period: `PerWindow`
intersects `usedAbilityWindows` (`countInWs`), `PerTest` is cleared at `SkillTestEnds`,
`PerMove` is rewritten to `PerMovement <id>` by `upgradePerMove` so the bucket stays
scoped to one move. Reactions keep `PlayerLimit` — every seat *should* get to react.

**The hard-lock this caused (#5761, "Cannot do any action"):** A Light in the Fog, 4
players. Finding the Path's `Objective $ forced $ Enters #after Anyone <Sunken Grotto>`
was materialised for all four seats. Rex resolved it, the act advanced and **left play**.
The other seats still held their initiation — and `UseAbility` is turned into
`Do (UseAbility …)` *only* by the entity that is the ability's source (each runner's
`UseAbility _ ab _ | isSource a ab.source` arm; 14 modules). With the act gone nothing
claimed it, so neither the recorded use nor `releaseInitiationEffects` — the two things
that consume an initiation — ever ran, and the identical button was re-offered forever.
Each click only appended another entry to `gameActiveAbilities`; the replayed state was
otherwise byte-identical. See [[project_window_effect_suspension]], which already warned
"unclaimed UseAbility is silently swallowed and the initiation queue loops".

`Game/Utils.sourceCanClaimUseAbility` now guards the re-filter structurally. Writing one:
`maybeAsset`/`getEventMaybe`/`maybeTreachery`/`maybeSkill`/`maybeEnemy`/`maybeLocation`/
`maybeEffect` already consult `actionRemovedEntitiesL` + the in-hand/in-discard/in-search
maps, so the entities the `ResolvedAbility` sweep parks (events, treacheries — Caught in
the Crossfire) keep answering True. **Acts, agendas, stories, concealed cards and scarlet
keys have no such lookup** — `getAct`/`maybeAgenda`/`maybeStory`/`project` are
`entitiesL`-only even though `Arkham/Entities.hs` does fan the message over the removed
map — so they need an explicit `entitiesL <|> actionRemovedEntitiesL` check. `selectAny`
is **not** a safe existence test (default matcher paths are `gameEntities`-only). Claim
comparison uses `ProxySource`'s **second** field (`isProxySource`), and the Enemy runner
also claims `isIndexed`.

`Game.Utils` is too high in the module graph to import from a runner; use the existing
`import {-# SOURCE #-} Arkham.Game.Utils (...)` pattern and add the signature to
`Game/Utils.hs-boot`.

**Why:** the fan-out is invisible from a card file — the ability reads like it fires once.

**How to apply:** a Forced ability on an act/agenda/location/enemy/story whose window
matcher is not `You` must stay group-limited; never put `noLimit`/`playerLimit` on one.
Related: [[project_yourlocation_window_handler_fanout]] (the mirror-image mistake — a
`You` window that the *handler* then fans out over).
