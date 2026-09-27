---
name: project_cards_beneath_asset_have_no_entity
description: "A card underneath an asset gets no entity, so its cdOutOfPlayEffects/InHandEffect modifiers never fire; grant AsIfInHandForEffects from the host asset (Astronomical Atlas + Long Shot, #5616)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 040ffe71-0c13-44e4-81a3-a800ead63f53
  modified: 2026-09-06T00:51:43.549Z
---

Out-of-play cards only produce modifiers if `preloadHandEntities` (`Game/Runner.hs`) built an
entity for them. It feeds on `investigatorHand` plus `getAsIfInHandEffectCards`
(`Helpers/Investigator.hs`), which matches only `AsIfInHand`, `AsIfInHandFor` and
`CanCommitToSkillTestsAsIfInHand`. A card sitting in an asset's `cardsUnderneath` matches none
of those, so `HasModifiersFor` for that card never runs and
`getModifiers (CardIdTarget c.id)` comes back empty.

That is why Astronomical Atlas (3) could not commit Long Shot to a connecting location's test
(#5616): Long Shot's permission is `CanCommitToSkillTestPerformedByAnInvestigatorAt`, emitted by
its own Skill entity, and `getIsCommittable` reads it off `CardIdTarget`.

**How to apply:** the host asset grants the permission on the investigator's behalf. Pick by
what the card actually allows:

- `AsIfInHandFor ForPlay` — Stick to the Plan / Backpack: playable from under the host.
- `CanCommitToSkillTestsAsIfInHand` — Crystallizer of Dreams: freely committable, appears in the
  **normal** commit window (`getCommittableCards`).
- `AsIfInHandForEffects CardId` — added for #5616: loads the entity and nothing else. Use when the
  card is only reachable through the host's own ability (Astronomical Atlas is limit-once-per-test
  and adds a `ReturnToHandAfterTest` rider, so exposing it in the normal commit window would be a
  free extra commit).

Grant it from an `AssetSource` — `preloadHandEntities`'s `forPlayHosts` map then places the entity
at `AttachedToAsset aid Nothing` instead of `StillInHand`, so "while in your hand" effects stay off.

Two traps in the same card:

- The ability was gated on `during SkillTestAtYourLocation`, a proxy for the commit rules that
  Long Shot exists to widen. Ask the real question: `exists (CommittableCard who (CardIsBeneathAsset ...))`.
  `#eligible` / `EligibleForCurrentSkillTest` only checks **icons**, so it silently disagrees with
  the handler's `getIsCommittable`.
- `You` inside such a criterion resolves to `gameActiveInvestigatorId`, not the ability's
  controller — `Criteria.ExtendedCardExists` only applies `replaceYouMatcher` when there is a card
  context, which an asset ability has none of. Use `InvestigatorWithId a.controller`
  (with `maybe Never`). Same trap as [[project_playability_uses_active_investigator]].

Related: [[project_handwith_excludes_asifinhand_cards]], [[project_playability_uses_active_investigator]].
