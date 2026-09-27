---
title: project_agenda_enters_with_doom
description: Make an agenda enter play already holding doom via the EntersPlayWithDoom modifier preloaded in AddAgenda
---

To have an agenda enter play already holding starting doom, apply an `EntersPlayWithDoom n`
modifier to its (deterministic) id BEFORE setting the deck, and let `AddAgenda`
(`Arkham/Game/Runner.hs`) bake it into the entity at construction:

- In the act/scenario: `phaseModifier source (AgendaId $ toCardCode agendaCard) (EntersPlayWithDoom n)`
  then `push $ SetCurrentAgendaDeck deck [agendaCard]`. The agenda id == `AgendaId (toCardCode card)`.
- `AddAgenda` reads `getModifiers aid`, sums `EntersPlayWithDoom`, and sets `doomL` at construction.
  This happens BEFORE the `EnterPlay` window frames are pushed, so the doom is present when the
  agenda's own forced/objective abilities first see a window.

**Why:** placing doom AFTER the deck is set (e.g. `placeDoom` / a separate `PlaceDoom`) lands the
doom behind the `EnterPlay` window that `AddAgenda` queues. A `forced AnyWindow` objective gated on
`AgendaWithDoom (EqualTo 0)` (e.g. The True Culprit final agendas in Murder at the Excelsior Hotel)
then fires on that window while the agenda is still at 0 doom. Reuses the existing
`EntersPlayWithDoom` already consumed by assets at `Arkham/Asset/Runner.hs`.

**How to apply:** prefer this over inventing new message variants or changing `AddAgenda` /
`SetCurrentAgendaDeck` arity — the user explicitly chose the modifier-preload approach for #4885
(`FollowingLeads.hs`, agenda `c84052` theTrueCulpritV10). Timing relies on `shouldPreloadModifiers`
defaulting True so `preloadModifiers` rebuilds `gameModifiers` (keyed by target even when the target
entity doesn't exist yet) between the modifier message and `AddAgenda`.
