---
title: project_resolves_story_window
description: ResolvesStory is the story-resolution window (added 2026-10-16); its #when half CANNOT cancel the story's effects, because every entity has handled ResolveStory before the Game raises the window
---

Until 2026-10-16 the engine had **no window of any kind for a story** —
`grep -n Story Window.hs Matcher/Window.hs` returned nothing. Added so an act
could carry its printed "**Forced** – After you resolve a card from the
Invisible Combat deck…":

    Arkham.Window:  ResolvesStory InvestigatorId StoryId
    Arkham.Matcher: ResolvesStory Timing Who StoryMatcher

    mkAbility a 1 $ forced $ ResolvesStory #after You (StoryWithTrait InvisibleCombat)

`#when` is raised on `ResolveStory`, `#after` on `ResolvedStory`, both gated on
`ResolveIt` (declining a read raises neither), and both while the story entity
is still in play — which is what lets `StoryWithTrait` resolve, since
`getStoriesMatching` reads traits off the live entity map.

`ResolvedStory` gained the resolver in the same change: it is now
`ResolvedStory InvestigatorId StoryMode StoryId`, mirroring `ResolveStory`.
Without it a story-only `Who` would offer the Forced to every seat — the #5685
shape, see [[project_forced_window_who_must_be_you]]. Old saves decode through a
legacy 2-tuple case filling in the `"00000"` placeholder id.

**`#when` does NOT let you cancel the story.** `Game/Runner.hs` raises it from
the Game's own handler for `ResolveStory`, which runs *after* every entity has
handled that same message — so the story card's handler has already pushed all
of its effects onto the queue. Resolving in front of them means you act first,
not that they disappear. There is no engine support either: `CancelEffects` is
honoured only on the encounter-card draw path (`Game/Runner.hs:3942`) and in
`SkillTest/Runner.hs`; nothing in the story path reads it. To cancel a story,
cancel the queued `ReadStory` before the entity exists — Meltwater Stream (The
Invisible Man) does this off a `DrawCard #after` window on the scenario deck,
which is strictly stronger: no entity, no effects, no `ResolveStory`, no
`ResolvedStory`, and so no clue for an act watching `#after`.

A hand-rolled read that pushes `ResolveStory` without a matching
`ResolvedStory` (`spotlightStory` in `AgainstTheWendigo/Helpers.hs`) gets the
`#when` and never the `#after`.
