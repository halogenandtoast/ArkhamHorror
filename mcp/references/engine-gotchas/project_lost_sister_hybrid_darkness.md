---
title: project_lost_sister_hybrid_darkness
description: "The Lost Sister hybrid In-the-Light/Dark flipping keys off the Dark trait; unrevealed \"Cavern\" back is dark, and reveals need a scenario flip hook"
---

In The Lost Sister (FoHV, scenario "10569"), location "darkness" = the `Dark` trait (per-location, not global day/night — the scenario sets time=Day globally and `HasModifiersFor` applies `time=Night` to `EnemyAt/InvestigatorAt (LocationWithTrait Dark)`). A Crustacean/Limulus Hybrid must always be on the side matching its location's darkness.

Two flip triggers exist:
1. The hybrid's own `SilentForcedAbility` on `EnemyEnters #after`/`EnemySpawns #after` (criterion `isDark a`/`isLight a`) — fires when the *enemy* moves/spawns onto a wrong-side location.
2. The scenario re-syncing on a *location* darkness change while a hybrid sits still: `ScenarioSpecific "locationDarknessChanged"` (Luminous Growth) and `Do (RevealLocation _ lid) → syncHybridDarkness` (location reveal, added for issue #4921/elusive report).

Gotcha that caused the bugs: the shared unrevealed "Cavern" back must be `[Cave, Dark]` on ALL seven Lost Sister caverns (10576-10582). Four were typo'd as `[Cave]`, so the engine treated unrevealed dark caverns as light and hybrids hunting/fleeing onto them never flipped. Reveal handler note: scenario (`modeL.there`) runs BEFORE `entitiesL` (locations) for a message, so reading a location's just-revealed traits must be deferred to a follow-up message.

Issue #5053: `locationDarknessChanged`'s wrong-side select used `enemyIs` (→ `EnemyIs`), which matches by BASE card code — `CardCode`'s `Eq` strips the a/b side suffix so `10584a == 10584b`. So the light-vs-dark `wrongSide` filter was a no-op: it matched hybrids on EITHER side and unconditionally flipped every hybrid on the location. On Rocky Shoreline's reveal (dark→light) it flipped the freshly-spawned correct light hybrid to dark. Fix = `enemyIsExact` (exact-string, `10584a ≠ 10584b`). General lesson: for flip/double-sided cards, side-sensitive matching MUST use `enemyIsExact`/`EnemyIsExact`, never `enemyIs`.
