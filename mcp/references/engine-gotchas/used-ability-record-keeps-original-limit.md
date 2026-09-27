# Used-ability records keep their ORIGINAL limit type; windows only accumulate

`handleRecordUsedAbility` (Investigator/Runner/Action.hs ~690) updates an existing record in
place — `Ability`'s Eq compares only source/index/cardCode, NOT the limit — and reset logic
(`filterDepthSpecificAbilities`, `EndCheckWindow`) reads the limit stored ON THE RECORD. So:
1. Changing an ability's limit in code does NOT affect records already in saved games — a
   record stamped with the old limit keeps behaving per the old limit forever (migration hazard).
2. `usedAbilityWindows` only grows (appends, never replaces); an AnyWindow forced ability
   accumulates generic window occurrences (FastPlayerWindow/NonFast/DuringTurn) that poison
   future overlap checks — one success can block the next trigger.
3. `PerDepthLevel`'s only reset is the Forgotten Age depth STORY counter — never use it outside
   that mechanism.
4. A bare `forced AnyWindow` silently defaults to `GroupLimit PerWindow 1` via
   `defaultAbilityLimit` — use `noLimit` explicitly when the card prints no limit and the
   handler is self-limiting.
Found 2026-08-25 in the Strange Moons Secrets of the Mind saga (three-bug chain).
