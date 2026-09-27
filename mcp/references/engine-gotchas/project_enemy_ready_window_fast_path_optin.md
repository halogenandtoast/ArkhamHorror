---
name: project_enemy_ready_window_fast_path_optin
description: The enemy-ready CheckWindows fast path in Game.hs skips the window unless an in-play card's CardDef carries the `enemyReadyTag` cdTags marker; untagged cards' EnemyReadies/EnemyWouldReady abilities silently never fire
metadata:
  type: project
---

`Arkham.Game` short-circuits `CheckWindows` when `all Window.isEnemyReadyWindow ws &&
not (hasEnemyReadyAbilities g)` — a perf fast path from `868f5281cc` (swarms). Originally
it tested a hand-written card-code list against **assets and events only**, so Gug
Sentinel (`06267`, an *enemy* with `forced $ EnemyReadies #after`) never fired its horror
(#5440). Fixed 2026-08-19: cards now opt in via `cdTags = [enemyReadyTag]` (constant in
`Arkham.Card.CardDef`); `enemyReadyCardCodes` is derived from
`allPlayerCards <> allEncounterCards` and `hasEnemyReadyAbilities` scans every
card-hosting entity map.

Tagged today: `90038` Tidal Memento, `02031` Bind Monster (2), `03199` Snare Trap (2),
`06267` Gug Sentinel — the only cards using `Matcher.EnemyReadies`/`EnemyWouldReady`.

**Why:** a window that is never emitted produces no error, no log line, and no UI hint —
the ability just silently does nothing, and only a player noticing missing horror finds it.

**How to apply:** any new card whose ability matches `EnemyReadies` or `EnemyWouldReady`
MUST also get `enemyReadyTag` in its `CardDef`'s `cdTags`. The fast path still suppresses
`forced AnyWindow` abilities (e.g. act objectives) during ready windows — a delay, not a
miss, since they are re-checked at the next window. See
[[project_swarm_cards_are_enemy_entities]] for why swarm cards bypass the window chain at
`Enemy/Runner.hs` instead. Related: [[project_alternate_printings_distinct_carddefs]] —
each printing's def must carry the tag (`toCardCodePairs` copies `cdTags`, so record-update
printings inherit it).
