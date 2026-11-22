---
name: project_engaged_follow_raised_no_move_window
description: "An engaged enemy dragged along by its investigator took the EnemyEnteredFollowing path, which raised no enemy-moved window at all, so every 'after this enemy moves' ability was dead there"
metadata:
  node_type: memory
  type: project
---

An enemy has **three** ways to change location, and until #6025 they raised three different
(and incomplete) sets of windows:

| path | message | windows raised |
| --- | --- | --- |
| hunter / patrol | `EnemyMove` **plus** a sibling `CheckWindows` | `Window.EnemyMoves` *and* `Window.EnemyMovesTo … MovedViaHunter/Patrol` |
| card or scenario effect (`moveToward`, `MoveToward`, `MoveUntil`, a bare `EnemyMove`) | `EnemyMove` | `Window.EnemyMoves` only |
| **engaged enemy following its investigator** | `EnemyEnteredFollowing` | **neither** — only `EnemyEnters` / `EnemyEntersYourLocation` |

`Investigator/Runner/Movement.hs` (`handleDoResolveMovement`, the `ToLocation` branch) pushes
`EnemyEnteredFollowing` for each engaged follower, never `EnemyMove`. Its two handlers in
`Enemy/Runner.hs` raised only the enters windows, so the follow path was invisible to every
"after this enemy moves" ability.

Cats from Saturn (06228) — *"**Forced** – After the host Cats from Saturn moves or is evaded:
Discard 1 of its swarm cards"* — made this total: a **hunter enemy never hunter-moves while
engaged**, so once it engaged you, following you was the only move it ever made. Its printed
ability was dead for the rest of the game.

**Why:** three producers and no single place that owns "this enemy moved". `MovedViaOther` is
still a dead constructor; the hunter/patrol `EnemyMovesTo` pushes are bolted on as siblings of
`EnemyMove` rather than derived from the `Movement`, which has no `MovesVia` field.

**How to apply:**

- For a card that says "after this enemy moves", use **`EnemyMoves #after`**, never
  `EnemyMovedTo … MovedViaAny`. `EnemyMoves` fires exactly once for hunter, patrol *and*
  card-effect moves; `EnemyMovesTo` is raised **in addition** on hunter/patrol, so listening on
  both double-fires on a hunter move. Reserve `EnemyMovedTo` for cards that genuinely name the
  means ("after an enemy moves via hunter").
- `After (EnemyEnteredFollowing)` now also raises `Window.EnemyMoves eid lid`, **once for the
  host** — Swarming X moves "as a single entity", and per-swarm-card windows would multiply
  reactions (see [[project_swarm_moves_as_one_unit]]).
- A debug export is captured *after* the bug, so replaying it straight gives a no-op diff.
  Verify with `--undo 1` plus an `--answers` file replaying the move action (ability index 104
  on the destination location). Fix verified on #6025's export: host swarm count 2 → 1.

Regression test: `tests/Arkham/Enemy/EngagementSpec.hs`, "fires a swarming host's
after-it-moves ability when it follows its investigator". Related:
[[project_swarm_moves_as_one_unit]], [[project_spawnplaced_never_raised_the_enters_windows]].
