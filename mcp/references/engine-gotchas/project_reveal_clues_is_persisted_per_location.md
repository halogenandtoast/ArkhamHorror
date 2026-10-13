---
name: project_reveal_clues_is_persisted_per_location
description: A location's clue value is serialized on the entity, so fixing a card def's PerPlayer/Static clues never reaches a game already in progress
metadata:
  type: project
---

`locationRevealClues` is a persisted `LocationAttrs` field, not re-read from the
`CardDef` on load. `Location/Runner.hs:116,350,410` all resolve clues via
`getGameValue (locationRevealClues attrs)`, and exports carry it
(`"revealClues": {"tag":"PerPlayer","contents":1}`).

So correcting a stubbed clue value in `location Ctor Cards.x <shroud> (PerPlayer n)`
only affects locations created *after* the rebuild. Every location already on the
table in a live game keeps the old value — including ones still **unrevealed**,
which carry the stale value into their eventual reveal.

**Why:** found on #5840 (Children of Blood / New Horizons side chambers stubbed at
`PerPlayer 1`, actually `PerPlayer 2`). The reporter's game could not be repaired by
the code fix.

**How to apply:** when a bug report is a wrong printed location stat, say explicitly
that the in-flight game needs a fresh run or a direct state patch. It also means
`arkham-replay --undo` cannot verify the fix — the replay re-reads the persisted
value, so pre-fix and post-fix runs are identical. Same family as
[[project_arkham_replay_loads_stale_modifier_snapshot]] and
[[project_db_undo_restores_recorded_modifiers]].
