# ah3e — Arkham Horror Third Edition engine

A rules engine for the AH3e board game. It is independent of `arkham-api`: its own
`Message`, `Game`, and queue, and no imports from the LCG engine.

Rules sources: `.claude/references/ah3e/` (FAQ > expansion rules > Rules Reference > Learn to Play).

## Shape

- `AH3e.Types.*` — pure data: ids, board, card definitions, the `Effect` DSL, engine state records.
- `AH3e.Message` — every engine step, plus `Question`/`Choice` (a choice carries the messages it pushes).
- `AH3e.Game` — the whole game state, including the queue and open questions; fully JSON-serializable.
- `AH3e.Engine.Run` — `runMessage`, the rules. `Effect`, `Test`, `Query`, `Helpers` split out the rest.
- `AH3e.Engine.Behavior` — hooks for card-specific code (component actions, test dice, codex triggers,
  reckonings, `Custom` effects and monster activations).
- `AH3e.Content` — the card, investigator, and scenario registries (empty until card text arrives).
- `AH3e.Engine` — `newGame`, `runEngine`, `answer`, `applyDebug`.

The engine is `State Game`: `runEngine` pops messages until a question is open or the queue is empty.
`answer pid idx` pushes the chosen choice's messages and runs again. Randomness is seeded from
`Game.seed`, so a game replays deterministically. `applyDebug` only works when the game was created
with `debug = True`.

Encounter text is data (`Effect`); anything the DSL can't express goes through `Custom` and a
handler registered in `Behaviors`.

## Build

`stack build ah3e --fast` builds only this package.
