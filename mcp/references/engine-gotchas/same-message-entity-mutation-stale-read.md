# Entity-stage mutations are invisible to runGameMessage in the same message

A pure entity mutation made in a runner's `entitiesL` dispatch stage (e.g. a `ReplaceLocation`
case in `Location.Runner`) is NOT visible to `Game.Runner`'s `runGameMessage` handler for the
SAME top-level message. `HasGame` queries (`getLocation`, `field`, …) read `GameEnv`'s live
`IORef Game` (`GameT.hs:38`), which is committed via `putGame` only after the entire
`instance RunMessage Game` chain for that message finishes (`overGameM`, `Game.hs:~6361`).
So a handler later in the same chain reads the PRE-mutation state.

**Fix pattern**: split the mutation into its own pushed message so it gets its own commit
before the dependent message runs. Example: `restoreCoveredSimulator` (Dark Matter,
`Homebrew/DarkMatter/Helpers.hs`) pushes `RemoveFromUnderneath` (new symmetric primitive to
`PlaceUnderneath`) BEFORE `swapLocation`, because `ReplaceLocation`'s field-copy reads
`locationCardsUnderneath` via `getLocation`.

Found 2026-08-24 fixing the Strange Moons Reality Simulator restore (self-reference survived
an entity-stage cleanup).
