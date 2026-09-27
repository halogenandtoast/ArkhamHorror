---
name: connection-symbols-are-matchers-blank-drops-source-modifiers
description: Printed location connections are `LocationWithSymbol` matchers, so "loses its X connection symbol" is subtractive; Blank deletes modifiers by source, which is the seam that turns such text back on
metadata:
  type: project
---

A location's printed connections become `[LocationWithSymbol sym]` in
`locationConnectedMatchers` / `locationRevealedConnectedMatchers`
(`Location/Types.hs`), and `getConnectedMatcher` (`Helpers/Location.hs`) unions those
with grid directions plus the *additive* `ConnectedToWhen` /
`ForMovementConnectedToWhen` modifiers. Nothing was subtractive, so text like Circus Ex
Mortis's "This location loses its ☾ connection symbol" had no primitive:
`LosesConnectionSymbol LocationSymbol` now filters matching `LocationWithSymbol`
entries out of the base list before the fold.

Connections are one-way per location: Circus Encampment listing `Square` does not let an
investigator at Misty Marsh move to it — only Misty Marsh's own ☾ does. `connectedTo`
(`ConnectedTo`) reads the *other* location's matcher, so suppressing a symbol also
shrinks the count in `selectCount $ connectedTo (be a)`.

**Why:** `Game.hs`'s `handleBlanked`/`applyBlank` drops every modifier whose *source*
equals the blanked card. So a location that models its own printed text with
`modifySelf` / `modifyEach` sourced from itself gets that text switched off for free
when something applies `Blank` — which is exactly how The Primrose Path's Forest of
Illusion opens the path to Circus Encampment.

**How to apply:** model "this location's printed text does X" as a modifier sourced from
the location itself, never from the scenario or an act, or `Blank` will not remove it.
Reach for `LosesConnectionSymbol` for connection loss; `ConnectedToWhen` remains the
additive direction. See [[project_move_restriction_must_be_cannotenter]] and
[[project_circus_ex_mortis_status]].

**Verifying against a live game:** `gameModifiers` is a *persisted* field
(`Game/Json.hs` reads `o .: "gameModifiers"`), recomputed only by `preloadModifiers`
while messages run. A GET on `/spectate` re-serializes the stored blob, so a change to
any `HasModifiersFor` does not show up until the game processes another message — check
`arkham_games.updated_at` before concluding the code is wrong.
