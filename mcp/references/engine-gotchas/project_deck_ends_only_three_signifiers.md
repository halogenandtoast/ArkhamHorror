---
name: project-deck-ends-only-three-signifiers
description: PutCardOnTopOfDeck/PutCardOnBottomOfDeck are only handled for InvestigatorDeck, EncounterDeck and ScenarioDeckByKey; every other DeckSignifier silently no-ops
metadata:
  type: project
---

`PutCardOnTopOfDeck` and `PutCardOnBottomOfDeck` take a `DeckSignifier`, but only three of the
eight have handlers:

- `InvestigatorDeck` (`Investigator/Runner.hs`)
- `EncounterDeck` (`Scenario/Runner.hs:661`, `:672`)
- `ScenarioDeckByKey` (`Scenario/Runner.hs:695`, `:713`)

`EncounterDeckByKey`, `EncounterDiscard`, `InvestigatorDiscard`, `InvestigatorDeckByKey` and
`NoDeck` fall through and do **nothing** — no error, no log line. `ShuffleCardsIntoDeck`, by
contrast, has a catch-all (`Scenario/Runner.hs:914`) and works for every signifier.

That silence is only harmless while the card still lives somewhere else. Pair a no-op
put-on-top with an `ObtainCard` (which strips the card from every zone) and the card is simply
destroyed. `DebugMoveCard` hit exactly this: `EncounterDeckByKey RegularEncounterDeck` +
`DebugDeckTop` obtained a treachery out of the encounter discard and then dropped it on the
floor. It now falls back to `ShuffleCardsIntoDeck` via `deckSupportsEnds`
(`Arkham/Debug/CardDestination.hs`).

Two neighbouring traps in the same handlers:

- `PutCardOnTopOfDeck _ EncounterDeck` `error`s on a `PlayerCard` (`Scenario/Runner.hs:670`) —
  a wrong-back card is a 500, not a bad state.
- `PutCardOnTopOfDeck _ EncounterDeck` clears only `setAsideCardsL`, while the *bottom* variant
  clears every zone including `victoryDisplayL`. The asymmetry is real, not a typo.

**Why:** the signifier type is far wider than the handler coverage, so the compiler cannot tell
you a combination is unimplemented.

**How to apply:** before pushing a deck-placement message for a signifier you have not used
before, grep for a matching handler. If you are obtaining the card first, treat a missing
handler as data loss and fall back to `ShuffleCardsIntoDeck`. See
[[project-removelocation-victory-diversion-is-not-universal]].
