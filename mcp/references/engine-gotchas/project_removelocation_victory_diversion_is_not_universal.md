---
name: project-removelocation-victory-diversion-is-not-universal
description: removeLocation silently sends a Victory X location with no clues to the victory display; effects that shuffle a location back into a deck must use removeLocationWithoutVictory
metadata: 
  node_type: memory
  type: project
  originSessionId: bcded1d8-434a-4124-b730-2a7a7081ebde
  modified: 2026-09-09T00:10:59.404Z
---

`removeLocation` (`Arkham/Message/Lifted/Location.hs`) does **not** just remove a location: if it
is Victory X with no clues, it pushes `addToVictory_` instead of `RemoveLocation`. That is a
convenience baked into the shared helper, not a rule — the Rules Reference only scores locations
"at the end of a scenario … in play, revealed, and with no clues".

So any effect that **shuffles a location back into a deck** must use
`removeLocationWithoutVictory` (added in #5662), never `removeLocation`. Two failure modes when
you get it wrong, and they compound:

1. The card scores XP it never earned.
2. If the caller already collected `field LocationCard` for the deck (the usual shape — gather
   cards, then remove), the card ends up in **both** the deck and the victory display. A
   `quantity = 1` location then exists twice and can be redrawn into play. Obsidian Canyons'
   `rebuildSkyline` did exactly this: Obsidian Cliffs sat in the victory display and on the board
   at once (#5662).

Two more traps in the same area:

- `AddToVictory _ (LocationTarget lid)` (`Game/Runner.hs`) pushes a **bare** `RemoveLocation`, not
  `resolve (RemoveLocation …)`. A location whose own text replaces its leave-play via a
  `When (RemoveLocation lid)` handler (Glyph Orrery 11662) therefore never fires it on the victory
  path — switching to `removeLocationWithoutVictory` unmasks that handler, so also stop returning
  such a card to the deck.
- `Homebrew/DarkMatter/Helpers.hs` `shuffleLocationIntoScanningDeck` hand-rolled the same
  workaround before the shared helper existed.

**Why:** the diversion is invisible at the call site — the guide text says "shuffle into the deck"
and the code says `removeLocation`, and nothing reads wrong.

**How to apply:** when guide text sends a location to a deck, set-aside, or out of the game
(anything other than "overcome"), reach for `removeLocationWithoutVictory`. Only the winds-style
"or place them in the victory display instead if they have Victory X and no clues on them" wording
earns `removeLocation`/`addToVictory_`. See
[[project-board-rebuild-in-one-handler-reads-stale-board]].
