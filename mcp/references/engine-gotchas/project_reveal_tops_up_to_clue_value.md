---
name: project_reveal_tops_up_to_clue_value
description: "A location entering play revealed is topped UP to its clue value, not restocked — clues carried over by ReplaceLocation DefaultReplace must be subtracted (FAQ 1.39, #5560)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 68614f92-0889-4d58-918e-13ad78eb8171
  modified: 2026-08-30T10:10:27.048Z
---

`Location/Runner.hs`'s `PlacedLocation` handler (`if locationRevealed` branch) placed the
full `locationRevealClues` value even when the entity already held clues, so a location
that came back into play carrying clues got them stacked on top. Fixed by
`cluesToPlace = max 0 (locationClueCount - currentClues)`; `currentClues` was already
computed there but only fed the `withoutClues` flag.

**Why:** FAQ 1.39 — "If a location is revealed, then leaves play, and later re-enters play
and is revealed again, place clues on it **up to** its clue value." Reported as The
Boundary Beyond's Templo Mayor returning with 9 clues (6 carried + a full 3 placed) on a
1-per-investigator location, #5560.

**How to apply:** the branch only sees `currentClues > 0` via
`ReplaceLocation … DefaultReplace` (`Game/Runner.hs`), which copies `locationTokens` and
then pushes `PlacedLocation`. `PlaceLocation`/`PlaceLocationWith`/`PlaceEnemyLocation`
always build empty entities, and `Swap` pushes no `PlacedLocation` at all, so the change is
arithmetically a no-op everywhere else. Reachable content: The Boundary Beyond (incl.
Return to) and the six Dim Carcosa story replacements. The sibling
`Do (RevealLocation …)` handler was deliberately left alone — At Death's Doorstep spectral
swaps and Dark Matter scanned locations arrive *unrevealed* carrying clues and are
first-time reveals, which 1.39 does not cover. See
[[project_tbb_place_on_top_transfers_all_tokens]] for why the carried clues are correct in
the first place, and [[project_flip_this_card_back_over_not_rearm]] for the flip case
(`FlipToLocation`/`FlipToEnemyLocation` skip `PlacedLocation` on purpose).
