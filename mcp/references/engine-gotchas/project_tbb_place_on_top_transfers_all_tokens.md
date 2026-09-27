---
name: project_tbb_place_on_top_transfers_all_tokens
description: "The Boundary Beyond's scenario rules override \"leaves play returns tokens to the pool\" — a location placed on top of another, and a location leaving play over another, transfer all tokens both ways"
metadata: 
  node_type: memory
  type: project
  originSessionId: 68614f92-0889-4d58-918e-13ad78eb8171
  modified: 2026-08-30T10:10:36.599Z
---

The Boundary Beyond's additional rules:

> When a location is placed on top of a location that is already in play, it takes its
> place. All tokens, attachments, investigators, enemies, and other cards at the former
> location are considered to now be at the new location (they have not "moved" — the
> location simply changed). If a location leaves play and there is another location
> underneath it, that location takes its place. All tokens, attachments, investigators,
> enemies, and other cards at the location leaving play are considered to now be at the
> former location.

**Why:** this overrides the general Leaves Play rule ("all tokens on the card are returned
to the token pool"). So when *Window to Another Time* shuffles an Ancient location back
into the exploration deck, its uncollected clues stay on the Present-Day location that
takes its place — and move back up when the Ancient location is explored again. Both
directions are already implemented and correct (`TheBoundaryBeyond.hs`'s `RemoveLocation`
handler pushes `ReplaceLocation … Swap`; `explore`'s `ReplaceExplored` pushes
`ReplaceLocation … DefaultReplace`; both copy `locationTokens`). Do not "fix" that.

**How to apply:** when a TBB clue count looks too high, the suspect is the *reveal*, not
the transfer — see [[project_reveal_tops_up_to_clue_value]] (#5560). All six Present-Day
Mexico City locations have printed clue value 0, so the transfer is only ever visible in
the Ancient → Present-Day → Ancient direction.
