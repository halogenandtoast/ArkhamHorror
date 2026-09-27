---
title: Lost the Trail cancels the move when there is no different connecting location
date_added: 2026-08-25
source: Internal decision (user ruling, 2026-08-25)
affects: [Lost the Trail, additional cost, movement, redirect]
---

# Lost the Trail cancels the move when there is no different connecting location

Lost the Trail reads: "As an additional cost to move from your location, reveal a random chaos
token. If you reveal a [skull], [cultist], [tablet], [elder_thing], [auto_fail], or {moon} token,
instead move to a different connecting location."

If the origin has no connecting location **other than** the one the investigator was moving to, the
redirect has no legal target. In that case **the move is cancelled** — the investigator does not
move at all. The revealed chaos token was still paid as a cost, and the reveal still happened.

The move is *not* allowed to proceed to its original destination: "instead" replaces the move, and a
replacement that cannot be performed does not fall back to the thing it replaced.

## Affected cards / systems

- Lost the Trail (`:circus-ex-mortis:261`) — file:
  `backend/arkham-api/library/Arkham/Homebrew/CircusExMortis/Treacheries/LostTheTrail.hs`
- Movement: `cancelMovement` / `CancelMovement` modifier

## Implementation status

verified — the redirect step selects `ConnectedFrom ForMovement (locationWithInvestigator iid)`
minus the intended destination; when that set is empty it calls `cancelMovement attrs iid` instead
of offering a choice.
