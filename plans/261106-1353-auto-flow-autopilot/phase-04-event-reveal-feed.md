# Phase 4: "What just happened" reveal feed

## Why
Instant auto-resolution hides events. The demo pauses on an encounter preview; we show the same information without blocking.

## Files
- New `frontend/src/arkham/components/AutoFlowReveal.vue` (toast stack / side ribbon).
- New `frontend/src/arkham/autoFlowEvents.ts` (pure: previous state + new state + new log entries -> events), tested in `frontend/tests/autoFlowEvents.test.mjs`.

## Events
- Round / phase banner ("Round 3 - Mythos").
- Doom placed, agenda advanced.
- Encounter card drawn: card image large for ~2 s, then shrinks into the feed; click to pin.
- Hunter moved (reuse the map's enemy move animation), enemy attacked (damage/horror numbers over the investigator).
- Upkeep: cards readied, +1 card, +1 resource.
- Auto answers from phase 3 ("Applied test results", "Passed fast window").

## Rules
Non-blocking, auto-dismiss, respects reduced motion, never covers an active real decision. Feed history browsable for the current round.

## Done when
Every auto-advanced step in a round is visible in the feed; nothing in the feed blocks input.
