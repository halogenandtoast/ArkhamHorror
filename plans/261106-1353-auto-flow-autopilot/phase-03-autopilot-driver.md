# Phase 3: Autopilot driver

## Files
- New `frontend/src/arkham/composables/useAutopilot.ts`.
- `frontend/src/arkham/views/Game.vue`: mount the composable, reuse the existing `choose()` / answer submission and `processing` flag. No new logic inside the 5k-line view.

## Behaviour
- Watch the current question for every seat this client controls (own seat; all seats in multiplayer solitaire). Send `playerId` explicitly with each answer (backend trusts `Answer.playerId`).
- Ask the policy. If it returns an answer, wait a short beat (default 350 ms, configurable) so the board visibly updates, then submit.
- Guards:
  - skip while `processing` or a submit is in flight;
  - question signature (tag + choices hash + game step) must differ from the last auto-submitted one;
  - cap consecutive auto answers (e.g. 40), then stop and show "Autopilot paused";
  - suspend after undo until the player answers once manually;
  - suspend while a player-opened modal (card zoom, log, settings) is up.
- Every auto answer pushes `{reason, label}` to the reveal feed (phase 4) and the game log.

## Done when
A solo Gathering round runs with zero trivial clicks; undo, reconnect and multiplayer seat checks pass manually; autopilot Off changes nothing.
