# Phase 5: Settings + phase stops

## Files
- `frontend/src/arkham/components/Settings.vue` ("pacing" panel already exists, next to "skip unrelated skill test triggers").
- Persist in the frontend settings store / localStorage per user. Do not extend Haskell `GlobalSettings` (that is a backend change).
- `frontend/src/locales/*/gameBoard/game_bar.json`: new keys (en first, other locales fall back).

## Controls
- Autopilot: Off / Standard / Full (default pending user decision).
- Phase stops (Full only): Mythos, start of Enemy phase, before attacks, Upkeep. Off by default.
- Speed: reveal delay slider.
- Game bar: one toggle showing current level + pause; hotkey (e.g. `P`) to pause/resume.

## Done when
Changing level takes effect without reload; Off restores today's behaviour; strings localized.
