# Port "map" mechanic from arkham-gathering into Arkham Horror LCG (frontend)

Status: scope confirmed (map + action panel; both views kept: card board and map are two modes the player picks; target `frontend/` only).

## Outcome

Players can switch the scenario board to a **Map mode**: locations as nodes with links, tokens for investigators/enemies, click a node to open a **grouped action panel** for that location, enemy movement animated along links. Pure presentation over the existing server-authoritative engine. No rules change, no backend change.

Non-goals: porting the demo's JS engine, campaign, save slots, sandbox, deck library, rooms (the project already has all of these in Haskell/Vue). Not touching `frontend-3ed` (it already has `BoardMap.vue`).

## What the demo does (source: `arkham-gathering/dist`)

| Demo module | Behavior | Project equivalent today |
|---|---|---|
| `house-map.js` `mapView` | Nodes at fixed % coords, SVG link lines, locked link dashed, per-node tokens (investigator / ally / enemy count) | `Scenario.vue` location cards + `connectionCurves` (cards, not abstract nodes) |
| `house-map.js` `roomPath` | BFS shortest path over links, skips locked | none in frontend |
| `map-controller.js` | `selectedRoom`, diff enemy locations per render -> `moves[]` -> `<animateMotion>` along path | none |
| `house-map.js` `mapDetails` | Panel for selected node: entities list, "next step" button if remote, full actions if current | `Location.vue` abilities popup (per card hover) |
| `location-actions.js` | One panel grouping actions: Basic / Location+scenario / Your cards / Encounter+threat / Hand | choices spread across card popups and the bottom action bar |
| `scene-*.js` | Point-and-click room scene with hotspots, free walking | none (art-heavy, per-room data) |

## Mapping design

Single new view layer, fed only by existing game state and choices.

- `LocationMap.vue` (new): renders nodes + links from `game.scenario.locations`. Position: reuse the layout the board already computes (grid/groups used by `Scenario.vue` and `connectionCurves`) normalised to 0-100%, instead of hand-authored coords like `MAP_POINTS`. Fallback auto-layout (force or layered) for scenarios without a grid.
- `useLocationMap.ts` (new composable, port of `MapController`): `selectedLocationId`, previous-render enemy locations, derived `moves[]`. Resets on scenario change.
- `locationGraph.ts` (new, pure, unit tested): `shortestPath(graph, from, to)` BFS, `locked` links, entity roster per location. Port of `roomPath` / `roomEntities`.
- `LocationActionPanel.vue` (new): groups the *currently offered choices* by source into the five demo groups. Each button emits the same `choose(index)` the board already uses. Remote (non-current) location: read-only info plus "next step" only when the engine offers a matching move choice. No client-side rule logic (demo's `engine.actionCost`, `canSpendClues` etc. are NOT ported; server decides enabled/disabled).
- Entry point: a view-mode switch (card board / map) in `Scenario.vue`, persisted per user via the existing settings store; default stays card board. Map mounts as a sibling component so `Scenario.vue` (5k lines) does not grow. The card board is untouched. Rare interactions the map does not cover get a "open in card board" shortcut instead of being duplicated.
- i18n: all strings via `vue-i18n` (`frontend/src/locales`), no hardcoded Vietnamese from demo.
- Multi-player: node tokens per investigator with player colour; panel shows actions of the *viewing* seat only.
- Mobile: demo's three breakpoints (<=1200, <=680, <=360) as reference for panel stacking.

## Phases

1. **Graph core** - `locationGraph.ts` + tests in `frontend/tests/*.test.mjs` (BFS, locked links, roster, disconnected graph, attached/stacked locations). Done when tests pass.
2. **Static map** - `LocationMap.vue` nodes/links/tokens, toggle in Scenario, layout from existing board geometry. Done when 3 representative scenarios (grid, cluster-group, linear) render correctly.
3. **Selection + panel** - `useLocationMap`, `LocationActionPanel` grouping existing choices; verify every choice reachable from panel is also reachable from the old UI and vice versa. Done when no choice is orphaned.
4. **Enemy movement animation** - diff-based `moves[]`, SVG `animateMotion`; respect reduced-motion. Done when hunter move is animated, no animation on initial load or rewind/undo.
5. **Polish** - mobile layout, i18n keys, keyboard focus, docs entry.
6. **(Optional, defer)** Room scene with hotspots: needs per-location art and hotspot data (demo hand-authors 5 rooms + generic fallback). Recommend skip unless you want it.

## Risks

- Choice grouping depends on classifying server choices (ability vs card vs encounter); classification may be ambiguous for some scenarios. Mitigation: unknown choices go in a visible "Other" group, never hidden.
- Layout for scenarios with many/nested locations. Mitigation: reuse board geometry, fall back to auto-layout.
- Performance on large location sets; keep node render cheap.
- Two views to keep in sync: any new choice type must work in both. Mitigation: map panel lists unclassified choices under "Other" so nothing is unreachable, and the card board remains the full-coverage fallback.

## Validation

`cd frontend && npm test` (node tests), `vue-tsc`/lint per `frontend/package.json`, manual pass through Night of the Zealot (The Gathering) as parity with the demo, plus one multi-player game.

## Open questions

None blocking. Assumed: `frontend/` (LCG) only; `frontend-3ed` untouched.
