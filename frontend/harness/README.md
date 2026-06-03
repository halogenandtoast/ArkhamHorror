# Frontend verification harness

Lightweight CLI for driving the Arkham frontend through real game flows so a
Claude session can verify UI changes (refactors, especially mega-component
splits) without breaking gameplay.

The harness itself is the boring half — auth, game creation, raw-message
debug, snapshot bookkeeping. Browser driving is the other half and is done by
Claude via the Chrome DevTools MCP. The two halves talk through:

- `node harness/cli.mjs ...` — the CLI Claude shells out to
- `harness/.state.json` — token + most-recent game id (gitignored)
- `harness/snapshots/{baseline,current}/<name>.png` — screenshots Claude saves
  with `take_screenshot --filePath`

## Prerequisites

- Backend up: `cd backend && make api.watch` (listens on `127.0.0.1:3002`)
- Frontend up: `cd frontend && npm run dev` (listens on `127.0.0.1:8080`)
- Chrome with remote debugging on `:9222`, isolated profile so it doesn't
  touch your personal browser:

  ```bash
  "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
    --remote-debugging-port=9222 \
    --user-data-dir=/tmp/chrome-arkham-harness \
    --no-first-run --no-default-browser-check --no-startup-window &
  ```

## CLI commands

All commands are runnable from `frontend/`:

```bash
node harness/cli.mjs login                           # bootstrap auth + cache token
node harness/cli.mjs whoami                          # verify session
node harness/cli.mjs state                           # dump .state.json
node harness/cli.mjs clear-state                     # wipe cached token + game id
node harness/cli.mjs new-game scenario <id>          # create a game (e.g. 01104)
node harness/cli.mjs new-game campaign <id>          # create from a campaign id
node harness/cli.mjs game-url [gameId]               # print frontend URL
node harness/cli.mjs raw <gameId> '<json>'           # send raw debug Message
node harness/cli.mjs diff [name]                     # compare baseline vs current
```

`new-game` and `game-url` print URLs in the form
`http://localhost:8080/#/games/<id>` — hash-router routes are required.

## Auth bootstrap

`login` tries `POST /authenticate` with the harness credentials, and on 4xx
falls back to `POST /register` to create the user. The resulting JWT is cached
in `.state.json` and reused on subsequent runs.

Defaults can be overridden with env vars: `HARNESS_USER`, `HARNESS_EMAIL`,
`HARNESS_PASSWORD`, `HARNESS_API_BASE`, `HARNESS_FRONTEND_BASE`.

## End-to-end loop a Claude session runs

1. **Prime auth in the browser.** Navigate to the frontend, set `arkham-token`
   in `localStorage` to the cached harness token:

   ```js
   localStorage.setItem('arkham-token', '<token from `harness state`>')
   ```

2. **Create a known game state.**

   ```bash
   node harness/cli.mjs new-game scenario 01104
   ```

3. **Navigate to it.** Use the URL the previous command printed.

4. **Install the in-page error trap.** Run this once after navigation,
   *before* driving interactions:

   ```js
   window.__harnessErrors = []
   window.addEventListener('error', e => window.__harnessErrors.push({kind:'error', message: e.message}))
   window.addEventListener('unhandledrejection', e => window.__harnessErrors.push({kind:'unhandledrejection', message: String(e.reason)}))
   ```

   (or import the same body from `harness/checks/dom.mjs#installErrorTrap`)

5. **Capture a baseline.** Take a screenshot for each significant view:

   ```
   mcp__chrome-devtools__take_screenshot --filePath frontend/harness/snapshots/baseline/<name>.png
   ```

6. **Drive interactions.** Click, drag, fill — using the snapshot's `uid`s.
   Between steps check `window.__harnessErrors` and `list_console_messages`.

7. **Refactor the code.**

8. **Repeat steps 1–6 with the refactored code,** saving screenshots to
   `snapshots/current/`.

9. **Diff.**

   ```bash
   node harness/cli.mjs diff
   ```

   `identical` ⇒ pixels match exactly; `differs` ⇒ eyeball the two files.
   (Pixel-level diffing is a follow-up, see "Roadmap" below.)

## Files

```
harness/
├── cli.mjs                 # entry point — `node harness/cli.mjs <cmd>`
├── lib/
│   ├── api.mjs             # fetch wrappers (auth, games, raw)
│   ├── config.mjs          # base URLs, harness user, paths
│   └── state.mjs           # .state.json reader/writer
├── flows/
│   ├── login.mjs           # ensureLogin() — authenticate-or-register
│   └── newGame.mjs         # createGame(), sendRaw()
├── checks/
│   ├── dom.mjs             # browser-side script strings (count, expectCount,
│   │                       #   gameStructure, installErrorTrap, readErrors)
│   └── screenshot.mjs      # save(kind, name, base64), diff(name)
├── snapshots/
│   ├── baseline/           # committed reference shots
│   └── current/            # gitignored, regenerated each run
└── fixtures/               # (reserved for saved game exports)
```

## Roadmap (not built yet)

- **Pixelmatch-based image diff.** Right now `diff` is byte/hash comparison —
  any rerender (timestamps, animations, anti-aliasing) shows up as `differs`.
  Adding `pixelmatch` + `pngjs` would give a threshold-based comparison and
  emit a third "diff highlight" PNG.
- **Fixture imports.** Wire up `POST /arkham/games/import` so a captured game
  export can be re-loaded by name (`harness load skill-test-active`).
- **Walkthrough scenarios.** Codify common flows ("engage enemy", "open and
  close a choice modal", "drag a card to commit") as runnable scripts so a
  refactor can be checked against the full set in one pass.
- **Pre-baked fixture for the mega-components.** Save snapshots of game state
  that exercise Scenario.vue / Question.vue / Location.vue so verifying their
  refactors becomes a one-liner.
