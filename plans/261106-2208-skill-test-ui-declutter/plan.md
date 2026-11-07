# Skill Test window: declutter chaos bag + "Xác nhận" button

Files: `frontend/src/arkham/components/ChaosBag.vue`, `SkillTest.vue`, locales (`en|vi/.../skill_test.json` or `game_bar.json`).

## Problems (from screenshot + code)
1. Token preview (`ChaosBag.vue:179-197`) always shows all N tokens (16 here) as a wide strip with value pills, next to the "tokens / Show odds" bar. Noise during ST.2 (commit step) when nobody needs it.
2. The draw action is a blank chaos-token image with a magenta ring (`ChaosBag.vue:170-175`, `.token--can-draw`). Not obviously a button; large, visually dominant.
3. Info lines stacked: skip-triggers notice, step bar, odds bar, plus token effects / modifiers below.

## Target design
- **Bag collapsed by default** during a skill test: one slim row `🎒 16 tokens ▾   [Show odds]`. Clicking the row (or caret) expands the token grid in place (and popover with odds stays as is). State is local `ref(false)`, reset when `game.skillTest?.id` changes (same pattern as `showOdds`, line 112).
- **Draw action = text button**: replace the `<img ct-blank>` with `<button class="draw-button" @click="choose(tokenAction)">{{ $t('skillTest.confirm') }}</button>` (vi: "Xác nhận", en: "Confirm"). Full-width primary style, same look as `.apply-results` in SkillTest.vue (`--button-2`), so the main action is the most prominent element and unambiguous.
- **Revealed tokens** (`<Token v-for revealedChaosTokens>`) stay as images, shown above the button after the draw. Unchanged.
- **Chaos Bag window (skillTest === null)** keeps its current always-open `token-big` grid; only skill-test mode collapses. The `Confirm` button is only introduced for `tokenAction` (it is the same `START_SKILL_TEST_BUTTON` choice in both windows; label is fine in both).
- Expanded grid keeps tooltips + debug force-draw/drag-seal behaviour untouched.
- Auto-expand the grid when it is useful: when `debug.active` (force draw needs it). Otherwise user-initiated only.

## Steps
1. `ChaosBag.vue`: add `bagOpen` ref + watch reset; `v-if="skillTest === null || bagOpen"` on `.token-preview`; make `.bag-toolbar` visible whenever `skillTest` (not only when `breakdown` exists) so the toggle row always exists; toggle on row click (keep `@click.stop` on Show-odds button and Dropdown behaviour).
2. `ChaosBag.vue`: swap the blank-token `<img>` for the button; delete `.token--can-draw` styles; add `.draw-button` styles (reuse CSS vars, 44px min height for touch).
3. Locales: add `skillTest.confirm` to `en` and `vi` (other locales fall back to en); verify the i18n fallback config.
4. Optional polish (ask-free, small): shrink the skip-triggers notice into a single-line chip; leave step bar as is.
5. Verify: run the dev server, open a skill test at ST.2 / ST.3, check: collapsed by default, toggle works, Confirm draws a token, expanded grid tooltips work, odds popover unchanged, mobile portrait (grid media query lines 305-334) still fine, Chaos Bag window unchanged. Run `frontend` lint + type-check (`npm run build` / vue-tsc).

## Acceptance
- Opening a skill test shows no token grid until the user clicks the bag row.
- The draw action is a labelled "Xác nhận" button; no ring token image.
- No behaviour change to drawing, odds, debug tools, or the non-skill-test chaos bag window.

## Open questions
- Should the grid re-collapse after the token is drawn (ST.3+)? Default: yes, via same reset per test only, so it stays as the user left it within a test.
- Label should be the generic "Xác nhận" everywhere, or "Rút token" in the ST.3 draw context? Default: "Xác nhận" as requested.
