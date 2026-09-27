---
name: project-arkham-replay-tool
description: "arkham-replay CLI: flags, and the fidelity limits of --undo and cached questions"
metadata: 
  node_type: memory
  type: project
  originSessionId: 00dfe5d1-a4c9-4af3-bf78-3c9bad9934fc
  modified: 2026-08-14T02:04:43.714Z
---

Merged index entry. Supersedes: [[project-action-diff-snapshot]], [[project_replay_undo_entity_token_fidelity]], [[project_replay_undo_drops_skilltest_modifiers]], [[project_stale_local_bin_arkham_replay]], [[project_replay_cached_question_needs_undo]].

## gameActionDiff is now a single lazy revert diff vs a transient snapshot; arkham-replay gained --simulate-server / --bench-action-diff / partial --undo replay

In-action undo bookkeeping (June 2026): `gameActionDiff` holds ONE lazy revert
patch against `gameActionSnapshot` (a `Transient Game` field — never serialized,
omitted by the hand-written `ToJSON Game` in `Arkham/Game/Json.hs`). It is
rebuilt after mid-action loads by folding saved patches (also covers legacy
multi-patch saves). Do not reintroduce per-message `diff new old` consing in
`handleActionDiff` — that cost up to ~10.5s per save on large games (918
messages) vs ~12ms now.

**Why:** every mid-action save forced M full-game serializations + tree diffs.

**How to apply:** any new revert/undo bookkeeping should follow the same
pattern: keep a runtime-only snapshot (`Transient` wrapper keeps JSON shape
unchanged) + one lazy diff, never per-message materialized diffs.

Benchmark tooling in `arkham-replay`:
- `--replay-all --undo N` replays only the last N steps (full undo to step 0
  crashes on most exports — campaign-level state without a scenario).
- `--simulate-server` mirrors updateGame's per-answer JSON work (forceActionDiff,
  diffDown, encodeGame, parseGame, encodePublicGame) as server/* metric spans.
- `--bench-action-diff K` measures the save cost after K in-action messages.
- `--replay-all` does NOT re-apply answers (exports store only leftover queues
  + revert patches), so replay trajectories diverge from the original session;
  final states are still deterministic per binary — compare old-vs-new binary
  finals to prove behavior parity (ignore gameActionDiff).

Remaining per-answer costs in updateGame (follow-up candidates, ~50ms total on
large games): full Game re-parse from the row (~19ms — needs a step-keyed cache
with invalidation in Undo/Old/Debug/Decks/PendingGames writers), diffDown +
replace double-serialization (~11ms — share toJSON via ArkhamGameRaw), and the
PublicGame broadcast encode (~12ms).

## "arkham-replay --undo restores a removed entity via choicePatchDown with its FINAL (pre-removal) token counts, so damage looks already-lethal at earlier steps — don't diagnose from undo-state counters"

`arkham-replay --undo N` reverses steps by applying `choicePatchDown`. When a step
**removed** an entity (an enemy-location leaving `enemyLocationsL`, an enemy discarded),
the patch re-adds it carrying the tokens it had at removal — not the tokens it had at the
step you rewound to. So `--undo 10` and `--undo 4` can both show `Damage 3` even though the
real game only reached 3 damage at the later step.

**Why:** Reading those counters as ground truth makes a healthy engine look broken —
"damage 3 >= health 3 but `defeated: false`, why didn't CheckDefeated fire?" is a phantom.
In #5162 the answer was that health was really 5 (`HealthModifier` from `perPlayer 2`), and
the damage number was patch noise.

**How to apply:** Treat undo-state *entity presence* as reliable and its *token counters* as
suspect. To learn what actually happened, replay **forward** from the undo point with
`--answers` + `--trace` and read the message stream, or compute the value from the card
(`modifySelf` / `HasModifiersFor`) rather than the JSON. Related: [[project_action_diff_snapshot]],
[[project_stale_local_bin_arkham_replay]].

## "arkham-replay --undo does not restore skill-test-scoped modifiers, so rewinding into a live skill test silently drops token-value replacements — check skillTestResultsChaosTokensValue before trusting the state"

`arkham-replay --undo N` reverses entity state via each step's `choicePatchDown`, but **skill-test-scoped modifiers do not come back**. Rewinding to a point inside a live skill test yields a state where `gameModifiers` has no `ChaosTokenTarget` entries and `gameEntities.effects` is empty, even though the real game had them.

Symptom: the trace's `SkillTestResults_` shows the *unmodified* token value. On #5352, `--undo 10` landed on the "apply results" question with `skillTestResultsChaosTokensValue = -4` (raw [elder_thing]) instead of `-1` (The Black Cat's replacement), so pre-fix and post-fix runs were byte-identical and proved nothing.

**How to apply:** before using an `--undo` state to verify a modifier-related fix, confirm the modifier is actually present — grep the `--trace` for `SkillTestResults_` and check the token value, or `jq '.gameModifiers'` on the output. If it's missing, the rewind is unusable for that bug; either undo far enough to re-take the choice that *creates* the modifier, or verify with a spec instead. Say so explicitly rather than claiming a no-op diff is a pass.

Related: [[project_replay_undo_entity_token_fidelity]] (the sibling fidelity gap for removed entities), [[project_canresolvetoken_replaces_value_only]].

## "A stale ~/.local/bin/arkham-replay shadows the fresh build whenever stack exec runs mid-rebuild, producing phantom parse errors and false replay results"

`stack exec arkham-replay` resolves to `backend/.stack-work/install/.../bin/arkham-replay`, but that file is **rewritten in place** at the end of each `make api.watch` cycle. Invoke `stack exec` during that window and it falls through `PATH` to a months-old `~/.local/bin/arkham-replay`, which runs happily and lies.

Two real failures this caused (issue #5148):

- **Phantom bug.** The Jun 18 binary rejected the export with `expected an Object with a tag field where the value is one of [...], but got SourceUsedBy`. `SourceUsedBy` was added 06-23 and is covered by `deriveJSON defaultOptions ''SourceMatcher` — there was never a FromJSON gap. This got reported to the user three times as a likely cause of a save-import failure before being checked.
- **False negative.** A verified-correct fix reported "did not work" because the run raced the binary swap. Re-running 60s later showed +5 resources.

**Why:** the binary's mtime and `stack exec`'s resolution are invisible in the output, so a stale run is indistinguishable from a real result.

**How to apply:** before trusting any surprising `arkham-replay` output — especially a parse error naming a constructor that exists in the source, or a fix that mysteriously does nothing — check what actually ran:

```bash
stack exec which arkham-replay              # which path resolved
ls -la ~/.local/bin/arkham-replay           # the stale shadow
ls -la backend/.stack-work/install/*/*/bin/arkham-replay   # must be newer than your edit
```

Compare the binary's mtime against the source edit, and re-run once before diagnosing.

**Workaround (confirmed #5234):** when a rebuild is running concurrently (`ps | grep 'stack build'` > 0), `stack exec` falls back to the stale PATH shadow *intermittently* — the SAME command alternates between success and a phantom parse error (e.g. `DuringYourAction` "not a valid WindowMatcher tag", which the Jun 18 binary predates). Fix: capture the resolved path once (`BIN=$(stack exec which arkham-replay)`) and invoke `"$BIN" …` by absolute path so it never falls through PATH. Related: [[feedback_stack_build_flags]].

**Mtime-is-not-enough variant — GHC downsweep snapshot (#5265, 2026-07-28):** the strongest trap, because every mtime check *passes*. If you edit a module while a full-package pass is already running, GHC's downsweep has already fingerprinted the file, so the pass compiles the **pre-edit** source — then finishes and relinks, giving you a binary whose mtime is newer than your edit but whose logic is old. The watcher does not queue a second pass (it saw no change while idle). Result: pre-fix and post-fix replay runs are byte-identical, which reads as "the fix does nothing" and tempts you to re-diagnose correct code. Lost ~40 min on #5265 this way.

Mtime comparison cannot detect this. The decisive check is the interface's recorded source hash:

```bash
HI=$(ls backend/.stack-work/install/*/*/lib/*/arkham-api-0.0.0-*/Arkham/<Path>/<Module>.hi)  # newest one
stack exec -- ghc --show-iface "$HI" | grep src_hash   # must equal:
md5 -q backend/arkham-api/library/Arkham/<Path>/<Module>.hs
```

A mismatch means the binary predates your edit. **`touch` alone does NOT wake the watcher** — `entr` on macOS ignores the NOTE_ATTRIB-only event, so the mtime moves and no pass starts. Rewrite the file's bytes instead (`cp f f.bak && cat f.bak > f && rm f.bak`, or any real write) and wait for `[Source file changed]` (not `[Flags changed]`) in `.claude/build.log`. Gate any wait loop on that hash equality plus the exe being newer than the `.hi`. Note BSD `find` has no `-newermt` (GNU-only) — a wait loop using it silently never fires and times out.

**Wrong-CWD variant (#5303, 2026-07-30) — the cheapest one to hit.** `stack exec` only resolves the project binary when the CWD is inside the stack project. Any command shaped `cd /tmp/issue-N && … && stack exec arkham-replay -- …` runs **outside** the project, so stack finds no local config and falls straight through to `~/.local/bin/arkham-replay`. This is not a race — it fails 100% of the time and looks like a fresh regression: the same export that parsed seconds earlier dies with `Failed to parse export: … key "clues" not found` (a field the old binary predates). **Always `cd` back into `backend/` before `stack exec`**, or invoke by absolute path. Building an answers file in the scratch dir is the usual way this creeps in — do the `cd` in the same compound command.

**Stale-LIBRARY variant (Do No Harm, 2026-07-24):** even the `.stack-work/install/.../bin/arkham-replay` binary can be a valid, non-shadowed file yet still run OLD card logic, because `make api.watch` only builds `arkham-api:lib` + the **api-server** exe — it never relinks the `arkham-replay` exe. So after implementing/fixing a card, the replay binary keeps the previously-linked library and shows the card's *old* behavior (for a brand-new card, the empty scaffold → no abilities). This produced a fully convincing false repro: "Do No Harm reaction never fires" — actually the replay binary just had the abilityless scaffold. **Before verifying any newly-written card behavior with arkham-replay, explicitly `stack build arkham-api:exe:arkham-replay --fast` first**, then confirm the new code is in it (`strings "$BIN" | grep <trait/string you added>`). A `Debug.Trace` on the suspect `getAbilities` proving it's called (or not) settles scaffold-vs-real instantly. Note: the prelude (ClassyPrelude) already exports `trace`/`traceShow`, so DON'T `import Debug.Trace` — it makes the name ambiguous and fails the build.

**The only path that reliably runs your build (#5380, 2026-08-11):** the
**dist-dir** binary,
`backend/arkham-api/.stack-work/dist/<arch>/ghc-<ver>/build/arkham-replay/arkham-replay`.
That is what `make api.watch` relinks. `stack exec arkham-replay` gave stale results
for two consecutive rebuild cycles here even though `stack exec which arkham-replay`
reported the `.stack-work/install/…/bin` path and both binaries' mtimes were newer
than the edit — so *neither* mtime nor `stack exec which` proves what ran. Verifying
the symbol was linked (`nm "$BIN" | grep <YourNewTopLevelName>`) confirmed the code
was present while `stack exec` still produced old behavior; invoking the dist-dir
binary by absolute path immediately gave the correct result. Do that from the start,
and prefer a distinctive new top-level binding you can `nm`-grep as the liveness check.

## "arkham-replay without --undo re-emits the export's saved gameQuestion verbatim, so ability-availability fixes look like no-ops"

`arkham-replay <export> --output state.json` drains the saved queue. If the queue is
already empty (the usual case for a `/file-bug` export, captured while a question is
pending), the engine has nothing to run and **re-emits `gameQuestion` exactly as
serialized**. Ability lists are not recomputed.

Consequence: a fix to what appears in a player window (ability availability, action
legality, playable cards) produces a byte-identical choice list before and after the
change — even when the recompiled binary demonstrably contains the fix. Verified on
#5282: with the ability's criteria override forced to `Never`, the offending Fight
action still appeared in the no-undo run; `--undo 1` immediately dropped it.

**How to apply:** to verify anything question-shaped, always `--undo N` (N≥1) so the
question is rebuilt from the queue. Confirm the fix is actually in the binary by
reading the relevant entry out of `.gameModifiers` in the output JSON rather than
trusting mtimes — the modifier map is serialized with its live criteria.

**`--undo N` alone is often still not enough (#5398).** Rewinding to step K restores that
step's *stored* `gameQuestion` from the patch; if K's queue is already drained the engine
re-emits it without recomputing the ability list. On #5398 `--undo 4` landed exactly on the
Safeguard reaction window and pre/post choice lists were byte-identical, while
`.gameModifiers` in the same output proved the new `CannotEnter` was live. Reliable shape:
**undo past the question's creation, then replay forward** — pick N so the pending question
is the one *before* the window you care about, answer it via `--answers`, and let the queue
rebuild the next question. If pre/post are identical but `.gameModifiers` shows the fix,
you are one `--undo` too shallow.

Corollary for positive tests: `--undo` also rewinds the actions/cards the user spent,
so the entity you want to exercise may not be in play yet at depth N. Inject
`{"tag":"Raw","contents":{"tag":"GainActions","contents":["<iid>",{"tag":"GameSource"},2]}}`
as the first answer to buy actions without rewinding further. Every `Raw` bumps
`gameScenarioSteps`, so each subsequent `Answer` needs the version from the *previous*
run's output — build the answers file incrementally.

Corollary for crashes mid-resolution: a `/file-bug` export taken when a *forced* effect
crashed rolls back to the pre-crash question, so the crashing message is unreachable from
the pending choices and a plain replay exits clean. Rather than hunting for an `--undo`
depth, inject the offending message directly as `Raw` — set-aside cards are still in
`.gameCards`, so e.g.
`{"tag":"Raw","contents":{"tag":"TakeControlOfSetAsideAsset","contents":["<iid>",<card JSON from .gameCards>]}}`
reproduces a mandatory take-control deterministically (used on #5290).

Related: [[project_stale_local_bin_arkham_replay]],
[[project_replay_undo_entity_token_fidelity]], [[project_action_diff_snapshot]].

## "arkham-replay uses ONE StdGen for the whole --answers run; the server re-seeds mkStdGen gameSeed per request"

`app-replay/Main.hs` creates `genRef <- newIORef (mkStdGen currentData.gameSeed)` **once**,
then loops over every answer against that generator. `updateGame` in
`Api/Handler/Arkham/Games/Shared.hs` does `genRef <- newIORef $ mkStdGen gameSeed`
**per HTTP request**, and `gameSeed` is a plain field the engine never advances — so
in production every answer starts from the identical RNG state.

**Why:** any answers file with 2+ entries where an early answer consumes randomness
puts later answers on a different draw than the real game took. Shuffles, random
location placement, and random enemy/card picks will diverge from the session being
investigated, silently.

**How to apply:** when the bug is downstream of a shuffle/random placement, replay the
suspect answer as the FIRST entry (rewind further with `--undo` rather than walking
forward through several answers). To test RNG sensitivity, patch the seed instead:
`jq --argjson s 42 '.campaignData.currentData.gameSeed = $s' export.json > seed.json`
and re-run — used on #5391 across 10 seeds to rule out a layout-dependent hang.
