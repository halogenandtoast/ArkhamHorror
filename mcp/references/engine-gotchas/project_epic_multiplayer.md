---
title: project_epic_multiplayer
description: "Epic Multiplayer mode — architecture, namespace, milestone plan, and key design seams"
---

**BLOB CORRECTION (2026-08-09; supersedes older Act 1/3 wording below):** only Epic Act 1 (Expose the Anomaly, 85005) has a global clue threshold. Epic Act 3 (Blackwater's Bane, 85008) has no clue ability/threshold and advances at the end of a later round, like 85009; its only shared concern is using the same seeded organizer story for each cycle. ActContribution/ActSpend are per-cycle and each group clears its cells when it settles. Epic Subject 8L-08 must mirror direct damage placement, assigned damage, and healing to shared health, and must retain the normal Subject's `ScenarioSpecific "devour"` behavior. Shared-state `sharedVersion` is now a monotonic delivery revision, threshold-crossing atomically arms AwaitingOrganizer before the parked GameUpdate is published, and authoritative Blob health/countermeasure deltas are clamped while recording the effective undo amount.

Implementing **Epic Multiplayer** (the side-story mode where N groups of 1–4 each play their own
game with shared global state + a non-playing "event organizer"). Approved plan:
`/Users/halogenandtoast/.claude/plans/abstract-singing-storm.md`.

**Namespace:** uses `Arkham.Epic.*` (engine) + `Entity.Arkham.Epic` (persistence) because
`Arkham.Event` is the player-card Event type. Env/role named `EpicEnv`/`EpicRole` to avoid clashing
with the card.

**Load-bearing design decisions (do not relitigate):**
- Each group is a SEPARATE `ArkhamGame` row; a new `ArkhamEpicEvent` aggregate owns them. Groups are
  linked via the `arkham_epic_groups` join table (FK to arkham_games) — the central `arkham_games`
  table is deliberately NOT given event columns.
- Engine is **write-only** toward shared state: handlers emit `SpendShared`/`RaiseShared SharedKey Int`
  messages, intercepted in `Arkham.Game.runMessages` (`captureSharedDelta`) into the per-action
  `EpicEnv.epicEnvDeltaRef`. Reads use a LOCAL replica (NOT an ambient IORef read — the modifier
  builder is pure `ReaderT Game` and can't do IO; that was the critical critique fix).
- Commit: `updateGame` (Games/Shared.hs) loads the event via `lookupGameEvent`, runs the action, then
  applies drained deltas to the locked event row in the SAME transaction (`applyEpicDeltasLocked`,
  late+brief FOR UPDATE). New shared state is published to the group's own ws as `SharedStateUpdate`.
- **Cross-group undo** works because deltas are additive/commutative: `revertDelta` subtracts the
  amount from the CURRENT value (see `revertEpicDeltasForGameStep`). Each mutation is an
  `arkham_epic_steps` row keyed by (event, gameId, gameStep).
- Total-investigator scaling (15/inv health, 2/inv clues, ceil(total/2) countermeasures) is FROZEN at
  event start in the event row; local `getPlayerCount` stays per-group.

**User decisions:** Milestone 1 = foundation + ONE display-only shared counter (countermeasures), 2
groups, read-only organizer dashboard. Undo target = full cross-group. Organizer MAY also hold a
playing seat (separate ArkhamPlayer row; no special-case needed).

**Milestone status:** M1 is CODE-COMPLETE (pending the user's backend build). Backend: `Arkham/Epic/Types.hs`,
`Entity/Arkham/Epic.hs`, Orphans PersistFields, `GameApp.appEvent`+`HasMaybeEpic`, `Shared*` messages +
`runMessages` interception, `Api/Arkham/Epic.hs` runner, `updateGame` commit hook, event handlers/routes
(`Api/Handler/Arkham/Events.hs`, `config/routes` `/events`), event websocket room (`appEventRooms` in
Foundation + `streamRoom`/`publishToEventRoom` in Games/Shared + `getEventRoom`/`eventChannel` in Helpers),
undo hook (`revertEpicDeltasForGameStep` called from `stepBack` in Undo.hs), migration
`migrations/*/arkham_epic.sql` (sqitch change `arkham_epic` — must be applied manually), pure test
`tests/Arkham/Epic/EpicMultiplayerSpec.hs`. Frontend (type-checks clean): `types/EpicEvent.ts`,
`stores/event.ts`, api.ts additions, `views/OrganizerDashboard.vue`, `components/SharedStatePanel.vue`,
route `/events/:id`, `SharedStateUpdate` ws branch in Game.vue. ENTRY POINT: the "Epic Multiplayer Mode"
option (vs "Single Group Mode") lives in the side-story flow (`components/NewCampaign/GameOptions.vue` +
`views/NewCampaign.vue` `start()` Epic branch → `createEvent` → `/events/:id`), shown ONLY when the selected
side story has `"epicMultiplayer": true` in `src/arkham/data/side-stories.json` (set on 70001/85001/86001/87001;
`epicMultiplayer?: boolean` added to the Scenario type in `src/arkham/data.ts`). There is NO standalone
new-event page (NewEvent.vue was removed). LOBBY/JOIN MODEL (post-feedback rework): each group is an OPEN WithFriends lobby (0 players, IsPending)
created by `createGroupGame` in Events.hs; players join asynchronously via the normal
`PUT /games/:id/join` (PendingGames.hs), which also enrolls them as an ArkhamEpicMember(GroupPlayer).
`getApiV1ArkhamGamesR` (Games.hs) hides event-group games via `notExists arkham_epic_groups`, so the
games list shows the EVENT as one entry (frontend Home.vue fetches events + renders EventRow.vue).
Frontend entry: side-story flow only (epicMultiplayer flag). Dashboard shows per-group seat counts +
invite links (reuse JoinGame route) and an organizer-only Delete button (`DELETE /events/:id`,
`requireOrganizer`, cascades group games + event). M2 IMPLEMENTED (pending build/integration verify): shared-state SYNC SEAM = `EpicShared Text`
ScenarioCountKey (in ScenarioLogKey.hs); updateGame prepends `ScenarioCountSet (EpicShared <sharedKeyText>)`
for every shared counter + "total-investigators" at each action start (pull). Seeding is scenario-aware
(`epicScenarioSeeds` in Events.hs): Blob (scenario "85001") seeds Countermeasures=ceil(total/2) +
SharedEnemyHealth(CardCode "85037")=15*total. Epic mode signalled by a baked scenario-meta flag
("epicMultiplayer"=True via new `setInitialScenarioMeta` in Arkham.Game, set in createGroupGame). Subject
8L-08 epic (85037): HealthModifier = EpicShared "enemy-health:85037" (shows global remaining); the
`Damaged` message handler emits SpendShared for the applied damage and resets local damage on each sync (so a
group can't solo-kill from accumulated damage); defeat = killing group immediate + others pull-defeat at
remaining<=0 -> Objective -> R2 (win). BUGFIX: originally hooked CheckDefeated and computed
`attrs'.damage - attrs.damage`, but CheckDefeated never changes damage (damage is added in the engine's
`Damaged` handler), so the diff was ALWAYS 0 and the pool never drained ("health never syncs"). Hook
`Damaged (isTarget attrs -> True) _` instead — that's where damage tokens are applied. Countermeasures: scenario hooks PlaceTokens/RemoveTokens ScenarioTarget Resource ->
Raise/SpendShared. GroupDigest.players uses gameInvestigators + attr investigatorPlayerId. FLAGGED for
integration verify: killing-blow SpendShared must commit before GameOver; pull-group R2 end-to-end. M3+ =
clue threshold, story card, other epic scenarios, cross-group transfer/sync.

M3 ACT CLUE THRESHOLD (done, needs build): The Blob has SEPARATE epic act cards — Act 1
`exposeTheAnomalyEpicMultiplayer` (85005), Act 3 `blackwatersBaneEpicMultiplayer` (85008); Act 2 shared
(`extraterrestrialPhysiology`). Scenario `setActDeck` picks them when isEpic (single-group = 85006/85009).
Defs in Act/CardDefs/Standalone.hs (BlobSingleGroup vs BlobEpicMultiplayer set). Each epic act's clue
requirement is a GLOBAL pool = 2 per investigator across ALL groups. Mechanic: round-start GroupClueCost
(PerPlayer 2) CONTRIBUTES (push RaiseShared (SharedActProgress N) =<< perPlayer 2) into the shared pool
instead of advancing; a SilentForcedAbility on `ScenarioCountIncremented #after (EpicShared
"act-progress:N")` auto-advances in EVERY group (incl. idle via propagateShared/syncOneGroup) when
`progress >= 2*total*(advances+1)`. CUMULATIVE threshold (no pool reset — avoids the fragile cross-group
exactly-once reset): a LOCAL per-group count `EpicActAdvances Int` (new ScenarioCountKey, NOT EpicShared so
the sync never clobbers it; survives ResetActDeckToStage) is incremented on each advance; deterministic
shared progress + equal local counts => all groups cross each multiple in lockstep. Act3b story `sample`
still marked `-- TODO(epic story card)`.

SHARED ACT-CLUE MECHANIC (FINAL — supersedes the earlier "clues-on-acts" + ResolveEpicActAdvance-injection
designs; commits 9d62523d3e, c1b787f245, 6041c3a512):
- CONTRIBUTION (ability 1, fast): each investigator SPENDS 1-3 of their OWN clues straight into shared state
  — `spendClues iid amount` + `RaiseShared (SharedActProgress N) amount` + `RaiseShared (ActContribution N
  (GroupOrdinal ordinal)) amount` (ordinal from `scenarioCount (EpicShared groupOrdinalKey)`). NO local act
  tokens (acts hold 0 clues; the global pool + per-group contribution live only in shared state, so a reset
  zeroes every group's view with no per-group game edit). Threshold = 2*sharedTotalInvestigators.
- ADVANCE is FULLY IN-GROUP, NEVER cross-group gameplay injection (the deleted ResolveEpicActAdvance /
  SpendEpicActClues / FlipEpicAct path injected into a parked group and clobbered its mythos question).
- FIRST-RESOLVER (ability 2, forced RoundBegins, pool>=threshold, once-per-cycle meta latch): `RaiseShared
  (AdvanceRequested N) 1` + PARKS on a single-option `leadChooseOneM $ labeled "$continue" $ push
  (NextAdvanceActStep attrs.id 1)` — does NOT advance/increment. A single-option chooseOne GENUINELY parks;
  NEVER chooseOrRun*/TargetLabel-to-removable (they collapse/skip at build time).
- COORDINATOR `coordinateEpicActAdvance` is now a GATE-SETTER only (post-commit): on an AdvanceRequested edge
  with pool>=threshold set `AwaitingOrganizer N=1` (idempotent) + clear AdvanceRequested + broadcast. No
  consume/bump/floor here.
- ORGANIZER ENDPOINT `POST events/:id/resolve-advance {stage, allocation:[{ordinal,spend}]}`:
  requireOrganizer; require AwaitingOrganizer==1; validate each spend in [0, that group's ActContribution] &
  sum==threshold; `settleOrganizerAdvance` under the event lock writes each group's `ActSpend N ordinal`,
  pool=0, bumps `ActAdvanceGen N`, clears AwaitingOrganizer. CRITICAL ORDER: mirror to EVERY group's replica
  (syncOneGroup) BEFORE the gate-clearing broadcast lifts the overlay (so the parked act reads its ActSpend)
  → THEN floorAllGroupsAtCurrentStep → THEN broadcast. Idempotent double-submit (re-checks AwaitingOrganizer).
- FOLLOWER (ability 3, forced RoundBegins, `ActAdvanceGen N` > local `EpicActAdvances N`) runs the helper.
- ADVANCE+LEFTOVER HELPER (run by the parked Continue's NextAdvanceActStep AND by ability 3): leftover =
  ActContribution - ActSpend; `scenarioCountIncrement (EpicActAdvances N)`; if leftover>0 return it to THIS
  group's OWN investigators (`targets getInvestigators` lead-pick, AUTO-assign for a solo group) via
  `gainClues` (in-group, NO shared writes); `advancedWithOther attrs`. Reads ActSpend EXPLICITLY (NOT in the
  side-B body) so it gets the freshly-mirrored value at answer-time. (Leftover currently goes to ONE
  lead-chosen investigator, not split — v1 default.)
- KEYS (Epic/Types.hs): SharedActProgress Int (global pool sum); ActContribution Int GroupOrdinal
  ("act-contribution:N:O"); ActSpend Int GroupOrdinal ("act-spend:N:O"); AwaitingOrganizer Int
  ("awaiting-organizer:N"); AdvanceRequested Int ("advance-requested:N", the park signal); ActAdvanceGen Int
  ("act-advance-gen:N", server-bumped generation followers read). DELETED: SpendEpicActClues/FlipEpicAct msgs,
  PendingActAdvance/ActAdvanceFollowers/ActAdvanceResolver keys, arkham_epic_act_settlements table.
  NextAdvanceActStep ActId Int is a PRE-EXISTING msg reused for the deferred park resolution. EpicActAdvances
  N still increments per advance (stage-3 seeded-story wave; story pick keys off BlobStorySeed).
- UI: SharedPools.vue "Shared Clues X/Y" (reads act-progress:N); OrganizerDashboard allocation panel (gated
  on awaiting-organizer:N, caps each group by act-contribution:N:O, sum==threshold); EventActAdvanceBarrier.vue
  player overlay (modeled on EventStartBarrier, gated on awaiting-organizer:N, lifts on gate-clear to surface
  the parked Continue, no auto-submit).

EPIC UNDO (FINAL): (1) COMMUTATIVE shared counters (countermeasures, SharedEnemyHealth, ActContribution,
SharedActProgress) undo via the additive inverse-delta `revertEpicDeltasForGameStep` (ArkhamEpicStep); Undo.hs
`stepBack` CAPTURES the restored state + `propagateShared`s it post-commit (shows on every group's board). (2)
A consumed advance is a GLOBAL CHECKPOINT via a per-game undo FLOOR `ArkhamGameUndoFloor` (table
arkham_game_undo_floors): `floorAllGroupsAtCurrentStep` (called from settleOrganizerAdvance's consume path)
floors EVERY group at its current step, so no contributor can rewind a consumed placement into a negative
pool. `stepBack` blocks `step<=floor`; `stepBackToScenarioStep` clamps `toStep=max floor` + selects `>toStep`
(ArkhamGame & ArkhamGameRaw share arkham_games.step). The leftover return is in-group ABOVE the floor so it
stays undoable. `modifySharedStateLockedWith` returns whether the claim consumed (only the consuming path
floors). REJECTED a cross-group Saga. CONSEQUENCE (user-approved): no undo at/before a consumed advance.

PRINCIPLE (load-bearing): the seam moves SHARED COUNTERS only — it NEVER injects a gameplay Message into
another group (that was the clobbering bug). Cross-group coordination = shared counters (mirrored in) + the
display-only overlay flip.

GOTCHAS: `P`=Import so `P.get`=MonadState.get NOT Persistent (use esqueleto
`selectOne`); `ord` shadows Import's `ord` under -Werror=name-shadowing (name loop vars `ordinal`); a new act
using leadChooseOneM/labeled/targets needs `import Arkham.Message.Lifted.Choose`.

ADVANCE OBJECTIVE WINDOW (both epic acts): both advance objectives are
`Objective $ triggered (RoundBegins #when) Free` (player-resolved prompt at the start of the round),
matching the base ExposeTheAnomaly's `Objective $ triggered (RoundBegins #when) $ GroupClueCost ...`. Do NOT
use `forced` — a forced ability mandatorily resolves at RoundBegins and disrupts the round flow (auto-advances
+ skips the upkeep draw / "jumps a whole step"). Ability 2 = first-resolver (criterion: pool>=2*total, gated
once-per-cycle by a per-act-instance meta latch via wrapCriteria) -> RaiseShared AdvanceRequested. Ability 3 =
follower (criterion: this group's bit set in ActAdvanceFollowers via testBit on EpicShared group-ordinal) ->
FlipEpicAct + SpendShared bit-clear. Both `| onSide A a`. `Free` is in scope via ability 1's `FastAbility Free`.

GOTCHA: in Games/Shared.hs `P` = Import (so P.get is MonadState.get, NOT Persistent!), unlike Events.hs
where P=Database.Persist. For a keyed fetch in Games/Shared.hs use esqueleto
`selectOne $ do { g <- from $ table @ArkhamGame; where_ $ g.id ==. val gid; pure g }`.

BARRIER ENGAGEMENT GOTCHA (two bugs that left groups stuck on "Waiting for all groups"): the
frontend-orchestrated barrier only works if EVERY group's client engages the event. Two things broke that:
(1) engagement keyed off the `?event` URL query, absent on the join/"take a seat" path → fixed by adding
`eventId` to GetGameJson (backend resolves via lookupGameEvent on the game/spectate/admin handlers) and a
frontend `resolvedEventId = ?event ?? payload.eventId`. (2) in-game engagement (timerEventId/playerEventId)
gated on the PER-CLIENT dev flag `epicMultiplayerEnabled`, which invited players don't have → fixed by
removing the flag check from in-game engagement (gate CREATION only, in GameOptions; once the backend says
you're in an event game, engage regardless of the local flag — no prod exposure since events can't be
created without the flag). organizerEventId never gated on the flag. Lesson: epic-event participation must
be driven by server-provided event membership, NOT client-local URL/flags.

M3 STORY CARD + TIME LIMIT + SPECTATE + BARRIER (done, needs build):
- Act3b SHARED STORY CARD (#11): one random per-event `BlobStorySeed` (SharedKey "blob-story-seed",
  seeded `mod 1000000` at event creation, synced). Epic Act3 (BlackwatersBaneEpicMultiplayer) picks
  deterministically `case (seed + wave) mod 4 -> [rescueTheChemist 85021, recoverTheSample 85022,
  driveOffTheMiGo 85023, defuseTheExplosives 85024]`, `readStoryWithPlacement_ lead chosen Global`. wave =
  the group's Kth Act3 advance read PRE-increment as `(+1) <$> scenarioCount (EpicActAdvances 3)`. All
  groups' 1st 3b match; re-rolls each loop. No set/race (deterministic from synced seed).
- PLAYERS SPECTATE (#12): `/games/:id/spectate` already auth-free; purely frontend `PlayerEventBar.vue`
  (any event member sees the group switcher, opens siblings via the Spectate route). `fetchEvent` is
  requireEventMember (not organizer). Organizer's OrganizerBar untouched.
- TIME LIMIT (#13): organizer sets `timeLimitMinutes` (default 180; 0 = none) at creation → SharedKey
  `TimeLimitMinutes`. `EventDetails.createdAt` exposed. On expiry the frontend countdown calls
  `POST /events/:id/time-up` → forces every still-playing group to agenda 3b via
  `AdvanceToAgenda 1 Agendas.theAnomalyConsumes Agenda.B` (deck-id 1 not stage!), idempotent in-lock via
  `agendaAtOrPastStage 3`, reusing `runMessagesInGroupWhen` (extracted from syncOneGroup).
- START BARRIER (#14): frontend-orchestrated (like skip-triggers-for-all). When a time limit is set, each
  group calls `POST /events/:id/ready` once it reaches the first investigation phase; the backend ORs the
  caller's group-ordinal bit into `GroupsReadyMask` and, when all bits set, records `TimerStartedAt` (epoch)
  — both DIRECT-set under the row lock via new `modifySharedStateLocked` (NOT undoable deltas). Frontend
  shows a blocking "waiting for all groups" overlay (gated on reachedInvestigation to avoid a deck-select
  deadlock) until TimerStartedAt>0, then the countdown runs from TimerStartedAt (not createdAt) and fires
  time-up at 0. AI-code aside: `Arkham/Ai/Questions.hs` had a `toName` shadow (Arkham.Name.toName is used
  at L431) — renamed the local/param to `tmName`.

REMAINING EPIC TASKS (tasks #11-13): #11 Act3b shared random story card (4 Part-1 cards; first group to a
3b "wave" rolls random, others reuse, re-roll on a group's 2nd 3b). Wave = the group's local EpicActAdvances
3; store choice per-wave; needs an exactly-once SET (race: two simultaneous first-rollers) — likely a new
seam op `SetSharedIfUnset SharedKey Int` (atomic check-and-set under the locked event row; random index
rolled per group, first committer wins). This is the plan's flagged non-commutative case. #12 players
spectate other groups (relax read-authz from ArkhamPlayer-of-game to event-member; frontend group switcher
for players like the organizer bar). #13 optional time limit (organizer sets at event creation, default 180m,
stored on event row; on expiry still-playing groups advance to agenda 3b).

MERGED TO MAIN + GATED: the whole feature (M1+M2) is on `main` (fast-forwarded from the
`epic-multiplayer-mode` branch, alongside the user's separate AI-investigator commits). It is gated behind
a DEV-ONLY setting `epicMultiplayerEnabled` in `frontend/src/stores/settings.ts` = `isDevBuild() &&
<localStorage "epicMultiplayerEnabled">` (always false in prod). Toggle lives in the Settings danger zone
(`components/SettingsForm.vue`, shown only when isDevBuild) with a "doesn't work yet" warning. The side-story
"Epic Multiplayer Mode" option (GameOptions.vue `scenarioSupportsEpic`) requires this flag AND
scenario.epicMultiplayer. So default/prod behavior is unchanged; enable the flag to test.

ROOT CAUSE of "nothing syncs across groups" (found via DB ground truth + a 7-agent pipeline trace):
the entire runtime pipeline (delta capture, commit, propagate, frontend) was CORRECT. The break was
that **`StartScenario` (Game/Runner.hs ~558) rebuilds the scenario from a fresh `lookupScenario sid
difficulty` (meta=Null) and `setScenario`-replaces it**, copying over campaign log + player decks but
NOT scenario meta. So the `epicMultiplayer` flag baked by `setInitialScenarioMeta` at createGroupGame
was WIPED at start → every epic group game ran the NON-epic branch: regular Subject 8L-08 (not 85037),
countermeasures as ordinary LOCAL Resource tokens, Raise/SpendShared hooks gated off → zero deltas →
nothing to propagate. DB symptom: `arkham_epic_events.shared_state.sharedAppliedDeltas = []` and the
group games' `current_data` had `"meta": null` + the regular blob. FIXES: (1) Game/Runner StartScenario
now carries the standalone scenario's own meta across the rebuild (`scenarioMetaValue = these (const
Null) (attr scenarioMeta) (\_ _ -> Null)`; only the `That scenario` standalone case, NOT campaigns).
(2) PendingGames injects the EpicShared sync AFTER the setup `runMessages` (the pre-StartCampaign
injection was wiped by the same rebuild), so a joining group reconciles to the live pool post-setup.
(3) epicSyncMessages + PendingGames now mirror `total-investigators` FIRST (entities derive from it).
GOTCHA: an event created before these fixes is permanently non-epic (regular blob already placed) — must
DELETE + RECREATE the event; cannot be retro-converted.

ENEMY DAMAGE IS ASYNC (key gotcha, cost many rounds): the engine's `Damaged (EnemyTarget eid) assignment`
handler does NOT add damage tokens synchronously — it queues a separate `AssignedDamage` message that
applies them afterward. So computing `applied = attrs'.damage - attrs.damage` across
`liftRunMessage (Damaged ...) attrs` is ALWAYS 0 (and CheckDefeated never changes damage either). To drain
the shared pool, read the amount straight off the assignment: `damageAssignmentAmount assignment` and
`push $ SpendShared (sharedHealthKey attrs) amount`. Diagnosed via temporary Debug.traceM in the card's
Damaged handler + captureSharedDelta (since runMessages debugLevel prints the queue, the trace showed
`[8L08] Damaged intercepted: before=0 after=0 applied=0` right before `AssignedDamage ... 1 0`). The full
chain otherwise verified correct: lookupGameEvent returns the event, pull syncs the counts into game
state, captureSharedDelta appends to epicEnvDeltaRef, updateGame commits + propagateShared pushes.

HEALTH = DAMAGE TOKENS (user-requested model): Subject8L08EpicMultiplayer now has a FIXED max health =
`15 * scenarioCount (EpicShared "total-investigators")` (HealthModifier), and on each `ScenarioCountSet
(EpicShared "enemy-health:85037") v` sync it SETS local Damage tokens = `max 0 (maxHealth - v)` (v =
shared REMAINING) instead of resetting to 0. So every group's board shows the same climbing damage
tokens toward the same max. Shared value stays "remaining" (seed 15*total; `Damaged` handler emits
SpendShared by applied damage). Defeat: engine defeats the group that drains remaining to 0 (damage
reaches max); other groups pull-defeat via the `v<=0` Defeated push → Objective → R2. Test updated to
set total-investigators first and assert EnemyDamage = max-remaining. (No scenario-level defeat→R2 test
yet — flagged for QA.)

SYNC FIXES (post-M2 feedback): the JOIN/SETUP path (PendingGames.putApiV1ArkhamPendingGameR) is now
event-aware — it loads the EpicEnv, injects the EpicShared sync (pushAll, after addPlayer so it precedes
StartCampaign) so setup reconciles to the CURRENT pool (countermeasures/blob health correct at setup even
if other groups already acted), and commits any setup deltas. `broadcastSharedToEvent` (Games/Shared.hs)
publishes SharedStateUpdate to the event room + EVERY group's room; called from updateGame and PendingGames
so shared counters propagate live to all groups' frontends/organizer bars. LIVE CROSS-GROUP BOARD PUSH
(Games/Shared.hs): `propagateShared eid mOrigin shared` = broadcastSharedToEvent + `syncOneGroup` on every
OTHER active group. syncOneGroup runs ONLY the epicSyncMessages (`epicSyncMessages` helper) in that group's
game (appEvent=Nothing so no delta feedback; reconciliation sets tokens/health directly), preserves its
pending queue, persists a new step, and broadcasts the GameUpdate — so the other groups' BOARDS (Resource
tokens + blob health) update without that group acting. Wired into updateGame (origin skipped), the counter
endpoint (mOrigin=Nothing), and the join path. Each syncOneGroup is wrapped in catch+$(logWarn) ("Epic
syncOneGroup failed for <gid>") so one sibling's failure can't break the acting turn and shows in the log.
The frontend applies pushed GameUpdate with NO version guard (Game.vue handleResult), so a pushed update
does update the board. Frontend join fix: epic lobbies (IsPending, 0 chosen investigators) must use the
JoinGame route (/games/:id/join, real Join button), not claim-seat; MultiplayerLobby effectiveInviteUrl +
a "Take a seat" button handle this.
Build loop is user-owned (see [[feedback_build_workflow]]).
