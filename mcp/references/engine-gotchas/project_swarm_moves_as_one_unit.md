---
name: project_swarm_moves_as_one_unit
description: "Engaged swarm cards are returned alongside their host and redirect their movement messages to it, so follow/disengage must be decided once per host"
metadata: 
  node_type: memory
  type: project
  originSessionId: 42c3dfdd-c8fc-49c4-b6b9-1e424a9d2fe4
  modified: 2026-07-31T00:05:39.295Z
---

`enemyEngagedWith iid` returns **swarm cards as well as their host** — `enemyEngagedInvestigators` (`Helpers/Enemy.hs`) recurses through `AsSwarm eid' _`. Callers that act per enemy must collapse to the host first (`Game.hs` does this with `<> not_ IsSwarm`).

This matters because `EnemyEnteredFollowing` and `DisengageEnemy` both **redirect a swarm card to its host** (`Enemy/Runner.hs`). Deciding follow-vs-disengage per card therefore lets one member's message overwrite another's: in #5313 a Nightriders host with Virescent Rot's `CannotMove` was correctly left behind by `DisengageEnemy`, then each swarm card's `EnemyEnteredFollowing` redirected to the host, which — no longer `InThreatArea` — fell into the generic branch and was written to `AtLocation <destination>` and re-engaged.

Rules backing (`.claude/references/rules/glossary/swarming_x.md`): *"The host enemy and all of its swarm cards move, engage, and exhaust as a single entity."* So `CannotMove`/`CannotBeMoved` on **any** member holds the whole group.

**Why:** the redirect-to-host indirection makes per-card iteration silently non-idempotent — the last message wins, and it also duplicated `EnemyEnters` windows (once per swarm card plus once for the host, whose handler already iterates `eid : swarm`).

**How to apply:** when iterating engaged enemies for movement, exhaustion, or engagement, map each to its host (`field EnemyPlacement`, `AsSwarm host _ -> host`) and `nub`, then evaluate group-wide conditions across `eid : select (SwarmOf eid)`. Fixed in `Investigator/Runner/Movement.hs` `handleDoResolveMovement` (#5313); regression tests in `tests/Arkham/Enemy/EngagementSpec.hs`. Related: [[project_enemyentered_threat_area_placement]], [[project_after_enter_engagement_timing]].
