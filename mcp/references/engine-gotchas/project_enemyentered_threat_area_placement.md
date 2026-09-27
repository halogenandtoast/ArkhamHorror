---
name: project_enemyentered_threat_area_placement
description: "Moving an engaged enemy left placement as InThreatArea until Do (EnemyMove), so EnemyCheckEngagement bailed and the enemy landed unengaged"
metadata: 
  node_type: memory
  type: project
  originSessionId: b70db97c-5e29-4672-a93f-82ef93b185c8
  modified: 2026-07-29T13:32:44.176Z
---

`EnemyEntered` used to keep an `InThreatArea` placement untouched and let the deferred `Do (EnemyMove)` do the threat-area → `AtLocation` rewrite. But `EnemyEntered` pushes `After msg` to the *front* of the queue, and `After (EnemyEntered)` runs `EnemyCheckEngagement` — so the engagement check ran while the enemy still looked engaged, its `unengaged` guard failed, and `Do (EnemyMove)` then silently disengaged it a message later. Net effect: an enemy moved out of a threat area by a card effect arrives at its destination engaged with nobody, and nothing re-checks.

Real case (issue #5292): *Redeem a Former Colleague* ability 1 pulling an engaged **Edwin Bennet** to another investigator's location — he moved but never engaged.

**Why:** the `InThreatArea {} -> pure a` branch exists for the *spawn* flow, which deliberately engages before `EnemyEntered` so after-enters windows see the threat area (Doppelgänger 11058). It was never meant to cover moves.

**How to apply:** `Arkham/Enemy/Runner.hs` `EnemyEntered` now writes `AtLocation lid` unless `isJust enemySpawnDetails` (a real spawn). No-op self-moves (Knight of the Inner Circle) don't need a second guard here because `EnemyMove`'s `willMove <- if enemyLocation == Just lid then pure False else …` (#5277) already stops them upstream — if that guard ever goes away, this needs a `getLocationOf eid == Just lid` exemption back. Placement writes that a `CheckWindows`-bearing message defers must be checked against what `After (…)` runs *before* them. Regression test: `tests/Arkham/Enemy/EngagementSpec.hs`.

Related: [[project_after_enter_engagement_timing]], [[project_deferred_enemymove_superseded]], [[project_placeasset_entity_order]].
