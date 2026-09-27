---
name: project_shadows_cancels_queued_engagement
description: "Do (EngageEnemy) queues the #when EnemyEngaged window and the threat-area placement together, so a reaction that returns the enemy to the shadows is undone by the stale placement (#5365)"
metadata: 
  node_type: memory
  type: project
  originSessionId: bc777c86-e7c9-4833-8fda-5ec6bc7c7fac
  modified: 2026-08-09T05:56:39.994Z
---

`Do (EngageEnemy iid eid _ False)` (`Arkham/Enemy/Runner.hs`) pushes
`[when-EnemyEngaged window, PlaceEnemy eid (InThreatArea iid), EnemyEntered?, after-window]`
as one batch. Everything a forced reaction resolves **inside that window happens before the
placement**, and the placement then applied unconditionally.

Shades of Suffering act 1 (*The Lady with the Red Parasol*) has a forced Objective on
`EnemyEngaged #when You (enemyIs tzuSanNiang)`, so its entire advance — act 1b plus
`DoStep 1` returning Tzu San Niang `InTheShadows` and redistributing her concealed
mini-cards — ran inside the window. The stale `PlaceEnemy` then dragged her back into the
investigator's threat area, leaving her in play twice: mini-card *and* enemy (#5365).

Fix: the `PlaceEnemy eid InTheShadows` branch of the Enemy runner now looks for a pending
`PlaceEnemy eid (InThreatArea _)` via `findFromQueue` and calls `cancelEnemyEngagement`
(which pops the message and `ignoreMatchingWindows` the EnemyEngaged windows). The act's
*failure* branch already called `cancelEnemyEngagement` by hand; this covers every other
route back into the shadows.

**Why not guard the placement instead:** an `IfEnemyExists (… <> not_ (EnemyWithPlacement
InTheShadows))` guard on `Do (EngageEnemy)` looks tempting but breaks Ghost Light 2b, which
deliberately flips Tzu San Niang and engages the lead investigator *straight out of the
shadows*. It also silently drops engagements for Omnipotent enemies (Dagon/Hydra/Azathoth),
which default matchers exclude — see [[project_omnipotent_matcher_exclusion]].

**How to apply:** when a window reaction takes an enemy off the board mid-engagement, cancel
at the *removal* site, not by second-guessing the placement. Related:
[[project_deferred_enemymove_superseded]], [[project_enemyentered_threat_area_placement]],
[[project_placeenemy_inplay_skips_engagement]].

Note: `arkham-replay --undo` cannot verify this class of fix — the exported queue is
serialized *after* `Do (EngageEnemy)` already expanded, so the bare `PlaceEnemy` is baked in
and the new code path never runs. Verify with a spec instead
([[project_replay_cached_question_needs_undo]]).
