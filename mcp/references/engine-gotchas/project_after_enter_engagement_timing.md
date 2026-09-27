---
title: project_after_enter_engagement_timing
description: "after you enter a location" resolves AFTER engagement (Zoey's Cross before On Their Heels); entry-time enemy conditions use the EnteringLocationWithEnemy snapshot window
---

A plain "after you enter a location" reaction (e.g. On Their Heels) resolves **after** enemies engage you — Zoey's Cross ("after an enemy becomes engaged") resolves first. Proof: Track Shoes is "after you move, **but before enemies engage you**", and FAQ v2.5 Q&A #034 treats that as a distinct timing point — the carve-out only makes sense if the default after-enter timing is post-engagement.

Engine order (in `handleDoResolveMovement`, `Investigator/Runner/Movement.hs`): `MovedButBeforeEnemyEngagement` → `CheckEnemyEngagement` → after-`Entering` window. Do **not** reorder after-entering before engagement (commit 75c7478f61 / #4759 set this deliberately; reverting it breaks the timing).

Because an enemy can be defeated during engagement before the after-entering window, the `Window.Entering` current-state enemy check is unreliable for "after you enter a location with 1+ enemies" triggers. The triggering condition is locked at entry (FAQ 1.3/1.4), so On Their Heels must still fire (to discover a clue) even if the enemy was defeated. Fix (#4813): a snapshot window `Window.EnteringLocationWithEnemy` (matcher `EntersLocationWithEnemy`) emitted at entry only when `selectAny (EnemyAt lid)`, offered in the same after-enter batch. Cards put effect-viability in ability criteria, not the window's Where-clause.

Cards whose *effect* needs the enemy (Helen Peters evade, Gene Beauregard move-enemy) don't need this — they correctly can't trigger once the enemy is gone.
