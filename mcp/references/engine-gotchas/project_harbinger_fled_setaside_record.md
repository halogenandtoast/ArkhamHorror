---
title: project_harbinger_fled_setaside_record
description: "TFA Harbinger of Valusia end-of-scenario damage recording must read the fled (set-aside) enemy, not fall back to stale record count"
---

Harbinger of Valusia flees mid-scenario at `2 × players` resources → `place attrs (OutOfPlay SetAsideZone)` (out of play, keeps damage). End-of-scenario code records `TheHarbingerIsStillAlive` = its damage. Default `selectOne enemyIs` does NOT match out-of-play enemies, so if it fled you must read damage via `OutOfPlayEnemy SetAsideZone` + `field @(OutOfPlayEntity 'SetAsideZone Enemy) (OutOfPlayEnemyField SetAsideZone EnemyDamage)`, else the stale prior record count carries over and this scenario's damage is lost (issue #5133).

`TheDoomOfEztli` had the correct two-branch version; `TheBoundaryBeyond`, `ThreadsOfFate`, `TheDepthsOfYoth`, `HeartOfTheElders` were missing the set-aside branch (fixed inline, not a shared helper — the module is built under `--pedantic`/-Werror and a helper forced cascading unused-import removals). This block is duplicated across all 5 scenarios; keep them in sync.
</content>
</invoke>
