---
title: project_facedown_threat_area_not_out_of_play_zone
description: FacedownInThreatArea is not an OutOfPlay *zone*, so plain enemy queries used to match face-down cards — The Quantum Maelstrom moved face-down Quantum Phantoms into play
---

Lost Quantum's face-down encounter cards live in the `FacedownInThreatArea iid` placement. They are
out of play (`isInPlayPlacement` is `False`, `isHiddenPlacement` is `True`) but they are **not** an
`OutOfPlay <zone>` placement, and `restrictToInPlayZones` (`Arkham/Game.hs`) — the default scoping
for every enemy query — only filtered `OutOfPlay {}`.

So a plain matcher matched them. The Quantum Maelstrom (`:dark-matter:090ba`) advances with

```haskell
selectEach (not_ $ EnemyWithTrait Liminal) \eid -> enemyMoveTo attrs eid lid
```

and dragged every face-down Quantum Phantom out of the threat area and into play at the new
location. `select AnyEnemy` in Entangled and Lt. Archer Michaels had the same hole.

Fixed by adding `FacedownInThreatArea {} -> True` to `restrictToInPlayZones`'s exclusion. The Dark
Matter helpers that *want* those enemies (`facedownEnemiesOf`, `getFacedownCardCount`, the
`drawFacedown*` family) all go through `EnemyWithPlacement (FacedownInThreatArea iid)`, which
`referencesOutOfPlay` already treats as an out-of-play reference (`isOutOfPlayPlacement`), so they
keep the full candidate set and still find them.

**Still open:** treachery and asset queries have no in-play scoping at all
(`getTreacheriesMatching` / `getAssetsMatching` filter only `outOfGame`), so a face-down treachery
(Cold Vacuum) or asset (Erwin Simmons) is still matchable by `TreacheryWithTrait` /
`AssetWithTrait`. Nothing in Lost Quantum exercises that today.

The frontend had a matching gap: `FacedownInThreatArea` was missing from `types/Placement.ts`, so it
decoded to `OtherPlacement` and face-down cards rendered nowhere. `Player.vue` now reads them off
the placement and draws encounter backs.
