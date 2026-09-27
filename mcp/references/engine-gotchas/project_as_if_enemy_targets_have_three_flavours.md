---
name: project_as_if_enemy_targets_have_three_flavours
description: "As-if-enemy fight targets come as location, concealed card AND asset; every widening of the fight chooser must cover all three or Key Loci silently vanish (#5657)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 05a50b0c-d160-4c9f-b1a4-29b1a759ec57
  modified: 2026-09-08T23:23:29.856Z
---

`CanBeAttackedAsIfEnemy` is carried by three different entity kinds, and
`ChooseFightEnemy` in `Arkham/Investigator/Runner.hs` selects each separately:

- **locations** — Mist-Pylons (`Location/Cards/EdgeOfTheEarth/TheHeartOfMadness/MistPylon_17*`)
- **concealed cards** — `getConcealedChoicesAt NotForExpose`
- **assets** — Dogs of War's **Key Locus** (`Asset/Assets/KeyLocus*`)

The three ids are all `coerce`d into the same `EnemyId` choice list, so a widening
applied to one select and not the others fails **silently** — the target simply
isn't offered, with no error.

#5072 widened the location and concealed selects to `asIfEnemyLocations`
(`orConnected ForMovement`) for Runic Axe's Inscription of the Hunt but left the
asset select on `at_ (locationWithInvestigator investigatorId)`. Result (#5657):
Hunt could reach a Mist-Pylon one step away but never a Key Locus one step away.

Downstream, a card that consumes the chosen id must branch on all three too.
`RunicAxe.hs` needed both an asset case in `needsHunt` (else a locus at your own
location forces a pointless Hunt, and the post-move re-entry loops) and an asset
case in the `Hunt` `DoStep` (else the fallback runs
`getLocationOf (eid :: EnemyId)` = `field EnemyLocation` on a non-enemy and throws).
`Locateable AssetId` already exists (`Helpers/Location.hs`).

Note the as-if-enemy selects only reach `orConnected` (distance 1) while the Hunt
enemy override reaches distance 3 with Ancient Power — the Runner derives
`canMoveToConnected` from the customization, not the charge count. Still true as
of 2026-09-09.

Related: [[project_enemy_attack_asset_target]],
[[project_concealed_target_breaks_enemy_scoped_matchers]],
[[project_composite_enemy_interact_as_one_of]].
