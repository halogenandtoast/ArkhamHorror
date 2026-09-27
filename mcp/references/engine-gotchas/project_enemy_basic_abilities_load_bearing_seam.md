---
name: project_enemy_basic_abilities_load_bearing_seam
description: "Basic fight/evade/engage abilities on EnemyAttrs are a load-bearing extension seam — moving them off the enemy (to investigator or central generation) breaks per-card overrides, sweep-keyed gates, and the #4887 source ruling; attempt abandoned Aug 2026, only the CanFightEnemy evaluator extraction survived"
metadata: 
  node_type: memory
  type: project
  originSessionId: c01f077e-baf8-4f76-a4fe-9a6a4253662c
  modified: 2026-08-12T05:14:03.471Z
---

An attempt (Aug 2026) to move basic fight off `HasAbilities EnemyAttrs` (Enemy/Types.hs:296) onto
the investigator / a central generation site was abandoned after discovery. Four anchors:

1. **Source ruling**: basic attack source must BE the enemy ([[project_basic_attack_enemy_source]],
   #4887/#5342) — resolution `UseCardAbility … AbilityAttack → FightEnemy eid (mkChooseFightPure sid iid
   (a.ability AbilityAttack))` in Enemy/Runner.hs:2370.
2. **Per-card override seam**: LurkerInTheDark patches its basic fight criteria (weapon-only),
   WatcherFromAnotherDimension and VengefulShade *replace* their basics wholesale (placement/bearer
   dependent). The newtype `HasAbilities` override is the seam; any central generation needs an
   isomorphic per-card hook.
3. **Sweep-keyed gates**: Tony's BountiesOnly, Lola's CanOnlyUseCardsInRole, Blank semantics
   (index >= 100 survives — Helpers/Action.hs blankPrevents), `hasFightActions`
   (`select (#basic <> #fight …)`), Marksmanship's `select (AbilityIsAction #fight)` — all read
   enemy-sourced abilities out of the global sweep. There are TWO parallel sweeps:
   `getGameAbilities` (monadic, blank-aware, Game.hs:2044) and pure `HasAbilities Game →
   getAllAbilities` (GameEnv.hs:195, feeds getActions); both would need mirrored generation.
4. **Plain enemies get basics via `deriving newtype HasAbilities`** (→ attrs instance); custom cards
   use `extend attrs […]`. Deleting from attrs propagates uniformly — but so does the breakage.

**Conclusion**: HasAbilities + criteria + `CriteriaOverride` modifiers already IS the
"entities nominate what you can do to them" protocol. What survived: `Arkham.Action.Nomination`
(`getFightCandidates`) — the extracted single evaluator behind `CanFightEnemy` (was inline in
Game.hs). Any future unification should extract `CanEvadeEnemy` the same way and route concealed's
injection (Investigator/Runner.hs:925/1049 coercion) through these evaluators, NOT move the
abilities. **How to apply:** treat "who advertises the basic action" (enemy, via HasAbilities) and
"who evaluates eligibility" (Nomination module) as deliberately separate; extend the evaluator, never
relocate the advertisement.
