---
name: project_basic_attack_enemy_source
description: "A basic attack's damage source is the enemy itself (UseAbilitySource <fighter> (EnemySource <enemy>) 100); EncounterCardSource now guards basic ability indices out, NotSource SourceIsPlayerCard still doesn't"
metadata: 
  node_type: memory
  type: project
  originSessionId: 6cf02492-3920-4f5e-8852-fe2923b67ac1
  modified: 2026-08-05T08:19:36.841Z
---

A basic Fight action is modeled as ability index 100 (`AbilityAttack`) whose `abilitySource` is the **enemy being fought**. `Investigator/Runner/Damage.hs` (`handleInvestigatorDamageEnemy`) preserves that, so the damage source reaching the enemy is `UseAbilitySource <fighter> (EnemySource <enemy>) 100`. The other basic actions have the same shape (`AbilityInvestigate`/`AbilityEvade`/`AbilityEngage`/`AbilityMove`), anchored on the location or enemy being acted on — `notPlayerAbilityIndex` in `Arkham/Constants.hs` is the canonical list.

Consequence: source-based gating that keys on the *underlying* source misreads an investigator's own basic action as an encounter/scenario effect. The performer is only recoverable via the `UseAbilitySource` wrapper / `SourceOwnedBy`.

**#4887** — the plain `CannotBeDamagedByPlayerSources matcher` variant (Words of Power, Cowl of Sekhmet, Your Worst Nightmare — all `SourceOwnedBy iid`) OR'd `EncounterCardSource` into the *blocked* set, blocking every fighter. Fix: the plain variant blocks only its own matcher. `inShadows` uses `AnySource`, which still matches everything.

**#5342** — the mirror-image bug on the `...Except` *whitelist* variant. `Matcher.EncounterCardSource` unwrapped `AbilitySource`/`UseAbilitySource` unconditionally, so a basic fight satisfied Poltergeist's printed "or encounter cards" clause and damaged it. Fix: `Matcher.EncounterCardSource` (`Helpers/Source.hs`) now guards those two cases with `| notPlayerAbilityIndex n`, exactly as `Matcher.ScenarioCardSource` already did. `Game.hs` `EnemyCanBeDamagedBySource` switched from `NotSource SourceIsPlayerCard` to `M.EncounterCardSource` so the matcher agrees with `sourceCanDamageEnemy`, the authority at damage time.

How `UseAbilitySource <iid> (EnemySource <enemy>) 100` now classifies:
- `EncounterCardSource` → **False** (guarded)
- `ScenarioCardSource` → **False** (guarded)
- `SourceIsScenarioCardEffect` → **True** (still unguarded — a latent trap)
- `NotSource SourceIsPlayerCard` → **True** (still a trap; prefer `EncounterCardSource`)
- `SourceIsAbility BasicAbility` → **True** (the basic abilities are `basicAbility`, `Enemy/Types.hs`) — this is what `immuneToPlayerEffect` whitelists

Cards relying on the "or encounter cards" whitelist: Poltergeist (03093), Ghost Light (72023), Miasmatic Shadow (10724). Brood of Yog-Sothoth and Lurker in the Dark don't print it, but their paired `CannotBeAttackedByPlayerSourcesExcept` (which never had the hatch) blocks the fight outright.

When adding "you cannot damage X" effects, scope by performer (`SourceOwnedBy (InvestigatorWithId iid)`). See [[project_heal_wording_source_matcher]].
