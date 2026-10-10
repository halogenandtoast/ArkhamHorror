# A fight override's reach: `CanModify` is availability-only, and overrides AND at selection

Two different code paths read `EnemyFightActionCriteria`, and they disagree about
which modifiers count. Getting this wrong makes a card's reach look right in the
ability list and wrong in the prompt (or the reverse).

**Availability** — `CanFightEnemy source` (`Game.hs:4429`) gathers overrides from
the enemy's modifiers **plus** `AbilityTarget iid ab.ref` for the ability named by
the source, and `combineOverrides` (`Criteria/Override.hs:14`) **ORs** them. So two
cards that each widen the same fight ability each make it available on their own.

**Selection** — `ChooseFightEnemy` (`Investigator/Runner.hs:1112`) runs its select
under `withAlteredGame withoutCanModifiers`, which drops every `CanModify`-wrapped
modifier (`Helpers/Modifiers.hs:71`). So:

- `canFightOverride` / `CanModify (EnemyFightActionCriteria …)` is **invisible**
  while choosing the target. It only answers "is this ability offered".
- Only a bare `EnemyFightActionCriteria` on the **investigator** becomes the
  `canFightMatcher`, and `>1` of those is `error "multiple overrides found"` — so a
  second investigator-targeted override is not an option.
- The `ChooseFight`'s own matcher is then **AND**ed onto it. Two independent
  permissions therefore **intersect** at selection even though they union for
  availability, and the narrower one wins.

That AND is why a card worded relative to another's reach cannot be written as a
fixed criteria. Springfield M1903's taboo targets "a non-Elite enemy up to one
location away from **its standard range**", and Telescopic Sight (3) sets that
range; the taboo article says the two were designed to stack to two locations. Both
sides have to agree on the number or the intersection clamps it back to one.

`getModifiers` inside `getModifiersFor` reads the **previous** `preloadModifiers`
snapshot (`Game.hs:7257`), so neither side can discover the other by reading
modifiers during collection. The seam used instead:

- `getAttackRangeBonus` (`Helpers/CombatTarget.hs`) answers "what does the
  attacking asset's own text add" with a direct `getAttrs @Asset` + taboo read.
  Telescopic Sight (3) and Marksmanship (1) each use `1 + bonus`: Scope in its
  ability-level `CanModify` override and, via `effectInt` metadata stamped at
  `UseThisAbility`, in the effect that does the selecting; Marksmanship in its
  in-hand ability-level override.
- Each range-setter's effect publishes `AttackRangeIncrease 1` on the investigator.
  Springfield reads it in its own `UseThisAbility` — ordinary message processing,
  where modifiers are current — because `PayCostFinished` pushes the `#when
  ActivateAbility` window **ahead of** `UseCardAbility` (`ActiveCost.hs:1920`), so
  the reaction's (or fast event's) effect already exists.
- `withinDistance` (`Matcher.hs:388`) keeps range 1 as plain `orConnected` and only
  pays for `LocationWithDistanceFromAtMost` beyond that.

Marksmanship needs **only** the investigator-side `AttackRangeIncrease`, not a
widened effect override: its effect publishes on *enemies*, so `ChooseFightEnemy`
(which reads overrides off the investigator) never sees it, and the `AnyEnemy <>
EnemyCanBeAttackedBy source` it falls back to checks only
`CanOnlyBeAttackedByAbilityOn` / `CannotBeAttackedByPlayerSourcesExcept`
(`Game.hs:4590`). Springfield's own `fightOverride` matcher is the sole authority
there.

Known residue, both predating the change: an attached Scope widens availability
whether or not its reaction is used, so declining the reaction can leave the fight
with no legal target (`unless (null choices)` just drops the prompt, after the ammo
is spent); and the Scope effect's override carries `NonEliteEnemy`, which — because
overrides AND — also blocks an Elite at your own location for that attack.

Related: [[project_cannot_trigger_ability_matching_exemptions]]
