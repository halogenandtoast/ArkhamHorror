---
name: project_investigate_ability_yourlocation_criterion
description: "investigateAbility bakes in `exists (YourLocation <> InvestigatableLocation)`, which IgnoreOnSameLocation does NOT neutralize — remote investigates read the wrong location"
metadata: 
  node_type: memory
  type: project
  originSessionId: 684692d6-3312-4475-889b-1f8332a8d136
  modified: 2026-08-05T05:38:37.855Z
---

`investigateAbility` / `investigateAbilityWith` (`Arkham/Ability.hs`) append a fixed
`exists (YourLocation <> InvestigatableLocation)` to `abilityCriteria`. That is correct for the
~50 **asset** call sites (Flashlight, Rite of Seeking, Lockpicks, …) but wrong for the basic
investigate action printed on a **location**, which can be evaluated remotely.

`applyAbilityCriteriaModifiers`' `IgnoreOnSameLocation` handling only rewrites `OnSameLocation`
and `OnLocation _` to `NoRestriction` — its `isLocationCheck` predicate does not see inside
`LocationExists`. So a `PerformableAbility [..., IgnoreOnSameLocation]` probe still evaluates
`YourLocation` against the *performer's* location.

Symptom (#5333): Beguile, attached to an enemy at a connected location, dropped its
`basicInvestigate` option because the investigator was standing on a location with Locked Door
(`CannotInvestigate`) — even though the enemy's location was perfectly investigatable.

Fix: `investigateAbilityAt entity matcher idx cost criteria` pins the check to a given
`LocationMatcher`. Used by `HasAbilities LocationAttrs` (`Location/Runner.hs`) and
`HasAbilities EnemyLocationAttrs` (`EnemyLocation/Runner.hs`) with `LocationWithId <self>.id`.
Behaviour-preserving in the normal case because `onLocation l` / `OnSameLocation` already force
`YourLocation == l`.

**Why:** any new "investigate somewhere you aren't" card hits the same trap, and the criterion
is invisible at the call site — it lives inside the helper, not in the ability definition.

**How to apply:** when adding a remote-action override, check whether the target ability's
criteria contain a `LocationExists (YourLocation <> ...)`; `IgnoreOnSameLocation` will not lift
it. Prefer `investigateAbilityAt` for anything sourced from a location.

Related: [[project_omnipotent_matcher_exclusion]], [[project_candiscoverclues_permission_not_presence]],
[[project_enemylocation_discovery_gap]].
