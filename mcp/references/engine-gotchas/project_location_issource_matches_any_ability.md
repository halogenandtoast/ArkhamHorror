# `isSource attrs` on a location matches EVERY ability of that location

`Sourceable LocationAttrs` (`library/Arkham/Location/Types.hs:253-259`) is not the default `(==) . toSource`:

```haskell
instance Sourceable LocationAttrs where
  toSource = LocationSource . toId
  isSource LocationAttrs {locationId} (LocationSource lid) = locationId == lid
  isSource attrs (AbilitySource source _) = isSource attrs source      -- index ignored!
  isSource attrs (UseAbilitySource _ source _) = isSource attrs source -- index ignored!
  isSource _ _ = False
```

It deliberately unwraps `AbilitySource`/`UseAbilitySource` **and throws the ability index away**. That is
what makes `UseThisAbility iid (isSource attrs -> True) n` work, but it means the guard also matches the
engine's *reserved* location abilities, which every location has for free:

| constant | index | `library/Arkham/Constants.hs` |
| --- | --- | --- |
| `AbilityInvestigate` | 103 | basic Investigate action |
| `AbilityMove` | 104 | basic Move action |
| ...plus `VeiledAbility`, key abilities 500-520, etc. | | |

The basic investigate's skill test is sourced as `AbilitySource (LocationSource lid) 103`
(`library/Arkham/Location/Runner.hs:564-570`). So:

```haskell
-- WRONG: also fires when the investigator fails a plain Investigate at this location
FailedThisSkillTestBy iid (isSource attrs -> True) n -> ...
```

**#5403** — The Great Web / "Prison of Cocoons" (06343). Its *Forced* ability tests agility (3) after
you enter; failing an ordinary **investigate** there also offered "lose N actions or place 1 doom".

## Rules

- To identify *a specific* test you started, use `isAbilitySource attrs n` (`Arkham/Source.hs:314`).
- If you started the test with `IndexedSource k …` (typical for `AdditionalCostToLeave $ SkillTestCost`),
  use `isIndexedSource k attrs` (`Arkham/Source.hs:187`). `isSource` has **no** `IndexedSource` case, so it
  silently never matches — Titanic Ramp 08682-08685's "cancel the effects of the move" was dead code for
  exactly this reason, while failed investigates matched it instead. That spurious match is *not* harmless:
  `cancelMovement` (`Message/Lifted.hs:3361`) falls back to `movementModifier source iid CannotMove` when no
  movement is pending, so a failed investigate planted an `EffectMoveWindow`-scoped `CannotMove` that ate the
  investigator's next move.
- To match the bare location source and nothing else, compare directly: `((== toSource attrs) -> True)`.

This applies to any `PassedThisSkillTest*` / `FailedThisSkillTest*` / `Successful` / `Failed` guard on a
location — the same trap exists for `EnemyAttrs`/`AssetAttrs` wherever their `isSource` unwraps abilities.

See also [[project_basic_attack_enemy_source]], [[project_afterskilltest_missing_ability_anchor]].
