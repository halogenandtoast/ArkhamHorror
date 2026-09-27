---
name: project_criteria_override_must_ride_on_the_choose_not_a_skilltest_modifier
description: "A CriteriaOverride handed to skillTestModifier is invisible to ChooseEvadeEnemy/ChooseFightEnemy — an EffectSkillTestWindow modifier only reports once that skill test exists, and the choose runs first. Put the override on the ChooseEvade/ChooseFight value (#5780)"
metadata:
  type: project
---

`skillTestModifier sid source target (EnemyEvadeActionCriteria override)` looks like the
way to say "for this one evade, widen what I may target". It is not. The override never
reaches the prompt.

`WindowModifierEffect`'s `HasModifiersFor`
(`Arkham/Effect/Effects/WindowModifierEffect.hs`) gates that window on the test being live:

```haskell
Just (EffectSkillTestWindow sid) -> do
  msid <- getSkillTestId
  when (msid == Just sid) $ tell $ MonoidalMap $ singletonMap target modifiers
```

But the skill test with that `sid` is not created until `EvadeEnemy`/`FightEnemy` runs — and
that is *downstream* of the prompt. `ChooseEvadeEnemy` (`Arkham/Investigator/Runner.hs`)
reads `getModifiers a` to find the override, finds none, and falls back to plain
`CanEvadeEnemy source`, which honours each enemy's own evade-ability criteria (i.e. at your
location). **Reordering the pushes does not help** — this is not a queue-order bug. The
`CreatedEffect` can sit well before `ChooseEvadeEnemy` and the modifier is still dark.

**The symptom is silence.** `ChooseEvadeEnemy` ends in `unless (null choices)`, so an empty
set means no prompt, no skill test, no error, no log line — the card just resolves and is
discarded. Bait and Switch (3)'s second mode ("evade a non-Elite enemy at a *connecting*
location and switch places with it") did exactly nothing whenever the only legal target was
the connecting enemy (#5780).

**The tell is a label/choose mismatch.** The availability check that decides whether to
*offer* the mode selects the override matcher directly:

```haskell
canEvadeConnecting <- selectAny $ CanEvadeEnemyWithOverride override  -- evaluates the override, fine
```

so the label appears and the follow-through is empty. Whenever a mode's label shows but
picking it does nothing, compare what gated the label against what the choose actually
selects.

**How to apply.** The override belongs on the `ChooseEvade`/`ChooseFight` value, where the
runner reads it synchronously. `mkChooseEvadeMatch` (`Arkham/Evade.hs`) sets the flag for
you when the matcher is a `CanEvadeEnemyWithOverride`:

```haskell
pushM $ setTarget attrs <$> mkChooseEvadeMatch sid iid attrs (CanEvadeEnemyWithOverride override)
```

Or edit the record, which is what Guerrilla Tactics does for both halves:

```haskell
chooseEvadeEnemyEdit sid iid attrs \ce ->
  ce { chooseEvadeEnemyMatcher = evadeOverride (EnemyAt (orConnected NotForMovement YourLocation))
     , chooseEvadeOverride = True
     }
```

With the flag set the runner uses `AnyEnemy <> EnemyCanBeEvadedBy source` as its base and
ANDs your matcher on top; `EnemyCanBeEvadedBy` is only "has an evade value, not
`CannotBeEvaded`" and imposes no location restriction, so nothing re-narrows it.

`skillTestModifier` is still right for anything the *test* consumes — Guerrilla Tactics'
`SkillModifier #combat 1` alongside its override is the correct split. Reach for
`nextSkillTestModifier`/`EffectNextSkillTestWindow` only when the effect genuinely belongs
to a test that does not exist yet and is not read before it starts.

The fight side is identical: `EnemyFightActionCriteria` + `ChooseFightEnemy` +
`mkChooseFightMatch`. Related: [[project_fight_override_is_a_widening_not_a_narrowing]],
[[project_perform_action_fight_bypasses_criteria_overrides]].
