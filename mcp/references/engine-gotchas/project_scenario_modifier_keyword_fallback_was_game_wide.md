# `ScenarioModifierKeyword`'s enemy fallback asked about EVERY enemy

`Keyword.ScenarioModifierKeyword k v kw` ("Aloof during the day / Massive
during the night") is resolved in the `EnemyKeywords` projection,
`Game.hs:5269`. It used to read:

```haskell
ok <- selectAny (ScenarioWithModifier (ScenarioModifierValue k v))
if ok then pure (Just kw)
      else selectAny (EnemyWithModifier (ScenarioModifierValue k v)) <&> ...
```

Two defects, both from `07fd709338 The Lost Sister progress`:

1. **The fallback is unscoped.** `selectAny (EnemyWithModifier …)` asks "does
   *any* enemy in the game carry this value?", not "does *this* enemy?". The
   Lost Sister (`10569`) is the one scenario that sets `time` per entity —
   `modifySelect a (EnemyAt $ LocationWithTrait Dark) [ScenarioModifierValue
   "time" (toJSON Night)]` — so a single enemy standing anywhere Dark turned
   **every** conditional keyword on **every** enemy on the board to Night.
   Black Amanita (`10738`) at Iridescent Passage (`10718`, revealed traits
   `[Cave]`, not Dark) became Massive because a Cavern Moss sat at Open Cave
   (`10581`, `[Cave, Dark]`), and so attacked on every action the co-located
   investigator took (#5853).
2. **The scenario value won over the enemy's own.** The scenario check ran
   first and The Lost Sister sets a global `time = Day` deliberately
   (`TheLostSister.hs:152`, "locations will apply Night to enemies/investigators
   directly"), so an enemy genuinely at a Dark location still took its **Day**
   keyword as well as — via defect 1 — its Night one. Aloof and Massive at once.

The fix reads the enemy's own value out of `modifiers'` (already bound at the
top of the branch, covering both `toTarget e` and its `CardIdTarget`) and falls
back to the scenario only when the enemy has no value for that key:

```haskell
case [v' | ScenarioModifierValue k' v' <- modifiers', k' == k] of
  (v' : _) -> pure $ if v' == v then Just kw else Nothing
  []       -> do
    ok <- selectAny (ScenarioWithModifier (ScenarioModifierValue k v))
    pure $ if ok then Just kw else Nothing
```

Per-enemy value as the override, scenario value as the default — the same
precedence `HasTimeOverride EnemyAttrs` already used via `thisExists`
(`Campaigns/TheFeastOfHemlockVale/Helpers.hs:248`). Every other Hemlock Vale
scenario sets only the global modifier, so the first branch is empty there and
the old code path is preserved exactly.

`ScenarioModifierKeyword` has 8 users, all Hemlock Vale enemies: `Myconids`,
`TheForest`, `HemlockHouse`, `AgentsOfTheColour`, `TheThingInTheDepths`.

**Verifying it needs a constructed position.** The export was captured with no
enemy at a Dark location carrying a conditional keyword, so replaying it only
proves the Day half. `arkham-replay --answers` with two `Raw` injections —
`PlaceEnemy <amanita> (AtLocation <openCave>)` and `PlaceInvestigator c10012
(AtLocation <openCave>)` — then the action answer, re-creates the night half:
the attack of opportunity comes back, which it can only do through the
per-enemy branch while the scenario is Day. Note that `EnemyEngagedInvestigators`
is a `virtual` field (`Enemy/Types.hs:491`) computed from `EnemyKeywords`, so it
never appears in `--output` — the attack in `--trace` is the observable, not the
state dump. Aloof-at-night is *not* separately observable in that position:
Massive forces engagement regardless, so the suppression is a code-level
consequence of the same branch.
