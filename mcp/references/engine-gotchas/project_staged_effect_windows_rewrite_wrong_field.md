---
name: staged-effect-windows-rewrite-wrong-field
description: Staged effect windows (until end of NEXT phase/turn) were advanced in effectWindow, but isEndOfWindow reads effectDisableWindow first — builder-DSL effects never expired
metadata:
  type: project
---

`EffectAttrs` carries **two** window fields and `isEndOfWindow`
(`Arkham/Effect/Types.hs`) resolves them **disable-window-first**:

```haskell
isEndOfWindow EffectAttrs {effectWindow, effectDisableWindow} w' =
  w' `elem` toEffectWindowList (effectDisableWindow <|> effectWindow)
```

Two windows are **staged** — they live one lifetime, then get rewritten into a
second, terminal window that the disable clause actually matches:

| staged window | rewritten at | terminal window |
| --- | --- | --- |
| `EffectUntilEndOfNextPhaseWindowFor p` | `Begin p` | `EffectUntilEndOfPhaseWindowFor p` |
| `EffectEndOfNextTurnWindow iid` | `BeginTurn iid` | `EffectTurnWindow iid` |

`Arkham/Effect/Runner.hs` used to do both rewrites as
`pure $ a {effectWindow = Just <terminal>}` — **the wrong field**. Which field is
authoritative depends on how the effect was created:

- `createWindowModifierEffect` / `endOfNextPhaseModifier` / `nextTurnModifier`
  (`Arkham/Helpers/Modifiers.hs`) set `effectWindow`, `effectDisableWindow = Nothing`
  → the rewrite landed correctly. These always worked.
- The `Arkham.Effect.Builder` DSL's `removeOn` sets **only**
  `effectBuilderDisableWindow` → the rewrite landed in the ignored field,
  `effectDisableWindow` stayed pinned at the un-advanced staged window forever, the
  `EndPhase` / `EndTurn` disable clause never matched, and **the modifier became
  permanent**.

Symptom in an export (`gameEntities.effects`) — the two fields disagree:

```
window:        EffectUntilEndOfPhaseWindowFor MythosPhase       <- rewritten, ignored
disableWindow: EffectUntilEndOfNextPhaseWindowFor MythosPhase   <- stale, authoritative
```

Fixed by `mapEffectWindow` / `advanceEffectWindow` in `Arkham/Effect/Types.hs`, which
rewrite whichever field supplied the window (and map *into* a `FirstEffectWindow` list
instead of clobbering it). `replaceNextSkillTest` had the same shape and now shares the
helper.

**Cards that were broken:** Reality Acid (`c85044`, The Blob That Ate Everything) —
"until the end of the next mythos phase, your base willpower/intellect/combat/agility is
0" was permanent (#5395); Heretics' Graves (Spectral) `171` ability 1 — its +1 willpower
"until the end of your next turn" was permanent.

**Still `effectWindow`-only, on purpose:** the `Setup` / `EffectForNextScenario` unwrap
and the `EndRound` / `EffectNextSetupWindow` clause pattern-match `effectWindow`
directly. Only `forNextScenarioModifier` creates those, via the helper path. If a card
ever builds one with `removeOn`, generalize those two clauses the same way.

**Persistence note:** the fix does not repair already-saved stale `disableWindow` values,
but such effects self-heal — the guard still matches on the next `Begin`, the rewrite
now lands correctly, and the effect dies at that phase's `EndPhase`.

Related: [[project_reality_acid_devourlevels_tree_blowup]],
[[project_window_key_vs_payload_guard]].
