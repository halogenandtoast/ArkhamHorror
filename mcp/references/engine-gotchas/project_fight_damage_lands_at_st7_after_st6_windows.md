---
name: project_fight_damage_lands_at_st7_after_st6_windows
description: A Fight's damage is dealt at ST.7, AFTER every ST.6 after-window has resolved — so a reaction that flips/replaces the enemy changes who takes the hit
metadata:
  type: project
---

`SkillTest/Runner.hs` splits success into two handlers, and the damage is in the
second one:

- `SkillTestApplyResults` (`:965`, "ST.6 Determine Success") pushes
  `SkillTestApplyResultsAfter` **first**, then `pushAll` the
  `When (PassedSkillTest …)` / `After (PassedSkillTest …)` messages. Because the
  queue prepends, the windows end up *in front* of ST.7.
- `SkillTestApplyResultsAfter` (`:884`, "ST.7 — apply results") pushes the bare
  `Priority $ PassedSkillTest …` per subscriber plus `CollectSkillTestOptions`.

The bare `PassedSkillTest` is what `Enemy/Runner.hs:1345` matches to register the
`SkillTestResultOption` labelled "Damage <enemy>", whose `Successful (Action.Fight, …)`
pushes `InvestigatorDamageEnemy`. So the real order for a Fight is:

1. `When (PassedSkillTest …)` → `SuccessfulAttackEnemy` **when**-window
2. `After (PassedSkillTest …)` → `SuccessfulAttackEnemy` **after**-window
3. ST.7 → the damage

So **anything a `#after` successful-attack reaction does happens before the
attack's damage is dealt.** If that reaction replaces the enemy
(`ReplaceEnemy … Swap`, a flip to the other face), the damage lands on the
*new* side: `sourceCanDamageEnemy` is evaluated at `Msg.DealDamage`, so a
`CannotBeDamaged` that lived only on the pre-flip side is already gone.

Do **not** reason as though `CannotBeDamaged` on the attacked side protects it
— that was the wrong call made and corrected while fixing Cthulhu's facets
(`local-faq/2026-10-06_cthulhu-flip-deals-no-damage-that-action.md`, #5809).
`PassedSkillTest` in an entity runner is ST.7, **not** the after-window; the
window is `After (PassedSkillTest …)` and it is strictly earlier.

**How to apply:** for "no damage from the action that flipped it", scope the
immunity to the test rather than relying on the pre-flip side's modifiers:
`withSkillTest \sid -> skillTestModifier sid source enemy CannotBeDamaged` at
flip time. It survives into ST.7 and expires with the test, which is exactly
"that same action". Related: [[project_window_entry_tick_timing]] for the evade
half (the flipped-in side cannot react to the window it entered during).
