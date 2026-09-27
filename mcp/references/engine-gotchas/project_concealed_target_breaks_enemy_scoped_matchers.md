---
name: project_concealed_target_breaks_enemy_scoped_matchers
description: "Attacking/evading a concealed mini-card builds the skill test with a ConcealedCardTarget, so WhileAttackingAnEnemy/WhileEvadingAnEnemy (the #fighting/#evading labels) never match; action-worded cards must use WhileAttacking/WhileEvading"
metadata: 
  node_type: memory
  type: project
  originSessionId: 756a6cbb-985d-4fc9-8e51-dbf6a32b6baf
  modified: 2026-08-21T03:32:26.835Z
---

`#fighting` / `#evading` (`Matcher/SkillTest.hs:89,95`) desugar to
`WhileAttackingAnEnemy AnyEnemy` / `WhileEvadingAnEnemy AnyEnemy`, and
`Helpers/SkillTest.hs:860-872` resolves them through `st.target.enemy`.
Fighting or evading a **concealed mini-card** builds the test with a
`ConcealedCardTarget` (`Campaigns/TheScarletKeys/Concealed/Runner.hs` —
`:83-101` for the basic fight/evade actions, `:154-181` for card-effect
`AttackEnemy` / `TryEvadeEnemy`), so `st.target.enemy` is `Nothing` and the
matcher fails. For a `SkillTestResult` window this kills the ability at the
outer gate in `Helpers/Window.hs:1448-1454` — it is never offered on *any*
window. Reported as #5475: Daniel Jameson (60555) couldn't trigger off a
failed Duke attack; Duke was incidental, the mini-card was the cause.

**Why:** this is rules-correct for enemy-scoped matchers. FAQ v2.5 Q&A #139 and
the Concealed Mini-Cards glossary entry both say a mini-card **is not** an
enemy — you attack/evade it only "as if it were an engaged enemy". So the split
is per-card, by printed wording, not an engine fix.

**How to apply:** if the card text keys off the *action* ("an attack or
evasion", "while fighting", "during an attack using X"), use the nullary
`WhileAttacking` / `WhileEvading` — they only check `skillTestAction`, and both
concealed entry points set it explicitly. If the text says "an enemy", leave the
enemy-scoped matcher. Note `WhileAttacking` has label `#attacking`; `WhileEvading`
has no label, use the constructor. Cards switched in commit b1b78a6394: Daniel
Jameson, Ice Pick (0/3), Custom Modifications, Fire Axe (0/4), Bloodlust, Hidden
Cove, Arkham Woods – Wooden Bridge, Directive: Due Diligence. Left enemy-scoped:
Lucius Galloway, Zan'et el Settat, Notre-Dame, Oops! (0/2), Belly of the Beast,
Exploit Weakness (its payload discards the attacked/evaded enemy).

Widening also picks up fights against `CanBeAttackedAsIfEnemy` locations/assets
(`Investigator/Runner.hs:960-982`), which reads correct for all of these.

**Frontend counterpart:** `skillTest.targetCard` is null for a concealed target
(`targetToMaybeCard` has no `ConcealedCardTarget` case), and `SkillTest.vue` fell
back to `sourceCard`, putting the attacking asset in the *target* slot and
blanking the source slot. `SkillTest.ts` now decodes `target`, and `isConcealed`
checks whether the target's `contents` is a key in `game.concealed` — this works
for `ProxyTarget` too, because `targetDecoder` flattens a proxy to the proxied
target's `contents`.

Related: [[project_concealed_fight_evade_difficulty_entry_points]],
[[project_omnipotent_matcher_exclusion]].
