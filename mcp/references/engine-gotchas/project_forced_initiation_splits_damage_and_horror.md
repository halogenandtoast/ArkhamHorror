---
name: project_forced_initiation_splits_damage_and_horror
description: "A forced ability's per-window initiation split treated an attack's damage and horror halves as two timing points, so Spectral Shield asked which identical target, cancelled damage without offering horror, then fired again (#5785)"
metadata:
  node_type: memory
  type: project
  originSessionId: f4d82b30-348f-4854-a305-f3b3abf86671
  modified: 2026-09-28T11:12:30.724Z
---

`runWindow`'s **forced** branch (`Arkham/Investigator/Runner.hs`, `initiationsFor`) makes one
initiation per matching *window* unless `windowIsSingleEvent` says otherwise (#5743).
`InvestigatorDealtDamageOrHorror`/`AssetDealtDamageOrHorror` match **both**
`Window.DealtDamage` and `Window.DealtHorror`, and `finalizeDeferredDamageAssignment` raises
both in the same `CheckWindows` batch — so a 1-damage/1-horror attack produced two
initiations of Spectral Shield's Forced:

1. `Do (UseAbility …)` saw 2 windows on a forced non-single-event ability and asked the
   player to pick one by `primaryWindowTarget` — **both were the same investigator**, so two
   identical buttons.
2. Each use got one window, so `getTotalDamageAmounts` returned `(1,0)` — the card's
   `damage > 0 && horror > 0` branch never fired and it silently cancelled the damage.
3. The unused initiation survived `GroupLimit PerWindow 1` (counted by window intersection,
   `Helpers/Ability.hs`), so the shield triggered again and ate the horror too, spending a
   second Charge.

The card was correct throughout; this was an engine split bug.

**Why not `windowIsSingleEvent`:** the three Amaranth cards are forced/silent on
`AssetDealtDamageOrHorror #when … AnyAsset` and read only the head window (`damagedAsset`),
so collapsing the whole batch would defeat just one of several assets.

**How to apply:** timing points are now `Window.windowEventGroups`, which groups
`DealtDamage`/`DealtHorror`/`TakeDamage`/`TakeHorror` sharing a source, target and timing;
everything else stays a singleton. Both sides of the split must use it — `initiationsFor`
*and* the `Do (UseAbility …)` target ask — or the invariant in the `initiationIsLive` haddock
breaks. The non-forced branch of `runWindow` deliberately never split, which is why Flesh
Ward / Idol of Xanatos / Guard Dog (2) were unaffected.
Related: [[project_open_windows_live_in_two_places]],
[[project_window_effect_suspension]],
[[project_ability_window_thislocation_unsubstituted_outside_getactions]],
[[project_forced_ability_defaults_are_group_limits]],
[[project_divided_damage_must_batch_per_enemy]].
