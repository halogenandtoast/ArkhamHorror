---
name: project_damage_on_you_reads_must_add_assigned_damage
description: "A card that counts \"damage on you\" can resolve mid-assignment, so it must read healthDamage + assignedHealthDamage, not field InvestigatorDamage alone"
metadata:
  node_type: memory
  type: project
  originSessionId: 6bb13943-86ea-497b-b09b-6447fa7b601c
  modified: 2026-11-18T04:12:14.032Z
---

A card is reachable **inside** a damage assignment, so `field InvestigatorDamage` (placed
tokens only) is the wrong read for printed text like "for every 2 damage on you".
`finalizeDeferredDamageAssignment` (`Investigator/Runner/Damage.hs`) places the **asset's**
tokens first — opening `After (PlacedToken … (AssetTarget …) Damage n)` — and only reaches
`CheckDefeated` → `AssignDamage` for the investigator's own share afterwards. Anything that
window triggers runs with part of the hit still sitting in `assignedHealthDamage`.

Mark Harrigan's `[reaction]` *"After damage is placed on a card you control: Draw 1 card"* is
exactly such a trigger, and it draws out of his own deck — so it can reveal **Shell Shock**
(03008) mid-assignment.

**Why:** #6011. Hunting Shadow dealt Mark 2 damage onto 3 already placed; he put 1 on Guard
Dog and 1 on himself; the asset placement's window fired his reaction, which drew Shell Shock,
whose Revelation read 3 instead of 4 and dealt 1 horror instead of 2. The player saw 4, because
the UI (`Game.hs`, investigator display) and the `InvestigatorWithDamage` matcher both add
`assignedHealthDamage` — Shell Shock and Mark's elder sign were the only two readers that did
not. Sophie already used `InvestigatorWithDamage (atLeast 5)` and was correct.

**How to apply:** for "damage on you" use `field InvestigatorDamage iid + field
InvestigatorAssignedDamage iid`, or the `InvestigatorWithDamage` matcher, which already sums
them. Do **not** try to fix this by flushing the investigator's tokens earlier in
`finalizeDeferredDamageAssignment`: the pending total is what `CancelAssignedDamage` rewrites at
the later `When PlacedDamage` window, so placing early breaks damage cancellation. Verify with
`arkham-replay --undo 1 --answers` on the reaction window and check the ask label
("Assign 2 horror"), and wait for the relink — a replay run that races `make api.watch`
silently uses the old binary.
See [[project_remaining_health_reads_pre_hit_during_damage_assignment]],
[[project_damage_question_wrappers]].
