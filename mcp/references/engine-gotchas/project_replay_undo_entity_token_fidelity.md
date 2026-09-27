---
title: project_replay_undo_entity_token_fidelity
description: "arkham-replay --undo restores a removed entity via choicePatchDown with its FINAL (pre-removal) token counts, so damage looks already-lethal at earlier steps — don't diagnose from undo-state counters"
---

`arkham-replay --undo N` reverses steps by applying `choicePatchDown`. When a step
**removed** an entity (an enemy-location leaving `enemyLocationsL`, an enemy discarded),
the patch re-adds it carrying the tokens it had at removal — not the tokens it had at the
step you rewound to. So `--undo 10` and `--undo 4` can both show `Damage 3` even though the
real game only reached 3 damage at the later step.

**Why:** Reading those counters as ground truth makes a healthy engine look broken —
"damage 3 >= health 3 but `defeated: false`, why didn't CheckDefeated fire?" is a phantom.
In #5162 the answer was that health was really 5 (`HealthModifier` from `perPlayer 2`), and
the damage number was patch noise.

**How to apply:** Treat undo-state *entity presence* as reliable and its *token counters* as
suspect. To learn what actually happened, replay **forward** from the undo point with
`--answers` + `--trace` and read the message stream, or compute the value from the card
(`modifySelf` / `HasModifiersFor`) rather than the JSON. Related: [[project_action_diff_snapshot]],
[[project_stale_local_bin_arkham_replay]].
