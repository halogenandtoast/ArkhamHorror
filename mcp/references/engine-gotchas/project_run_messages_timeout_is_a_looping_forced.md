---
name: project_run_messages_timeout_is_a_looping_forced
description: A PUT that hangs ~30s and returns 500 with RunMessagesTimeout in the log is almost always a Forced ability whose criterion stays true, firing forever inside one window
metadata:
  type: project
---

`RunMessagesTimeout (ArkhamGameKey …) 30000000` in the server log, with the
request coming back **500** after about thirty seconds, means the message queue
never drained. In practice that is a **Forced ability whose criterion never
falsifies**, re-firing inside a window that is raised again while it resolves.

The tell in the log is a repeated `Do (UseAbility … Ability { abilitySource = … })`
block naming the same source.

What stops a Forced repeating is its *limit period*, which `defaultAbilityLimit`
picks from the window: `SkillTestResult` is `PerTest`, `Moves` / non-after
`Enters` are `PerMove`, `EnemySpawns` is `PerSpawn`, everything else
`PerWindow`. `PerWindow` only intersects `usedAbilityWindows`, so a window that
is re-raised during resolution gets a fresh bucket and the ability fires again.

`SilentForcedAbility` took a bare `PerWindow 1` whatever its window until this
was fixed; all three Forced shapes now share the one table.

Contrast the quiet version of the same mistake — a Forced in a *play* window —
which answers 200 and simply does nothing: [[project_always_true_forced_on_anywindow_makes_a_card_unplayable]].
