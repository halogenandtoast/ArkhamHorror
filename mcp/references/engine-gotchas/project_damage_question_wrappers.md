---
title: project_damage_question_wrappers
description: "Damage/horror assignment questions are wrapped (QuestionWithSource+QuestionLabel); test helpers must strip them, and a lone AoO is still a chooseOneAtATime to resolve first"
---

The "Label damage assignment with source and remaining totals" change (Damage.hs `assignDamageDivided`) now pushes the damage/horror assignment `ChooseOne` wrapped as `QuestionWithSource <source>` (board source-highlight) → `QuestionLabel "Assign N damage"` (totals header) → `ChooseOne [ComponentLabel ...]`. `DamageLabel`/`AssetDamageLabel` are pattern synonyms for `ComponentLabel (InvestigatorComponent/AssetComponent _ DamageToken)`.

Test harness consequence: helpers that pattern-match `case question of ChooseOne ...` stopped seeing the choices (errored "unsupported questions type" or silently got 0 damage). Fix is `stripQuestionWrappers` (in `tests/TestImport.hs`, re-exported via TestImport.New) which peels `QuestionLabel`/`QuestionWithSource`/`PayCostQuestion`; applied across `applyAllDamage`, `applyAllHorror`, `assert*`, `chooseOnlyOption`/`chooseFirstOption`/`chooseOptionMatching`, and `chooseOptionAcrossQuestions`' `findIn`.

Separate gotcha: an attack of opportunity (even a single enemy) is ALWAYS presented as `chooseOneAtATime` (Game/Runner.hs `EnemyAttacks -> chooseOneAtATime`). A test that provokes an AoO must resolve it (e.g. `chooseOnlyOption`) BEFORE `applyAllDamage` — the attack only deals/assigns damage once executed. `applyAllDamage` only drains `ChooseOne`, not the attack-resolution `ChooseOneAtATime`.

Relates to [[project_aoo_gated_at_callsite]] and [[project_simultaneous_damage_window_targets]].
