---
name: project_skilltestatyourlocation_was_affect_gated
description: "SkillTestAtYourLocation used to bake in the CannotAffectOtherPlayersWithPlayerEffectsExceptDamage check, silencing self-only reactions under Self-Centered; the affect gate now lives per-card as SkillTestOfInvestigator (affectsOthers Anyone)"
metadata: 
  node_type: memory
  type: project
  originSessionId: 7565f437-0723-4b9d-b205-6f2559354521
  modified: 2026-09-19T09:22:53.612Z
---

`Matcher.SkillTestAtYourLocation` (`Arkham/Helpers/SkillTest.hs`) used to return False for
*another* investigator's test whenever you had
`CannotAffectOtherPlayersWithPlayerEffectsExceptDamage` (Self-Centered `06035`, multiplayer-only
weakness). That conflated a location predicate with an affect-others permission, so it silenced
every card gated on it whose effect only touches you: Control Variable "discover 1 clue at your
location" (#5738), plus Jewel of Aureolus 3, Randolph Carter, Keeper of the Key, Broken Diadem 5,
Seal of the Elders 5, Servant of Brass. Symptom is a reaction window that simply never opens for
that seat — nothing in the trace, just `Do (CheckWindows …)` then `EndCheckWindow`.

As of 2026-09-19 the matcher is `lid1 == lid2` only, and the ~12 cards whose effect really does
touch the performing investigator carry the gate themselves:
`DuringSkillTest (SkillTestAtYourLocation <> SkillTestOfInvestigator (affectsOthers Anyone))`
(Police Dog 0/1, Ancient Covenant 2, Blessing of Isis 3, Curse of Aeons 3, Uncanny Specimen,
Guided by the Unseen 3, Thomas Olney, Livre d'Eibon, Darrell Simmons, Amalthea Weaver, Dawn Star 1,
Practice Makes Perfect). `TheEyeOfRavens.hs` already used that idiom.

**How to apply:** when adding a card with "during a skill test at your location", decide whether
its *effect* touches the performer. If it does, append
`SkillTestOfInvestigator (affectsOthers Anyone)`; if it only affects you or the board, don't —
`SkillTestAtYourLocation` no longer restricts anything on its own. Bare `affectsOthers` is safe in
these criteria despite [[project_affectsothers_active_investigator]] because both evaluation paths
scope the active investigator to the card's owner: `getIsPlayableWithResources'` wraps in
`asActive iid` (Helpers/Playable.hs) and `getCanPerformAbility` in `withActiveInvestigator iid`
(Helpers/Ability.hs). Related: [[project_skilltestat_matches_target_location.md]] — don't reach for
`SkillTestAt YourLocation` as a substitute.
