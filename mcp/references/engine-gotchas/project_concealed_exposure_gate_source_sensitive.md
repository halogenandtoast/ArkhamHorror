---
name: project_concealed_exposure_gate_source_sensitive
description: "Exposing a concealed mini-card is gated by getCanExpose/getCanExposeAt, which BOTH need the effect's source: Coterie Envoy blocks only player-card effects while Void Chimera/Qaitbay/Tenebrous Eclipse are blanket; the card-effect fight/evade paths had no gate at all"
metadata:
  node_type: memory
  type: project
  modified: 2026-08-21T18:05:00.000Z
---

Every exposure route has a permission gate, and the gate is **source-sensitive**. Four cards
restrict exposure, and only one of them cares who is doing it:

| card | text | scope |
|---|---|---|
| Void Chimera (09626) | "You cannot expose any concealed mini-cards while one of Void Chimera's other forms is in play" | blanket (`CannotExpose` on the investigator) |
| Tenebrous Eclipse (09740) | "No more than 1 concealed mini-card can be exposed by each investigator each round" | blanket (`CannotExpose`) |
| Qaitbay Citadel (09647) | "You cannot ... expose concealed mini-cards at Qaitbay Citadel for the remainder of the round" | blanket, per-location (`noExposeAt lid` on the investigator) |
| **Coterie Envoy (09720)** | "concealed mini-cards at its location cannot be exposed **via player card effects**" | **source-qualified** (`NoExposeAt` on the *location*) |

Only Coterie Envoy's `LocationWithModifier NoExposeAt` may be conditioned on
`sourceMatches source SourceIsPlayerCard`. The other three must stay unconditional.

**The basic action is not a player card effect**, and the engine gets this right for free
*if you thread the real source*, because each basic action is sourced to the scenario-side
entity that offers it:

| route | basic action's source | `SourceIsPlayerCard` |
|---|---|---|
| fight | `c.ability AbilityAttack` → `AbilitySource (ConcealedCardSource _) 100` | False ✔ |
| evade | `c.ability AbilityEvade` → `AbilitySource (ConcealedCardSource _) 101` | False ✔ |
| investigate | `a.ability AbilityInvestigate` → `AbilitySource (LocationSource _) 103` | False ✔ |

No `InvestigatorSource` carve-out is needed — but beware that `SourceIsPlayerCard` maps
`InvestigatorSource -> True` (`Helpers/Source.hs:342`), so passing `toSource iid` as a stand-in
silently makes an effect look like a player card. `withExposeInsteadOfInvestigating` did exactly
that (`ForExpose $ toSource iid`), which is why Envoy over-blocked every investigation.
`ThisCard` is safe: during a playability check it becomes a `CardCostSource`, which resolves via
`isEncounterCard`.

**What was broken (fixed 33e4d5310a + follow-up):**

- `Concealed/Runner.hs` gated only the two `PassedThisSkillTest … AbilityAttack/AbilityEvade`
  (basic) handlers. The `PassedSkillTest … (Just Action.Fight/Evade) … | isEnemyTarget c target`
  handlers — reached by **every card effect** that fights or evades — pushed `Flip`
  unconditionally, bypassing all four restrictions. Same basic-vs-card-effect split as
  [[project_concealed_fight_evade_difficulty_entry_points]]; treat that file's four-entry-point
  table as the checklist whenever you touch concealed fight/evade.
- `getCanExpose` / `getCanExposeAt` took no source, so Envoy was applied **backwards**: it blocked
  the basic action (which it does not restrict) and not the card effects (which it does).

**Playability must mirror the gate, and only for unqualified matchers.** A mini-card is not an
enemy (FAQ v2.5 Q&A #139, Concealed Mini-Cards glossary), so it can satisfy only an *unqualified*
matcher — it cannot be known to be non-Elite. Three places encode that same rule in three
vocabularies; keep them consistent:

- `Criteria.hs` — `canDamageEnemyAtMatch` / `canEvadeEnemyAtMatch` add their
  `LocationWithExposableConcealedCard source` arm only when `enemyMatcher == AnyEnemy`.
  `canFightSomething` / `canEvadeSomething` are the unqualified fight/evade counterparts (added
  because there was no fight helper at all, leaving Spectral Razor 06201/10102 unplayable when a
  mini-card was the only target).
- `Investigator/Runner.hs` — `ChooseFightEnemy` and `ChooseEvadeEnemy` gate their concealed
  injection on `coveredByAnyInPlayEnemy enemyMatcher`.
- Use `LocationWithExposableConcealedCard source` (honours `ForExpose`), **not** raw
  `LocationWithConcealedCard`, whenever the criterion is about *acting on* a mini-card. Raw is
  right only for presence checks — Agent Ari Quinn (09763) "While there is a concealed mini-card
  at your location, you get +1 …".

**How to apply:** any new exposure route needs (1) a `getCanExpose`/`getCanExposeAt` call with the
*effect's* source, not the investigator's, and (2) a matching criterion built from
`canFightSomething`/`canEvadeSomething`/`canDamageEnemyAtMatch`/`canEvadeEnemyAtMatch` so
playability and resolution can't disagree.

Related: [[project_concealed_fight_evade_difficulty_entry_points]],
[[project_concealed_target_breaks_enemy_scoped_matchers]], [[project_omnipotent_matcher_exclusion]].
