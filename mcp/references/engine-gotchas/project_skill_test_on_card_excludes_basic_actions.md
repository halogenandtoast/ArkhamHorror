---
name: project_skill_test_on_card_excludes_basic_actions
description: "\"During a skill test on a X card\" never matches a basic action ability — a basic investigate is not a test ON the location, even though we model the ability as living there (#5588)"
metadata:
  type: project
---

FAQ v2.5 Q19 defines "a skill test on a card" as *the ability that directly prompts the test*
(a "test skill (X)" template, or a Fight/Evade/Investigate action designator). Matthew's ruling
on #5588 goes one step further: the **basic** actions do not count at all. A basic investigate is
not a skill test "on" the location even though the engine models the basic ability as hosted by
the location (`AbilitySource (LocationSource lid) 103`); the same goes for basic fight/evade on
an enemy.

`Arkham/Game.hs` already encoded this for abilities (`AbilityOnCard _ | abilityBasic -> pure False`)
and `SkillTestOnLocation` already gated on `n < 100`, but `SkillTestOnCardWithTrait` /
`SkillTestOnCard` (`Arkham/Helpers/SkillTest.hs`) went straight to `sourceTraits (skillTestSource st)`
with an `st.sourceCard` fallback, so both matched the location/enemy card. Fixed by adding
`isBasicAbilitySource` to `Arkham/Source.hs` (peels `PaymentSource`/`AbilitySource`/`UseAbilitySource`,
true for indices 100-104 = Attack/Evade/Engage/Investigate/Move) and short-circuiting both matchers
*before* the trait check and the `sourceCard` fallback.

Cards on this path: Alien Tablet (11763, Glyph/R'lyeh), Grounded (3), Sleuth (3), Crafty (3),
Bruiser (3), Prophetic (3), Antiquary (3), Lab Coat (1).

#5588 reported it as "Infernal Machinery shouldn't stop Alien Tablet". It never did — Infernal
Machinery's `CannotTriggerAbilityMatching (AbilityOnCard (CardWithTrait Glyph|Artifact))` correctly
ignores the tablet (Item. Relic. R'lyeh.). The tablet was unavailable because the player investigated
with **Flashlight**, making the test source `AbilitySource (AssetSource flashlight) 1` (Item. Tool.).
The real defect was the opposite direction: a *basic* investigate at an R'lyeh location was wrongly
offering it.

**How to apply:** when a "during a skill test on a ... card" rider misfires, look at
`skillTestSource` first — it is the ability that started the test, never the target. Related:
[[project_basic_attack_enemy_source]], [[project_enemy_basic_abilities_load_bearing_seam]].
