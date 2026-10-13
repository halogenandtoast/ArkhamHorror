---
name: project_unrevealed_location_traits_need_their_own_list
description: "A location's trait matchers read `cdCardTraits` while it is unrevealed, so a shared back face needs its full printed trait list — the Summit back dropped `Rlyeh` and Alien Tablet never fired (#5847)"
metadata:
  type: project
---

`LocationTraits` (`Game.hs:4869`) picks `cdRevealedCardTraits` **or** `cdCardTraits` on
`locationRevealed`, and every trait-scoped matcher goes through it: `sourceTraits
(LocationSource lid)` (`Helpers/Source.hs:59`), `sourceCardMatches`, `LocationWithTrait`.
So the unrevealed trait list in `locationWithUnrevealed_` is not decoration — it is the whole
answer for any ability that fires while the location is face down.

The Drowned City Summit deck shares one back face printed `R'lyeh. Summit.`, and all 13
Obsidian Canyons defs declared it `summit [Summit]`. That face's own Forced ("When you
would enter this location, if you do not control the Obsidian Claw: … test [agility] (2)")
is a skill test on an R'lyeh card, so Alien Tablet – Prehistoric Writings (11763, "during a
skill test on a [[Glyph]] or [[R'lyeh]] card") should have been offered — and the player got
nothing but **Start Skill Test** (#5847). Fixed by giving all 13 the real back traits,
`[Rlyeh, Summit]`. `vaultChamber` (`CardDefs/Helpers.hs`) already had this right with
`[Rlyeh, Vault]`.

The attractive wrong answer is [[project_skill_test_on_card_excludes_basic_actions]]: that
#5588 guard also silences Alien Tablet, so it looks like the same bug. It is not — read the
ability INDEX on `skillTestSource` first. `AbilitySource (LocationSource _) 9` is a printed
ability, `isBasicAbilitySource` is `False`, and the guard never ran. Only indices 100–104
(Attack/Evade/Engage/Investigate/Move) take that path.

The arkham.build data is an unreliable cross-check here: `real_back_traits` is recorded for
9 of the 11 Summit-deck entries and simply absent on the other two, and the *revealed* faces
of the five plain Summit locations (11657–11661) are transcribed as `Summit.` only while
their siblings all carry `R'lyeh.` — unverified against the printed cards, so those were left
alone.

**How to apply:** when a trait matcher misses a two-faced location, check which face is live
before touching the matcher. A shared back face is a real card side with its own printed
traits, not a placeholder. Related:
[[project_location_front_side_modifiers_need_revealed_guard]].
