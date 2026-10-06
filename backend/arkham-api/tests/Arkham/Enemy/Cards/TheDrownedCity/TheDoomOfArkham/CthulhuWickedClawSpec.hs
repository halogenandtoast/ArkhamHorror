module Arkham.Enemy.Cards.TheDrownedCity.TheDoomOfArkham.CthulhuWickedClawSpec (spec) where

import Arkham.Enemy.CardDefs.TheDrownedCity.TheDoomOfArkham qualified as Enemies
import Arkham.Matcher
import Arkham.ScenarioLogKey (ScenarioCountKey (CthulhuRage))
import TestImport.New

-- Cthulhu (Wicked Claw) (11704) is "cannot be damaged, defeated, or exhausted"
-- with a reaction: "After you successfully fight or evade this enemy: Flip it to
-- its Enraged side." Its Enraged side (11704b) drops the damage immunity.
--
-- FFG's rules team, asked whether the flipping action still lands its damage:
--   "No; when you flip a Cthulhu card to its Enraged side, you cannot deal 1
--    damage to it with that same action, and it cannot be exhausted."
-- See mcp/references/local-faq/2026-10-06_cthulhu-flip-deals-no-damage-that-action.md.
--
-- Regression coverage for #5809. The fight half is the counter-intuitive one:
-- the flip resolves in an ST.6 after-window, but a Fight's damage is not dealt
-- until ST.7, so by the time it lands the Enraged side is in play and takes the
-- full weapon damage. Reasoning that the pre-flip side's CannotBeDamaged
-- protects it is wrong -- it left with the other face. See
-- mcp/references/engine-gotchas/project_fight_damage_lands_at_st7_after_st6_windows.md.
--
-- Wicked Claw is the facet used here because its Enraged Forced ability ("place
-- 1 doom on it") is self-contained; Hoary Wings' draws from the Cthulhu deck,
-- which this harness does not set up.

-- Cthulhu's Rage sets the facets' fight/evade AND the Enraged side's health, so
-- it must be nonzero or the Enraged side has 0 health and dies to anything.
rage :: Int
rage = 3

spec :: Spec
spec = describe "Cthulhu (Wicked Claw)" do
  context "after you successfully fight it" do
    it "takes no damage from the action that flipped it (#5809)"
      . scenarioTest "11688a"
      $ \self -> do
        run $ ScenarioCountSet CthulhuRage rage
        withProp @"combat" 5 self
        location <- testLocation
        cthulhu <- testEnemyWithDef Enemies.cthulhuWickedClaw id
        cthulhu `spawnAt` location
        self `moveTo` location
        setChaosTokens [Zero]
        void $ self `fightEnemy` cthulhu
        startSkillTest
        applyResults
        useReaction -- "Flip it to its Enraged side."
        useForcedAbility -- Enraged: "After you flip this enemy to this side: Place 1 doom on it."
        -- The flip really happened, so this is not passing by the fight failing.
        assertAny $ enemyIs Enemies.cthulhuWickedClawEnraged
        cthulhu.damage `shouldReturn` 0

  context "once it is already Enraged" do
    -- The immunity is scoped to the test that flipped it ("that same action"),
    -- not permanent -- being damageable is how a facet reaches the victory
    -- display at all.
    it "takes damage from a fight on a later action" . scenarioTest "11688a" $ \self -> do
      run $ ScenarioCountSet CthulhuRage rage
      withProp @"combat" 5 self
      location <- testLocation
      cthulhu <- testEnemyWithDef Enemies.cthulhuWickedClawEnraged id
      cthulhu `spawnAt` location
      self `moveTo` location
      setChaosTokens [Zero]
      void $ self `fightEnemy` cthulhu
      startSkillTest
      applyResults
      cthulhu.damage `shouldReturn` 1
