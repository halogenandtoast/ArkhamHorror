module Arkham.Enemy.Cards.TheDrownedCity.TheDoomOfArkham.CthulhuFierceVisageSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.TheDrownedCity.TheDoomOfArkham qualified as Enemies
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.ScenarioLogKey (ScenarioCountKey (CthulhuRage))
import TestImport.New

-- Cthulhu (Fierce Visage) (11703) has a reaction: "After you successfully fight
-- or evade this enemy: Flip it to its Enraged side." Its Enraged side (11703b)
-- then prints "Forced - After __you__ flip this enemy to this side: Discard 1
-- card at random from your hand."
--
-- Regression coverage for #6062. The window the flip raises has to carry the
-- flipper and the Enraged side's forced ability has to say 'You'. Without both
-- halves the window matches every seat, and the GroupLimit PerWindow default
-- that forced abilities inherit then hands the discard to an
-- arbitrary-but-fixed seat -- in the reported 3-player game, always the same
-- investigator rather than the one who fought and flipped it.

-- Cthulhu's Rage sets the facets' fight/evade AND the Enraged side's health, so
-- it must be nonzero or the Enraged side has 0 health and dies to anything.
rage :: Int
rage = 3

spec :: Spec
spec = describe "Cthulhu (Fierce Visage)" do
  context "the Enraged side's Forced discard" do
    it "hits the investigator who flipped it, not another seat (#6062)"
      . scenarioTest "11688a"
      $ \self -> do
        other <- addInvestigator Investigators.rolandBanks
        run $ ScenarioCountSet CthulhuRage rage
        withProp @"combat" 5 self
        location <- testLocation
        cthulhu <- testEnemyWithDef Enemies.cthulhuFierceVisage id
        cthulhu `spawnAt` location
        moveAllTo location

        -- Both seats hold cards, so "discard 1 at random from your hand" is
        -- resolvable for either one and the assertion is about which was asked.
        self `withHand` [Assets.knife, Assets.flashlight]
        other `withHand` [Assets.knife, Assets.flashlight]

        setChaosTokens [Zero]
        void $ self `fightEnemy` cthulhu
        startSkillTest
        applyResults
        useReaction -- "Flip it to its Enraged side."
        useForcedAbility -- Enraged: the discard.

        -- The flip really happened, so this is not passing by the fight failing.
        assertAny $ enemyIs Enemies.cthulhuFierceVisageEnraged
        self.hand `shouldSatisfyM` ((== 1) . length)
        other.hand `shouldSatisfyM` ((== 2) . length)
