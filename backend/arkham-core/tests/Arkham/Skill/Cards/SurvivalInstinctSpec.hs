module Arkham.Skill.Cards.SurvivalInstinctSpec (spec) where

import Arkham.Skill.Cards qualified as Cards
import TestImport.New

spec :: Spec
spec = describe "Survival Instinct" $ do
  context "this skill test is successful during an evasion attempt" $ do
    it "the evading investigator may immediately disengage each enemy and move to a connecting location" . gameTest $ \self -> do
      withProp @"agility" 1 self
      (location1, location2) <- testConnectedLocations id id
      survivalInstinct <- genCard Cards.survivalInstinct
      self `addToHand` survivalInstinct
      enemy1 <- testEnemy & prop @"evade" 0
      enemy2 <- testEnemy
      setChaosTokens [Zero]
      self `moveTo` location1
      enemy1 `spawnAt` location1
      enemy2 `spawnAt` location1
      self `evadeEnemy` enemy1
      commit survivalInstinct
      startSkillTest
      applyResults
      chooseOptionMatching "disengage each other enemy" \case
        Label _ [] -> False
        Label _ _ -> True
        _ -> False
      chooseTarget location2
      self.engagedEnemies `shouldReturn` []
      self.location `shouldReturn` Just (toId location2)
      enemy1.location `shouldReturn` Just (toId location1)
      enemy2.location `shouldReturn` Just (toId location1)
