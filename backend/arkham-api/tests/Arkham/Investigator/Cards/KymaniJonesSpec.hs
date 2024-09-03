module Arkham.Investigator.Cards.KymaniJonesSpec (spec) where

import Arkham.Event.Cards qualified as Event
import Arkham.Investigator.Cards (kymaniJones)
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Kymani Jones" $ do
  context "When you attempt to evade an exhausted non-Elite enemy" $ do
    it "If you succeed by at least X, discard that enemy. X is that enemy's remaining health." . gameTestWith kymaniJones $ \self -> do
      -- Kymani's Evade is 5 so we want +1
      enemy <- testEnemy & prop @"evade" 4 & prop @"health" 1
      location <- testLocation
      setChaosTokens [Zero]
      enemy `spawnAt` location
      self `moveTo` location
      exhaust enemy
      void $ self `evadeEnemy` enemy
      useReaction
      click "start skill test"
      click "apply results"
      assertNone AnyEnemy

    it "Also works when nested" . gameTestWith kymaniJones $ \self -> do
      -- Kymani's Evade is 5 so we want +1
      withProp @"resources" 2 self
      swiftReflexes <- genCard Event.swiftReflexes
      self `addToHand` swiftReflexes
      enemy <- testEnemy & prop @"evade" 4 & prop @"health" 2
      location <- testLocation
      setChaosTokens [Zero]
      enemy `spawnAt` location
      self `moveTo` location
      exhaust enemy
      void $ self `fightEnemy` enemy
      chooseTarget swiftReflexes
      void $ self `evadeEnemy` enemy
      -- it's nested so we need to do the fight
      click "start skill test"
      click "apply results"
      -- standard fight damage
      enemy.damage `shouldReturn` 1

      -- now the evade resumes
      useReaction
      click "start skill test"
      click "apply results"
      assertNone AnyEnemy
