module Arkham.Event.Cards.BackstabSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = do
  describe "Backstab" $ do
    it "should use agility and do +2 damage" . gameTest $ \self -> do
      withProp @"combat" 1 self
      withProp @"agility" 4 self
      location <- testLocation
      enemy <- testEnemy & prop @"fight" 3 & prop @"health" 4
      setChaosTokens [MinusOne]
      enemy `spawnAt` location
      self `moveTo` location
      self `playEvent` Events.backstab
      chooseTarget enemy
      startSkillTest
      applyResults
      enemy.damage `shouldReturn` 3
