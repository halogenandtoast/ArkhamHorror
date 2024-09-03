module Arkham.Event.Cards.BlindingLightSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = do
  describe "Blinding Light" $ do
    it "Uses willpower to evade an enemy" . gameTest $ \self -> do
      withProp @"willpower" 5 self
      withProp @"agility" 3 self
      enemy <- testEnemy & prop @"evade" 4 & prop @"health" 2
      location <- testLocation
      setChaosTokens [MinusOne]
      enemy `spawnAt` location
      self `moveTo` location
      self `playEvent` Events.blindingLight
      chooseOnlyOption "Evade enemy"
      chooseOnlyOption "Run skill check"
      chooseOnlyOption "Apply results"

      assert $ Events.blindingLight `isInDiscardOf` self
      assert $ enemy `evadedBy` self

    it "deals 1 damage to the evaded enemy" $ gameTest $ \self -> do
      withProp @"willpower" 5 self
      enemy <- testEnemy & prop @"evade" 4 & prop @"health" 2
      location <- testLocation
      setChaosTokens [MinusOne]
      enemy `spawnAt` location
      self `moveTo` location
      self `playEvent` Events.blindingLight
      chooseOnlyOption "Evade enemy"
      chooseOnlyOption "Run skill check"
      chooseOnlyOption "Apply results"

      assert $ Events.blindingLight `isInDiscardOf` self
      enemy.damage `shouldReturn` 1

    it
      "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> gameTest $ \self -> do
        withProp @"willpower" 5 self
        enemy <- testEnemy & prop @"evade" 4 & prop @"health" 2
        location <- testLocation
        setChaosTokens [token]
        enemy `spawnAt` location
        self `moveTo` location
        self `playEvent` Events.blindingLight
        chooseOnlyOption "Evade enemy"
        chooseOnlyOption "Run skill check"
        chooseOnlyOption "Apply results"

        assert $ Events.blindingLight `isInDiscardOf` self
        self.remainingActions `shouldReturn` 2
