module Arkham.Event.Cards.BlindingLight2Spec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = do
  describe "Blinding Light (2)" $ do
    it "Uses willpower to evade an enemy" . gameTest $ \self -> do
      withProp @"willpower" 5 self
      withProp @"agility" 3 self
      enemy <- testEnemy & prop @"evade" 4 & prop @"health" 3
      location <- testLocation
      setChaosTokens [MinusOne]
      enemy `spawnAt` location
      self `moveTo` location
      self `playEvent` Events.blindingLight2
      chooseOnlyOption "Evade enemy"
      chooseOnlyOption "Run skill check"
      chooseOnlyOption "Apply results"
      assert $ Events.blindingLight2 `isInDiscardOf` self
      self.engagedEnemies `shouldReturn` []

    fit "deals 2 damage to the evaded enemy" . gameTest $ \self -> do
      withProp @"willpower" 5 self
      enemy <- testEnemy & prop @"evade" 4 & prop @"health" 3
      location <- testLocation
      setChaosTokens [MinusOne]
      enemy `spawnAt` location
      self `moveTo` location
      self `playEvent` Events.blindingLight2
      chooseOnlyOption "Evade enemy"
      chooseOnlyOption "Run skill check"
      chooseOnlyOption "Apply results"
      assert $ Events.blindingLight2 `isInDiscardOf` self
      enemy.damage `shouldReturn` 2

    for_ [Skull, Cultist, Tablet, ElderThing, AutoFail] \token -> do
      it
        ("On " <> show token <> " the investigator loses an action and takes 1 horror")
        . gameTest
        $ \self -> do
          withProp @"willpower" 5 self
          enemy <- testEnemy & prop @"evade" 4 & prop @"health" 3
          location <- testLocation
          setChaosTokens [token]
          enemy `spawnAt` location
          self `moveTo` location
          self `playEvent` Events.blindingLight2
          chooseOnlyOption "Evade enemy"
          chooseOnlyOption "Run skill check"
          chooseOnlyOption "Apply results"
          chooseOnlyOption "take event damage"
          when (token == Cultist) $ chooseOnlyOption "take scenario effect damage"
          assert $ Events.blindingLight2 `isInDiscardOf` self
          self.remainingActions `shouldReturn` 2
          self.horror `shouldReturn` (if token == Cultist then 2 else 1)
