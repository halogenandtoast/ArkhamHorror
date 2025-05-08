module Arkham.Scenario.Scenarios.ReturnToBlackStarsRise (returnToBlackStarsRise) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.BlackStarsRise
import Arkham.Scenarios.BlackStarsRise.Helpers

newtype ReturnToBlackStarsRise = ReturnToBlackStarsRise BlackStarsRise
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToBlackStarsRise :: Difficulty -> ReturnToBlackStarsRise
returnToBlackStarsRise difficulty =
  scenarioWith
    (ReturnToBlackStarsRise . BlackStarsRise)
    "52054"
    "Return to Black Stars Rise"
    difficulty
    [ ".                cloister      .           northTower      ."
    , "knightsHall      cloister      .           northTower      ."
    , "knightsHall      abbeyChurch    brokenSteps .               outerWall"
    , "chapelOfStAubert abbeyChurch    brokenSteps .               outerWall"
    , "chapelOfStAubert chœurGothique  .           grandRue        ."
    , ".                chœurGothique  .           grandRue        ."
    , ".                abbeyTower     .           porteDeLAvancée ."
    , ".                abbeyTower     .           porteDeLAvancée ."
    ]
    $ (decksLayoutL .~ ["act1 agenda1 agenda2 act2"])
    . (referenceL .~ "03274")

instance RunMessage ReturnToBlackStarsRise where
  runMessage msg (ReturnToBlackStarsRise blackStarsRise'@(BlackStarsRise attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToBlackStarsRise . BlackStarsRise)
        attrs
        (setIsReturnTo >> setupBlackStarsRise attrs)
    _ -> ReturnToBlackStarsRise <$> liftRunMessage msg blackStarsRise'
