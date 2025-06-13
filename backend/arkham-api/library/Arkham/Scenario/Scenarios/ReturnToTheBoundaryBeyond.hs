module Arkham.Scenario.Scenarios.ReturnToTheBoundaryBeyond (returnToTheBoundaryBeyond) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheBoundaryBeyond
import Arkham.Scenarios.TheBoundaryBeyond.Helpers

newtype ReturnToTheBoundaryBeyond = ReturnToTheBoundaryBeyond TheBoundaryBeyond
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheBoundaryBeyond :: Difficulty -> ReturnToTheBoundaryBeyond
returnToTheBoundaryBeyond difficulty =
  scenarioWith
    (ReturnToTheBoundaryBeyond . TheBoundaryBeyond)
    "53038"
    "Return to The Boundary Beyond"
    difficulty
    [ ".        .        .    circle  circle   .      .      ."
    , "triangle triangle star star    diamond diamond square square"
    , ".        .        .    heart   heart   .       .      ."
    ]
    (referenceL .~ "04161")

instance RunMessage ReturnToTheBoundaryBeyond where
  runMessage msg (ReturnToTheBoundaryBeyond theBoundaryBeyond'@(TheBoundaryBeyond attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTheBoundaryBeyond . TheBoundaryBeyond)
        attrs
        (setIsReturnTo >> setupTheBoundaryBeyond attrs)
    _ -> ReturnToTheBoundaryBeyond <$> liftRunMessage msg theBoundaryBeyond'
