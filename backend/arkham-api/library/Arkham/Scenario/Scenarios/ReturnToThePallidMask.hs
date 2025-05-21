module Arkham.Scenario.Scenarios.ReturnToThePallidMask (returnToThePallidMask) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.ThePallidMask
import Arkham.Scenarios.ThePallidMask.Helpers

newtype ReturnToThePallidMask = ReturnToThePallidMask ThePallidMask
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToThePallidMask :: Difficulty -> ReturnToThePallidMask
returnToThePallidMask difficulty =
  scenarioWith
    (ReturnToThePallidMask . ThePallidMask)
    "52048"
    "Return to The Pallid Mask"
    difficulty
    []
    (referenceL .~ "03240")

instance RunMessage ReturnToThePallidMask where
  runMessage msg (ReturnToThePallidMask thePallidMask'@(ThePallidMask attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup (ReturnToThePallidMask . ThePallidMask) attrs
        $ setIsReturnTo
        >> setupThePallidMask attrs
    _ -> ReturnToThePallidMask <$> liftRunMessage msg thePallidMask'
