module Arkham.Scenario.Scenarios.ReturnToBeforeTheBlackThrone (returnToBeforeTheBlackThrone) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.BeforeTheBlackThrone
import Arkham.Scenarios.BeforeTheBlackThrone.Helpers

newtype ReturnToBeforeTheBlackThrone
  = ReturnToBeforeTheBlackThrone BeforeTheBlackThrone
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToBeforeTheBlackThrone :: Difficulty -> ReturnToBeforeTheBlackThrone
returnToBeforeTheBlackThrone difficulty =
  scenarioWith
    (ReturnToBeforeTheBlackThrone . BeforeTheBlackThrone)
    "54056"
    "Return to Before the Black Throne"
    difficulty
    []
    (referenceL .~ "05325")

instance RunMessage ReturnToBeforeTheBlackThrone where
  runMessage msg (ReturnToBeforeTheBlackThrone beforeTheBlackThrone'@(BeforeTheBlackThrone attrs)) =
    runQueueT $ scenarioI18n $ case msg of
      Setup ->
        runScenarioSetup
          (ReturnToBeforeTheBlackThrone . BeforeTheBlackThrone)
          attrs
          (setIsReturnTo >> setupBeforeTheBlackThrone attrs)
      _ -> ReturnToBeforeTheBlackThrone <$> liftRunMessage msg beforeTheBlackThrone'
