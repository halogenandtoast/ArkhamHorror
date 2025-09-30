module Arkham.Scenario.Scenarios.ReturnToInTheClutchesOfChaos (returnToInTheClutchesOfChaos) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.InTheClutchesOfChaos
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers

newtype ReturnToInTheClutchesOfChaos
  = ReturnToInTheClutchesOfChaos InTheClutchesOfChaos
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToInTheClutchesOfChaos :: Difficulty -> ReturnToInTheClutchesOfChaos
returnToInTheClutchesOfChaos difficulty =
  scenarioWith
    (ReturnToInTheClutchesOfChaos . InTheClutchesOfChaos)
    "54049"
    "Return to The Witching Hour"
    difficulty
    [ ".            .            .      merchantDistrict merchantDistrict rivertown   rivertown  .          .                   ."
    , "hangmansHill hangmansHill uptown uptown           southside        southside   frenchHill frenchHill silverTwilightLodge silverTwilightLodge"
    , ".            .            .      .                southChurch      southChurch .          .          .                   ."
    ]
    (referenceL .~ "05284")

instance RunMessage ReturnToInTheClutchesOfChaos where
  runMessage msg (ReturnToInTheClutchesOfChaos inTheClutchesOfChaos'@(InTheClutchesOfChaos attrs)) =
    runQueueT $ scenarioI18n $ case msg of
      Setup ->
        runScenarioSetup
          (ReturnToInTheClutchesOfChaos . InTheClutchesOfChaos)
          attrs
          (setIsReturnTo >> setupInTheClutchesOfChaos attrs)
      _ -> ReturnToInTheClutchesOfChaos <$> liftRunMessage msg inTheClutchesOfChaos'
