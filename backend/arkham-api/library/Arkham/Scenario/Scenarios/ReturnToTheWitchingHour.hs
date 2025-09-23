module Arkham.Scenario.Scenarios.ReturnToTheWitchingHour (returnToTheWitchingHour) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheWitchingHour
import Arkham.Scenarios.TheWitchingHour.Helpers

newtype ReturnToTheWitchingHour
  = ReturnToTheWitchingHour TheWitchingHour
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheWitchingHour :: Difficulty -> ReturnToTheWitchingHour
returnToTheWitchingHour difficulty =
  scenarioWith
    (ReturnToTheWitchingHour . TheWitchingHour)
    "54017"
    "Return to The Witching Hour"
    difficulty
    [ ".      .     woods1        .      . "
    , ".      .     .             .      . "
    , "woods2 .     witchesCircle .      woods3"
    , ".      .     .             .      ."
    , ".      wood4 .             woods5 ."
    ] -- lost and separated, do we label 4 zones, or do a different placement
    (referenceL .~ "05050")

instance RunMessage ReturnToTheWitchingHour where
  runMessage msg ( ReturnToTheWitchingHour
                     theWitchingHour'@(TheWitchingHour attrs)
                   ) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTheWitchingHour . TheWitchingHour)
        attrs
        (setIsReturnTo >> setupTheWitchingHour attrs)
    _ ->
      ReturnToTheWitchingHour <$> liftRunMessage msg theWitchingHour'
