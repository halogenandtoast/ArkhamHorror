module Arkham.Scenario.Scenarios.ReturnToTheUnspeakableOath (returnToTheUnspeakableOath) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheUnspeakableOath
import Arkham.Scenarios.TheUnspeakableOath.Helpers

newtype ReturnToTheUnspeakableOath = ReturnToTheUnspeakableOath TheUnspeakableOath
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheUnspeakableOath :: Difficulty -> ReturnToTheUnspeakableOath
returnToTheUnspeakableOath difficulty =
  scenarioWith
    (ReturnToTheUnspeakableOath . TheUnspeakableOath)
    "52034"
    "Return to The Unspeakable Oath"
    difficulty
    [ ".       .       .        .        garden                        garden                        .                             .                             .                   ."
    , ".       .       .        .        yard                          yard                          .                             .                             .                   ."
    , "kitchen kitchen messHall messHall asylumHallsWesternPatientWing asylumHallsWesternPatientWing asylumHallsEasternPatientWing asylumHallsEasternPatientWing infirmary           infirmary"
    , ".       .       .        .        patientConfinement1           patientConfinement1           basementHall                  basementHall                  patientConfinement2 patientConfinement2"
    , ".       .       .        .        .                             patientConfinement3           patientConfinement3           patientConfinement4           patientConfinement4 ."
    ]
    (referenceL .~ "03159")

instance RunMessage ReturnToTheUnspeakableOath where
  runMessage msg (ReturnToTheUnspeakableOath theUnspeakableOath'@(TheUnspeakableOath attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup (ReturnToTheUnspeakableOath . TheUnspeakableOath) attrs
        $ setIsReturnTo
        >> setupTheUnspeakableOath attrs
    _ -> ReturnToTheUnspeakableOath <$> liftRunMessage msg theUnspeakableOath'
