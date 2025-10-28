module Arkham.Scenario.Scenarios.ReturnToTheCityOfArchives (returnToTheCityOfArchives) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheCityOfArchives
import Arkham.Scenarios.TheCityOfArchives.Helpers

newtype ReturnToTheCityOfArchives = ReturnToTheCityOfArchives TheCityOfArchives
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheCityOfArchives :: Difficulty -> ReturnToTheCityOfArchives
returnToTheCityOfArchives difficulty =
  scenarioWith
    (ReturnToTheCityOfArchives . TheCityOfArchives)
    "53053"
    "Return to The City of Archives"
    difficulty
    [ ".                yithianOrrery                   laboratoryOfTheGreatRace         deconstructionRoom              ."
    , ".                .                               hallsOfPnakotusNorthernCorridors .                               interviewRoom1"
    , "towersOfPnakotus hallsOfPnakotusWesternCorridors alienConservatory                hallsOfPnakotusEasternCorridors interviewRoom2"
    , ".                greatLibrary                    hallsOfPnakotusSouthernCorridors cyclopeanVaults                  interviewRoom3"
    ]
    (referenceL .~ "04237")

instance RunMessage ReturnToTheCityOfArchives where
  runMessage msg (ReturnToTheCityOfArchives theCityOfArchives'@(TheCityOfArchives attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTheCityOfArchives . TheCityOfArchives)
        attrs
        (setIsReturnTo >> setupTheCityOfArchives attrs)
    _ -> ReturnToTheCityOfArchives <$> liftRunMessage msg theCityOfArchives'
