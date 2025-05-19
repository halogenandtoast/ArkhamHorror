module Arkham.Scenario.Scenarios.ReturnToEchoesOfThePast (returnToEchoesOfThePast) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.EchoesOfThePast
import Arkham.Scenarios.EchoesOfThePast.Helpers

newtype ReturnToEchoesOfThePast = ReturnToEchoesOfThePast EchoesOfThePast
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToEchoesOfThePast :: Difficulty -> ReturnToEchoesOfThePast
returnToEchoesOfThePast difficulty =
  scenarioWith
    (ReturnToEchoesOfThePast . EchoesOfThePast)
    "52028"
    "Return to Echoes of the Past"
    difficulty
    [ "thirdFloor1  quietHalls2 thirdFloor2  . ."
    , "secondFloor1 quietHalls1 secondFloor2 . hiddenLibrary"
    , "groundFloor1 entryHall   groundFloor2 . ."
    , "basement1    quietHalls3 basement2    . ."
    ]
    (referenceL .~ "03120")

instance RunMessage ReturnToEchoesOfThePast where
  runMessage msg (ReturnToEchoesOfThePast echoesOfThePast'@(EchoesOfThePast attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToEchoesOfThePast . EchoesOfThePast)
        attrs
        (setIsReturnTo >> setupEchoesOfThePast attrs)
    _ -> ReturnToEchoesOfThePast <$> liftRunMessage msg echoesOfThePast'
