module Arkham.Scenario.Scenarios.ReturnToCurtainCall (returnToCurtainCall) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.CurtainCall
import Arkham.Scenarios.CurtainCall.Helpers

newtype ReturnToCurtainCall = ReturnToCurtainCall CurtainCall
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToCurtainCall :: Difficulty -> ReturnToCurtainCall
returnToCurtainCall difficulty =
  scenarioWith
    (ReturnToCurtainCall . CurtainCall)
    "52014"
    "Return to Curtain Call"
    difficulty
    [ "lobbyDoorway1 .     balcony .         backstageDoorway1"
    , "lobbyDoorway3 lobby theatre backstage backstageDoorway3"
    , "lobbyDoorway2 .     .       .         backstageDoorway2"
    ]
    (referenceL .~ "03043")

instance RunMessage ReturnToCurtainCall where
  runMessage msg (ReturnToCurtainCall curtainCall'@(CurtainCall attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToCurtainCall . CurtainCall) attrs (setIsReturnTo >> performSetup attrs)
    _ -> ReturnToCurtainCall <$> liftRunMessage msg curtainCall'
