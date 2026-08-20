module Arkham.Scenario.Scenarios.ReturnToCurtainCall (returnToCurtainCall) where

import Arkham.Enemy.CardDefs.ThePathToCarcosa.CurtainCall qualified as Enemies
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.CurtainCall
import Arkham.Scenarios.CurtainCall.Helpers

newtype ReturnToCurtainCall = ReturnToCurtainCall CurtainCall
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

instance HasModifiersFor ReturnToCurtainCall where
  getModifiersFor (ReturnToCurtainCall (CurtainCall attrs)) = do
    modifySelect attrs (enemyIs Enemies.royalEmissary) [StayInVictory]
    modifySelect attrs (DefeatedEnemy $ enemyIs Enemies.royalEmissary) [StayInVictory]

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
    Setup ->
      runScenarioSetup (ReturnToCurtainCall . CurtainCall) attrs (setIsReturnTo >> setupCurtainCall attrs)
    _ -> ReturnToCurtainCall <$> liftRunMessage msg curtainCall'
