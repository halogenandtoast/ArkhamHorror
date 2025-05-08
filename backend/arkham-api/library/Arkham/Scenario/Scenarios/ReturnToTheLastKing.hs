module Arkham.Scenario.Scenarios.ReturnToTheLastKing (returnToTheLastKing) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheLastKing
import Arkham.Scenarios.TheLastKing.Helpers

newtype ReturnToTheLastKing = ReturnToTheLastKing TheLastKing
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheLastKing :: Difficulty -> ReturnToTheLastKing
returnToTheLastKing difficulty =
  scenarioWith
    (ReturnToTheLastKing . TheLastKing)
    "52021"
    "Return to The Last King"
    difficulty
    [ "diningRoom .         gallery"
    , "ballroom   courtyard livingRoom"
    , ".          foyer     ."
    ]
    (referenceL .~ "03061")

instance RunMessage ReturnToTheLastKing where
  runMessage msg (ReturnToTheLastKing theLastKing'@(TheLastKing attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup (ReturnToTheLastKing . TheLastKing) attrs (setIsReturnTo >> setupTheLastKing attrs)
    _ -> ReturnToTheLastKing <$> liftRunMessage msg theLastKing'
