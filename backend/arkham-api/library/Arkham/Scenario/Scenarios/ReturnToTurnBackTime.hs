module Arkham.Scenario.Scenarios.ReturnToTurnBackTime (returnToTurnBackTime) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TurnBackTime
import Arkham.Scenarios.TurnBackTime.Helpers

newtype ReturnToTurnBackTime = ReturnToTurnBackTime TurnBackTime
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTurnBackTime :: Difficulty -> ReturnToTurnBackTime
returnToTurnBackTime difficulty =
  scenarioWith
    (ReturnToTurnBackTime . TurnBackTime)
    "53066"
    "Return to Shattered Aeons"
    difficulty
    [ ".      square  heart t        .         .    .    .    ."
    , ".      square  heart t        .         .    .    .    ."
    , "circle diamond .     .        hourglass .    .    .    ."
    , "circle diamond .     .        hourglass .    .    .    ."
    , ".      star    plus  triangle .         .    .    .    ."
    , ".      star    plus  triangle .         .    .    .    ."
    , "pos1   pos2    pos3  pos4     pos5      pos6 pos7 pos8 pos9"
    ]
    (referenceL .~ "04344")

instance RunMessage ReturnToTurnBackTime where
  runMessage msg (ReturnToTurnBackTime turnBackTime'@(TurnBackTime attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToTurnBackTime . TurnBackTime)
        attrs
        (setIsReturnTo >> setupTurnBackTime attrs)
    _ -> ReturnToTurnBackTime <$> liftRunMessage msg turnBackTime'
