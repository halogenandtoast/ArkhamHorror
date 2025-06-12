module Arkham.Scenario.Scenarios.ReturnToThreadsOfFate (returnToThreadsOfFate) where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.ThreadsOfFate
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype ReturnToThreadsOfFate = ReturnToThreadsOfFate ThreadsOfFate
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToThreadsOfFate :: Difficulty -> ReturnToThreadsOfFate
returnToThreadsOfFate difficulty =
  scenarioWith
    (ReturnToThreadsOfFate . ThreadsOfFate)
    "53028"
    "Return to Threads of Fate"
    difficulty
    [ ".                .                trainTracks  trainTracks          townHall             townHall     arkhamPoliceStation arkhamPoliceStation .           ."
    , "curiositieShoppe curiositieShoppe northside    northside            downtown             downtown     easttown            easttown            velmasDiner velmasDiner"
    , ".                eztliExhibit     eztliExhibit miskatonicUniversity miskatonicUniversity rivertown    rivertown           blackCave           blackCave   ."
    , ".                .                .            .                    .                    loadingDocks loadingDocks        .                   .           ."
    ]
    (( decksLayoutL
        .~ [ ".       act1 act2"
           , "agenda1 act1 act2"
           , "agenda1 act3 act4"
           , ".       act3 act4"
           ]
    ) . (referenceL .~ "04113"))

instance RunMessage ReturnToThreadsOfFate where
  runMessage msg (ReturnToThreadsOfFate threadsOfFate'@(ThreadsOfFate attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup
        (ReturnToThreadsOfFate . ThreadsOfFate)
        attrs
        (setIsReturnTo >> setupThreadsOfFate attrs)
    _ -> ReturnToThreadsOfFate <$> liftRunMessage msg threadsOfFate'
