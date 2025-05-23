module Arkham.Scenario.Scenarios.ReturnToTheGathering where

import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheGathering
import Arkham.Scenarios.TheGathering.Helpers

newtype ReturnToTheGathering = ReturnToTheGathering TheGathering
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasChaosTokenValue)

returnToTheGathering :: Difficulty -> ReturnToTheGathering
returnToTheGathering difficulty =
  scenarioWith
    (ReturnToTheGathering . TheGathering)
    "50011"
    "Return to The Gathering"
    difficulty
    [ ".     .         farAboveYourHouse  ."
    , ".     bedroom   attic              ."
    , "study guestHall holeInTheWall      parlor"
    , ".     bathroom  cellar             ."
    , ".     .         deepBelowYourHouse ."
    ]
    (referenceL .~ "01104")

instance RunMessage ReturnToTheGathering where
  runMessage msg (ReturnToTheGathering theGathering'@(TheGathering attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup ->
      runScenarioSetup (ReturnToTheGathering . TheGathering) attrs do
        setIsReturnTo
        setupTheGathering attrs
    _ -> ReturnToTheGathering <$> liftRunMessage msg theGathering'
