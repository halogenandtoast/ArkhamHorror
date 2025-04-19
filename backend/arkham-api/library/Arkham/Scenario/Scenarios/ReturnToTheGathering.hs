module Arkham.Scenario.Scenarios.ReturnToTheGathering where

import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
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
    Setup -> runScenarioSetup (ReturnToTheGathering . TheGathering) attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"

      gather EncounterSet.ReturnToTheGathering
      gather EncounterSet.TheGathering
      gather EncounterSet.Rats
      gather EncounterSet.GhoulsOfUmordhoth
      gather EncounterSet.StrikingFear
      gather EncounterSet.AncientEvils
      gather EncounterSet.ChillingCold

      startAt =<< place Locations.studyAberrantGateway

      placeAll [Locations.guestHall, Locations.bedroom, Locations.bathroom]

      attic <- sample2 Locations.returnToAttic Locations.attic
      cellar <- sample2 Locations.returnToCellar Locations.cellar

      setAside
        [ Enemies.ghoulPriest
        , Assets.litaChantler
        , attic
        , cellar
        , Locations.holeInTheWall
        , Locations.deepBelowYourHouse
        , Locations.farAboveYourHouse
        , Locations.parlor
        ]

      setAgendaDeck theGatheringAgendaDeck
      setActDeck [Acts.mysteriousGateway, Acts.theBarrier, Acts.whatHaveYouDone]
    _ -> ReturnToTheGathering <$> liftRunMessage msg theGathering'
