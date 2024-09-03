module Arkham.Scenario.Scenarios.ReturnToTheGathering where

import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Prelude
import Arkham.Scenario.Runner hiding (placeLocationCard, story)
import Arkham.Scenario.Scenarios.TheGathering
import Arkham.Scenario.Setup

newtype ReturnToTheGathering = ReturnToTheGathering TheGathering
  deriving stock (Generic)
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
  runMessage msg s@(ReturnToTheGathering theGathering'@(TheGathering attrs)) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "nightOfTheZealot.theGathering.intro"
      pure s
    Setup -> runScenarioSetup (ReturnToTheGathering . TheGathering) attrs do
      gather EncounterSet.ReturnToTheGathering
      gather EncounterSet.TheGathering
      gather EncounterSet.Rats
      gather EncounterSet.GhoulsOfUmordhoth
      gather EncounterSet.StrikingFear
      gather EncounterSet.AncientEvils
      gather EncounterSet.ChillingCold

      study <- place Locations.studyAberrantGateway
      startAt study

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
    _ -> ReturnToTheGathering <$> lift (runMessage msg theGathering')
