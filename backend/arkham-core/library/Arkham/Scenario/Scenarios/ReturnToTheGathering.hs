module Arkham.Scenario.Scenarios.ReturnToTheGathering where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios.TheGathering
import Arkham.Scenarios.TheGathering.Story

newtype ReturnToTheGathering = ReturnToTheGathering TheGathering
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToTheGathering :: Difficulty -> ReturnToTheGathering
returnToTheGathering difficulty =
  scenarioWith
    (ReturnToTheGathering . TheGathering)
    "50011"
    "Return To The Gathering"
    difficulty
    [ ".     .         farAboveYourHouse  ."
    , ".     bedroom   attic              ."
    , "study guestHall holeInTheWall      parlor"
    , ".     bathroom  cellar             ."
    , ".     .         deepBelowYourHouse ."
    ]
    (referenceL .~ "01104")

instance HasChaosTokenValue ReturnToTheGathering where
  getChaosTokenValue iid chaosTokenFace (ReturnToTheGathering theGathering') =
    getChaosTokenValue iid chaosTokenFace theGathering'

instance RunMessage ReturnToTheGathering where
  runMessage msg (ReturnToTheGathering theGathering'@(TheGathering attrs)) =
    case msg of
      Setup -> do
        investigatorIds <- allInvestigatorIds

        encounterDeck <-
          buildEncounterDeckExcluding
            [Enemies.ghoulPriest]
            [ EncounterSet.ReturnToTheGathering
            , EncounterSet.TheGathering
            , EncounterSet.Rats
            , EncounterSet.GhoulsOfUmordhoth
            , EncounterSet.StrikingFear
            , EncounterSet.AncientEvils
            , EncounterSet.ChillingCold
            ]

        (studyId, placeStudy) <-
          placeLocationCard
            Locations.studyAberrantGateway
        placeRest <-
          traverse
            placeLocationCard_
            [Locations.guestHall, Locations.bedroom, Locations.bathroom]

        pushAll $
          [ SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeStudy
          ]
            <> placeRest
            <> [ RevealLocation Nothing studyId
               , MoveAllTo (toSource attrs) studyId
               , story investigatorIds theGatheringIntro
               ]

        attic <- sample $ Locations.returnToAttic :| [Locations.attic]
        cellar <- sample $ Locations.returnToCellar :| [Locations.cellar]

        setAsideCards <-
          genCards
            [ Enemies.ghoulPriest
            , Assets.litaChantler
            , attic
            , cellar
            , Locations.holeInTheWall
            , Locations.deepBelowYourHouse
            , Locations.farAboveYourHouse
            , Locations.parlor
            ]

        agendas <- genCards theGatheringAgendaDeck
        acts <-
          genCards
            [Acts.mysteriousGateway, Acts.theBarrier, Acts.whatHaveYouDone]

        ReturnToTheGathering . TheGathering
          <$> runMessage
            msg
            ( attrs
                & (setAsideCardsL .~ setAsideCards)
                & (actStackL . at 1 ?~ acts)
                & (agendaStackL . at 1 ?~ agendas)
            )
      _ -> ReturnToTheGathering <$> runMessage msg theGathering'
