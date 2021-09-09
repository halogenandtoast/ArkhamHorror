module Arkham.Types.Scenario.Scenarios.ReturnToTheGathering where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Acts
import qualified Arkham.Agenda.Cards as Agendas
import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Scenario.Scenarios.TheGathering
import Arkham.Types.Trait (Trait)

newtype ReturnToTheGathering = ReturnToTheGathering TheGathering
  deriving stock Generic
  deriving anyclass (IsScenario, HasRecord)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToTheGathering :: Difficulty -> ReturnToTheGathering
returnToTheGathering difficulty =
  ReturnToTheGathering
    . TheGathering
    $ baseAttrs
        "50011"
        "Return To The Gathering"
        [ Agendas.whatsGoingOn
        , Agendas.riseOfTheGhouls
        , Agendas.theyreGettingOut
        ]
        [Acts.mysteriousGateway, Acts.theBarrier, Acts.whatHaveYouDone]
        difficulty
    & locationLayoutL
    ?~ [ ".     .         fieldOfGraves .     "
       , ".     bedroom   attic         .     "
       , "study guestHall hallway       parlor"
       , ".     bathroom  cellar        .     "
       , ".     .         ghoulPits     .     "
       ]

instance (HasTokenValue env InvestigatorId, HasCount EnemyCount env (InvestigatorLocation, [Trait])) => HasTokenValue env ReturnToTheGathering where
  getTokenValue (ReturnToTheGathering theGathering') iid =
    getTokenValue theGathering' iid

instance ScenarioRunner env => RunMessage env ReturnToTheGathering where
  runMessage msg (ReturnToTheGathering theGathering'@(TheGathering attrs)) =
    case msg of
      Setup -> do
        investigatorIds <- getInvestigatorIds

        encounterDeck <- buildEncounterDeckExcluding
          [Enemies.ghoulPriest]
          [ EncounterSet.ReturnToTheGathering
          , EncounterSet.TheGathering
          , EncounterSet.Rats
          , EncounterSet.GhoulsOfUmordhoth
          , EncounterSet.StrikingFear
          , EncounterSet.AncientEvils
          , EncounterSet.ChillingCold
          ]

        studyAberrantGateway <- genCard Locations.studyAberrantGateway
        let studyId = toLocationId studyAberrantGateway

        guestHall <- genCard Locations.guestHall
        bedroom <- genCard Locations.bedroom
        bathroom <- genCard Locations.bathroom

        pushAllEnd
          [ SetEncounterDeck encounterDeck
          , AddAgenda "01105"
          , AddAct "50012"
          , PlaceLocation studyAberrantGateway
          , PlaceLocation guestHall
          , PlaceLocation bedroom
          , PlaceLocation bathroom
          , RevealLocation Nothing studyId
          , MoveAllTo (toSource attrs) studyId
          , story investigatorIds theGatheringIntro
          ]

        attic <- sample $ Locations.returnToAttic :| [Locations.attic]
        cellar <- sample $ Locations.returnToCellar :| [Locations.cellar]

        setAsideCards <- traverse
          genCard
          [ Enemies.ghoulPriest
          , Assets.litaChantler
          , attic
          , cellar
          , Locations.deepBelowYourHouse
          , Locations.farAboveYourHouse
          ]

        ReturnToTheGathering . TheGathering <$> runMessage
          msg
          (attrs & setAsideCardsL .~ setAsideCards)
      _ -> ReturnToTheGathering <$> runMessage msg theGathering'
