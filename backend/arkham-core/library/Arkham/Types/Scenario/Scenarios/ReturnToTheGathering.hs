module Arkham.Types.Scenario.Scenarios.ReturnToTheGathering where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Locations
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Scenario.Scenarios.TheGathering
import Arkham.Types.Trait (Trait)

newtype ReturnToTheGathering = ReturnToTheGathering TheGathering
  deriving stock Generic
  deriving anyclass HasRecord
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToTheGathering :: Difficulty -> ReturnToTheGathering
returnToTheGathering difficulty = ReturnToTheGathering . TheGathering $ base
  { scenarioLocationLayout = Just
    [ ".     .         fieldOfGraves .     "
    , ".     bedroom   attic         .     "
    , "study guestHall holeInTheWall parlor"
    , ".     bathroom  cellar        .     "
    , ".     .         ghoulPits     .     "
    ]
  }
 where
  base = baseAttrs
    "50011"
    "Return To The Gathering"
    ["01105", "01106", "01107"]
    ["50012", "01109", "01110"]
    difficulty

instance (HasTokenValue env InvestigatorId, HasCount EnemyCount env (InvestigatorLocation, [Trait])) => HasTokenValue env ReturnToTheGathering where
  getTokenValue (ReturnToTheGathering theGathering') iid =
    getTokenValue theGathering' iid

instance (HasId (Maybe LocationId) env LocationMatcher, ScenarioRunner env) => RunMessage env ReturnToTheGathering where
  runMessage msg (ReturnToTheGathering theGathering'@(TheGathering attrs)) =
    case msg of
      Setup -> do
        investigatorIds <- getInvestigatorIds
        encounterDeck <- buildEncounterDeck
          [ EncounterSet.ReturnToTheGathering
          , EncounterSet.TheGathering
          , EncounterSet.Rats
          , EncounterSet.GhoulsOfUmordhoth
          , EncounterSet.StrikingFear
          , EncounterSet.AncientEvils
          , EncounterSet.ChillingCold
          ]
        studyId <- getRandom
        guestHallId <- getRandom
        bedroomId <- getRandom
        bathroomId <- getRandom
        pushAllEnd
          [ SetEncounterDeck encounterDeck
          , AddAgenda "01105"
          , AddAct "50012"
          , PlaceLocation studyId Locations.studyAberrantGateway
          , PlaceLocation guestHallId Locations.guestHall
          , PlaceLocation bedroomId Locations.bedroom
          , PlaceLocation bathroomId Locations.bathroom
          , RevealLocation Nothing studyId
          , MoveAllTo studyId
          , AskMap
          . mapFromList
          $ [ (iid, ChooseOne [Run [Continue "Continue", theGatheringIntro]])
            | iid <- investigatorIds
            ]
          ]
        attic <- sample $ Locations.returnToAttic :| [Locations.attic]
        cellar <- sample $ Locations.returnToCellar :| [Locations.cellar]
        let
          locations' = locationNameMap
            [ Locations.studyAberrantGateway
            , Locations.guestHall
            , Locations.bedroom
            , Locations.bathroom
            , Locations.holeInTheWall
            , attic
            , Locations.farAboveYourHouse
            , cellar
            , Locations.deepBelowYourHouse
            , Locations.parlor
            ]
        ReturnToTheGathering . TheGathering <$> runMessage
          msg
          (attrs & locationsL .~ locations')
      _ -> ReturnToTheGathering <$> runMessage msg theGathering'
