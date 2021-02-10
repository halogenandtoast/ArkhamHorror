module Arkham.Types.Scenario.Scenarios.ReturnToTheGathering where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Scenario.Scenarios.TheGathering
import Arkham.Types.Trait (Trait)
import Data.List.NonEmpty (NonEmpty(..))

newtype ReturnToTheGathering = ReturnToTheGathering TheGathering
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToTheGathering :: Difficulty -> ReturnToTheGathering
returnToTheGathering difficulty = ReturnToTheGathering . TheGathering $ base
  { scenarioLocationLayout = Just
    [ ".     .         fieldOfGraves .     "
    , ".     bedroom   attic         .     "
    , "study guestHall hallway       parlor"
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

instance ScenarioRunner env => RunMessage env ReturnToTheGathering where
  runMessage msg (ReturnToTheGathering theGathering'@(TheGathering attrs@ScenarioAttrs {..}))
    = case msg of
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
        pushMessages
          [ SetEncounterDeck encounterDeck
          , AddAgenda "01105"
          , AddAct "50012"
          , PlaceLocation "50013"
          , PlaceLocation "50014"
          , PlaceLocation "50015"
          , PlaceLocation "50016"
          , RevealLocation Nothing "50013"
          , MoveAllTo "50013"
          , AskMap
          . mapFromList
          $ [ (iid, ChooseOne [Run [Continue "Continue", theGatheringIntro]])
            | iid <- investigatorIds
            ]
          ]
        attic <- sample $ "50018" :| ["01113"]
        cellar <- sample $ "50020" :| ["01114"]
        let
          locations' = mapFromList $ map
            (second pure . toFst (getLocationName . lookupLocation))
            [ "50013"
            , "50014"
            , "50015"
            , "50016"
            , "50017"
            , attic
            , "50019"
            , cellar
            , "50021"
            , "01115"
            ]
        ReturnToTheGathering . TheGathering <$> runMessage
          msg
          (attrs & locationsL .~ locations')
      _ -> ReturnToTheGathering <$> runMessage msg theGathering'
