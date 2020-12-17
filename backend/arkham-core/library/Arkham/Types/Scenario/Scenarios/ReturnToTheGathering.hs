{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.ReturnToTheGathering where

import Arkham.Import

import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Scenario.Scenarios.TheGathering
import Arkham.Types.Trait (Trait)
import Data.List.NonEmpty (NonEmpty(..))

newtype ReturnToTheGathering = ReturnToTheGathering TheGathering
  deriving newtype (Show, ToJSON, FromJSON)

returnToTheGathering :: Difficulty -> ReturnToTheGathering
returnToTheGathering difficulty =
  ReturnToTheGathering . TheGathering $ (baseAttrs
                                          "50011"
                                          "Return To The Gathering"
                                          ["01105", "01106", "01107"]
                                          ["50012", "01109", "01110"]
                                          difficulty
                                        )
    { scenarioLocationLayout = Just
      [ ".     .         fieldOfGraves .     "
      , ".     bedroom   attic         .     "
      , "study guestHall hallway       parlor"
      , ".     bathroom  cellar        .     "
      , ".     .         ghoulPits     .     "
      ]
    }

instance (HasTokenValue env InvestigatorId, HasCount EnemyCount env (InvestigatorLocation, [Trait])) => HasTokenValue env ReturnToTheGathering where
  getTokenValue (ReturnToTheGathering theGathering') iid =
    getTokenValue theGathering' iid

instance ScenarioRunner env => RunMessage env ReturnToTheGathering where
  runMessage msg (ReturnToTheGathering theGathering'@(TheGathering attrs@Attrs {..}))
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
        attic <- liftIO $ sample ("50018" :| ["01113"])
        cellar <- liftIO $ sample ("50020" :| ["01114"])
        let
          locations' = mapFromList
            [ ("Study", ["50013"])
            , ("Guest Hall", ["50014"])
            , ("Bedroom", ["50015"])
            , ("Bathroom", ["50016"])
            , ("Hallway", ["50017"])
            , ("Attic", [attic])
            , ("Field of Graves", ["50019"])
            , ("Cellar", [cellar])
            , ("Ghoul Pits", ["50021"])
            , ("Parlor", ["01115"])
            ]
        ReturnToTheGathering . TheGathering <$> runMessage
          msg
          (attrs & locationsL .~ locations')
      _ -> ReturnToTheGathering <$> runMessage msg theGathering'
