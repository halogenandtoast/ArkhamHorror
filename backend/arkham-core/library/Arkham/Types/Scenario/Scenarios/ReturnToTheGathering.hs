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

newtype ReturnToTheGathering = ReturnToTheGathering TheGathering
  deriving newtype (Show, ToJSON, FromJSON)

returnToTheGathering :: Difficulty -> ReturnToTheGathering
returnToTheGathering = ReturnToTheGathering . TheGathering . baseAttrs
  "50011"
  "Return To The Gathering"
  ["01105", "01106", "01107"]
  ["50012", "01109", "01110"]

instance (HasTokenValue env InvestigatorId, HasCount EnemyCount (InvestigatorLocation, [Trait]) env, HasQueue env) => HasTokenValue env ReturnToTheGathering where
  getTokenValue (ReturnToTheGathering theGathering') iid =
    getTokenValue theGathering' iid

instance ScenarioRunner env => RunMessage env ReturnToTheGathering where
  runMessage msg (ReturnToTheGathering theGathering'@(TheGathering attrs@Attrs {..}))
    = case msg of
      Setup -> do
        investigatorIds <- getInvestigatorIds
        encounterDeck <- buildEncounterDeck
          [ EncounterSet.ReturnToTheGathering
          , EncounterSet.Rats
          , EncounterSet.GhoulsOfUmordhoth
          , EncounterSet.StrikingFear
          , EncounterSet.AncientEvils
          , EncounterSet.ChillingCold
          ]
        pushMessages
          [ SetEncounterDeck encounterDeck
          , AddAgenda "01105"
          , AddAct "01108"
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
        ReturnToTheGathering . TheGathering <$> runMessage msg attrs
      _ -> ReturnToTheGathering <$> runMessage msg theGathering'
