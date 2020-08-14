{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Json
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card.Class
import Arkham.Types.Card.EncounterCard (lookupEncounterCard)
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet (gatherEncounterSet)
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Helpers
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Runner
import ClassyPrelude
import Data.List.NonEmpty (NonEmpty(..))
import Data.UUID.V4
import System.Random.Shuffle

newtype TheMidnightMasks = TheMidnightMasks Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theMidnightMasks :: Difficulty -> TheMidnightMasks
theMidnightMasks difficulty =
  TheMidnightMasks $ (baseAttrs "01120" "The Midnight Masks" [] [] difficulty)
     { scenarioLocationLayout = Just [ "northside downtown easttown", "miskatonicUniversity rivertown graveyard", "stMarysHospital southside yourHouse"] }

instance (ScenarioRunner env) => RunMessage env TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs) = case msg of
    Setup -> do
      count <- unPlayerCount <$> asks (getCount ())
      (acolytes, darkCult) <- splitAt (count - 1)
        <$> gatherEncounterSet EncounterSet.DarkCult
      -- ^ we will spawn these acolytes
      southside <- liftIO $ sample $ "01126" :| ["01127"]
      downtown <- liftIO $ sample $ "01130" :| ["01131"]
      houseBurnedDown <- asks $ hasRecord YourHouseHasBurnedToTheGround
      ghoulPriestAlive <- asks $ hasRecord GhoulPriestIsStillAlive
      ghoulPriestCard <-
        liftIO $ lookupEncounterCard "01116" . CardId <$> nextRandom
      let
        startingLocationMessages = if houseBurnedDown
          then [RevealLocation "01125", MoveAllTo "01125"]
          else
            [PlaceLocation "01124", RevealLocation "01124", MoveAllTo "01124"]
        ghoulPriestMessages =
          if ghoulPriestAlive then [AddToEncounterDeck ghoulPriestCard] else []
        spawnAcolyteMessages =
          [ CreateEnemyAt (getCardCode c) l
          | (c, l) <- zip acolytes [southside, downtown, "01133"]
          ]
      encounterDeck <- liftIO $ shuffleM . (<> darkCult) . concat =<< traverse
        gatherEncounterSet
        [ EncounterSet.TheMidnightMasks
        , EncounterSet.ChillingCold
        , EncounterSet.Nightgaunts
        , EncounterSet.DarkCult
        , EncounterSet.LockedDoors
        ]
      s <$ pushMessages
        ([ SetEncounterDeck encounterDeck
         , AddAgenda "01121"
         , AddAct "01123"
         , PlaceLocation "01125"
         , PlaceLocation southside
         , PlaceLocation "01128"
         , PlaceLocation "01129"
         , PlaceLocation downtown
         , PlaceLocation "01132"
         , PlaceLocation "01133"
         , PlaceLocation "01134"
         ]
        <> startingLocationMessages
        <> ghoulPriestMessages
        <> spawnAcolyteMessages
        )
    _ -> TheMidnightMasks <$> runMessage msg attrs
