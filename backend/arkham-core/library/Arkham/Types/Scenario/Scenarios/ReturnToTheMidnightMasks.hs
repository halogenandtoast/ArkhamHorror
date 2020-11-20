{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Scenario.Scenarios.ReturnToTheMidnightMasks where

import Arkham.Import hiding (Cultist)

import Arkham.Types.CampaignLogKey
import Arkham.Types.Difficulty
import Arkham.Types.EncounterSet (gatherEncounterSet)
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Scenario.Scenarios.TheMidnightMasks
import Arkham.Types.Trait (Trait)
import Data.List.NonEmpty (NonEmpty(..))
import Data.UUID.V4
import System.Random.Shuffle

newtype ReturnToTheMidnightMasks = ReturnToTheMidnightMasks TheMidnightMasks
  deriving newtype (Show, ToJSON, FromJSON)

theMidnightMasks :: Difficulty -> ReturnToTheMidnightMasks
theMidnightMasks difficulty =
  ReturnToTheMidnightMasks
    . TheMidnightMasks
    $ (baseAttrs "50025" "Return to the Midnight Masks" [] [] difficulty)
        { scenarioLocationLayout = Just
          [ "northside downtown easttown"
          , "miskatonicUniversity rivertown graveyard"
          , "stMarysHospital southside yourHouse"
          ]
        , scenarioDeck = Just []
        }

instance (HasTokenValue env InvestigatorId, HasCount env DoomCount (), HasCount env DoomCount EnemyId, HasSet EnemyId env Trait) => HasTokenValue env ReturnToTheMidnightMasks where
  getTokenValue (ReturnToTheMidnightMasks theMidnightMasks') iid =
    getTokenValue theMidnightMasks' iid

instance (ScenarioRunner env) => RunMessage env ReturnToTheMidnightMasks where
  runMessage msg (ReturnToTheMidnightMasks theMidnightMasks'@(TheMidnightMasks attrs@Attrs {..}))
    = case msg of
      Setup -> do
        count' <- getPlayerCount
        investigatorIds <- getInvestigatorIds
        (acolytes, theDevourersCult) <- splitAt (count' - 1)
          <$> gatherEncounterSet EncounterSet.TheDevourersCult
        -- ^ we will spawn these disciples of the devourer

        southside <- liftIO $ sample $ "01126" :| ["01127"]
        downtown <- liftIO $ sample $ "01130" :| ["01131"]
        easttown <- liftIO $ sample $ "01132" :| ["50027"]
        northside <- liftIO $ sample $ "01134" :| ["50028"]
        rivertown <- liftIO $ sample $ "01125" :| ["50030"]
        miskatonicUniversity <- liftIO $ sample $ "01129" :| ["50029"]

        predatorOrPrey <- liftIO $ sample $ "01121" :| ["50026"]
        houseBurnedDown <- asks $ hasRecord YourHouseHasBurnedToTheGround
        ghoulPriestAlive <- asks $ hasRecord GhoulPriestIsStillAlive
        litaForcedToFindOthersToHelpHerCause <- asks
          $ hasRecord LitaWasForcedToFindOthersToHelpHerCause
        ghoulPriestCard <-
          liftIO $ lookupEncounterCard "01116" . CardId <$> nextRandom
        cultistCards <-
          liftIO
          $ for
              [ "01137"
              , "01138"
              , "01139"
              , "01140"
              , "01141"
              , "50044"
              , "50045"
              , "50046"
              ]
          $ \cardCode -> lookupEncounterCard cardCode . CardId <$> nextRandom
        cultistDeck' <- liftIO $ drop 3 <$> shuffleM cultistCards
        let
          startingLocationMessages = if houseBurnedDown
            then [RevealLocation Nothing rivertown, MoveAllTo rivertown]
            else
              [ PlaceLocation "01124"
              , RevealLocation Nothing "01124"
              , MoveAllTo "01124"
              ]
          ghoulPriestMessages =
            [ AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive ]
          spawnAcolyteMessages =
            [ CreateEnemyAt (getCardCode c) l
            | (c, l) <- zip acolytes [southside, downtown, "01133"]
            ]
        encounterDeck <- buildEncounterDeckWith
          (<> theDevourersCult)
          [ EncounterSet.ReturnToTheMidnightMasks
          , EncounterSet.TheMidnightMasks
          , EncounterSet.ChillingCold
          , EncounterSet.Nightgaunts
          , EncounterSet.LockedDoors
          ]
        let
          intro1or2 = if litaForcedToFindOthersToHelpHerCause
            then TheMidnightMasksIntroOne
            else TheMidnightMasksIntroTwo
        pushMessages
          $ [ story investigatorIds (introPart1 intro1or2)
            , story investigatorIds introPart2
            , SetEncounterDeck encounterDeck
            , AddAgenda predatorOrPrey
            , AddAct "01123"
            , PlaceLocation rivertown
            , PlaceLocation southside
            , PlaceLocation "01128"
            , PlaceLocation miskatonicUniversity
            , PlaceLocation downtown
            , PlaceLocation easttown
            , PlaceLocation "01133"
            , PlaceLocation northside
            ]
          <> startingLocationMessages
          <> ghoulPriestMessages
          <> spawnAcolyteMessages
        let
          locations' = mapFromList
            [ ("Your House", ["01124"])
            , ("Rivertown", [rivertown])
            , ("Southside", [southside])
            , ("St. Mary's Hospital", ["01128"])
            , ("Miskatonic University", [miskatonicUniversity])
            , ("Downtown", [downtown])
            , ("Easttown", [easttown])
            , ("Graveyard", ["01133"])
            , ("Northside", [northside])
            ]
        ReturnToTheMidnightMasks . TheMidnightMasks <$> runMessage
          msg
          (attrs
            { scenarioDeck = Just cultistDeck'
            , scenarioLocations = locations'
            }
          )
      _ -> ReturnToTheMidnightMasks <$> runMessage msg theMidnightMasks'
