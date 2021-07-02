module Arkham.Types.Scenario.Scenarios.ReturnToTheMidnightMasks where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.EncounterSet (gatherEncounterSet)
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Scenario.Attrs
import Arkham.Types.Scenario.Helpers
import Arkham.Types.Scenario.Runner
import Arkham.Types.Scenario.Scenarios.TheMidnightMasks
import Arkham.Types.Trait (Trait)

newtype ReturnToTheMidnightMasks = ReturnToTheMidnightMasks TheMidnightMasks
  deriving stock Generic
  deriving anyclass HasRecord
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToTheMidnightMasks :: Difficulty -> ReturnToTheMidnightMasks
returnToTheMidnightMasks difficulty =
  ReturnToTheMidnightMasks
    . TheMidnightMasks
    $ (baseAttrs "50025" "Return to the Midnight Masks" [] [] difficulty)
        { scenarioLocationLayout = Just
          [ "northside downtown easttown"
          , "miskatonicUniversity rivertown graveyard"
          , "stMarysHospital southside yourHouse"
          ]
        , scenarioDeck = Just $ CultistDeck []
        }

instance (HasTokenValue env InvestigatorId, HasCount DoomCount env (), HasCount DoomCount env EnemyId, HasSet EnemyId env Trait) => HasTokenValue env ReturnToTheMidnightMasks where
  getTokenValue (ReturnToTheMidnightMasks theMidnightMasks') iid =
    getTokenValue theMidnightMasks' iid

instance (HasId (Maybe LocationId) env LocationMatcher, ScenarioRunner env) => RunMessage env ReturnToTheMidnightMasks where
  runMessage msg (ReturnToTheMidnightMasks theMidnightMasks'@(TheMidnightMasks attrs))
    = case msg of
      Setup -> do
        count' <- getPlayerCount
        investigatorIds <- getInvestigatorIds
        (acolytes, theDevourersCult) <- splitAt (count' - 1)
          <$> gatherEncounterSet EncounterSet.TheDevourersCult
        -- we will spawn these disciples of the devourer

        southside <- sample $ "01126" :| ["01127"]
        downtown <- sample $ "01130" :| ["01131"]
        easttown <- sample $ "01132" :| ["50027"]
        northside <- sample $ "01134" :| ["50028"]
        rivertown <- sample $ "01125" :| ["50030"]
        miskatonicUniversity <- sample $ "01129" :| ["50029"]
        predatorOrPrey <- sample $ "01121" :| ["50026"]

        yourHouseId <- getRandom
        rivertownId <- getRandom
        southsideId <- getRandom
        stMarysHospitalId <- getRandom
        miskatonicUniversityId <- getRandom
        downtownId <- getRandom
        easttownId <- getRandom
        graveyardId <- getRandom
        northsideId <- getRandom

        houseBurnedDown <- getHasRecord YourHouseHasBurnedToTheGround
        ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
        litaForcedToFindOthersToHelpHerCause <- getHasRecord
          LitaWasForcedToFindOthersToHelpHerCause
        ghoulPriestCard <- lookupEncounterCard "01116" <$> getRandom
        cultistCards <-
          for
              [ "01137"
              , "01138"
              , "01139"
              , "01140"
              , "01141"
              , "50044"
              , "50045"
              , "50046"
              ]
            $ \cardCode -> lookupEncounterCard cardCode <$> getRandom
        cultistDeck' <- drop 3 <$> shuffleM cultistCards
        let
          startingLocationMessages = if houseBurnedDown
            then [RevealLocation Nothing rivertownId, MoveAllTo rivertownId]
            else
              [ PlaceLocation "01124" yourHouseId
              , RevealLocation Nothing yourHouseId
              , MoveAllTo yourHouseId
              ]
          ghoulPriestMessages =
            [ AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive ]
          spawnAcolyteMessages =
            [ CreateEnemyAt (EncounterCard c) l Nothing
            | (c, l) <- zip acolytes [southsideId, downtownId, graveyardId]
            ]
          intro1or2 = if litaForcedToFindOthersToHelpHerCause
            then TheMidnightMasksIntroOne
            else TheMidnightMasksIntroTwo
        encounterDeck <- buildEncounterDeckWith
          (<> theDevourersCult)
          [ EncounterSet.ReturnToTheMidnightMasks
          , EncounterSet.TheMidnightMasks
          , EncounterSet.ChillingCold
          , EncounterSet.Nightgaunts
          , EncounterSet.LockedDoors
          ]
        pushMessages
          $ [ story investigatorIds (introPart1 intro1or2)
            , story investigatorIds introPart2
            , SetEncounterDeck encounterDeck
            , AddAgenda predatorOrPrey
            , AddAct "01123"
            , PlaceLocation rivertown rivertownId
            , PlaceLocation southside southsideId
            , PlaceLocation "01128" stMarysHospitalId
            , PlaceLocation miskatonicUniversity miskatonicUniversityId
            , PlaceLocation downtown downtownId
            , PlaceLocation easttown easttownId
            , PlaceLocation "01133" graveyardId
            , PlaceLocation northside northsideId
            ]
          <> startingLocationMessages
          <> ghoulPriestMessages
          <> spawnAcolyteMessages
        let
          locations' = mapFromList $ map
            (second pure . toFst (getLocationName . lookupLocationStub))
            [ "01124"
            , rivertown
            , southside
            , "01128"
            , miskatonicUniversity
            , downtown
            , easttown
            , "01133"
            , northside
            ]
        ReturnToTheMidnightMasks . TheMidnightMasks <$> runMessage
          msg
          (attrs
            { scenarioDeck = Just $ CultistDeck cultistDeck'
            , scenarioLocations = locations'
            }
          )
      _ -> ReturnToTheMidnightMasks <$> runMessage msg theMidnightMasks'
