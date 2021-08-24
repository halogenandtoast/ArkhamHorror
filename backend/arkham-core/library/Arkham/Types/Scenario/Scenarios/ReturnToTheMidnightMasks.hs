module Arkham.Types.Scenario.Scenarios.ReturnToTheMidnightMasks where

import Arkham.Prelude

import Arkham.EncounterSet (gatherEncounterSet)
import qualified Arkham.Enemy.Cards as Enemies
import qualified Arkham.Location.Cards as Locations
import Arkham.Types.CampaignLogKey
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Id
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

instance ScenarioRunner env => RunMessage env ReturnToTheMidnightMasks where
  runMessage msg (ReturnToTheMidnightMasks theMidnightMasks'@(TheMidnightMasks attrs))
    = case msg of
      Setup -> do
        count' <- getPlayerCount
        investigatorIds <- getInvestigatorIds
        (acolytes, theDevourersCult) <- splitAt (count' - 1)
          <$> gatherEncounterSet EncounterSet.TheDevourersCult
        -- we will spawn these disciples of the devourer

        southside <-
          sample
          $ Locations.southsideHistoricalSociety
          :| [Locations.southsideMasBoardingHouse]
        downtown <-
          sample
          $ Locations.downtownFirstBankOfArkham
          :| [Locations.downtownArkhamAsylum]
        easttown <-
          sample $ Locations.easttown :| [Locations.easttownArkhamPoliceStation]
        northside <-
          sample $ Locations.northside :| [Locations.northsideTrainStation]
        rivertown <-
          sample
          $ Locations.rivertown
          :| [Locations.rivertownAbandonedWarehouse]
        miskatonicUniversity <-
          sample
          $ Locations.miskatonicUniversity
          :| [Locations.miskatonicUniversityMiskatonicMuseum]
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
        ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest
        cultistCards <- liftM2
          (<>)
          (gatherEncounterSet EncounterSet.CultOfUmordhoth)
          (gatherEncounterSet EncounterSet.ReturnCultOfUmordhoth)
        cultistDeck' <- drop 3 <$> shuffleM cultistCards
        let
          startingLocationMessages = if houseBurnedDown
            then [RevealLocation Nothing rivertownId, MoveAllTo rivertownId]
            else
              [ PlaceLocation yourHouseId Locations.yourHouse
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
        pushAllEnd
          $ [ story investigatorIds (introPart1 intro1or2)
            , story investigatorIds introPart2
            , SetEncounterDeck encounterDeck
            , AddAgenda predatorOrPrey
            , AddAct "01123"
            , PlaceLocation rivertownId rivertown
            , PlaceLocation southsideId southside
            , PlaceLocation stMarysHospitalId Locations.stMarysHospital
            , PlaceLocation miskatonicUniversityId miskatonicUniversity
            , PlaceLocation downtownId downtown
            , PlaceLocation easttownId easttown
            , PlaceLocation graveyardId Locations.graveyard
            , PlaceLocation northsideId northside
            ]
          <> startingLocationMessages
          <> ghoulPriestMessages
          <> spawnAcolyteMessages
        let
          locations' = locationNameMap
            [ Locations.yourHouse
            , rivertown
            , southside
            , Locations.stMarysHospital
            , miskatonicUniversity
            , downtown
            , easttown
            , Locations.graveyard
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
