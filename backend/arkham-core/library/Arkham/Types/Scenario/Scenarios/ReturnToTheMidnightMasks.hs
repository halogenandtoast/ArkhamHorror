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
  deriving anyclass (IsScenario, HasRecord)
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


        yourHouse <- genCard Locations.yourHouse
        rivertown <- genCard =<< sample
          (Locations.rivertown :| [Locations.rivertownAbandonedWarehouse])
        southside <- genCard =<< sample
          (Locations.southsideHistoricalSociety
          :| [Locations.southsideMasBoardingHouse]
          )
        stMarysHospital <- genCard Locations.stMarysHospital
        miskatonicUniversity <- genCard =<< sample
          (Locations.miskatonicUniversity
          :| [Locations.miskatonicUniversityMiskatonicMuseum]
          )
        downtown <-
          genCard
            =<< sample
                  (Locations.downtownFirstBankOfArkham
                  :| [Locations.downtownArkhamAsylum]
                  )
        easttown <- genCard =<< sample
          (Locations.easttown :| [Locations.easttownArkhamPoliceStation])
        graveyard <- genCard Locations.graveyard
        northside <-
          genCard =<< sample
            (Locations.northside :| [Locations.northsideTrainStation])

        predatorOrPrey <- sample $ "01121" :| ["50026"]

        houseBurnedDown <- getHasRecord YourHouseHasBurnedToTheGround
        ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
        litaForcedToFindOthersToHelpHerCause <- getHasRecord
          LitaWasForcedToFindOthersToHelpHerCause
        ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest
        cultistCards <- liftM2
          (<>)
          (gatherEncounterSet EncounterSet.CultOfUmordhoth)
          (gatherEncounterSet EncounterSet.ReturnCultOfUmordhoth)
        cultistDeck' <- map EncounterCard . drop 3 <$> shuffleM cultistCards
        let
          startingLocationMessages = if houseBurnedDown
            then
              [ RevealLocation Nothing $ toLocationId rivertown
              , MoveAllTo (toSource attrs) $ toLocationId rivertown
              ]
            else
              [ PlaceLocation yourHouse
              , RevealLocation Nothing $ toLocationId yourHouse
              , MoveAllTo (toSource attrs) $ toLocationId yourHouse
              ]
          ghoulPriestMessages =
            [ AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive ]
          spawnAcolyteMessages =
            [ CreateEnemyAt (EncounterCard c) l Nothing
            | (c, l) <- zip acolytes
              $ map toLocationId [southside, downtown, graveyard]
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
            , PlaceLocation rivertown
            , PlaceLocation southside
            , PlaceLocation stMarysHospital
            , PlaceLocation miskatonicUniversity
            , PlaceLocation downtown
            , PlaceLocation easttown
            , PlaceLocation graveyard
            , PlaceLocation northside
            ]
          <> startingLocationMessages
          <> ghoulPriestMessages
          <> spawnAcolyteMessages
        ReturnToTheMidnightMasks . TheMidnightMasks <$> runMessage
          msg
          (attrs { scenarioDeck = Just $ CultistDeck cultistDeck' })
      _ -> ReturnToTheMidnightMasks <$> runMessage msg theMidnightMasks'
