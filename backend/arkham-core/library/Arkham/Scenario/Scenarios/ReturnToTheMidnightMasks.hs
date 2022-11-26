module Arkham.Scenario.Scenarios.ReturnToTheMidnightMasks where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios.TheMidnightMasks
import Arkham.Scenarios.TheMidnightMasks.Story

newtype ReturnToTheMidnightMasks = ReturnToTheMidnightMasks TheMidnightMasks
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToTheMidnightMasks :: Difficulty -> ReturnToTheMidnightMasks
returnToTheMidnightMasks difficulty = scenarioWith
  (ReturnToTheMidnightMasks . TheMidnightMasks)
  "50025"
  "Return to the Midnight Masks"
  difficulty
  [ "northside downtown easttown"
  , "miskatonicUniversity rivertown graveyard"
  , "stMarysHospital southside yourHouse"
  ]
  (decksL .~ mapFromList [(CultistDeck, [])])

instance HasTokenValue ReturnToTheMidnightMasks where
  getTokenValue iid tokenFace (ReturnToTheMidnightMasks theMidnightMasks') =
    getTokenValue iid tokenFace theMidnightMasks'

instance RunMessage ReturnToTheMidnightMasks where
  runMessage msg (ReturnToTheMidnightMasks theMidnightMasks'@(TheMidnightMasks attrs))
    = case msg of
      Setup -> do
        count' <- getPlayerCount
        investigatorIds <- allInvestigatorIds
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

        predatorOrPrey <-
          sample $ Agendas.predatorOrPrey :| [Agendas.returnToPredatorOrPrey]

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
            , SetAgendaDeck
            , SetActDeck
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
          (attrs
          & (decksL . at CultistDeck ?~ cultistDeck')
          & (actStackL . at 1 ?~ [Acts.uncoveringTheConspiracy])
          & (agendaStackL
            . at 1
            ?~ [predatorOrPrey, Agendas.timeIsRunningShort]
            )
          )
      _ -> ReturnToTheMidnightMasks <$> runMessage msg theMidnightMasks'
