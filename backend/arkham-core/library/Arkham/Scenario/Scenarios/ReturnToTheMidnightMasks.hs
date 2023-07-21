module Arkham.Scenario.Scenarios.ReturnToTheMidnightMasks where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Card
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
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToTheMidnightMasks :: Difficulty -> ReturnToTheMidnightMasks
returnToTheMidnightMasks difficulty =
  scenarioWith
    (ReturnToTheMidnightMasks . TheMidnightMasks)
    "50025"
    "Return to the Midnight Masks"
    difficulty
    [ "northside downtown easttown"
    , "miskatonicUniversity rivertown graveyard"
    , "stMarysHospital southside yourHouse"
    ]
    ((decksL .~ mapFromList [(CultistDeck, [])]) . (referenceL .~ "01120"))

instance HasChaosTokenValue ReturnToTheMidnightMasks where
  getChaosTokenValue iid chaosTokenFace (ReturnToTheMidnightMasks theMidnightMasks') =
    getChaosTokenValue iid chaosTokenFace theMidnightMasks'

instance RunMessage ReturnToTheMidnightMasks where
  runMessage msg (ReturnToTheMidnightMasks theMidnightMasks'@(TheMidnightMasks attrs)) =
    case msg of
      Setup -> do
        count' <- getPlayerCount
        investigatorIds <- allInvestigatorIds
        (acolytes, theDevourersCult) <-
          splitAt (count' - 1)
            <$> gatherEncounterSet EncounterSet.TheDevourersCult
        -- we will spawn these disciples of the devourer

        (yourHouseId, placeYourHouse) <- placeLocationCard Locations.yourHouse
        (rivertownId, placeRivertown) <-
          placeLocationCard
            =<< sample
              (Locations.rivertown :| [Locations.rivertownAbandonedWarehouse])
        (southsideId, placeSouthside) <-
          placeLocationCard
            =<< sample
              ( Locations.southsideHistoricalSociety
                  :| [Locations.southsideMasBoardingHouse]
              )
        (downtownId, placeDowntown) <-
          placeLocationCard
            =<< sample
              ( Locations.downtownFirstBankOfArkham
                  :| [Locations.downtownArkhamAsylum]
              )

        (graveyardId, placeGraveyard) <- placeLocationCard Locations.graveyard

        placeStMarysHospital <- placeLocationCard_ Locations.stMarysHospital
        placeMiskatonicUniversity <-
          placeLocationCard_
            =<< sample
              ( Locations.miskatonicUniversity
                  :| [Locations.miskatonicUniversityMiskatonicMuseum]
              )
        placeEasttown <-
          placeLocationCard_
            =<< sample
              (Locations.easttown :| [Locations.easttownArkhamPoliceStation])
        placeNorthside <-
          placeLocationCard_
            =<< sample
              (Locations.northside :| [Locations.northsideTrainStation])

        predatorOrPrey <-
          sample $ Agendas.predatorOrPrey :| [Agendas.returnToPredatorOrPrey]

        houseBurnedDown <- getHasRecord YourHouseHasBurnedToTheGround
        ghoulPriestAlive <- getHasRecord GhoulPriestIsStillAlive
        litaForcedToFindOthersToHelpHerCause <-
          getHasRecord
            LitaWasForcedToFindOthersToHelpHerCause
        ghoulPriestCard <- genEncounterCard Enemies.ghoulPriest
        cultistCards <-
          liftM2
            (<>)
            (gatherEncounterSet EncounterSet.CultOfUmordhoth)
            (gatherEncounterSet EncounterSet.ReturnCultOfUmordhoth)
        cultistDeck' <- map EncounterCard . drop 3 <$> shuffleM cultistCards
        let
          startingLocationMessages =
            if houseBurnedDown
              then
                [ RevealLocation Nothing rivertownId
                , MoveAllTo (toSource attrs) rivertownId
                ]
              else
                [ placeYourHouse
                , RevealLocation Nothing yourHouseId
                , MoveAllTo (toSource attrs) yourHouseId
                ]
          ghoulPriestMessages =
            [AddToEncounterDeck ghoulPriestCard | ghoulPriestAlive]
          intro1or2 =
            if litaForcedToFindOthersToHelpHerCause
              then TheMidnightMasksIntroOne
              else TheMidnightMasksIntroTwo

        spawnAcolyteMessages <-
          for (zip acolytes [southsideId, downtownId, graveyardId]) $
            \(c, l) -> createEnemyAt_ (EncounterCard c) l Nothing

        encounterDeck <-
          buildEncounterDeckWith
            (<> theDevourersCult)
            [ EncounterSet.ReturnToTheMidnightMasks
            , EncounterSet.TheMidnightMasks
            , EncounterSet.ChillingCold
            , EncounterSet.Nightgaunts
            , EncounterSet.LockedDoors
            ]
        pushAll $
          [ story investigatorIds (introPart1 intro1or2)
          , story investigatorIds introPart2
          , SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeRivertown
          , placeSouthside
          , placeStMarysHospital
          , placeMiskatonicUniversity
          , placeDowntown
          , placeEasttown
          , placeGraveyard
          , placeNorthside
          ]
            <> startingLocationMessages
            <> ghoulPriestMessages
            <> spawnAcolyteMessages

        agendas <- genCards [predatorOrPrey, Agendas.timeIsRunningShort]
        acts <- genCards [Acts.uncoveringTheConspiracy]

        ReturnToTheMidnightMasks
          . TheMidnightMasks
          <$> runMessage
            msg
            ( attrs
                & (decksL . at CultistDeck ?~ cultistDeck')
                & (actStackL . at 1 ?~ acts)
                & (agendaStackL . at 1 ?~ agendas)
            )
      _ -> ReturnToTheMidnightMasks <$> runMessage msg theMidnightMasks'
