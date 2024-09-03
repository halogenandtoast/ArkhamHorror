module Arkham.Scenario.Scenarios.ReturnToTheMidnightMasks where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Prelude
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner hiding (placeLocationCard, story)
import Arkham.Scenario.Scenarios.TheMidnightMasks
import Arkham.Scenario.Setup
import Arkham.Scenarios.TheMidnightMasks.Story

newtype ReturnToTheMidnightMasks = ReturnToTheMidnightMasks TheMidnightMasks
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasChaosTokenValue)

returnToTheMidnightMasks :: Difficulty -> ReturnToTheMidnightMasks
returnToTheMidnightMasks difficulty =
  scenarioWith
    (ReturnToTheMidnightMasks . TheMidnightMasks)
    "50025"
    "Return to The Midnight Masks"
    difficulty
    [ "northside downtown easttown"
    , "miskatonicUniversity rivertown graveyard"
    , "stMarysHospital southside yourHouse"
    ]
    ((decksL .~ mapFromList [(CultistDeck, [])]) . (referenceL .~ "01120"))

instance RunMessage ReturnToTheMidnightMasks where
  runMessage msg s@(ReturnToTheMidnightMasks theMidnightMasks'@(TheMidnightMasks attrs)) = runQueueT $ case msg of
    PreScenarioSetup -> do
      forcedToFindOthers <- getHasRecord LitaWasForcedToFindOthersToHelpHerCause
      story
        $ introPart1
        $ if forcedToFindOthers then TheMidnightMasksIntroOne else TheMidnightMasksIntroTwo
      story introPart2
      pure s
    Setup -> runScenarioSetup (ReturnToTheMidnightMasks . TheMidnightMasks) attrs do
      gather EncounterSet.ReturnToTheMidnightMasks
      gather EncounterSet.TheMidnightMasks
      gather EncounterSet.ChillingCold
      gather EncounterSet.Nightgaunts
      gather EncounterSet.LockedDoors

      predatorOrPrey <- sample2 Agendas.predatorOrPrey Agendas.returnToPredatorOrPrey
      setAgendaDeck [predatorOrPrey, Agendas.timeIsRunningShort]
      setActDeck [Acts.uncoveringTheConspiracy]

      rivertown <- placeOneOf (Locations.rivertown, Locations.rivertownAbandonedWarehouse)
      southside <- placeOneOf (Locations.southsideHistoricalSociety, Locations.southsideMasBoardingHouse)
      downtown <- placeOneOf (Locations.downtownFirstBankOfArkham, Locations.downtownArkhamAsylum)
      graveyard <- place Locations.graveyard
      place_ Locations.stMarysHospital
      placeOneOf_ (Locations.miskatonicUniversity, Locations.miskatonicUniversityMiskatonicMuseum)
      placeOneOf_ (Locations.easttown, Locations.easttownArkhamPoliceStation)
      placeOneOf_ (Locations.northside, Locations.northsideTrainStation)

      cultOfUmordhoth <- gatherEncounterSet EncounterSet.CultOfUmordhoth
      returnCultOfUmordhoth <- gatherEncounterSet EncounterSet.ReturnCultOfUmordhoth
      cultistCards <- drop 3 <$> shuffleM (cultOfUmordhoth <> returnCultOfUmordhoth)
      addExtraDeck CultistDeck cultistCards

      houseBurnedDown <- getHasRecord YourHouseHasBurnedToTheGround
      if houseBurnedDown
        then startAt rivertown
        else startAt =<< place Locations.yourHouse

      count' <- getPlayerCount
      let acolytes = replicate (count' - 1) Enemies.discipleOfTheDevourer
      zipWithM_ enemyAt acolytes [southside, downtown, graveyard]

      whenHasRecord GhoulPriestIsStillAlive $ addToEncounterDeck (Only Enemies.ghoulPriest)
    _ -> ReturnToTheMidnightMasks <$> lift (runMessage msg theMidnightMasks')
