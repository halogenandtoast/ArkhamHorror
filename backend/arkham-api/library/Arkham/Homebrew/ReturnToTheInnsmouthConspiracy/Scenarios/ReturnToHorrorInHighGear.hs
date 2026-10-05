module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToHorrorInHighGear (returnToHorrorInHighGear) where

import Arkham.Act.CardDefs.TheInnsmouthConspiracy.HorrorInHighGear qualified as Acts
import Arkham.Agenda.CardDefs.TheInnsmouthConspiracy.HorrorInHighGear qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (officialSetup, scenarioI18n)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Location.CardDefs.TheInnsmouthConspiracy.HorrorInHighGear qualified as Locations
import Arkham.Matcher hiding (assetAt)
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.HorrorInHighGear
import Arkham.Trait (Trait (Trap, Vehicle))

newtype ReturnToHorrorInHighGear = ReturnToHorrorInHighGear HorrorInHighGear
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToHorrorInHighGear :: Difficulty -> ReturnToHorrorInHighGear
returnToHorrorInHighGear difficulty =
  scenarioWith
    (ReturnToHorrorInHighGear . HorrorInHighGear)
    ":return-to-the-innsmouth-conspiracy:035"
    "Return to Horror in High Gear"
    difficulty
    []
    (referenceL .~ "07198")

instance RunMessage ReturnToHorrorInHighGear where
  runMessage msg (ReturnToHorrorInHighGear inner@(HorrorInHighGear attrs)) =
    runQueueT $ scenarioI18n "returnToHorrorInHighGear" $ case msg of
      Setup -> runScenarioSetup (ReturnToHorrorInHighGear . HorrorInHighGear) attrs do
        setIsReturnTo
        replaceSet Set.FogOverInnsmouth Sets.InnsmouthHaze

        theTerrorOfDevilReefIsDead <- getHasRecord TheTerrorOfDevilReefIsDead
        playerCount <- getPlayerCount

        setup $ ul do
          li.nested "gatherSets" do
            li.returnTo "replacedSets"
          li.returnTo "roadDeck"
          officialSetup "horrorInHighGear" do
            li.nested "roadDeck" do
              li "findRoadLocations"
              li "bottomThree"
              li "remainingOnTop"
              li "unrevealedSide"
            li "putRoadIntoPlay"
            li.nested "chooseVehicles" do
              li "vehiclesBeginAtFront"
              li "runningSide"
              li "beginInVehicle"
              li "triggerRoad"
            li.nested "chooseDrivers" do
              li "driverNote"
            li.nested "playerCount" do
              li.validate (playerCount == 1) "onePlayer"
              li.validate (playerCount `elem` [2, 3]) "twoOrThreePlayers"
              li.validate (playerCount == 4) "fourPlayers"
            li.nested "checkCampaignLog" do
              li.validate theTerrorOfDevilReefIsDead "theChaseIsOnV2"
              li.validate (not theTerrorOfDevilReefIsDead) "theChaseIsOnV1"
          li.returnTo "endOfRound"
          unscoped $ li "shuffleRemainder"

        gather Sets.ReturnToHorrorInHighGear
        gather Set.HorrorInHighGear
        gather Set.FogOverInnsmouth
        gather Set.Malfunction
        gather Set.ShatteredMemories
        gather Set.AncientEvils

        let agenda1 = if theTerrorOfDevilReefIsDead then Agendas.theChaseIsOnV2 else Agendas.theChaseIsOnV1

        setAgendaDeck [agenda1, Agendas.hotPursuit]
        setActDeck [Acts.pedalToTheMetal]

        -- "Shuffle the Mud Tracks and Straight Section locations in with the other
        -- (non-Long Way Around) locations, then remove two random locations from the Roads
        -- deck from the game without looking at them."
        roadPool <-
          drop 2
            <$> shuffleM
              [ HBLocations.mudTrack
              , HBLocations.straightSection
              , Locations.dimlyLitRoad_a
              , Locations.dimlyLitRoad_b
              , Locations.dimlyLitRoad_c
              , Locations.cliffsideRoad_a
              , Locations.cliffsideRoad_b
              , Locations.forkInTheRoad_a
              , Locations.forkInTheRoad_b
              , Locations.intersection_a
              , Locations.intersection_b
              , Locations.tightTurn_a
              , Locations.tightTurn_b
              , Locations.tightTurn_c
              , Locations.desolateRoad_a
              , Locations.desolateRoad_b
              ]
        let (bottom, top) = splitAt 2 roadPool

        bottom' <- shuffleM $ Locations.falconPointApproach : bottom
        setAside $ replicate 6 Locations.longWayAround

        let (inPlay, roadDeck) = splitAt 3 (top <> bottom')

        placed <- for (withIndex1 inPlay) $ \(n, location) -> placeLabeled ("road" <> tshow n <> "a") location

        for_ (zip placed (drop 1 placed)) \(left, right) -> do
          push $ PlacedLocationDirection left LeftOf right

        for_ (headMay $ reverse placed) \front -> do
          assetAt_ Assets.thomasDawsonsCarRunning front
          assetAt_ Assets.elinaHarpersCarRunning front
          eachInvestigator (`forInvestigator` DoStep 1 Setup)
          doStep 2 Setup
          reveal front

        addExtraDeck RoadDeck roadDeck

        lead <- getLead
        case playerCount of
          2 -> findRandomEncounterCard lead ScenarioTarget (#enemy <> CardWithTrait Vehicle)
          3 -> findRandomEncounterCard lead ScenarioTarget (#enemy <> CardWithTrait Vehicle)
          4 -> do
            findRandomEncounterCard lead ScenarioTarget (#enemy <> CardWithTrait Vehicle)
            findRandomEncounterCard lead ScenarioTarget (#enemy <> CardWithTrait Vehicle)
          _ -> pure ()
      -- "For the duration of this scenario, the following additional ability is active:
      -- Forced - At the end of the round: Discard all non-Trap cards attached to
      -- locations." Printed on the scenario card rather than on any one card, so it
      -- lives here.
      EndRound -> do
        selectEach (TreacheryAttachedToLocation Anywhere <> not_ (TreacheryWithTrait Trap))
          $ toDiscard ScenarioSource
        ReturnToHorrorInHighGear <$> liftRunMessage msg inner
      _ -> ReturnToHorrorInHighGear <$> liftRunMessage msg inner
