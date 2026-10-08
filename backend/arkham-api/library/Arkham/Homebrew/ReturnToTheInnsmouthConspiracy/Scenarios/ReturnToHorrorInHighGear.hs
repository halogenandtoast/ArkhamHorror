module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Scenarios.ReturnToHorrorInHighGear (returnToHorrorInHighGear) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as HBLocations
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (officialSetup, scenarioI18n)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Sets qualified as Sets
import Arkham.Matcher hiding (assetAt)
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheInnsmouthConspiracy.HorrorInHighGear
import Arkham.Scenarios.TheInnsmouthConspiracy.HorrorInHighGear.Helpers (scenarioLayout)
import Arkham.Trait (Trait (Trap))

newtype ReturnToHorrorInHighGear = ReturnToHorrorInHighGear HorrorInHighGear
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue, HasModifiersFor)

returnToHorrorInHighGear :: Difficulty -> ReturnToHorrorInHighGear
returnToHorrorInHighGear difficulty =
  scenarioWith
    (ReturnToHorrorInHighGear . HorrorInHighGear)
    ":return-to-the-innsmouth-conspiracy:035"
    "Return to Horror in High Gear"
    difficulty
    scenarioLayout
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

        {- "Shuffle the Mud Tracks and Straight Section locations in with the other
        (non-Long Way Around) locations, then remove two random locations from the Roads
        deck from the game without looking at them." -}
        addToPool "roads" [HBLocations.mudTracks, HBLocations.straightSection]
        thinPool "roads" 2

        {- "Attach this card to the scenario reference card": the box's own card carries the
        extra ability below, so it sits beside the Campaign Guide's reference. -}
        addAdditionalReferences [":return-to-the-innsmouth-conspiracy:035b"]

        gather Sets.ReturnToHorrorInHighGear
        setupHorrorInHighGear attrs
      -- "For the duration of this scenario, the following additional ability is active:
      -- Forced - At the end of the round: Discard all non-Trap cards attached to
      -- locations." Printed on the scenario card rather than on any one card, so it
      -- lives here.
      EndRound -> do
        {- "Discard all non-Trap cards attached to locations." Any card can be attached to a
        location, not just treacheries -- Shortcut is an event -- so each kind is asked for
        by its placement. -}
        locations <- select Anywhere
        for_ locations \lid -> do
          selectEach (TreacheryAttachedToLocation (LocationWithId lid) <> not_ (TreacheryWithTrait Trap))
            $ toDiscard ScenarioSource
          selectEach (EventWithPlacement (AttachedToLocation lid) <> not_ (EventWithTrait Trap))
            $ toDiscard ScenarioSource
          selectEach (AssetWithPlacement (AttachedToLocation lid) <> not_ (AssetWithTrait Trap))
            $ toDiscard ScenarioSource
        ReturnToHorrorInHighGear <$> liftRunMessage msg inner
      _ -> ReturnToHorrorInHighGear <$> liftRunMessage msg inner
