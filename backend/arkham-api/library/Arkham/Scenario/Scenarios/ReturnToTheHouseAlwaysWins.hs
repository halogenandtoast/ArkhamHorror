module Arkham.Scenario.Scenarios.ReturnToTheHouseAlwaysWins (returnToTheHouseAlwaysWins) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Locations
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheHouseAlwaysWins
import Arkham.Scenarios.TheHouseAlwaysWins.Helpers

newtype ReturnToTheHouseAlwaysWins = ReturnToTheHouseAlwaysWins TheHouseAlwaysWins
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheHouseAlwaysWins :: Difficulty -> ReturnToTheHouseAlwaysWins
returnToTheHouseAlwaysWins difficulty =
  scenarioWith
    (ReturnToTheHouseAlwaysWins . TheHouseAlwaysWins)
    "51015"
    "Return to The House Always Wins"
    difficulty
    [ ".    .      .        backHallDoorway1 ."
    , ".    .      triangle backHallDoorway1 ."
    , "moon circle triangle diamond          backHallDoorway2"
    , "moon circle square   diamond          backHallDoorway2"
    , ".    heart  square   backHallDoorway3 ."
    , ".    heart  .        backHallDoorway3 ."
    ]
    (referenceL .~ "02062")

instance RunMessage ReturnToTheHouseAlwaysWins where
  runMessage msg (ReturnToTheHouseAlwaysWins theHouseAlwaysWins'@(TheHouseAlwaysWins attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToTheHouseAlwaysWins . TheHouseAlwaysWins) attrs do
      setup do
        ul do
          li "gatherSets"
          li "setAsideEncounterSets"
          li "placeLocations"
          li "placeCloverClubPitBoss"
          li "setAside"
          unscoped $ li "shuffleRemainder"
        p "note"

      gather Set.ReturnToTheHouseAlwaysWins
      gather Set.TheHouseAlwaysWins
      gather Set.BadLuck
      gather Set.NaomisCrew
      gather Set.Rats
      gatherAndSetAside Set.HideousAbominations
      gatherAndSetAside Set.ErraticFear

      startAt =<< place Locations.laBellaLuna
      cloverClubLounge <- place =<< sample2 Locations.cloverClubLounge Locations.returnToCloverClubLounge
      placeAll [Locations.cloverClubBar, Locations.cloverClubCardroom]
      enemyAt_ Enemies.cloverClubPitBoss cloverClubLounge

      setAside
        [ Locations.darkenedHall
        , Assets.peterClover
        , Assets.drFrancisMorgan
        , Locations.artGallery
        , Locations.vipArea
        , Locations.backAlley
        , Locations.cloverClubStage
        ]

      setActDeck [Acts.beginnersLuck, Acts.skinGame, Acts.allIn, Acts.fold]
      setAgendaDeck [Agendas.theCloverClub, Agendas.undergroundMuscle, Agendas.chaosInTheCloverClub]
    _ -> ReturnToTheHouseAlwaysWins <$> liftRunMessage msg theHouseAlwaysWins'
