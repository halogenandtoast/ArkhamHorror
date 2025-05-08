module Arkham.Scenario.Scenarios.ReturnToTheEssexCountyExpress (returnToTheEssexCountyExpress) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.EncounterSet qualified as Set
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Modifier
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.TheEssexCountyExpress
import Arkham.Scenarios.TheEssexCountyExpress.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype ReturnToTheEssexCountyExpress = ReturnToTheEssexCountyExpress TheEssexCountyExpress
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToTheEssexCountyExpress :: Difficulty -> ReturnToTheEssexCountyExpress
returnToTheEssexCountyExpress difficulty =
  scenario
    (ReturnToTheEssexCountyExpress . TheEssexCountyExpress)
    "51025"
    "Return to The Essex County Express"
    difficulty
    []

instance RunMessage ReturnToTheEssexCountyExpress where
  runMessage msg (ReturnToTheEssexCountyExpress theEssexCountyExpress'@(TheEssexCountyExpress attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToTheEssexCountyExpress . TheEssexCountyExpress) attrs do
      setUsesGrid

      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li "revealTrainCar"
          li "setAside"
          li "adjustChaosBag"
          unscoped $ li "shuffleRemainder"

      scope "moving" $ flavor do
        setTitle "title"
        p "body"

      gather Set.ReturnToTheEssexCountyExpress
      gather Set.TheEssexCountyExpress
      gather Set.BeyondTheThreshold
      gather Set.ErraticFear
      gather Set.ResurgentEvils
      gather Set.DarkCult

      engineCar <-
        sample
          $ Locations.engineCar_175
          :| [ Locations.engineCar_176
             , Locations.engineCar_177
             , Locations.returnToEngineCar
             ]

      (firstCar :| trainCars) <-
        sampleNonEmptyN 6
          $ Locations.passengerCar_167
          :| [ Locations.passengerCar_168
             , Locations.passengerCar_169
             , Locations.passengerCar_170
             , Locations.passengerCar_171
             , Locations.sleepingCar
             , Locations.diningCar
             , Locations.parlorCar
             , Locations.freightCar
             , Locations.baggageCar
             ]

      start <- placeInGrid (Pos 0 0) firstCar
      for_ (zip [1 ..] (trainCars <> [engineCar])) \(idx, trainCarCard) -> do
        placeInGrid (Pos idx 0) trainCarCard

      let
        token = case attrs.difficulty of
          Easy -> MinusTwo
          Standard -> MinusThree
          Hard -> MinusFour
          Expert -> MinusFive

      addChaosToken token
      setupModifier ScenarioSource (LocationTarget start) Blank
      startAt start

      setAside
        [ Treacheries.acrossSpaceAndTime
        , Treacheries.acrossSpaceAndTime
        , Treacheries.acrossSpaceAndTime
        , Treacheries.acrossSpaceAndTime
        , Assets.engineer
        ]
      setAgendaDeck
        [ Agendas.whereTheresSmoke
        , Agendas.aTearInRealityV2
        , Agendas.theMawWidens
        , Agendas.rollingBackwards
        , Agendas.drawnIn
        , Agendas.outOfTime
        ]
      setActDeck [Acts.run, Acts.getTheEngineRunning]
    _ -> ReturnToTheEssexCountyExpress <$> liftRunMessage msg theEssexCountyExpress'
