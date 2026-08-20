module Arkham.Scenario.Scenarios.ChildrenOfBlood.RiverOfBlood (riverOfBlood) where

import Arkham.Act.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Acts
import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Agendas
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Locations
import Arkham.Matcher
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ChildrenOfBlood.RiverOfBlood.Helpers
import Arkham.Trait (Trait (Dawn, Dusk))

newtype RiverOfBlood = RiverOfBlood ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverOfBlood :: Difficulty -> RiverOfBlood
riverOfBlood difficulty =
  scenario
    RiverOfBlood
    "13001"
    "River of Blood"
    difficulty
    [ ".             heart     ."
    , "square        heart     triangle"
    , "square        .         triangle"
    , "unvisitedIsle circle    ."
    , "unvisitedIsle circle    ."
    , "t             .         moon"
    , "t             hourglass moon"
    , ".             hourglass ."
    ]

instance HasChaosTokenValue RiverOfBlood where
  getChaosTokenValue iid tokenFace (RiverOfBlood attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RiverOfBlood where
  runMessage msg s@(RiverOfBlood attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    Setup -> runScenarioSetup RiverOfBlood attrs do
      setup $ ul do
        li "gatherSets"
        li "gatherAdditionalSets"
        li.nested "placeLocations" do
          li "dawn"
          li "dusk"
          li "startAt"
        li.nested "waterfrontCivilians" do
          li "additionalWaterfrontCivilians"
        li.nested "juliaStern" do
          li "onTheRun"
          li "stalkingTheStreets"
          li "preyingUponArkham"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        li "lairs"
        unscoped $ li "readyToBegin"

      n <- getPlayerCount
      gather Set.RiverOfBlood
      gather Set.BloodBlight
      gather Set.BloodMoon
      gather Set.ChildrenOfBlood
      gather Set.Hunted
      gather Set.Infected
      gather Set.Misinformation
      if isEasyStandard attrs
        then gather Set.PreyedUpon
        else do
          gather Set.AgentsOfZburamoarte
          gather Set.Mongrels
      gather Set.Vermin
      gather Set.DeadEnds
      gather Set.FlyingTerrors
      when (n >= 3) $ gather Set.Afflicted

      setAgendaDeck
        [Agendas.theFirstDay, Agendas.theFirstNight, Agendas.theSecondDay, Agendas.theSecondNight]
      setActDeck [Acts.locateTheLair, Acts.cornerTheSuspect]

      if isEasyStandard attrs
        then do
          removeCards =<< amongGathered (#location <> CardWithTrait Dusk)
          startAt =<< place Locations.waterStreetDawn
          mainStreet <- place Locations.mainStreetDawn
          garrisonStreet <- place Locations.garrisonStreetDawn
          enemyAt_ Enemies.waterfrontCivilian mainStreet
          when (n >= 3) $ enemyAt_ Enemies.waterfrontCivilian garrisonStreet
          placeAll
            [ Locations.riverDocksDawn
            , Locations.erwinBridgeDawn
            , Locations.unvisitedIsleDawn
            , Locations.waterfrontWarehouseDawn
            , Locations.backAlleyDawn
            ]
        else do
          removeCards =<< amongGathered (#location <> CardWithTrait Dawn)
          startAt =<< place Locations.waterStreetDusk
          mainStreet <- place Locations.mainStreetDusk
          garrisonStreet <- place Locations.garrisonStreetDusk
          enemyAt_ Enemies.waterfrontCivilian mainStreet
          when (n >= 3) $ enemyAt_ Enemies.waterfrontCivilian garrisonStreet
          placeAll
            [ Locations.riverDocksDusk
            , Locations.erwinBridgeDusk
            , Locations.unvisitedIsleDusk
            , Locations.waterfrontWarehouseDusk
            , Locations.backAlleyDusk
            ]

      setAside
        [ case attrs.difficulty of
            Easy -> Enemies.juliaSternOnTheRun
            Standard -> Enemies.juliaSternStalkingTheStreets
            _ -> Enemies.juliaSternPreyingUponArkham
        ]
      removeCards =<< amongGathered (#enemy <> CardWithTitle "Julia Stern")
      setAside =<< amongGathered (cardIs Enemies.waterfrontCivilian)
    _ -> RiverOfBlood <$> liftRunMessage msg attrs
