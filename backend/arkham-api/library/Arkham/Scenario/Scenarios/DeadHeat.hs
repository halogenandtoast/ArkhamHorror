module Arkham.Scenario.Scenarios.DeadHeat (deadHeat) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Campaigns.TheScarletKeys.Helpers (getTime)
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DeadHeat.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token

newtype DeadHeat = DeadHeat ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deadHeat :: Difficulty -> DeadHeat
deadHeat difficulty =
  scenario
    DeadHeat
    "09520"
    "Dead Heat"
    difficulty
    [ "heart squiggle square"
    , ".     diamond   triangle"
    ]

instance HasChaosTokenValue DeadHeat where
  getChaosTokenValue iid tokenFace (DeadHeat attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DeadHeat where
  runMessage msg s@(DeadHeat attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      n <- getTime
      flavor do
        setTitle "title"
        p "intro1"
        ul do
          li.validate (n < 15) "lessThan15"
          li.validate (n >= 15 && n <= 24) "between15And24"
          li.validate (n > 24) "moreThan24"
      when (n < 15) $ doStep 2 PreScenarioSetup
      when (n >= 15 && n <= 24) $ doStep 3 PreScenarioSetup
      when (n > 24) $ doStep 4 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro2"
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      push R5
      pure s
    Setup -> runScenarioSetup DeadHeat attrs do
      n <- getTime
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "beginPlay"
        li.nested "civilians" do
          li.validate (n <= 10) "tenOrFewerTime"
          li.validate (n >= 11 && n <= 17) "elevenToSeventeenTime"
          li.validate (n >= 18 && n <= 24) "eighteentoTwentyFourTime"
        li "setOutOfPlay"
        li.nested "decks" do
          li.validate (n < 15) "fewerThanFifteenTime"
          li.validate (n >= 15 && n <= 24) "fifteenToTwentyFourTime"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.DeadHeat
      gather Set.ScarletSorcery
      gather Set.SpreadingCorruption
      gather Set.Ghouls
      gather Set.StrikingFear

      setAgendaDeck [Agendas.gnashingTeeth, Agendas.emptyStreets]
      setActDeck [Acts.findAmaranth, Acts.ritualOfLifeAndDeath, Acts.queenOfNothingAtAll]

      startAt =<< place Locations.marrakeshRailwayStation
      placeAll
        [ Locations.jemaaElFnaaSquare
        , Locations.saadiansTombs
        , Locations.tanneries
        , Locations.bahiaPalaceGardens
        ]

      setAside
        [ Locations.marrakeshRailwayStationAbandoned
        , Locations.jemaaElFnaaSquareAbandoned
        , Locations.saadiansTombsAbandoned
        , Locations.tanneriesAbandoned
        , Locations.bahiaPalaceGardensAbandoned
        , Enemies.amaranthLurkingCorruption
        , Enemies.razinFarhiReanimatedArtificer
        , Enemies.khalidBelovedCompanion
        , Enemies.ancientRaider
        , Enemies.ancientRaider
        , Keys.theLastBlossom
        , Stories.saveTheCivilians
        ]

      doStep 2 Setup
    DoStep 2 Setup -> do
      locations <- select Anywhere
      n <- perPlayer 1
      for_ locations $ placeTokensOn ScenarioSource Civilian (n + 1)
      time <- getTime
      lead <- getLead
      when (time >= 11 && time <= 17) do
        chooseNM lead n $ targets locations $ removeTokensOn ScenarioSource Civilian 1
      when (time >= 18 && time <= 24) do
        if n * 2 >= length locations
          then do
            for_ locations $ removeTokensOn ScenarioSource Civilian 1
            chooseNM lead ((n * 2) - length locations)
              $ targets locations
              $ removeTokensOn ScenarioSource Civilian 1
          else chooseNM lead (n * 2) $ targets locations $ removeTokensOn ScenarioSource Civilian 1
      pure s
    ScenarioResolution r -> do
      case r of
        Resolution 5 -> scope "resolution" do
          flavor $ setTitle "title" >> p "resolution5"
          endOfScenario
        _ -> error "Unknown resolution for Dead Heat"
      pure s
    _ -> DeadHeat <$> liftRunMessage msg attrs
