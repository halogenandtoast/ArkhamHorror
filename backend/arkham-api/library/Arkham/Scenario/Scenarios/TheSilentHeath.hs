module Arkham.Scenario.Scenarios.TheSilentHeath (theSilentHeath) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheSilentHeath.Helpers
import Arkham.Story.Cards qualified as Stories

newtype TheSilentHeath = TheSilentHeath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilentHeath :: Difficulty -> TheSilentHeath
theSilentHeath difficulty = scenario TheSilentHeath "10549" "The Silent Heath" difficulty []

instance HasChaosTokenValue TheSilentHeath where
  getChaosTokenValue iid tokenFace (TheSilentHeath attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheSilentHeath where
  runMessage msg s@(TheSilentHeath attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      pure s
    Setup -> runScenarioSetup TheSilentHeath attrs do
      setUsesGrid

      gather Set.TheSilentHeath
      gather Set.AgentsOfTheColour
      gather Set.Blight
      gather Set.HorrorsInTheRock
      gather Set.Refractions
      gather Set.Transfiguration
      gather Set.StrikingFear

      setScenarioDayAndTime
      day <- getCampaignDay
      time <- getCampaignTime

      case day of
        Day1 -> do
          gather Set.TheFirstDay
          placeStory $ case time of
            Day -> Stories.dayOne
            Night -> Stories.nightOne
        Day2 -> do
          gather Set.TheSecondDay
          placeStory $ case time of
            Day -> Stories.dayTwo
            Night -> Stories.nightTwo
        Day3 -> do
          gather Set.TheFinalDay
          placeStory $ case time of
            Day -> Stories.dayThree
            Night -> Stories.nightThree

      let
        agenda2 =
          case day of
            Day1 -> Agendas.desolationV1
            _ -> Agendas.desolationV2

      setAgendaDeck [Agendas.unsettlingSilence, agenda2]
      setActDeck [Acts.aLostLegacy, Acts.searchingTheHeath]

      ruins <- placeInGrid (Pos 0 0) Locations.pearlEstateRuins
      slope <- placeInGrid (Pos 2 0) Locations.ashenSlope
      grove <- placeInGrid (Pos 1 (-1)) Locations.crystalGrove

      startAt ruins

      setAside
        [ Enemies.broodQueenDyingMother
        , Enemies.crystalParasite
        , Enemies.crystalParasite
        , Locations.saltChamber
        , Locations.larvalTunnel
        , Locations.crystalNursery
        ]

      setAsideFacedown
        =<< shuffle
          [Assets.crystalRemainsTheChild, Assets.crystalRemainsTheFather, Assets.crystalRemainsTheMother]

      horrorsInTheRockLocations <- fromGathered (CardFromEncounterSet Set.HorrorsInTheRock <> #location)
      setAsideFacedown =<< sampleListN 3 horrorsInTheRockLocations
      void $ fromGathered (CardFromEncounterSet Set.HorrorsInTheRock)

      when (time == Day) $ case day of
        Day1 -> assetAt_ Assets.leahAtwoodTheValeCook ruins
        Day2 -> assetAt_ Assets.drRosaMarquezBestInHerField grove
        Day3 -> assetAt_ Assets.motherRachelKindlyMatron slope
    _ -> TheSilentHeath <$> liftRunMessage msg attrs
