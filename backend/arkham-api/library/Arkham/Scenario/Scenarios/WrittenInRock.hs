module Arkham.Scenario.Scenarios.WrittenInRock (writtenInRock) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.WrittenInRock.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Token
import Arkham.Trait (Trait (Cave))

newtype WrittenInRock = WrittenInRock ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

writtenInRock :: Difficulty -> WrittenInRock
writtenInRock difficulty =
  scenarioWith WrittenInRock "10501" "Written in Rock" difficulty []
    $ referenceL
    .~ if difficulty `elem` [Easy, Standard] then "10501" else "10502"

instance HasChaosTokenValue WrittenInRock where
  getChaosTokenValue iid tokenFace (WrittenInRock attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WrittenInRock where
  runMessage msg s@(WrittenInRock attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    Setup -> runScenarioSetup WrittenInRock attrs do
      setup $ ul do
        li "gatherSets"
        li "currentDaySet"
        li "currentDayMarker"
        li "caves"
        li "otherLocations"
        li.nested "scrap" do
          li "startAt"
        li.nested "residents" do
          li "riverHawthorne"
          li "simeonAtwood"
          li "leahAtwood"
          li "remainingResidents"
        li "subterraneanBeast"
        li "scenarioReference"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      setUsesGrid
      gather Set.WrittenInRock
      gather Set.HorrorsInTheRock
      gather Set.Refractions
      gather Set.ChillingCold
      gather Set.Ghouls

      setAgendaDeck [Agendas.undergroundSurvey, Agendas.dangerousRide]
      setActDeck [Acts.descentIntoTheMines, Acts.theUndergroundMaze]

      day <- getCampaignDay
      time <- getCampaignTime

      case day of
        Day1 -> do
          gather Set.TheFirstDay
          placeStory $ case time of
            Day -> Stories.dayOne
            Night -> Stories.nightOne
          setAside [Assets.simeonAtwoodDedicatedTroublemaker]
        Day2 -> do
          gather Set.TheSecondDay
          placeStory $ case time of
            Day -> Stories.dayTwo
            Night -> Stories.nightTwo
          setAside [Assets.simeonAtwoodDedicatedTroublemaker]
        Day3 -> do
          gather Set.TheFinalDay
          placeStory $ case time of
            Day -> Stories.dayThree
            Night -> Stories.nightThree
          setAside [Assets.leahAtwoodTheValeCook]

      controlStation <- placeInGrid (Pos 0 0) Locations.controlStation
      placeTokens ScenarioSource controlStation Scrap 1

      caves <- shuffle =<< fromGathered (#location <> withTrait Cave)
      setAside =<< fromGathered #location

      for_ (zip caves [-1, -2, -3, -4]) \(cave, x) -> do
        loc <- placeCardInGrid (Pos x 0) cave
        placeTokens ScenarioSource loc Scrap 1
        when (x == -4) $ startAt loc
        when (x == -2 && day == Day1) $ assetAt_ Assets.riverHawthorneBigInNewYork loc

      when (time == Day) $ removeEvery [Enemies.subterraneanBeast]
      setAside =<< fromGathered (CardFromEncounterSet Set.WrittenInRock)
      setAside =<< fromGathered (cardIs Enemies.crystalParasite)
    _ -> WrittenInRock <$> liftRunMessage msg attrs
