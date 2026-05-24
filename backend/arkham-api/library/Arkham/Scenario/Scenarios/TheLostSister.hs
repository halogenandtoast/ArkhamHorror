module Arkham.Scenario.Scenarios.TheLostSister (theLostSister) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheLostSister.Helpers
import Arkham.Story.Cards qualified as Stories

newtype TheLostSister = TheLostSister ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLostSister :: Difficulty -> TheLostSister
theLostSister difficulty = scenario TheLostSister "10569" "The Lost Sister" difficulty []

instance HasChaosTokenValue TheLostSister where
  getChaosTokenValue iid tokenFace (TheLostSister attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheLostSister where
  runMessage msg s@(TheLostSister attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      day <- getCampaignDay
      time <- getCampaignTime
      let isNight = time == Night
      story $ i18nWithTitle "intro1"
      flavor do
        p.basic "body"
        ul $ li.nested.validate isNight "nightSkip" do
          li.validate (not isNight && day == Day1) "day1"
          li.validate (not isNight && day == Day2) "day2"
          li.validate (not isNight && day == Day3) "day3"
      case (day, time) of
        (Day1, Day) -> story $ i18nWithTitle "intro2"
        (Day2, Day) -> story $ i18nWithTitle "intro3"
        (Day3, Day) -> story $ i18nWithTitle "intro4"
        _ -> story $ i18nWithTitle "intro5"
      pure s
    Setup -> runScenarioSetup TheLostSister attrs do
      setup $ ul do
        li "gatherSets"
        li "currentDaySet"
        li "currentDayMarker"
        li "putAkwan"
        li.nested "cavernsDeck" do
          li "gatherCavernsLocations"
          li "setAsideFungalCave"
          li "removeTwoCaves"
          li "shuffleCavernsDeck"
          li "putTopThree"
        li.nested "dayResidents" do
          li "helenPeters"
          li "theoPeters"
          li "gideonMizrah"
          li "williamHemlock"
          li "removeResidents"
        li.nested "nightResidents" do
          li "helenPetersNight"
          li "removeResidentsNight"
        li "setAsideEnemies"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      setUsesGrid

      gather Set.TheLostSister
      gather Set.Blight
      gather Set.HorrorsInTheRock
      gather Set.Mutations
      gather Set.Myconids

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

      setAgendaDeck [Agendas.intoTheCaves, Agendas.darknessClosesIn]
      setActDeck [Acts.theMissingSibling, Acts.onTheTrail, Acts.faceToCarapace]

      akwan <- placeInGrid (Pos 0 0) Locations.akwan
      startAt akwan

      -- Construct the Caverns deck
      setAside [Locations.fungalCave]

      horrorsInTheRockLocations <-
        fromGathered (CardFromEncounterSet Set.HorrorsInTheRock <> #location)
      toRemove <- sampleListN 2 horrorsInTheRockLocations
      let remainingHorrorsLocations = filter (`notElem` toRemove) horrorsInTheRockLocations

      lostSisterLocations <-
        fromGathered (CardFromEncounterSet Set.TheLostSister <> #location)

      cavernsCards <- shuffle (lostSisterLocations <> remainingHorrorsLocations)
      let (topThree, rest) = splitAt 3 cavernsCards

      placeCardInGrid_ (Pos (-1) 1) =<< maybe (error "empty caverns deck") pure (topThree !!? 0)
      placeCardInGrid_ (Pos 0 1) =<< maybe (error "empty caverns deck") pure (topThree !!? 1)
      placeCardInGrid_ (Pos 1 1) =<< maybe (error "empty caverns deck") pure (topThree !!? 2)

      addExtraDeck CavernsDeck rest

      -- Set aside enemies
      setAside
        [ Enemies.limulusHybridInTheLight
        , Enemies.crystalParasite
        , Enemies.crystalParasite
        , Enemies.crustaceanHybridInTheLight
        , Enemies.crustaceanHybridInTheLight
        ]

      -- Residents
      investigators <- allInvestigators
      when (time == Day) do
        helenPeters <- createAsset =<< genCard Assets.helenPetersTheEldestSister
        leadChooseOneM do
          unscoped
            $ nameVar Assets.helenPetersTheEldestSister
            $ questionLabeled' "chooseInvestigatorToTakeControlOf"
          questionLabeledCard Assets.helenPetersTheEldestSister
          portraits investigators (`takeControlOfAsset` helenPeters)

        when (day == Day1 || day == Day2) do
          theo <- createAsset =<< genCard Assets.theoPetersJackOfAllTrades
          leadChooseOneM do
            unscoped
              $ nameVar Assets.theoPetersJackOfAllTrades
              $ questionLabeled' "chooseInvestigatorToTakeControlOf"
            questionLabeledCard Assets.theoPetersJackOfAllTrades
            portraits investigators (`takeControlOfAsset` theo)

        when (day == Day2 || day == Day3) do
          gideon <- createAsset =<< genCard Assets.gideonMizrahSeasonedSailor
          leadChooseOneM do
            unscoped
              $ nameVar Assets.gideonMizrahSeasonedSailor
              $ questionLabeled' "chooseInvestigatorToTakeControlOf"
            questionLabeledCard Assets.gideonMizrahSeasonedSailor
            portraits investigators (`takeControlOfAsset` gideon)

        when (day == Day3) do
          assetAt_ Assets.williamHemlockAspiringPoet akwan

      when (time == Night) do
        helenPeters <- createAsset =<< genCard Assets.helenPetersTheEldestSister
        leadChooseOneM do
          unscoped
            $ nameVar Assets.helenPetersTheEldestSister
            $ questionLabeled' "chooseInvestigatorToTakeControlOf"
          questionLabeledCard Assets.helenPetersTheEldestSister
          portraits investigators (`takeControlOfAsset` helenPeters)

    _ -> TheLostSister <$> liftRunMessage msg attrs
