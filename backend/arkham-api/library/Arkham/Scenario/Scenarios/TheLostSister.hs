module Arkham.Scenario.Scenarios.TheLostSister (theLostSister) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheLostSister.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Abomination, Cave))

newtype TheLostSister = TheLostSister ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheLostSister where
  getModifiersFor (TheLostSister a) = do
    modifySelect a (assetIs Assets.helenPetersTheEldestSister) [DoNotTakeUpSlot #ally]

theLostSister :: Difficulty -> TheLostSister
theLostSister difficulty = scenario TheLostSister "10569" "The Lost Sister" difficulty []

instance HasChaosTokenValue TheLostSister where
  getChaosTokenValue iid tokenFace (TheLostSister attrs) = case tokenFace of
    Skull -> do
      n <- selectCount $ RevealedLocation <> LocationWithTrait Cave
      pure $ toChaosTokenValue attrs Skull (ceiling @Double $ fromIntegral n / 2.0) n
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 1
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
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

      horrorsInTheRockLocations <-
        fmap (drop 2)
          $ shuffle
          =<< fromGathered (CardFromEncounterSet Set.HorrorsInTheRock <> #location)

      lostSisterLocations <-
        fromGathered
          $ CardFromEncounterSet Set.TheLostSister
          <> #location
          <> not_ (cardIs Locations.fungalCave)

      (topThree, cavernsDeck) <- splitAt 3 <$> shuffle (lostSisterLocations <> horrorsInTheRockLocations)

      for_ (zip [Pos (-1) 0, Pos 0 (-1), Pos 1 0] topThree) (uncurry placeCardInGrid_)

      addExtraDeck CavernsDeck cavernsDeck

      setAside
        [ Enemies.limulusHybridInTheLight
        , Enemies.crystalParasite
        , Enemies.crystalParasite
        , Enemies.crustaceanHybridInTheLight
        , Enemies.crustaceanHybridInTheLight
        , Locations.fungalCave
        ]

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
    ResolveChaosToken drawnToken Tablet iid -> do
      atCave <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Cave
      when atCave do
        withSkillTest \sid ->
          skillTestModifier sid Tablet drawnToken
            $ ChangeChaosTokenModifier (NegativeModifier $ if isEasyStandard attrs then 4 else 6)
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      hasAbomination <- selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Abomination
      when hasAbomination do
        if isEasyStandard then drawAnotherChaosToken iid else failSkillTest
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n | token.face == Cultist -> do
      when (isEasyStandard attrs || n >= 2) do
        healHorrorIfCan iid ScenarioSource 1
      pure s
    _ -> TheLostSister <$> liftRunMessage msg attrs
