module Arkham.Scenario.Scenarios.TheLostSister (theLostSister) where

import Arkham.Ability.Types (AbilityRef (..))
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (AssetLocation))
import Arkham.Calculation
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modified_, modifySelect)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (LocationPosition))
import Arkham.Matcher
import Arkham.Message (pattern PassedThisSkillTest)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (record, remember, remembered)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLostSister.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Abomination, Cave, Dark))

newtype TheLostSister = TheLostSister ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor TheLostSister where
  getModifiersFor (TheLostSister a) = do
    modifySelect a (assetIs Assets.helenPetersTheEldestSister) [DoNotTakeUpSlot #ally]
    modifySelect
      a
      (InvestigatorAt $ LocationWithTrait Dark)
      [ScenarioModifierValue "time" (toJSON Night)]
    modifySelect a (EnemyAt $ LocationWithTrait Dark) [ScenarioModifierValue "time" (toJSON Night)]
    runMaybeT_ do
      liftGuardM $ remembered TheoIsArguingWithHelen
      liftGuardM $ not <$> getHasRecord TheoReconciledWithHelen
      theo <- MaybeT $ selectOne $ assetIs Assets.theoPetersJackOfAllTrades
      helen <- MaybeT $ selectOne $ assetIs Assets.helenPetersTheEldestSister
      theoLoc <- MaybeT $ field AssetLocation theo
      helenLoc <- MaybeT $ field AssetLocation helen
      guard $ theoLoc == helenLoc
      lift do
        selectEach Anyone \iid -> do
          modified_ a (AbilityTarget iid (AbilityRef (toSource theo) 1)) [IgnoreLimit]

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

      scope "darkLocations" $ flavor $ setTitle "title" >> p "body"
      scope "doubleSidedEnemies" $ flavor $ setTitle "title" >> p "body"

      setUsesGrid

      gather Set.TheLostSister
      gather Set.Blight
      gather Set.HorrorsInTheRock
      gather Set.Mutations
      gather Set.Myconids

      -- do not setScenarioDayAndTime
      day <- getCampaignDay
      time <- getCampaignTime
      gameModifier ScenarioSource ScenarioTarget (ScenarioModifierValue "day" (toJSON day))
      -- We default the time to Day, locations will apply Night to enemies/investigators directly
      gameModifier ScenarioSource ScenarioTarget (ScenarioModifierValue "time" (toJSON Day))

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
        if isEasyStandard attrs then drawAnotherChaosToken iid else failSkillTest
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n | token.face == Cultist -> do
      when (isEasyStandard attrs || n >= 2) do
        healHorrorIfCan iid ScenarioSource 1
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      let entry x = scope x $ flavor $ setTitle "title" >> p.green "body"
      case n of
        4 -> do
          entry "williamHemlock"
          increaseRelationshipLevel WilliamHemlock 1
          interludeXpAll (toBonus "bonus" 1)
        6 -> do
          entry "gideonMizrah"
          remember GideonIsSearchingForAnHeirloom
        8 -> do
          arguing <- remembered TheoIsArguingWithHelen
          theo <- selectJust $ assetIs Assets.theoPetersJackOfAllTrades
          helen <- selectJust $ assetIs Assets.helenPetersTheEldestSister
          theoLoc <- fieldJust AssetLocation theo
          helenLoc <- field AssetLocation helen
          let sameLoc = helenLoc == Just theoLoc
          if arguing && sameLoc
            then do
              scope "theoPeters" $ flavor do
                setTitle "title"
                compose.green do
                  p.validate True "arguing"
                  hr
                  p.validate False "otherwise"
              iids <- select $ InvestigatorAt (LocationWithId theoLoc)
              leadChooseOneM do
                portraits iids \chosen -> do
                  sid <- getRandom
                  beginSkillTest sid chosen attrs attrs #willpower (Fixed 4)
            else do
              scope "theoPeters" $ flavor do
                setTitle "title"
                compose.green do
                  p.validate False "arguing"
                  hr
                  p.validate True "otherwise"
              remember TheoIsArguingWithHelen
        Sigma -> do
          theoReconciled <- getHasRecord TheoReconciledWithHelen
          mTheo <- selectOne $ assetIs Assets.theoPetersJackOfAllTrades
          if theoReconciled
            then do
              scope "lobstrosity" $ flavor do
                setTitle "title"
                compose.green do
                  p.validate True "reconciled"
                  hr
                  p.validate False "otherwise"
              for_ mTheo \theo -> do
                limulusHybrid <-
                  selectJust
                    $ mapOneOf enemyIs [Enemies.limulusHybridInTheLight, Enemies.limulusHybridInTheDark]
                moveTokens source theo limulusHybrid #damage 1
            else do
              scope "lobstrosity" $ flavor do
                setTitle "title"
                compose.green do
                  p.validate False "reconciled"
                  hr
                  p.validate True "otherwise"
              for_ mTheo removeFromGame
        Theta -> do
          entry "drRosaMarquez"
          getScenarioDeck CavernsDeck >>= \case
            [] -> pure ()
            (card : rest) -> do
              focusCards [card] do
                nearestCaves <- select $ NearestLocationTo iid (LocationWithTrait Cave)
                chooseOneM iid do
                  for_ nearestCaves \cave -> do
                    pos <- fieldJust LocationPosition cave
                    emptyPositions <- filterM (selectNone . LocationInPosition) pos.adjacents
                    for_ emptyPositions \emptyPos ->
                      gridLabeled_ emptyPos do
                        setScenarioDeck CavernsDeck rest
                        placeLocationInGrid_ emptyPos card
                  labeled' "bottom" do
                    setScenarioDeck CavernsDeck (rest <> [card])
        _ -> error "invalid codex entry"
      pure s
    PassedThisSkillTest _iid (isSource attrs -> True) -> scope "codex" do
      scope "theoPeters" $ flavor do
        setTitle "title"
        p.green "passed"
      record TheoReconciledWithHelen
      increaseRelationshipLevel TheoPeters 1
      interludeXpAll (toBonus "bonus" 1)
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          interludeXpAll (toBonus "noResolutionBonus" 1)
          push R4
        Resolution 1 -> do
          theoReconciled <- getHasRecord TheoReconciledWithHelen
          resolutionFlavor do
            setTitle "resolution1.title"
            p "resolution1.body"
            ul do
              li.validate theoReconciled "resolution1.proceedToResolution2"
              li.validate (not theoReconciled) "resolution1.skipToResolution3"
          if theoReconciled then push R2 else push R3
        Resolution 2 -> do
          resolution "resolution2"
          interludeXpAll (toBonus "helenBonus" 2)
          record ThePetersFamilyWereReunited
          push R4
        Resolution 3 -> do
          resolution "resolution3"
          interludeXpAll (toBonus "helenBonus" 2)
          record ElizabethPetersWasSaved
          push R4
        Resolution 4 -> do
          gideonSearching <- remembered GideonIsSearchingForAnHeirloom
          clues <- getSum <$> selectAgg Sum InvestigatorClues Anyone
          let gideonBonus =
                if gideonSearching && clues >= 2
                  then toBonus "gideonBonus" 1
                  else mempty
          when (gideonSearching && clues >= 2) do
            increaseRelationshipLevel GideonMizrah 1
            record GideonFoundHisTreasure
          resolutionWithXp "resolution4" $ allGainXpWithBonus' attrs gideonBonus
          record $ AreasSurveyed AkwanShoreline
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheLostSister <$> liftRunMessage msg attrs
