{- | Guide introductions only. Scenario setup reads the remembered lead/start
keys when its board implementation is added; no encounter cards are spawned here.
-}
module Arkham.Homebrew.NightOfVespers.Intros where

import Arkham.Card
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getPlayerCount)
import Arkham.Homebrew.NightOfVespers.Key
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier (ModifierType (StartingClues))
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey (ScenarioLogKey (HomebrewScenarioLogKey))
import Arkham.Strategy
import Arkham.Trait qualified as Trait
import Arkham.Zone (Zone (FromDeck))

readIntro :: (HasI18n, ReverseQueue m) => Int -> m ()
readIntro n = flavor $ setTitle "intro.title" >> p ("intro.part" <> tshow n)

introStep :: ReverseQueue m => Int -> m ()
introStep n = doStep n PreScenarioSetup

rememberIntro :: ReverseQueue m => Int -> m ()
rememberIntro n = remember $ HomebrewScenarioLogKey ("nightOfVespers.intro" <> tshow n)

chooseLead :: (HasI18n, ReverseQueue m) => [(Int, NightOfVespersKey)] -> m ()
chooseLead leads = do
  available <- filterM (getHasRecord . snd) leads
  storyWithChooseOneM (setTitle "intro.title" >> p.basic "intro.chooseLead") do
    for_ available \(n, _) -> labeled (tshow n) do
      rememberIntro n
      introStep n

runScenarioIntro :: (HasI18n, ReverseQueue m) => ScenarioId -> Int -> m ()
runScenarioIntro sid step = case sid of
  ":night-of-vespers:001" -> case step of
    1 -> do
      readIntro 1
      readers <-
        select
          $ mapOneOf InvestigatorWithTrait [Trait.Criminal, Trait.Detective, Trait.Police, Trait.Reporter]
      unless (null readers) $ leadChooseOneM do
        questionLabeled "reader"
        targets readers \iid -> do
          storyOnlyBuild [iid] $ setTitle "intro.title" >> p "intro.underworld" >> p.basic "intro.bonus"
          n <- getPlayerCount
          setupModifier ScenarioSource iid $ StartingClues n
      introStep 2
    _ -> readIntro 2
  ":night-of-vespers:037" -> case step of
    1 -> do
      readIntro 1
      finlay <- getHasRecord FinlayODoyleIsAssistingTheInvestigation
      let branch = if finlay then 2 else 3
      rememberIntro branch
      introStep branch
    _ -> readIntro step
  ":night-of-vespers:075" -> case step of
    1 -> do
      readIntro 1
      gigi <- getHasRecord GigiGiulianoIsAssistingTheInvestigation
      let branch = if gigi then 2 else 3
      rememberIntro branch
      introStep branch
    _ -> readIntro step
  ":night-of-vespers:105" -> case step of
    1 -> do
      readIntro 1
      readers <- select $ mapOneOf InvestigatorWithTrait [Trait.Dreamer, Trait.Sorcerer, Trait.Witch]
      unless (null readers) $ leadChooseOneM do
        questionLabeled "reader"
        targets readers \iid -> do
          storyOnlyBuild [iid] $ setTitle "intro.title" >> p "intro.dejaVu" >> p.basic "intro.bonus"
          -- The guide says put into play, not play: defer the search so the
          -- selected card bypasses playing restrictions and additional costs.
          search
            iid
            ScenarioSource
            iid
            [(FromDeck, ShuffleBackIn)]
            (basic $ #asset <> CardFillsSlot #arcane)
            (defer ScenarioTarget IsNotDraw)
      introStep 2
    _ -> readIntro 2
  ":night-of-vespers:136" -> case step of
    1 -> do
      readIntro 1
      chooseLead
        [ (2, YouLearnedAboutTheTenebrarumMurders)
        , (3, YouWereInvitedToJoinTheTenebrarumLodge)
        , (4, DrArmitageTaughtYouAboutTheThreeMothers)
        , (5, AnnaKaslowDirectedYouTowardAnOldColleague)
        , (6, MaddalenaGuidesYourPath)
        ]
    7 -> readIntro 7 >> pEffect "companion"
    n -> do
      readIntro n
      when (n `elem` [2, 3]) $ pEffect ("lead" <> tshow n)
      whenM (getHasRecord FinlayODoyleIsAssistingTheInvestigation) $ introStep 7
  ":night-of-vespers:168" -> case step of
    1 -> do
      readIntro 1
      chooseLead
        [ (2, YouLearnedWhereToFindHelenaNussbaum)
        , (3, YouReadSgtMandelsFile)
        , (4, DrArmitageTaughtYouAboutTheThreeMothers)
        , (5, AnnaKaslowDirectedYouTowardAnOldColleague)
        , (6, MaddalenaGuidesYourPath)
        ]
    6 -> do
      readIntro 6
      storyWithChooseOneM (setTitle "intro.title" >> p.basic "intro.chooseStart") do
        for_
          [ ("city", "nightOfVespers.startCity")
          , ("mountain", "nightOfVespers.startMountain")
          , ("forest", "nightOfVespers.startForest")
          ]
          \(label, key) ->
            labeled label $ remember $ HomebrewScenarioLogKey key
    n -> do
      readIntro n
      when (n `elem` [2, 3]) $ pEffect ("lead" <> tshow n)
      introStep 6
  ":night-of-vespers:214" -> case step of
    1 -> do
      readIntro 1
      chooseLead
        [ (2, YouHadAPropheticDream)
        , (3, YouDiscoveredAnaGiulianosConnectionToAmaliaVerdi)
        , (4, DrArmitageTaughtYouAboutTheThreeMothers)
        , (5, AnnaKaslowDirectedYouTowardAnOldColleague)
        , (6, MaddalenaGuidesYourPath)
        ]
    6 -> do
      readIntro 6
      whenM (getHasRecord GigiGiulianoIsAssistingTheInvestigation) $ introStep 7
    7 -> readIntro 7 >> pEffect "companion"
    n -> do
      readIntro n
      when (n `elem` [2, 3]) $ pEffect ("lead" <> tshow n)
      introStep 6
  ":night-of-vespers:257" -> readIntro step >> when (step == 1) (introStep 2)
  _ -> pure ()
 where
  pEffect key = flavor $ setTitle "intro.title" >> p.basic ("intro.effects." <> key)

{- | The Teachings of Aradia's optional asset. Returning zero targets (an empty
deck or no arcane asset) leaves a normal empty search, not an empty prompt.
-}
resolveIntroSearch :: (HasI18n, ReverseQueue m) => InvestigatorId -> [Card] -> m ()
resolveIntroSearch iid cards = unless (null cards) $ focusCards cards do
  chooseOneM iid do
    targets cards $ putCardIntoPlay iid
    labeled "declineAsset" nothing
