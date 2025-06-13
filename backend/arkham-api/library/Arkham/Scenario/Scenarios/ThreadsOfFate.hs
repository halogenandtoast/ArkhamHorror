module Arkham.Scenario.Scenarios.ThreadsOfFate (setupThreadsOfFate, threadsOfFate, ThreadsOfFate (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as Act
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Campaigns.TheForgottenAge.Meta
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card hiding (addCampaignCardToDeckChoice, forceAddCampaignCardToDeckChoice)
import Arkham.Helpers.Doom
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

newtype ThreadsOfFate = ThreadsOfFate ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

threadsOfFate :: Difficulty -> ThreadsOfFate
threadsOfFate difficulty =
  scenarioWith
    ThreadsOfFate
    "04113"
    "Threads of Fate"
    difficulty
    [ ".                .                trainTracks  trainTracks          townHall             townHall  arkhamPoliceStation  arkhamPoliceStation .           ."
    , "curiositieShoppe curiositieShoppe northside    northside            downtown             downtown  easttown             easttown            velmasDiner velmasDiner"
    , ".                eztliExhibit     eztliExhibit miskatonicUniversity miskatonicUniversity rivertown rivertown            blackCave           blackCave   ."
    ]
    (decksLayoutL .~ [". act1", "agenda1 act2", ". act3"])

instance HasChaosTokenValue ThreadsOfFate where
  getChaosTokenValue iid chaosTokenFace (ThreadsOfFate attrs) = case chaosTokenFace of
    Skull -> do
      n <- fieldMax EnemyDoom (EnemyWithTrait Trait.Cultist)
      toChaosTokenValue attrs Skull n <$> getDoomCount
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 2
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , Zero
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFive
  , Skull
  , Skull
  , Cultist
  , ElderThing
  , AutoFail
  , ElderSign
  ]

setupThreadsOfFate :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupThreadsOfFate _attrs = do
  setup do
    ul do
      li "gatherSets"
      li "beforeDrawingOpeningHands"
      li "placeLocations"
      li "setAside"
      li "actDecks"
      unscoped $ li "shuffleRemainder"

  scope "threeActsThreeThreads" $ flavor do
    setTitle "title"
    h "title"
    p "body"

  whenReturnTo $ gather Set.ReturnToThreadsOfFate
  gather Set.ThreadsOfFate
  gather Set.PnakoticBrotherhood
  gather Set.LockedDoors
  gather Set.Nightgaunts
  gather Set.DarkCult `orWhenReturnTo` gather Set.CultOfPnakotus
  gatherJust Set.TheMidnightMasks [Treacheries.huntingShadow, Treacheries.falseLead]

  startAt =<< place Locations.rivertown
  placeAll
    [ Locations.northside
    , Locations.downtownFirstBankOfArkham
    , Locations.easttown
    , Locations.miskatonicUniversity
    , Locations.velmasDiner
    , Locations.curiositieShoppe
    ]

  gaveCustodyToHarlan <- getHasRecord TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
  setActDeckN 1 =<< do
    if gaveCustodyToHarlan
      then do
        harlansCurse <- sample2 Acts.harlansCurseSafekeeping Acts.harlansCurseHarlanEarnstone
        pure [Acts.harlanIsInDanger, harlansCurse, Acts.findTheRelic, Acts.recoverTheRelic]
      else do
        atTheExhibit <- sample2 Acts.atTheExhibitTheRelicsLocation Acts.atTheExhibitTheBrotherhoodsPlot
        pure [Acts.theRelicIsMissing, atTheExhibit, Acts.findTheRelic, Acts.recoverTheRelic]

  setAside
    [ Locations.townHall
    , Assets.ichtacaTheForgottenGuardian
    , Assets.expeditionJournal
    ]

  whenReturnTo $ setAside [Assets.vedaWhitsleySkilledBotanist]

  findCard (`cardMatch` cardIs Assets.alejandroVela) >>= \case
    Nothing -> setAside [Assets.alejandroVela]
    Just card -> setAside [card]

  findCard (`cardMatch` cardIs Assets.relicOfAgesADeviceOfSomeSort) >>= \case
    Nothing -> setAside [Assets.relicOfAgesADeviceOfSomeSort]
    Just card -> setAside [card]

  setAgendaDeck
    [ Agendas.threeFates
    , Agendas.behindTheCurtain
    , Agendas.hiddenEntanglements
    ]

  lead <- getLead
  act2Deck1 <- do
    atTheStation <- sample2 Acts.atTheStationInShadowedTalons Acts.atTheStationTrainTracks
    genCards [Acts.missingPersons, atTheStation, Acts.alejandrosPrison, Acts.alejandrosPlight]
  act2Deck2 <- do
    friendsInHighPlaces <-
      sample2 Acts.friendsInHighPlacesHenrysInformation Acts.friendsInHighPlacesHenryDeveau
    genCards
      [Acts.searchForAlejandro, friendsInHighPlaces, Acts.alejandrosPrison, Acts.alejandrosPlight]
  chooseOneM lead $ scope "setup" do
    labeled' "goToThePolice" $ push $ SetActDeckCards 2 act2Deck1
    labeled' "lookOnYourOwn" $ push $ SetActDeckCards 2 act2Deck2

  listenedToIchtacasTale <- remembered YouListenedToIchtacasTale
  setActDeckN 3
    =<< if listenedToIchtacasTale
      then do
        strangeRelics <- sample2 Acts.strangeRelicsMariaDeSilva Acts.strangeRelicsMariasInformation
        pure
          [ Acts.theGuardiansInquiry
          , strangeRelics
          , Acts.strangeOccurences
          , Acts.theBrotherhoodIsRevealed
          ]
      else do
        theCaveOfDarkness <-
          sample2 Acts.theCaveOfDarknessEmbroiledInBattle Acts.theCaveOfDarknessTunnelsInTheDark
        pure
          [ Acts.trialOfTheHuntress
          , theCaveOfDarkness
          , Acts.strangeOccurences
          , Acts.theBrotherhoodIsRevealed
          ]
  whenReturnTo $ leadChooseOneM $ scope "setup" do
    questionLabeled' "chooseActDeck4"
    questionLabeledCard (CardCode "53028b")
    labeled' "findSource" $ doStep 1 Setup
    labeled' "findRoot" $ doStep 2 Setup

instance RunMessage ThreadsOfFate where
  runMessage msg s@(ThreadsOfFate attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      traverse_ obtainCard =<< findCard (`cardMatch` cardIs Assets.relicOfAgesADeviceOfSomeSort)
      traverse_ obtainCard =<< findCard (`cardMatch` cardIs Assets.alejandroVela)
      gaveCustodyToHarlan <- getHasRecord TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
      storyWithContinue' (h "title" >> p "intro1")
      doStep (if gaveCustodyToHarlan then 3 else 2) PreScenarioSetup
      isReturnTo <- Arkham.Helpers.Scenario.getIsReturnTo
      when isReturnTo $ doStep 7 PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (h "title" >> p "intro2") do
        labeled' "skipToIntro4" $ doStep 4 PreScenarioSetup
        labeled' "skipToIntro5" $ doStep 5 PreScenarioSetup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (h "title" >> p "intro3") do
        labeled' "skipToIntro4" $ doStep 4 PreScenarioSetup
        labeled' "skipToIntro5" $ doStep 5 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      storyWithContinue' (h "title" >> p "intro4")
      remember YouListenedToIchtacasTale
      unlessStandalone $ addChaosToken Cultist
      pure s
    DoStep 5 PreScenarioSetup -> scope "intro" do
      storyWithContinue' (h "title" >> p "intro5")
      remember IchtacaLeftWithoutYou
      whenHasRecord TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone $ doStep 6 PreScenarioSetup
      pure s
    DoStep 6 PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (h "title" >> p "intro6") do
        labeled' "beWary" do
          record YouAreForgingYourOwnWay
          unlessStandalone do
            removeAllChaosTokens Cultist
            removeAllChaosTokens Tablet
            addChaosToken ElderThing
        labeled' "listen" nothing
      pure s
    DoStep 7 PreScenarioSetup -> scope "intro" do
      storyWithContinue' (h "title" >> p "intro7")
      pure s
    StandaloneSetup -> scope "standalone" do
      lead <- getLead
      setChaosTokens standaloneChaosTokens
      chooseOneM lead do
        labeled' "gaveCustodyToAlejandro" $ record TheInvestigatorsGaveCustodyOfTheRelicToAlejandro
        labeled' "gaveCustodyToHarlan" $ record TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
      pure s
    Setup -> runScenarioSetup ThreadsOfFate attrs $ setupThreadsOfFate attrs
    DoStep 1 Setup -> do
      seekingTrouble <- sample2 Acts.seekingTroubleSentFromAnotherTime Acts.seekingTroubleLoadingDocks
      Msg.setActDeckN
        4
        [Acts.searchForTheSource, seekingTrouble, Acts.discoverTheTruth, Acts.impossiblePursuit]
      push SetActDeck
      pure s
    DoStep 2 Setup -> do
      overgrownEstate <-
        sample2 Acts.theOvergrownEstateSentFromAnotherTime Acts.theOvergrownEstateClintonFreeman
      Msg.setActDeckN
        4
        [Acts.searchForTheMeaning, overgrownEstate, Acts.discoverTheTruth, Acts.impossiblePursuit]
      push SetActDeck
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case chaosTokenFace token of
        Cultist | isEasyStandard attrs && n < 1 -> assignDamage iid Cultist 1
        Cultist | isHardExpert attrs && n < 2 -> directDamage iid Cultist 1
        Tablet | isEasyStandard attrs && n < 1 -> do
          es <- selectTargets $ NearestEnemyToFallback iid (EnemyWithTrait Trait.Cultist)
          chooseOrRunOneM iid do
            targets es \target -> placeDoom Tablet target 1
        Tablet | isHardExpert attrs && n < 2 -> do
          es <- selectTargets $ NearestEnemyToFallback iid (EnemyWithTrait Trait.Cultist)
          for_ es \target -> placeDoom Tablet target 1
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist | isEasyStandard attrs -> assignDamage iid Cultist 1
        Cultist | isHardExpert attrs -> directDamage iid Cultist 1
        Tablet | isEasyStandard attrs -> do
          es <- selectTargets $ NearestEnemyToFallback iid (EnemyWithTrait Trait.Cultist)
          chooseOrRunOneM iid $ targets es \target -> placeDoom Tablet target 1
        Tablet | isHardExpert attrs -> do
          es <- selectTargets $ NearestEnemyToFallback iid (EnemyWithTrait Trait.Cultist)
          for_ es \target -> placeDoom Tablet target 1
        ElderThing -> removeTokens ElderThing iid #clue 1
        _ -> pure ()
      pure s
    ScenarioResolution _ -> scope "resolutions" do
      -- because we have multiple acts we might have an act that triggered the
      -- resolution and would not be counted so we need to determine that as
      -- well

      let counted x = if x then 1 else 0
      actPairCountMap :: IntMap Int <-
        IntMap.fromList
          <$> sequence
            [ (1,) . counted <$> selectAny (ActWithSide Act.B)
            , (2,) . counted <$> selectAny (ActWithSide Act.D)
            , (3,) . counted <$> selectAny (ActWithSide Act.F)
            , (4,) . counted <$> selectAny (ActWithSide Act.H)
            ]
      let
        completedStack :: Int -> Bool
        completedStack n =
          (== 3)
            . (+ findWithDefault 0 n actPairCountMap)
            . length
            . fromMaybe []
            $ lookup n attrs.completedActStack

        act3bCompleted = completedStack 1
        act3dCompleted = completedStack 2
        act3fCompleted = completedStack 3
        act3hCompleted = completedStack 4
        act1sCompleted = length $ keys attrs.completedActStack

      iids <- allInvestigators
      lead <- getLead

      record $ if act3bCompleted then TheInvestigatorsFoundTheMissingRelic else TheRelicIsMissing
      record $ if act3dCompleted then TheInvestigatorsRescuedAlejandro else AlejandroIsMissing
      record $ if act3fCompleted then TheInvestigatorsForgedABondWithIchtaca else IchtacaIsInTheDark
      recordWhen act3hCompleted TheInvestigatorsRecruitedTheHelpOfAnotherExpedition

      unless act3bCompleted $ removeCampaignCard Assets.relicOfAgesADeviceOfSomeSort
      unless act3dCompleted $ removeCampaignCard Assets.alejandroVela

      forgingYourOwnPath <- getHasRecord YouAreForgingYourOwnWay
      alejandroOwned <- getIsAlreadyOwned Assets.alejandroVela

      isReturnTo <- Arkham.Helpers.Scenario.getIsReturnTo
      if isReturnTo
        then do
          meta <- getCampaignMeta @Metadata
          let completedCount =
                (+ sum (IntMap.elems actPairCountMap)) . sum . map length $ toList attrs.completedActStack
          push $ SetCampaignMeta $ toJSON $ meta {bonusXp = Map.fromList $ map (,completedCount) iids}
          resolutionWithXp "resolution1"
            $ allGainXpWithBonus' attrs
            $ mconcat
            $ [toBonus "forgingYourOwnPath" 2 | act3dCompleted && not alejandroOwned && forgingYourOwnPath]
            <> [toBonus "forgingYourOwnPath" 2 | act3fCompleted && forgingYourOwnPath]
        else do
          resolutionWithXp "resolution1"
            $ allGainXpWithBonus' attrs
            $ mconcat
            $ toBonus "bonus" act1sCompleted
            : [toBonus "forgingYourOwnPath" 2 | act3dCompleted && not alejandroOwned && forgingYourOwnPath]
              <> [toBonus "forgingYourOwnPath" 2 | act3fCompleted && forgingYourOwnPath]

      relicOwned <- getIsAlreadyOwned Assets.relicOfAgesADeviceOfSomeSort
      when (act3bCompleted && not relicOwned) do
        forceAddCampaignCardToDeckChoice iids DoNotShuffleIn Assets.relicOfAgesADeviceOfSomeSort

      when (act3dCompleted && not alejandroOwned && not forgingYourOwnPath) do
        addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.alejandroVela

      when (act3fCompleted && not forgingYourOwnPath) do
        addCampaignCardToDeckChoice iids DoNotShuffleIn Assets.ichtacaTheForgottenGuardian

      addCampaignCardToDeckChoice [lead] DoNotShuffleIn Assets.expeditionJournal
      endOfScenario
      pure s
    _ -> ThreadsOfFate <$> liftRunMessage msg attrs
