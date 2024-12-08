module Arkham.Scenario.Scenarios.ThreadsOfFate (ThreadsOfFate (..), threadsOfFate) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence qualified as Act
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Card hiding (addCampaignCardToDeckChoice)
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice)
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ThreadsOfFate.Helpers
import Arkham.Scenarios.ThreadsOfFate.Story
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries
import Data.IntMap.Strict qualified as IntMap

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
      doom <- getDoomCount
      pure $ toChaosTokenValue attrs Skull n doom
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

instance RunMessage ThreadsOfFate where
  runMessage msg s@(ThreadsOfFate attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      gaveCustodyToHarlan <- getHasRecord TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
      story intro1
      storyWithChooseOneM (if gaveCustodyToHarlan then intro3 else intro2) do
        labeled "“You’re not going anywhere until you tell me what is going on.” - Skip to Intro 4."
          $ doStep 4 msg
        labeled "“Have it your way.” - Skip to Intro 5." $ doStep 5 msg
      pure s
    DoStep 4 PreScenarioSetup -> do
      remember YouListenedToIchtacasTale
      unlessStandalone $ addChaosToken Cultist
      pure s
    DoStep 5 PreScenarioSetup -> do
      remember IchtacaLeftWithoutYou
      whenHasRecord TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone $ doStep 6 PreScenarioSetup
      pure s
    DoStep 6 PreScenarioSetup -> do
      storyWithChooseOneM intro6 do
        labeled "“We should be wary of them.”" do
          record YouAreForgingYourOwnWay
          unlessStandalone do
            removeAllChaosTokens Cultist
            removeAllChaosTokens Tablet
            addChaosToken ElderThing
        labeled "“Maybe I should listen to them after all...”" nothing
      pure s
    StandaloneSetup -> do
      lead <- getLead
      setChaosTokens standaloneChaosTokens
      chooseOneM lead do
        labeled
          "The investigators gave custody of the relic to Alejandro."
          $ record TheInvestigatorsGaveCustodyOfTheRelicToAlejandro
        labeled
          "The investigators gave custody of the relic to Harlan Earnstone."
          $ record TheInvestigatorsGaveCustodyOfTheRelicToHarlanEarnstone
      pure s
    Setup -> runScenarioSetup ThreadsOfFate attrs do
      gather Set.ThreadsOfFate
      gather Set.PnakoticBrotherhood
      gather Set.LockedDoors
      gather Set.Nightgaunts
      gather Set.DarkCult
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
            pure
              [ Acts.harlanIsInDanger
              , harlansCurse
              , Acts.findTheRelic
              , Acts.recoverTheRelic
              ]
          else do
            atTheExhibit <- sample2 Acts.atTheExhibitTheRelicsLocation Acts.atTheExhibitTheBrotherhoodsPlot
            pure
              [ Acts.theRelicIsMissing
              , atTheExhibit
              , Acts.findTheRelic
              , Acts.recoverTheRelic
              ]

      setAside
        [ Locations.townHall
        , Assets.ichtacaTheForgottenGuardian
        , Assets.expeditionJournal
        , Assets.relicOfAgesADeviceOfSomeSort
        , Assets.alejandroVela
        ]

      setAgendaDeck
        [ Agendas.threeFates
        , Agendas.behindTheCurtain
        , Agendas.hiddenEntanglements
        ]

      removeCampaignCard Assets.relicOfAgesADeviceOfSomeSort
      removeCampaignCard Assets.alejandroVela

      lead <- getLead
      act2Deck1 <- do
        atTheStation <- sample2 Acts.atTheStationInShadowedTalons Acts.atTheStationTrainTracks
        genCards
          [ Acts.missingPersons
          , atTheStation
          , Acts.alejandrosPrison
          , Acts.alejandrosPlight
          ]
      act2Deck2 <- do
        friendsInHighPlaces <-
          sample2 Acts.friendsInHighPlacesHenrysInformation Acts.friendsInHighPlacesHenryDeveau
        genCards
          [ Acts.searchForAlejandro
          , friendsInHighPlaces
          , Acts.alejandrosPrison
          , Acts.alejandrosPlight
          ]
      chooseOneM lead do
        labeled "Go to the police to inform them of Alejandro's disappearance"
          $ push
          $ SetActDeckCards 2 act2Deck1
        labeled "Look for Alejandro on your own" $ push $ SetActDeckCards 2 act2Deck2

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
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case chaosTokenFace token of
        Cultist | isEasyStandard attrs && n < 1 -> assignDamage iid Cultist 1
        Cultist | isHardExpert attrs && n < 2 -> directDamage iid Cultist 1
        Tablet | isEasyStandard attrs && n < 1 -> do
          es <- selectTargets $ NearestEnemyTo iid (EnemyWithTrait Trait.Cultist)
          chooseOrRunOneM iid do
            targets es \target -> placeDoom Tablet target 1
        Tablet | isHardExpert attrs && n < 2 -> do
          es <- selectTargets $ NearestEnemyTo iid (EnemyWithTrait Trait.Cultist)
          for_ es \target -> placeDoom Tablet target 1
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist | isEasyStandard attrs -> assignDamage iid Cultist 1
        Cultist | isHardExpert attrs -> directDamage iid Cultist 1
        Tablet | isEasyStandard attrs -> do
          es <- selectTargets $ NearestEnemyTo iid (EnemyWithTrait Trait.Cultist)
          chooseOrRunOneM iid do
            targets es \target -> placeDoom Tablet target 1
        Tablet | isHardExpert attrs -> do
          es <- selectTargets $ NearestEnemyTo iid (EnemyWithTrait Trait.Cultist)
          for_ es \target -> placeDoom Tablet target 1
        ElderThing -> removeTokens ElderThing iid #clue 1
        _ -> pure ()
      pure s
    ScenarioResolution _ -> do
      -- because we have multiple acts we might have an act that triggered the
      -- resolution and would not be counted so we need to determine that as
      -- well

      let counted x = if x then 1 else 0
      actPairCountMap <-
        IntMap.fromList
          <$> sequence
            [ (1,) . counted <$> selectAny (ActWithSide Act.B)
            , (2,) . counted <$> selectAny (ActWithSide Act.D)
            , (3,) . counted <$> selectAny (ActWithSide Act.F)
            ]
      let
        completedStack n =
          (== 3)
            . (+ findWithDefault 0 n actPairCountMap)
            . length
            . fromMaybe []
            $ lookup n attrs.completedActStack

        act3bCompleted = completedStack 1
        act3dCompleted = completedStack 2
        act3fCompleted = completedStack 3
        act1sCompleted = length $ keys attrs.completedActStack

      iids <- allInvestigators
      lead <- getLead

      story resolution1

      record $ if act3bCompleted then TheInvestigatorsFoundTheMissingRelic else TheRelicIsMissing

      relicOwned <- getIsAlreadyOwned Assets.relicOfAgesADeviceOfSomeSort
      when (act3bCompleted && not relicOwned) do
        addCampaignCardToDeckChoice iids Assets.relicOfAgesADeviceOfSomeSort

      unless act3bCompleted do
        removeCampaignCard Assets.relicOfAgesADeviceOfSomeSort

      record $ if act3dCompleted then TheInvestigatorsRescuedAlejandro else AlejandroIsMissing

      alejandroOwned <- getIsAlreadyOwned Assets.alejandroVela
      when (act3dCompleted && not alejandroOwned) do
        addCampaignCardToDeckChoice iids Assets.alejandroVela

      unless act3dCompleted do
        removeCampaignCard Assets.alejandroVela

      record $ if act3fCompleted then TheInvestigatorsForgedABondWithIchtaca else IchtacaIsInTheDark

      when act3fCompleted do
        addCampaignCardToDeckChoice iids Assets.ichtacaTheForgottenGuardian

      addCampaignCardToDeckChoice [lead] Assets.expeditionJournal
      allGainXpWithBonus attrs $ toBonus "bonus" act1sCompleted
      endOfScenario
      pure s
    _ -> ThreadsOfFate <$> liftRunMessage msg attrs
