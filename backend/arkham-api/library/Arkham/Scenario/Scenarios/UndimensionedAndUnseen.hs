module Arkham.Scenario.Scenarios.UndimensionedAndUnseen (undimensionedAndUnseen, UndimensionedAndUnseen (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Exception
import Arkham.Helpers.Effect
import Arkham.Helpers.FlavorText
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (ChosenRandomLocation, RevealLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.SkillTest
import Arkham.Trait hiding (Cultist, ElderThing)

newtype UndimensionedAndUnseen = UndimensionedAndUnseen ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undimensionedAndUnseen :: Difficulty -> UndimensionedAndUnseen
undimensionedAndUnseen difficulty =
  scenario
    UndimensionedAndUnseen
    "02236"
    "Undimensioned and Unseen"
    difficulty
    [ ". blastedHeath devilsHopYard"
    , ". blastedHeath devilsHopYard"
    , "dunwichVillage tenAcreMeadow ."
    , "dunwichVillage tenAcreMeadow whateleyRuins"
    , ". coldSpringGlen whateleyRuins"
    , ". coldSpringGlen ."
    ]

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecordedSets =
        mapFromList [(toCampaignLogKey SacrificedToYogSothoth, [recorded @CardCode "02040"])]
    }

instance HasChaosTokenValue UndimensionedAndUnseen where
  getChaosTokenValue iid chaosTokenFace (UndimensionedAndUnseen attrs) =
    case chaosTokenFace of
      Skull -> do
        broodCount <- length <$> getBroodOfYogSothoth
        pure $ toChaosTokenValue attrs Skull broodCount (2 * broodCount)
      Cultist -> pure $ ChaosTokenValue Cultist NoModifier
      Tablet -> pure $ ChaosTokenValue Tablet ZeroModifier
      ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
      otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage UndimensionedAndUnseen where
  runMessage msg s@(UndimensionedAndUnseen attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      storyWithChooseOneM' (h "title" >> p "body") do
        labeled' "calm" $ doStep 1 msg
        labeled' "warn" $ doStep 2 msg
      pure s
    DoStep 1 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "part1"
      record YouCalmedTheTownsfolk
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ h "title" >> p "part2"
      record YouWarnedTheTownsfolk
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure . UndimensionedAndUnseen $ attrs & standaloneCampaignLogL .~ standaloneCampaignLog
    Setup -> runScenarioSetup UndimensionedAndUnseen attrs do
      standalone <- getIsStandalone
      sacrificedToYogSothoth <-
        if standalone
          then pure 3
          else length <$> getRecordSet SacrificedToYogSothoth

      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li.nested "sacrificedToYogSothoth.instructions" do
            li.validate (sacrificedToYogSothoth >= 4) "sacrificedToYogSothoth.fourOrMore"
            li.validate (sacrificedToYogSothoth == 3) "sacrificedToYogSothoth.exactlyThree"
            li.validate (sacrificedToYogSothoth == 2) "sacrificedToYogSothoth.exactlyTwo"
            li.validate (sacrificedToYogSothoth <= 1) "sacrificedToYogSothoth.oneOrFewer"
          li "setAside"
          li "powderOfIbnGhazi"
          li "randomBasicWeakness"
          unscoped $ li "shuffleRemainder"

      scope "choosingARandomLocation" $ flavor do
        setTitle "title"
        p "body"

      gather Set.UndimensionedAndUnseen
      gather Set.Whippoorwills
      gather Set.BeastThralls
      gather Set.Dunwich
      gather Set.StrikingFear

      removeEvery [Enemies.broodOfYogSothoth]

      tenAcreMeadow <- sample2 Locations.tenAcreMeadow_246 Locations.tenAcreMeadow_247
      whateleyRuins <- sample2 Locations.whateleyRuins_250 Locations.whateleyRuins_251
      devilsHopYard <- sample2 Locations.devilsHopYard_252 Locations.devilsHopYard_253

      startAt =<< placeOneOf (Locations.dunwichVillage_242, Locations.dunwichVillage_243)
      coldSpringGlen <- placeOneOf (Locations.coldSpringGlen_244, Locations.coldSpringGlen_245)
      blastedHeath <- placeOneOf (Locations.blastedHeath_248, Locations.blastedHeath_249)
      placeAll [tenAcreMeadow, whateleyRuins, devilsHopYard]

      setAside $ replicate 4 Assets.esotericFormula

      let createBroodAt l = genCard Enemies.broodOfYogSothoth >>= (`createEnemyAt_` l)
      let setAsideBrood n = setAside $ replicate n Enemies.broodOfYogSothoth
      case sacrificedToYogSothoth of
        2 -> do
          createBroodAt coldSpringGlen
          setAsideBrood 3
        3 -> do
          createBroodAt coldSpringGlen
          setAsideBrood 2
        x ->
          if x <= 2
            then do
              createBroodAt coldSpringGlen
              createBroodAt blastedHeath
              setAsideBrood 3
            else setAsideBrood 2

      eachInvestigator \iid -> do
        mcard <- findCardMatch Assets.powderOfIbnGhazi <$> field InvestigatorDeck iid
        for_ mcard $ \card -> do
          chooseOneM iid do
            labeled "Play Powder of Ibn-Ghazi" $ putCardIntoPlay iid card
            labeled "Do no play Powder of Ibn-Ghazi" nothing
        unlessStandalone do
          searchCollectionForRandom iid attrs
            $ BasicWeaknessCard
            <> mapOneOf CardWithTrait [Madness, Injury, Pact]

      setAgendaDeck [Agendas.rampagingCreatures, Agendas.bidingItsTime, Agendas.horrorsUnleashed]
      setActDeck [Acts.saracenicScript, Acts.theyMustBeDestroyed]
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    ResolveChaosToken drawnToken Tablet iid -> do
      builder <- makeEffectBuilder "02236" Nothing (ChaosTokenSource drawnToken) iid
      push $ CreateEffect builder
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      getSkillTestAction >>= \case
        Just action | action `elem` [#evade, #fight] -> do
          getSkillTestTarget >>= \case
            Just (EnemyTarget eid) -> do
              let allBroods = ["02255", "51042", "51043", "51044", "51045"]
              enemyCardCode <- field EnemyCardCode eid
              when (enemyCardCode `elem` allBroods) $ initiateEnemyAttack eid attrs iid
            _ -> pure ()
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> assignDamageAndHorror iid Cultist (if isHardExpert attrs then 1 else 0) 1
        _ -> pure ()
      pure s
    RequestedPlayerCard iid source mcard _ | isSource attrs source -> do
      for_ mcard $ \card -> shuffleCardsIntoDeck iid [PlayerCard card]
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          story $ i18nWithTitle "noResolution"
          do_ R1
        _ -> do_ msg
      pure s
    Do (ScenarioResolution r) -> scope "resolutions" do
      case r of
        Resolution 1 -> do
          let allBroods = ["02255", "51042", "51043", "51044", "51045"]
          broodEscapedIntoTheWild <-
            (+ count ((`elem` allBroods) . toCardCode) attrs.setAside)
              . length
              <$> getBroodOfYogSothoth
          resolutionWithXp "resolution1" $ allGainXp' attrs
          recordCount BroodEscapedIntoTheWild broodEscapedIntoTheWild
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXp' attrs
          record NoBroodEscapedIntoTheWild
        other -> throwIO $ UnknownResolution other

      removeCampaignCard Assets.powderOfIbnGhazi
      endOfScenario
      pure s
    _ -> UndimensionedAndUnseen <$> liftRunMessage msg attrs
