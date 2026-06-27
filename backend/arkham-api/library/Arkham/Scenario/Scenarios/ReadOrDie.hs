module Arkham.Scenario.Scenarios.ReadOrDie (readOrDie, ReadOrDie (..)) where

import Arkham.Id
import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTargetedEnemy)
import Arkham.Helpers.Xp (getInitialVictory)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (assetAt, enemyAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted hiding ((.=))
import Arkham.Scenario.Types (setAsideCardsL)
import Arkham.Scenarios.ReadOrDie.Helpers
import Arkham.Trait (Trait (Tome))
import Arkham.Xp (XpBreakdown (..), XpEntry (..))
import Control.Lens (use, (.=))

{- FOURMOLU_DISABLE -}
easyTokens, standardTokens, hardTokens, expertTokens :: [ChaosTokenFace]
easyTokens =
  [ PlusOne , PlusOne , Zero , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree
  , MinusFour , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree , MinusFour , MinusFive
  , MinusSix , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
expertTokens =
  [ Zero , MinusOne , MinusTwo , MinusThree , MinusFour , MinusFive , MinusSix , MinusSeven
  , MinusEight , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

newtype ReadOrDie = ReadOrDie ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

readOrDie :: Difficulty -> ReadOrDie
readOrDie difficulty =
  sideStory
    ReadOrDie
    "90004"
    "Read or Die"
    difficulty
    [ "triangle plus    hourglass"
    , "square   diamond circle"
    , ".        equals  t"
    ]

-- Since Dormitories and Faculty Offices start the game in play and Alchemy
-- Labs is removed from the game, ignore any Forced abilities that would put
-- these locations into play.
instance HasModifiersFor ReadOrDie where
  getModifiersFor (ReadOrDie a) = do
    locations <-
      select
        $ mapOneOf
          locationIs
          [Locations.studentUnion, Locations.scienceBuilding, Locations.administrationBuilding]
    let matchers = [AbilityIsForcedAbility <> AbilityIs (toSource l) 1 | l <- locations]
    unless (null matchers) do
      modifySelect a Anyone [CannotTriggerAbilityMatching (AbilityOneOf matchers)]

instance HasChaosTokenValue ReadOrDie where
  getChaosTokenValue iid tokenFace (ReadOrDie attrs) = case tokenFace of
    Skull -> do
      n <- selectCount $ AssetWithTrait Tome <> AssetControlledBy daisyWalker
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
    otherFace -> getChaosTokenValue iid otherFace attrs

gainXpWithTomeBonus :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> m Int
gainXpWithTomeBonus attrs = do
  tomes <- selectCount $ AssetWithTrait Tome <> AssetControlledBy daisyWalker
  victory <- getInitialVictory
  mDaisy <- selectOne (IncludeEliminated daisyWalker)
  case mDaisy of
    Just daisy | tomes > victory -> do
      -- Daisy earns experience equal to her Tome assets in play, *or* the
      -- victory display, whichever is higher. When the Tomes win she takes that
      -- value instead of the victory points, so exclude her from the shared
      -- victory-display experience and attribute her full total to the Tomes.
      others <- filter (/= daisy) <$> select InvestigatorCanGainXp
      let
        excludeDaisyFromVictory (XpBreakdown entries) =
          XpBreakdown $ flip concatMap entries \case
            AllGainXp detail -> [InvestigatorGainXp iid detail | iid <- others]
            InvestigatorGainXp iid _ | iid == daisy -> []
            entry -> [entry]
      void $ allGainXpEdit' attrs excludeDaisyFromVictory (second $ filter ((/= daisy) . fst))
      gainXp daisy (toSource attrs) (ikey "xp.tomes") tomes
    _ -> void $ allGainXp' attrs
  pure victory

namerOfTheDeadAttacks :: ReverseQueue m => InvestigatorId -> m ()
namerOfTheDeadAttacks iid = void $ runMaybeT do
  action <- MaybeT getSkillTestAction
  guard $ action `elem` [#fight, #evade]
  eid <- MaybeT getSkillTestTargetedEnemy
  liftGuardM $ eid <=~> enemyIs Enemies.namerOfTheDead
  lift $ initiateEnemyAttackEdit eid Tablet iid despiteExhausted

instance RunMessage ReadOrDie where
  runMessage msg s@(ReadOrDie attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" do
        h "title"
        p "body"
      whenJustM (selectOne daisyWalker) \daisy -> do
        flavor $ scope "investigatorSetup" do
          h "title"
          p "body"
        tomes <- fieldMap InvestigatorDeck (map toCard . filter (`cardMatch` nonWeaknessTomeAsset) . unDeck) daisy
        push $ SetAsideCards tomes
      pure s
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> easyTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> expertTokens
      pure s
    Setup -> runScenarioSetup ReadOrDie attrs do
      setup $ ul do
        li "gatherSets"
        li "newCards"
        li "removeCards"
        li.nested "placeLocations" do
          li "daisyWalker"
          li "otherInvestigators"
          li "ignoreForced"
        li "placeTomes"
        li "jazzMulligan"
        li "namerOfTheDead"
        li "drHenryArmitage"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.ReadOrDie
      gather Set.ExtracurricularActivity
      gather Set.Sorcery
      gather Set.TheBeyond
      gather Set.BishopsThralls
      gather Set.Whippoorwills
      gather Set.AncientEvils
      gather Set.LockedDoors
      gather Set.AgentsOfYogSothoth
      gather Set.ArmitagesFate

      removeEvery
        [ Locations.facultyOfficesTheHourIsLate
        , Locations.alchemyLabs
        , Assets.alchemicalConcoction
        , Enemies.theExperiment
        , Assets.professorWarrenRice
        ]

      setAgendaDeck [Agendas.mortalInquiry]
      setActDeck [Acts.speedReading]

      quad <- place Locations.miskatonicQuad
      orneLibrary <- place Locations.orneLibrary
      scienceBuilding <- place Locations.scienceBuilding
      humanitiesBuilding <- place Locations.humanitiesBuilding
      studentUnion <- place Locations.studentUnion
      administrationBuilding <- place Locations.administrationBuilding
      dormitories <- place Locations.dormitories
      facultyOffices <- place Locations.facultyOfficesTheNightIsStillYoung

      namer <- enemyAt Enemies.namerOfTheDead orneLibrary
      exhaustWith attrs namer

      investigators <- allInvestigators
      mDaisy <- selectOne daisyWalker
      for_ mDaisy \daisy -> do
        chooseOneM daisy do
          targeting orneLibrary do
            reveal orneLibrary
            moveTo_ attrs daisy orneLibrary

      let quadInvestigators = maybe investigators (`deleteFirst` investigators) mDaisy
      unless (null quadInvestigators) do
        lead <- getLead
        chooseOneM lead do
          targeting quad do
            reveal quad
            for_ quadInvestigators \iid -> moveTo_ attrs iid quad

      tomes <- shuffle =<< traverse (setFacedown True) =<< use (attrsL . setAsideCardsL)
      attrsL . setAsideCardsL .= []
      unless (null tomes) do
        -- These locations have only been queued for placement, so connection
        -- based distance queries cannot see them yet. Use the printed layout to
        -- order locations by distance from the Orne Library (farthest first),
        -- randomizing within ties.
        ordered <-
          concat <$> traverse shuffle
            [ [dormitories, facultyOffices]
            , [scienceBuilding, studentUnion, administrationBuilding]
            , [humanitiesBuilding]
            ]
        let (perLocation, rest) = splitAt (length ordered) tomes
        for_ (zip ordered perLocation) \(lid, card) -> placeUnderneath lid [card]
        unless (null rest) $ placeUnderneath orneLibrary rest

      assetAt_ Assets.jazzMulligan scienceBuilding

      for_ mDaisy \daisy -> do
        mFromDeck <- runMaybeT do
          owner <- MaybeT $ getOwner Assets.drHenryArmitage
          inDeck <- lift $ fieldMap InvestigatorDeck (map toCard . unDeck) owner
          hand <- lift $ field InvestigatorHand owner
          card <- hoistMaybe $ find ((== Assets.drHenryArmitage) . toCardDef) (inDeck <> hand)
          pure (owner, card)
        case mFromDeck of
          Just (owner, card) -> do
            excludeFromEncounterDeck [Assets.drHenryArmitage]
            obtainCard card
            shuffleDeck owner
            push $ TakeControlOfSetAsideAsset daisy card
          Nothing -> beginWithStoryAsset daisy Assets.drHenryArmitage
    ResolveChaosToken _ Cultist iid -> do
      drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> discardTopOfDeck iid Cultist $ if isEasyStandard attrs then 2 else 3
        Tablet -> namerOfTheDeadAttacks iid
        ElderThing -> selectEach daisyWalker \daisy -> assignHorror daisy ElderThing 1
        _ -> pure ()
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n | token.face == Tablet -> do
      when (n < if isEasyStandard attrs then 2 else 3) $ namerOfTheDeadAttacks iid
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          push R2
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ gainXpWithTomeBonus attrs
          whenJustM (selectOne $ IncludeEliminated daisyWalker) \daisy -> do
            hasToteBag <- hasInPool daisy Assets.daisysToteBag
            hasAdvancedNecronomicon <- hasInPool daisy Assets.theNecronomiconAdvanced
            chooseOneM daisy do
              questionLabeled' "maySwapSignatureCards"
              when hasToteBag do
                labeled' "upgradeDaisysToteBag" do
                  removeCampaignCardFromDeck daisy Assets.daisysToteBag
                  addCampaignCardToDeck daisy DoNotShuffleIn Assets.daisysToteBagAdvanced
              when hasAdvancedNecronomicon do
                labeled' "downgradeTheNecronomicon" do
                  removeCampaignCardFromDeck daisy Assets.theNecronomiconAdvanced
                  addCampaignCardToDeck daisy DoNotShuffleIn Assets.theNecronomicon
              labeled' "doNotSwap" nothing
          endOfScenario
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ gainXpWithTomeBonus attrs
          whenJustM (selectOne $ IncludeEliminated daisyWalker) \daisy -> do
            hasNecronomicon <- hasInPool daisy Assets.theNecronomicon
            hasAdvancedToteBag <- hasInPool daisy Assets.daisysToteBagAdvanced
            when (hasNecronomicon || hasAdvancedToteBag) do
              chooseOrRunOneM daisy do
                questionLabeled' "mustSwapSignatureCards"
                when hasNecronomicon do
                  labeled' "upgradeTheNecronomicon" do
                    removeCampaignCardFromDeck daisy Assets.theNecronomicon
                    addCampaignCardToDeck daisy DoNotShuffleIn Assets.theNecronomiconAdvanced
                when hasAdvancedToteBag do
                  labeled' "downgradeDaisysToteBag" do
                    removeCampaignCardFromDeck daisy Assets.daisysToteBagAdvanced
                    addCampaignCardToDeck daisy DoNotShuffleIn Assets.daisysToteBag
          endOfScenario
        other -> throwIO $ UnknownResolution other
      pure s
    _ -> ReadOrDie <$> liftRunMessage msg attrs
