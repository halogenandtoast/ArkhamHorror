module Arkham.Helpers.SkillTest (module X, module Arkham.Helpers.SkillTest) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action
import Arkham.Action qualified as Action
import Arkham.Calculation
import Arkham.Card
import Arkham.ChaosToken
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, popMessageMatching_, pushAfter)
import Arkham.Classes.Query hiding (matches)
import Arkham.CommitRestriction
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.GameEnv as X (getSkillTest, getSkillTestId)
import Arkham.Helpers.Calculation
import Arkham.Helpers.Card
import Arkham.Helpers.Cost
import Arkham.Helpers.Investigator hiding (investigator)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Target as X
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword (Keyword (Peril))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (Message (..), pattern BeginSkillTest)
import Arkham.Name
import Arkham.Projection
import Arkham.Question
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.SkillType
import Arkham.Source
import Arkham.Stats
import Arkham.Target
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window
import Data.Foldable (foldrM)

getBaseValueDifferenceForSkillTest
  :: HasGame m => InvestigatorId -> SkillTest -> m Int
getBaseValueDifferenceForSkillTest iid st = do
  base <- getBaseValueForSkillTest iid st
  difficulty <- getModifiedSkillTestDifficulty st
  pure $ difficulty - base

getBaseValueForSkillTest
  :: HasGame m => InvestigatorId -> SkillTest -> m Int
getBaseValueForSkillTest iid st = getBaseValueForSkillTestType iid st.action st.kind

getBaseValueForSkillTestType
  :: HasGame m => InvestigatorId -> Maybe Action -> SkillTestType -> m Int
getBaseValueForSkillTestType iid mAction = \case
  SkillSkillTest skillType -> baseSkillValueFor skillType mAction iid
  AndSkillTest types -> sum <$> traverse (\skillType -> baseSkillValueFor skillType mAction iid) types
  ResourceSkillTest -> field InvestigatorResources iid
  BaseValueSkillTest x _ -> pure x

inSkillTest :: HasGame m => m Bool
inSkillTest = isJust <$> getSkillTest

getSkillTestRevealedChaosTokens :: HasGame m => m [ChaosToken]
getSkillTestRevealedChaosTokens = maybe [] skillTestRevealedChaosTokens <$> getSkillTest

getSkillTestResolvedChaosTokens :: HasGame m => m [ChaosToken]
getSkillTestResolvedChaosTokens = maybe [] skillTestResolvedChaosTokens <$> getSkillTest

getSkillTestInvestigator :: HasGame m => m (Maybe InvestigatorId)
getSkillTestInvestigator = fmap skillTestInvestigator <$> getSkillTest

isSkillTestInvestigator :: HasGame m => InvestigatorId -> m Bool
isSkillTestInvestigator iid = maybe False (== iid) <$> getSkillTestInvestigator

getSkillTestSource :: HasGame m => m (Maybe Source)
getSkillTestSource = getsSkillTest skillTestSource

isSkillTestSource :: (HasGame m, Sourceable source) => source -> m Bool
isSkillTestSource source = maybe False (isSource source) <$> getSkillTestSource

getSkillTestBaseSkill :: HasGame m => InvestigatorId -> m (Maybe Int)
getSkillTestBaseSkill iid = do
  mSkillTest <- getSkillTest
  case mSkillTest of
    Nothing -> pure Nothing
    Just sTest -> Just <$> getSkillTestBaseSkillForSkillTest iid sTest

getSkillTestBaseSkillForSkillTest :: HasGame m => InvestigatorId -> SkillTest -> m Int
getSkillTestBaseSkillForSkillTest iid sTest =
  getBaseValueForSkillTestType iid (skillTestAction sTest) (skillTestType sTest)

getsSkillTest :: HasGame m => (SkillTest -> a) -> m (Maybe a)
getsSkillTest f = fmap f <$> getSkillTest

getSkillTestAction :: HasGame m => m (Maybe Action)
getSkillTestAction = join <$> getsSkillTest skillTestAction

getSkillTestSkillTypes :: HasGame m => m [SkillType]
getSkillTestSkillTypes =
  getsSkillTest skillTestType <&> \case
    Just (SkillSkillTest skillType) -> [skillType]
    Just (AndSkillTest types) -> types
    Just (BaseValueSkillTest _ _types) -> []
    Just ResourceSkillTest -> []
    Nothing -> []

getSkillTestMatchingSkillIcons :: HasGame m => m (Set SkillIcon)
getSkillTestMatchingSkillIcons = maybe mempty keysSet <$> getsSkillTest skillTestIconValues

isInvestigation :: HasGame m => m Bool
isInvestigation = (== Just #investigate) <$> getSkillTestAction

isInvestigationOf :: HasGame m => LocationMatcher -> m Bool
isInvestigationOf matcher =
  isJust <$> runMaybeT do
    Action.Investigate <- MaybeT getSkillTestAction
    LocationTarget lid <- MaybeT getSkillTestTarget
    liftGuardM $ lid <=~> matcher

isSkillTestAt :: (HasGame m, AsId location, IdOf location ~ LocationId) => location -> m Bool
isSkillTestAt location =
  isJust <$> runMaybeT do
    iid <- MaybeT getSkillTestInvestigator
    liftGuardM $ asId location <=~> locationWithInvestigator iid

isFightWith :: HasGame m => EnemyMatcher -> m Bool
isFightWith matcher =
  isJust <$> runMaybeT do
    Action.Fight <- MaybeT getSkillTestAction
    eid <- hoistMaybe . (.enemy) =<< MaybeT getSkillTestTarget
    liftGuardM $ eid <=~> matcher

isEvadeWith :: HasGame m => EnemyMatcher -> m Bool
isEvadeWith matcher =
  isJust <$> runMaybeT do
    Action.Evade <- MaybeT getSkillTestAction
    eid <- hoistMaybe . (.enemy) =<< MaybeT getSkillTestTarget
    liftGuardM $ eid <=~> matcher

isEvading :: (HasGame m, AsId enemy, IdOf enemy ~ EnemyId) => enemy -> m Bool
isEvading enemy =
  isJust <$> runMaybeT do
    Action.Evade <- MaybeT getSkillTestAction
    eid <- hoistMaybe . (.enemy) =<< MaybeT getSkillTestTarget
    guard $ asId enemy == eid

isFighting :: (HasGame m, AsId enemy, IdOf enemy ~ EnemyId) => enemy -> m Bool
isFighting enemy =
  isJust <$> runMaybeT do
    Action.Fight <- MaybeT getSkillTestAction
    eid <- hoistMaybe . (.enemy) =<< MaybeT getSkillTestTarget
    guard $ asId enemy == eid

isParley :: HasGame m => m Bool
isParley =
  orM
    [ (== Just #parley) <$> getSkillTestAction
    , any (`abilityIs` #parley) <$> getActiveAbilities
    , selectAny $ EventIsAction #parley
    ]

getIsBeingInvestigated
  :: (HasGame m, AsId location, IdOf location ~ LocationId) => location -> m Bool
getIsBeingInvestigated lid = do
  mTarget <- getSkillTestTarget
  mAction <- getSkillTestAction
  pure
    $ mAction
    == Just #investigate
    && maybe False (\target -> Just (asId lid) == target.location) mTarget

revelationSkillTest
  :: Sourceable source
  => SkillTestId
  -> InvestigatorId
  -> source
  -> SkillType
  -> GameCalculation
  -> Message
revelationSkillTest sid iid (toSource -> source) sType calc = RevelationSkillTest sid iid source sType (SkillTestDifficulty calc)

beginSkillTest
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
beginSkillTest sid iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest $ initSkillTest sid iid source target sType (SkillTestDifficulty n)

parley
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
parley sid iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest sid iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just Parley
      }

exploreTest
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
exploreTest sid iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest sid iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just Arkham.Action.Explore
      }

fight
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
fight sid iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest sid iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just Fight
      }

evade
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
evade sid iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest sid iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just Evade
      }

investigate
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
investigate sid iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest sid iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just #investigate
      }

-- NOTE: 100 and 102 are the range for the basic abilities
getIsScenarioAbility :: HasGame m => m Bool
getIsScenarioAbility = do
  source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
  go source
 where
  go = \case
    AbilitySource s n | n < 100 || n > 102 -> go s
    ProxySource inner1 inner2 -> orM [go inner1, go inner2]
    IndexedSource _ inner -> go inner
    EnemySource _ -> pure True
    AgendaSource _ -> pure True
    LocationSource _ -> pure True
    TreacherySource tid ->
      -- If treachery has a subtype then it is a weakness not an encounter card
      isNothing . cdCardSubType <$> field TreacheryCardDef tid
    ActSource _ -> pure True
    _ -> pure False

getAttackedEnemy :: HasGame m => m (Maybe EnemyId)
getAttackedEnemy = getSkillTestTargetedEnemy

getSkillTestTargetedEnemy :: HasGame m => m (Maybe EnemyId)
getSkillTestTargetedEnemy = join . fmap (.enemy) <$> getSkillTestTarget

isInvestigating
  :: (HasGame m, AsId location, IdOf location ~ LocationId) => InvestigatorId -> location -> m Bool
isInvestigating iid location =
  andM
    [ (== Just (asId location)) . join . fmap (.location) <$> getSkillTestTarget
    , (== Just #investigate) <$> getSkillTestAction
    , (== Just iid) <$> getSkillTestInvestigator
    ]

inAttackSkillTest :: HasGame m => m Bool
inAttackSkillTest = (== Just #fight) <$> getSkillTestAction

inEvasionSkillTest :: HasGame m => m Bool
inEvasionSkillTest = (== Just #evade) <$> getSkillTestAction

getIsPerilous :: HasGame m => SkillTest -> m Bool
getIsPerilous skillTest = case skillTestSource skillTest of
  TreacherySource tid -> do
    keywords <- field TreacheryKeywords tid
    pure $ Peril `elem` keywords
  EnemySource eid -> do
    keywords <- field EnemyKeywords eid
    pure $ Peril `elem` keywords
  _ -> pure False

getSkillTestModifiedSkillValue :: HasGame m => m Int
getSkillTestModifiedSkillValue = do
  st <- getJustSkillTest
  currentSkillValue <- getCurrentSkillValue st
  iconCount <- skillIconCount st
  pure $ max 0 (currentSkillValue + iconCount)

getSkillTestDifficulty :: (HasCallStack, HasGame m) => m (Maybe Int)
getSkillTestDifficulty = do
  mSkillTest <- getSkillTest
  case mSkillTest of
    Nothing -> pure Nothing
    Just st -> Just <$> getModifiedSkillTestDifficulty st

getCurrentSkillValue :: HasGame m => SkillTest -> m Int
getCurrentSkillValue st = do
  mods <- getModifiers st.investigator
  case skillTestBaseValue st of
    SkillBaseValue sType -> do
      sType' <- getAlternateSkill st sType
      let canBeIncreased = SkillCannotBeIncreased sType' `notElem` mods
      let
        applyModifier (AnySkillValue m) n | canBeIncreased || m < 0 = max 0 (n + m)
        applyModifier _ n = n
      stats <- modifiedStatsOf (skillTestAction st) (skillTestInvestigator st)
      pure $ foldr applyModifier (statsSkillValue stats sType') mods
    AndSkillBaseValue types -> do
      types' <- traverse (getAlternateSkill st) types
      let canBeIncreased = any (\sType' -> SkillCannotBeIncreased sType' `notElem` mods) types'
      let
        applyModifier (AnySkillValue m) n | canBeIncreased || m < 0 = max 0 (n + m)
        applyModifier _ n = n
      values <- for types' $ \sType -> do
        stats <- modifiedStatsOf (skillTestAction st) (skillTestInvestigator st)
        pure $ statsSkillValue stats sType
      pure $ foldr applyModifier (sum values) mods
    HalfResourcesOf iid -> do
      result <- fieldMap InvestigatorResources (`div` 2) iid
      let
        applyModifier (AnySkillValue m) n = max 0 (n + m)
        applyModifier _ n = n
      pure $ foldr applyModifier result mods
    FixedBaseValue x -> do
      let
        applyModifier (AnySkillValue m) n = max 0 (n + m)
        applyModifier _ n = n
      pure $ foldr applyModifier x mods

skillIconCount :: HasGame m => SkillTest -> m Int
skillIconCount SkillTest {..} = do
  mods <- getModifiers (SkillTestTarget skillTestId)
  let
    addedIcons = foldMap (Sum . toValue) . filter matches $ flip concatMap mods \case
      AddSkillIcons icons -> icons
      _ -> []
  cardIcons <- flip foldMapM (mapToList skillTestCommittedCards) \(iid, cards) -> do
    flip foldMapM cards \card -> do
      icons <- sort . map toValue . filter matches <$> iconsForCard card
      imods <- getModifiers iid
      let less = sum [n | FewerMatchingIconsPerCard n <- imods]
      pure $ foldMap Sum $ drop less icons

  let totalIcons = getSum (cardIcons <> addedIcons)

  case skillTestType of
    SkillSkillTest sType -> do
      investigatorModifiers <- getModifiers skillTestInvestigator
      pure
        $ if SkillCannotBeIncreased sType `elem` investigatorModifiers
          then 0
          else totalIcons
    AndSkillTest types -> do
      investigatorModifiers <- getModifiers skillTestInvestigator
      pure
        $ if any (\sType -> SkillCannotBeIncreased sType `elem` investigatorModifiers) types
          then 0
          else totalIcons
    BaseValueSkillTest _ _types -> pure totalIcons
    ResourceSkillTest -> pure totalIcons
 where
  matches WildMinusIcon = False
  matches icon = member icon skillTestIconValues
  toValue icon = fromMaybe 0 $ lookup icon skillTestIconValues

getAlternateSkill :: HasGame m => SkillTest -> SkillType -> m SkillType
getAlternateSkill st sType = do
  modifiers' <- getModifiers (skillTestInvestigator st)
  pure $ foldr applyModifier sType modifiers'
 where
  applyModifier (UseSkillInsteadOf original replacement) a | original == a = replacement
  applyModifier _ a = a

getModifiedSkillTestDifficulty :: (HasCallStack, HasGame m) => SkillTest -> m Int
getModifiedSkillTestDifficulty s = do
  modifiers' <- getModifiers (SkillTestTarget s.id)
  baseDifficulty <- getBaseSkillTestDifficulty s
  let preModifiedDifficulty = foldr applyPreModifier baseDifficulty modifiers'
  foldrM applyModifier preModifiedDifficulty modifiers'
 where
  applyModifier (Difficulty m) n = pure $ max 0 (n + m)
  applyModifier (CalculatedDifficulty calc) n = do
    m <- calculate calc
    pure $ max 0 (n + m)
  applyModifier DoubleDifficulty n = pure $ n * 2
  applyModifier _ n = pure n
  applyPreModifier (SetDifficulty m) _ = m
  applyPreModifier _ n = n

getBaseSkillTestDifficulty :: (HasGame m, HasCallStack) => SkillTest -> m Int
getBaseSkillTestDifficulty s = go (skillTestDifficulty s)
 where
  go (SkillTestDifficulty c) = calculate c

skillTestLabel
  :: (Sourceable source, Targetable target)
  => Text
  -> SkillType
  -> SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> GameCalculation
  -> UI Message
skillTestLabel lbl sType sid iid source target n = SkillLabelWithLabel lbl sType [beginSkillTest sid iid source target sType n]

pushAfterSkillTest :: HasQueue Message m => Message -> m ()
pushAfterSkillTest = pushAfter \case
  SkillTestEnds {} -> True
  _ -> False

getIsCommittable :: HasGame m => InvestigatorId -> Card -> m Bool
getIsCommittable a c = do
  getSkillTest >>= \case
    Nothing -> pure False
    Just skillTest -> do
      let iid = skillTest.investigator
      modifiers' <- getModifiers a
      mlid <- getMaybeLocation a
      allowedToCommit <-
        if iid /= a
          then do
            cardModifiers <- getModifiers (CardIdTarget $ toCardId c)
            let locationsCardCanBePlayedAt = [matcher | CanCommitToSkillTestPerformedByAnInvestigatorAt matcher <- cardModifiers]
            otherLocation <- field InvestigatorLocation iid
            otherLocationOk <-
              maybe
                (pure False)
                ( \l ->
                    orM
                      [ canCommitToAnotherLocation a l
                      , if notNull locationsCardCanBePlayedAt
                          then l <=~> oneOf locationsCardCanBePlayedAt
                          else pure False
                      ]
                )
                otherLocation
            perilous <- getIsPerilous skillTest
            alreadyCommitted <- fieldMap InvestigatorCommittedCards notNull a
            pure
              $ and
                [ not perilous
                , CannotCommitToOtherInvestigatorsSkillTests `notElem` modifiers'
                , isJust mlid && (mlid == otherLocation || otherLocationOk)
                , not alreadyCommitted || IgnoreCommitOneRestriction `elem` modifiers'
                ]
          else pure True
      if not allowedToCommit
        then pure False
        else do
          allCommittedCards <- selectAll InvestigatorCommittedCards Anyone
          let
            onlyCardCommittedToTest = elem OnlyCardCommittedToTest . cdCommitRestrictions . toCardDef
            onlyCardComittedToTestCommitted = any onlyCardCommittedToTest allCommittedCards
          let cannotCommitCards = CannotCommitCards AnyCard `elem` modifiers'
          if c `elem` allCommittedCards || cannotCommitCards || onlyCardComittedToTestCommitted
            then pure False
            else case c of
              PlayerCard card -> do
                let
                  committedCardTitles = map toTitle allCommittedCards
                  passesCommitRestriction = \case
                    CommittableTreachery -> error "unhandled"
                    AnyCommitRestriction cs -> anyM passesCommitRestriction cs
                    OnlyFightAgainst matcher -> case skillTest.target.enemy of
                      Just eid -> andM [pure $ skillTestAction skillTest == Just #fight, eid <=~> matcher]
                      _ -> pure False
                    OnlyEvasionAgainst matcher -> case skillTestTarget skillTest of
                      EnemyTarget eid -> andM [pure $ skillTestAction skillTest == Just #evade, eid <=~> matcher]
                      _ -> pure False
                    MaxOnePerTest -> pure $ toTitle card `notElem` committedCardTitles
                    OnlyInvestigator matcher -> iid <=~> replaceYouMatcher a matcher
                    OnlyCardCommittedToTest -> pure $ null committedCardTitles
                    OnlyYourTest -> pure $ iid == a
                    OnlyTestDuringYourTurn -> iid <=~> TurnInvestigator
                    OnlyNotYourTest -> pure $ iid /= a
                    MustBeCommittedToYourTest -> pure $ iid == a
                    OnlyIfYourLocationHasClues -> maybe (pure False) (fieldMap LocationClues (> 0)) mlid
                    OnlyTestWithActions as -> pure $ maybe False (`elem` as) (skillTestAction skillTest)
                    ScenarioAbility -> getIsScenarioAbility
                    SelfCanCommitWhen matcher -> notNull <$> select (You <> matcher)
                    MinSkillTestValueDifference n -> do
                      x <- getSkillTestDifficultyDifferenceFromBaseValue a skillTest
                      pure $ x >= n
                  prevented = flip any modifiers' $ \case
                    CanOnlyUseCardsInRole role ->
                      null $ intersect (cdClassSymbols $ toCardDef card) (setFromList [Neutral, role])
                    CannotCommitCards matcher -> cardMatch card matcher
                    _ -> False

                passesCommitRestrictions <- allM passesCommitRestriction (cdCommitRestrictions $ toCardDef card)
                icons <- iconsForCard c
                cmods <- getModifiers (CardIdTarget $ toCardId c)
                let costToCommit = fold [cst | AdditionalCostToCommit iid' cst <- cmods, iid' == a]
                affordable <- getCanAffordCost a (toSource a) [] [] costToCommit
                skillIcons <- getSkillTestMatchingSkillIcons

                pure
                  $ and
                    [ or
                        [ any (`member` skillIcons) icons
                        , and [null icons, cdCanCommitWhenNoIcons (toCardDef card)]
                        ]
                    , not prevented
                    , affordable
                    , passesCommitRestrictions
                    ]
              EncounterCard card -> pure $ CommittableTreachery `elem` cdCommitRestrictions (toCardDef card)
              VengeanceCard _ -> error "vengeance card"

getMustBeCommittableCards :: HasGame m => InvestigatorId -> m [Card]
getMustBeCommittableCards = filterM (`hasModifier` MustBeCommitted) <=< getCommittableCards

getCommittableCards :: HasGame m => InvestigatorId -> m [Card]
getCommittableCards iid = do
  modifiers' <- getModifiers iid
  let asIfInHandForCommit = mapMaybe (preview _CanCommitToSkillTestsAsIfInHand) modifiers'
  hand <- field InvestigatorHand iid
  committableTreacheries <- filterM (field TreacheryCanBeCommitted) =<< select (treacheryInHandOf iid)
  treacheryCards <- traverse (field TreacheryCard) committableTreacheries
  filterM (getIsCommittable iid) (asIfInHandForCommit <> hand <> treacheryCards)

getCommittedCards :: HasGame m => InvestigatorId -> m [Card]
getCommittedCards = field InvestigatorCommittedCards

cancelTokenDraw :: HasQueue Message m => m ()
cancelTokenDraw = do
  let
    removeWindow window = case windowType window of
      Window.WouldRevealChaosToken {} -> True
      _ -> False
  popMessageMatching_ $ \case
    CheckWindows windows' -> any removeWindow windows'
    Do (CheckWindows windows') -> any removeWindow windows'
    _ -> False
  popMessageMatching_ $ \case
    NextChaosBagStep {} -> True
    _ -> False
  popMessageMatching_ $ \case
    RunBag {} -> True
    _ -> False

getSkillTestDifficultyDifferenceFromBaseValue :: HasGame m => InvestigatorId -> SkillTest -> m Int
getSkillTestDifficultyDifferenceFromBaseValue iid skillTest = do
  skillDifficulty <- getModifiedSkillTestDifficulty skillTest
  case skillTestType skillTest of
    SkillSkillTest skillType -> do
      baseValue <- baseSkillValueFor skillType Nothing iid
      pure $ skillDifficulty - baseValue
    AndSkillTest types -> do
      baseValue <- sum <$> traverse (\skillType -> baseSkillValueFor skillType Nothing iid) types
      pure $ skillDifficulty - baseValue
    ResourceSkillTest -> do
      resources <- field InvestigatorResources iid
      pure $ skillDifficulty - resources
    BaseValueSkillTest x _ -> pure $ skillDifficulty - x

withSkillTest :: HasGame m => (SkillTestId -> m ()) -> m ()
withSkillTest = whenJustM getSkillTestId

getCanCancelSkillTestEffects :: HasGame m => m Bool
getCanCancelSkillTestEffects = do
  getSkillTestTarget >>= \case
    Nothing -> pure False
    Just target -> withoutModifier target EffectsCannotBeCanceled
