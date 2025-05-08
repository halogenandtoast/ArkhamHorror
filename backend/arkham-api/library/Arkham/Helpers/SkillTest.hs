module Arkham.Helpers.SkillTest (module X, module Arkham.Helpers.SkillTest) where

import {-# SOURCE #-} Arkham.GameEnv as X (getSkillTest, getSkillTestId)
import Arkham.Helpers.SkillTest.Target as X

import Arkham.Ability
import Arkham.Action
import Arkham.Action qualified as Action
import Arkham.Calculation
import Arkham.Card
import Arkham.ChaosToken.Types
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, popMessageMatching_, pushAfter)
import Arkham.Classes.Query hiding (matches)
import Arkham.Classes.Query qualified as Query
import Arkham.CommitRestriction
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Action
import Arkham.Helpers.Calculation
import Arkham.Helpers.Card
import Arkham.Helpers.Cost
import Arkham.Helpers.GameValue
import Arkham.Helpers.Investigator hiding (investigator)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Ref (sourceToCard)
import Arkham.Helpers.Source
import Arkham.Helpers.Target
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword (Keyword (Peril))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message (Message (..), pattern BeginSkillTest)
import Arkham.Modifier
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Question
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.SkillTestResult
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
isSkillTestInvestigator iid = (== Just iid) <$> getSkillTestInvestigator

getSkillTestSource :: HasGame m => m (Maybe Source)
getSkillTestSource = getsSkillTest skillTestSource

getSkillTestAbilitySource :: HasGame m => m (Maybe Source)
getSkillTestAbilitySource = runMaybeT do
  source <- MaybeT getSkillTestSource
  case source of
    AbilitySource {} -> pure source
    UseAbilitySource _ a b -> pure $ AbilitySource a b
    _ -> mzero

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
getSkillTestMatchingSkillIcons = getSkillTest >>= \case
  Nothing -> pure mempty
  Just st -> do
    mods <- getModifiers st.investigator
    pure $ setFromList $ foldr applyModifiers (keys $ skillTestIconValues st) mods
 where
  applyModifiers (UseSkillInsteadOf x y) = map (\z -> if z == SkillIcon x then SkillIcon y else z)
  applyModifiers _ = id

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
    , selectAny $ ActiveEvent <> EventIsAction #parley
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

beginSkillTestEdit
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> (SkillTest -> SkillTest)
  -> Message
beginSkillTestEdit sid iid (toSource -> source) (toTarget -> target) sType n f =
  BeginSkillTest $ f $ initSkillTest sid iid source target sType (SkillTestDifficulty n)

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
    UseAbilitySource _ s n | n < 100 || n > 102 -> go s
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
getSkillTestTargetedEnemy = ((.enemy) =<<) <$> getSkillTestTarget

isInvestigating
  :: (HasGame m, AsId location, IdOf location ~ LocationId) => InvestigatorId -> location -> m Bool
isInvestigating iid location =
  andM
    [ (== Just (asId location)) . ((.location) =<<) <$> getSkillTestTarget
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
    keywords <- fromMaybe mempty <$> fieldMay TreacheryKeywords tid
    pure $ Peril `elem` keywords
  EnemySource eid -> do
    keywords <- fromMaybe mempty <$> fieldMay EnemyKeywords eid
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
  let
    applyModifier canIncrease (AnySkillValue m) n | canIncrease || m < 0 = pure $ max 0 (n + m)
    applyModifier canIncrease (AnySkillValueCalculated m) n = do
      v <- calculate m
      pure $ if canIncrease || v < 0 then max 0 (n + v) else n
    applyModifier _ _ n = pure n
  case skillTestBaseValue st of
    SkillBaseValue sType -> do
      sType' <- getAlternateSkill st sType
      let canIncrease = SkillCannotBeIncreased sType' `notElem` mods
      stats <- modifiedStatsOf st.action st.investigator
      foldrM (applyModifier canIncrease) (statsSkillValue stats sType') mods
    AndSkillBaseValue types -> do
      types' <- traverse (getAlternateSkill st) types
      let canIncrease = any (\sType' -> SkillCannotBeIncreased sType' `notElem` mods) types'
      values <- for types' $ \sType -> do
        stats <- modifiedStatsOf st.action st.investigator
        pure $ statsSkillValue stats sType
      foldrM (applyModifier canIncrease) (sum values) mods
    HalfResourcesOf iid -> do
      result <- fieldMap InvestigatorResources (`div` 2) iid
      foldrM (applyModifier True) result mods
    FixedBaseValue x -> foldrM (applyModifier True) x mods

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
  let doubledDifficulty = foldr applyDoubler preModifiedDifficulty modifiers'
  max 0 <$> foldrM applyModifier doubledDifficulty modifiers'
 where
  applyModifier (Difficulty m) n = pure $ n + m
  applyModifier (CalculatedDifficulty calc) n = do
    m <- calculate calc
    pure $ n + m
  applyModifier _ n = pure n
  applyDoubler DoubleDifficulty n = n * 2
  applyDoubler _ n = n
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
            sameLocation <- maybe (pure False) (\x -> (mlid == Just x &&) <$> withoutModifier x CountsAsDifferentLocation) otherLocation
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
                , isJust mlid && (sameLocation || otherLocationOk)
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
                    OnlySkillTestSource matcher -> sourceMatches skillTest.source matcher
                    OnlySkillTest matcher -> skillTestMatches iid skillTest.source skillTest matcher
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
                otherAdditionalCosts <- fold <$> for allCommittedCards \c' -> do
                  mods <- getModifiers c'
                  pure $ fold [cst | AdditionalCostToCommit iid' cst <- mods, iid' == a]
                cmods <- getModifiers (CardIdTarget $ toCardId c)
                let costToCommit = fold [cst | AdditionalCostToCommit iid' cst <- cmods, iid' == a]
                affordable <- getCanAffordCost a (toSource a) [] [] (costToCommit <> otherAdditionalCosts)
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
      Window.WouldRevealChaosTokens {} -> True
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

skillTestMatches
  :: HasGame m
  => InvestigatorId
  -> Source
  -> SkillTest
  -> Matcher.SkillTestMatcher
  -> m Bool
skillTestMatches iid source st mtchr = case Matcher.replaceYouMatcher iid mtchr of
  Matcher.PerilousSkillTest -> getIsPerilous st
  Matcher.IfSkillTestMatcher p thenMatcher elseMatcher -> do
    p' <- skillTestMatches iid source st p
    skillTestMatches iid source st $ if p' then thenMatcher else elseMatcher
  Matcher.SkillTestWithDifficulty gv ->
    getSkillTestDifficulty >>= \case
      Nothing -> pure False
      Just n -> gameValueMatches n gv
  Matcher.SkillTestOnEncounterCard -> skillTestSource st `sourceMatches` Matcher.EncounterCardSource
  Matcher.NotSkillTest matcher ->
    not <$> skillTestMatches iid source st matcher
  Matcher.AnySkillTest -> pure True
  Matcher.SkillTestWasFailed -> pure $ case skillTestResult st of
    FailedBy _ _ -> True
    _ -> False
  Matcher.YourSkillTest matcher ->
    liftA2
      (&&)
      (pure $ skillTestInvestigator st == iid)
      (skillTestMatches iid source st matcher)
  Matcher.UsingThis -> pure $ case skillTestSource st of
    AbilitySource (ProxySource _ s) _ -> s == source
    AbilitySource s _ -> s == source
    UseAbilitySource _ (ProxySource _ s) _ -> s == source
    UseAbilitySource _ s _ -> s == source
    ProxySource (CardIdSource _) s -> s == source
    ProxySource _ s -> s == source
    IndexedSource _ s -> s == source
    s -> s == source
  Matcher.SkillTestSourceMatches sourceMatcher ->
    sourceMatches (skillTestSource st) sourceMatcher
  Matcher.SkillTestBeforeRevealingChaosTokens ->
    pure $ null $ skillTestRevealedChaosTokens st
  Matcher.SkillTestWithRevealedChaosToken matcher ->
    anyM (`Query.matches` Matcher.IncludeSealed matcher)
      $ skillTestRevealedChaosTokens st
  Matcher.SkillTestWithRevealedChaosTokenCount n matcher ->
    (>= n)
      <$> countM
        (`Query.matches` Matcher.IncludeSealed matcher)
        (skillTestRevealedChaosTokens st)
  Matcher.SkillTestOnCardWithTrait t -> elem t <$> sourceTraits (skillTestSource st)
  Matcher.SkillTestOnCard match -> (`cardMatch` match) <$> sourceToCard (skillTestSource st)
  Matcher.SkillTestOnLocation match -> case skillTestSource st of
    AbilitySource s n | n < 100 -> case s.location of
      Just lid -> lid <=~> match
      Nothing -> pure False
    UseAbilitySource _ s n | n < 100 -> case s.location of
      Just lid -> lid <=~> match
      Nothing -> pure False
    _ -> pure False
  Matcher.SkillTestWithResolvedChaosTokenBy whoMatcher matcher -> do
    iids <- select whoMatcher
    anyM (`Query.matches` Matcher.IncludeSealed matcher)
      . filter (maybe False (`elem` iids) . chaosTokenRevealedBy)
      $ skillTestRevealedChaosTokens st
  Matcher.SkillTestFromRevelation -> pure $ skillTestIsRevelation st
  Matcher.SkillTestWithAction actionMatcher -> case skillTestAction st of
    Just action -> actionMatches iid action actionMatcher
    Nothing -> pure False
  Matcher.WhileInvestigating locationMatcher -> case skillTestAction st of
    Just Action.Investigate -> case skillTestTarget st of
      LocationTarget lid -> elem lid <$> select locationMatcher
      ProxyTarget (LocationTarget lid) _ ->
        elem lid <$> select locationMatcher
      BothTarget (LocationTarget lid1) (LocationTarget lid2) -> do
        selectAny $ locationMatcher <> Matcher.mapOneOf Matcher.LocationWithId [lid1, lid2]
      _ -> pure False
    _ -> pure False
  Matcher.SkillTestOnTreachery treacheryMatcher -> case st.source.treachery of
    Just tid -> elem tid <$> select treacheryMatcher
    _ -> pure False
  Matcher.SkillTestOnAsset assetMatcher -> case st.source.asset of
    Just aid -> elem aid <$> select assetMatcher
    _ -> pure False
  Matcher.SkillTestOnEvent eventMatcher -> case st.source.event of
    Just eid -> elem eid <$> select eventMatcher
    _ -> pure False
  Matcher.WhileAttackingAnEnemy enemyMatcher -> case skillTestAction st of
    Just Action.Fight -> case st.target.enemy of
      Just eid -> elem eid <$> select enemyMatcher
      _ -> pure False
    _ -> pure False
  Matcher.WhileEvadingAnEnemy enemyMatcher -> case skillTestAction st of
    Just Action.Evade -> case st.target.enemy of
      Just eid -> elem eid <$> select enemyMatcher
      _ -> pure False
    _ -> pure False
  Matcher.WhileParleyingWithAnEnemy enemyMatcher ->
    case st.target.enemy of
      Just eid -> andM [isParley, elem eid <$> select enemyMatcher]
      _ -> pure False
  Matcher.WhileParleying -> isParley
  Matcher.SkillTestWithSkill sk -> selectAny sk
  Matcher.SkillTestWithSkillType sType -> pure $ case skillTestType st of
    SkillSkillTest sType' -> sType' == sType
    AndSkillTest types -> sType `elem` types
    ResourceSkillTest -> False
    BaseValueSkillTest _ _ -> False
  Matcher.SkillTestAtYourLocation -> do
    canAffectOthers <- withoutModifier iid CannotAffectOtherPlayersWithPlayerEffectsExceptDamage
    mlid1 <- field InvestigatorLocation iid
    mlid2 <- field InvestigatorLocation st.investigator
    case (mlid1, mlid2) of
      (Just lid1, Just lid2) ->
        pure $ lid1 == lid2 && (canAffectOthers || iid == st.investigator)
      _ -> pure False
  Matcher.SkillTestAt locationMatcher -> targetMatches st.target (Matcher.TargetAtLocation locationMatcher)
  Matcher.SkillTestOfInvestigator whoMatcher -> st.investigator <=~> whoMatcher
  Matcher.SkillTestMatches ms -> allM (skillTestMatches iid source st) ms
  Matcher.SkillTestOneOf ms -> anyM (skillTestMatches iid source st) ms

skillTestValueMatches
  :: HasGame m
  => InvestigatorId
  -> Maybe Action
  -> SkillTestType
  -> Matcher.SkillTestValueMatcher
  -> m Bool
skillTestValueMatches iid maction skillTestType = \case
  Matcher.AnySkillTestValue -> pure True
  Matcher.SkillTestGameValue valueMatcher -> do
    maybe (pure False) (`gameValueMatches` valueMatcher) =<< getSkillTestDifficulty
  Matcher.GreaterThanBaseValue -> do
    getSkillTestDifficulty >>= \case
      Nothing -> pure False
      Just n -> case skillTestType of
        SkillSkillTest skillType -> do
          baseSkill <- baseSkillValueFor skillType maction iid
          pure $ n > baseSkill
        AndSkillTest types -> do
          baseSkill <- sum <$> traverse (\skillType -> baseSkillValueFor skillType maction iid) types
          pure $ n > baseSkill
        ResourceSkillTest -> do
          resources <- field InvestigatorResources iid
          pure $ n > resources
        BaseValueSkillTest x _ -> pure $ n > x

onSucceedByEffect
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> ValueMatcher
  -> source
  -> target
  -> [Message]
  -> Message
onSucceedByEffect sid matchr source target msgs = CreateOnSucceedByEffect sid matchr (toSource source) (toTarget target) msgs

onFailedByEffect
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> ValueMatcher
  -> source
  -> target
  -> [Message]
  -> Message
onFailedByEffect sid matchr source target msgs = CreateOnFailedByEffect sid matchr (toSource source) (toTarget target) msgs

onNextTurnEffect
  :: (Sourceable source, AsId investigator, IdOf investigator ~ InvestigatorId)
  => source
  -> investigator
  -> [Message]
  -> Message
onNextTurnEffect source investigator msgs = CreateOnNextTurnEffect (toSource source) (asId investigator) msgs

