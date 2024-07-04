module Arkham.Helpers.SkillTest (module X, module Arkham.Helpers.SkillTest) where

import Arkham.Prelude

import Arkham.Action
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
import {-# SOURCE #-} Arkham.GameEnv as X (getSkillTest)
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

getSkillTestRevealedChaosTokens :: HasGame m => m [ChaosToken]
getSkillTestRevealedChaosTokens = maybe [] skillTestRevealedChaosTokens <$> getSkillTest

getSkillTestResolvedChaosTokens :: HasGame m => m [ChaosToken]
getSkillTestResolvedChaosTokens = maybe [] skillTestResolvedChaosTokens <$> getSkillTest

getSkillTestInvestigator :: HasGame m => m (Maybe InvestigatorId)
getSkillTestInvestigator = fmap skillTestInvestigator <$> getSkillTest

getSkillTestSource :: HasGame m => m (Maybe Source)
getSkillTestSource = getsSkillTest skillTestSource

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
    Just ResourceSkillTest -> []
    Nothing -> []

getSkillTestMatchingSkillIcons :: HasGame m => m (Set SkillIcon)
getSkillTestMatchingSkillIcons = maybe mempty keysSet <$> getsSkillTest skillTestIconValues

getIsBeingInvestigated :: HasGame m => LocationId -> m Bool
getIsBeingInvestigated lid = do
  mTarget <- getSkillTestTarget
  mAction <- getSkillTestAction
  pure $ mAction == Just #investigate && mTarget == Just (LocationTarget lid)

revelationSkillTest
  :: Sourceable source
  => InvestigatorId
  -> source
  -> SkillType
  -> GameCalculation
  -> Message
revelationSkillTest iid (toSource -> source) sType calc = RevelationSkillTest iid source sType (SkillTestDifficulty calc)

beginSkillTest
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
beginSkillTest iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest $ initSkillTest iid source target sType (SkillTestDifficulty n)

parley
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
parley iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just Parley
      }

exploreTest
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
exploreTest iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just Arkham.Action.Explore
      }

fight
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
fight iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just Fight
      }

evade
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
evade iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just Evade
      }

investigate
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> GameCalculation
  -> Message
investigate iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest iid source target sType (SkillTestDifficulty n))
      { skillTestAction = Just #investigate
      }

getIsScenarioAbility :: HasGame m => m Bool
getIsScenarioAbility = do
  source <- fromJustNote "damage outside skill test" <$> getSkillTestSource
  case source of
    EnemySource _ -> pure True
    AgendaSource _ -> pure True
    LocationSource _ -> pure True
    TreacherySource tid ->
      -- If treachery has a subtype then it is a weakness not an encounter card
      isNothing . cdCardSubType <$> field TreacheryCardDef tid
    ActSource _ -> pure True
    _ -> pure False

getAttackedEnemy :: HasGame m => m (Maybe EnemyId)
getAttackedEnemy = do
  mTarget <- getSkillTestTarget
  case mTarget of
    Just (EnemyTarget eid) -> pure (Just eid)
    Just (ProxyTarget (EnemyTarget eid) _) -> pure (Just eid)
    Just _ -> pure Nothing
    Nothing -> pure Nothing

isInvestigating :: HasGame m => InvestigatorId -> LocationId -> m Bool
isInvestigating iid lid =
  andM
    [ (== Just (LocationTarget lid)) <$> getSkillTestTarget
    , (== Just #investigate) <$> getSkillTestAction
    , (== Just iid) <$> getSkillTestInvestigator
    ]

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

skillIconCount :: HasGame m => SkillTest -> m Int
skillIconCount SkillTest {..} = do
  mods <- getModifiers SkillTestTarget
  let
    addedIcons = filter matches $ flip concatMap mods \case
      AddSkillIcons icons -> icons
      _ -> []
  totalIcons <-
    foldr ((+) . toValue) 0
      . (<> addedIcons)
      <$> concatMapM iconsForCard (concat $ toList skillTestCommittedCards)
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
  modifiers' <- getModifiers SkillTestTarget
  baseDifficulty <- getBaseSkillTestDifficulty s
  let
    preModifiedDifficulty =
      foldr applyPreModifier baseDifficulty modifiers'
  pure $ foldr applyModifier preModifiedDifficulty modifiers'
 where
  applyModifier (Difficulty m) n = max 0 (n + m)
  applyModifier DoubleDifficulty n = n * 2
  applyModifier _ n = n
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
  -> InvestigatorId
  -> source
  -> target
  -> GameCalculation
  -> UI Message
skillTestLabel lbl sType iid source target n = SkillLabelWithLabel lbl sType [beginSkillTest iid source target sType n]

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
                , not alreadyCommitted
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
                    MaxOnePerTest -> pure $ toTitle card `notElem` committedCardTitles
                    OnlyInvestigator matcher -> iid <=~> matcher
                    OnlyCardCommittedToTest -> pure $ null committedCardTitles
                    OnlyYourTest -> pure $ iid == a
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
                        , and [null icons, toCardType card == SkillType]
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
    RunWindow _ windows' -> any removeWindow windows'
    _ -> False
  popMessageMatching_ $ \case
    NextChaosBagStep {} -> True
    _ -> False
  popMessageMatching_ $ \case
    RunBag {} -> True
    _ -> False
  popMessageMatching_ $ \case
    RunSkillTest {} -> True
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
