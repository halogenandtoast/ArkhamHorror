module Arkham.Helpers.SkillTest where

import Arkham.Prelude

import Arkham.Action
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Classes.Query hiding (matches)
import Arkham.CommitRestriction
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Helpers.Investigator hiding (investigator)
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword (Keyword (Peril))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message (Message (RevelationSkillTest), pattern BeginSkillTest)
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

getBaseValueForSkillTestType
  :: HasGame m => InvestigatorId -> Maybe Action -> SkillTestType -> m Int
getBaseValueForSkillTestType iid mAction = \case
  SkillSkillTest skillType -> baseSkillValueFor skillType mAction [] iid
  AndSkillTest types -> sum <$> traverse (\skillType -> baseSkillValueFor skillType mAction [] iid) types
  ResourceSkillTest -> field InvestigatorResources iid

getSkillTestInvestigator :: HasGame m => m (Maybe InvestigatorId)
getSkillTestInvestigator = fmap skillTestInvestigator <$> getSkillTest

getSkillTestTarget :: HasGame m => m (Maybe Target)
getSkillTestTarget = fmap skillTestTarget <$> getSkillTest

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
  pure $ mAction == Just Investigate && mTarget == Just (LocationTarget lid)

revelationSkillTest
  :: Sourceable source
  => InvestigatorId
  -> source
  -> SkillType
  -> Int
  -> Message
revelationSkillTest iid (toSource -> source) = RevelationSkillTest iid source

beginSkillTest
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> Int
  -> Message
beginSkillTest iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest $ initSkillTest iid source target sType n

parley
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> Int
  -> Message
parley iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest iid source target sType n)
      { skillTestAction = Just Parley
      }

fight
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> Int
  -> Message
fight iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest iid source target sType n)
      { skillTestAction = Just Fight
      }

evade
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> Int
  -> Message
evade iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest iid source target sType n)
      { skillTestAction = Just Evade
      }

investigate
  :: (Sourceable source, Targetable target)
  => InvestigatorId
  -> source
  -> target
  -> SkillType
  -> Int
  -> Message
investigate iid (toSource -> source) (toTarget -> target) sType n =
  BeginSkillTest
    $ (initSkillTest iid source target sType n)
      { skillTestAction = Just Investigate
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
    , (== Just Investigate) <$> getSkillTestAction
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

getSkillTestDifficulty :: HasGame m => m (Maybe Int)
getSkillTestDifficulty = do
  mSkillTest <- getSkillTest
  case mSkillTest of
    Nothing -> pure Nothing
    Just st -> Just <$> getModifiedSkillTestDifficulty st

getCurrentSkillValue :: HasGame m => SkillTest -> m Int
getCurrentSkillValue st = do
  case skillTestBaseValue st of
    SkillBaseValue sType -> do
      sType' <- getAlternateSkill st sType
      stats <- modifiedStatsOf (skillTestAction st) (skillTestInvestigator st)
      pure $ statsSkillValue stats sType'
    AndSkillBaseValue types -> do
      values <- for types $ \sType -> do
        sType' <- getAlternateSkill st sType
        stats <- modifiedStatsOf (skillTestAction st) (skillTestInvestigator st)
        pure $ statsSkillValue stats sType'
      pure $ sum values
    HalfResourcesOf iid -> fieldMap InvestigatorResources (`div` 2) iid
    StaticBaseValue n -> pure n

skillIconCount :: HasGame m => SkillTest -> m Int
skillIconCount SkillTest {..} = do
  mods <- getModifiers SkillTestTarget
  let
    addedIcons = filter matches $ flip concatMap mods \case
      AddSkillIcons icons -> icons
      _ -> []
  totalIcons <-
    foldr ((+) . toValue) 0
      <$> (<> addedIcons)
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

getModifiedSkillTestDifficulty :: HasGame m => SkillTest -> m Int
getModifiedSkillTestDifficulty s = do
  modifiers' <- getModifiers SkillTestTarget
  let
    preModifiedDifficulty =
      foldr applyPreModifier (skillTestDifficulty s) modifiers'
  pure $ foldr applyModifier preModifiedDifficulty modifiers'
 where
  applyModifier (Difficulty m) n = max 0 (n + m)
  applyModifier DoubleDifficulty n = n * 2
  applyModifier _ n = n
  applyPreModifier (SetDifficulty m) _ = m
  applyPreModifier _ n = n

getIsCommittable :: HasGame m => InvestigatorId -> Card -> m Bool
getIsCommittable investigator card = do
  mSkillTest <- getSkillTest
  case mSkillTest of
    Nothing -> pure False
    Just skillTest -> do
      let sInvestigator = skillTestInvestigator skillTest
      modifiers' <- getModifiers (toTarget investigator)
      committedCards <- field InvestigatorCommittedCards sInvestigator
      allCommittedCards <- selectAgg id InvestigatorCommittedCards Anyone
      let
        skillDifficulty = skillTestDifficulty skillTest
        onlyCardComittedToTestCommitted =
          any
            (any (== OnlyCardCommittedToTest) . cdCommitRestrictions . toCardDef)
            allCommittedCards
        committedCardTitles = map toTitle allCommittedCards
      isScenarioAbility <- getIsScenarioAbility
      mLocation <- field InvestigatorLocation investigator
      clueCount <- maybe (pure 0) (field LocationClues) mLocation
      skillIcons <- getSkillTestMatchingSkillIcons
      cannotCommitCards <- elem (CannotCommitCards AnyCard) <$> getModifiers (toTarget investigator)

      if cannotCommitCards || onlyCardComittedToTestCommitted
        then pure False
        else do
          case card of
            PlayerCard pc -> do
              let
                passesCommitRestriction = \case
                  CommittableTreachery -> error "unhandled"
                  OnlyInvestigator matcher -> investigator <=~> matcher
                  OnlyCardCommittedToTest -> pure $ null committedCardTitles
                  MaxOnePerTest -> pure $ toTitle pc `notElem` committedCardTitles
                  OnlyYourTest -> pure True
                  MustBeCommittedToYourTest -> pure True
                  OnlyIfYourLocationHasClues -> pure $ clueCount > 0
                  OnlyTestWithActions as ->
                    pure $ maybe False (`elem` as) (skillTestAction skillTest)
                  ScenarioAbility -> pure isScenarioAbility
                  SelfCanCommitWhen matcher -> notNull <$> select (InvestigatorWithId investigator <> matcher)
                  MinSkillTestValueDifference n ->
                    case skillTestType skillTest of
                      SkillSkillTest skillType -> do
                        baseValue <- baseSkillValueFor skillType Nothing [] investigator
                        pure $ (skillDifficulty - baseValue) >= n
                      AndSkillTest types -> do
                        baseValue <-
                          sum
                            <$> traverse (\skillType -> baseSkillValueFor skillType Nothing [] investigator) types
                        pure $ (skillDifficulty - baseValue) >= n
                      ResourceSkillTest -> do
                        resources <- field InvestigatorResources investigator
                        pure $ (skillDifficulty - resources) >= n
                prevented = flip any modifiers' $ \case
                  CanOnlyUseCardsInRole role ->
                    null
                      $ intersect
                        (cdClassSymbols $ toCardDef pc)
                        (setFromList [Neutral, role])
                  CannotCommitCards matcher -> cardMatch pc matcher
                  _ -> False
              passesCommitRestrictions <- allM passesCommitRestriction (cdCommitRestrictions $ toCardDef pc)

              icons <- iconsForCard card

              pure
                $ card
                `notElem` committedCards
                && (any (`member` skillIcons) icons || (null icons && toCardType pc == SkillType))
                && passesCommitRestrictions
                && not prevented
            EncounterCard ec ->
              pure $ CommittableTreachery `elem` (cdCommitRestrictions $ toCardDef ec)
            VengeanceCard _ -> error "vengeance card"

skillTestLabel
  :: (Sourceable source, Targetable target)
  => Text
  -> SkillType
  -> InvestigatorId
  -> source
  -> target
  -> Int
  -> UI Message
skillTestLabel lbl sType iid source target n = SkillLabelWithLabel lbl sType [beginSkillTest iid source target sType n]
