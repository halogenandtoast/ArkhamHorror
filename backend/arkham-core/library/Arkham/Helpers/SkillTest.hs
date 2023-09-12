module Arkham.Helpers.SkillTest where

import Arkham.Prelude

import Arkham.Action
import Arkham.Card.CardDef
import Arkham.Enemy.Types (Field (..))
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Helpers.Investigator
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Keyword (Keyword (Peril))
import Arkham.Message (Message (BeginSkillTest, RevelationSkillTest))
import Arkham.Projection
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

getsSkillTest :: HasGame m => (SkillTest -> a) -> m (Maybe a)
getsSkillTest f = fmap f <$> getSkillTest

getSkillTestAction :: HasGame m => m (Maybe Action)
getSkillTestAction = join <$> getsSkillTest skillTestAction

getSkillTestSkillTypes :: HasGame m => m [SkillType]
getSkillTestSkillTypes =
  getsSkillTest skillTestType <&> \case
    Just (SkillSkillTest skillType) -> [skillType]
    _ -> []

getSkillTestMatchingSkillIcons :: HasGame m => m (Set SkillIcon)
getSkillTestMatchingSkillIcons =
  getsSkillTest skillTestType <&> \case
    Just stType -> case stType of
      SkillSkillTest skillType -> setFromList [#wildMinus, #wild, SkillIcon skillType]
      AndSkillTest types -> setFromList $ #wildMinus : #wild : map SkillIcon types
      ResourceSkillTest -> setFromList [#wildMinus, #wild]
    _ -> mempty

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
  totalIcons <-
    count matches
      <$> concatMapM
        iconsForCard
        (concat $ toList skillTestCommittedCards)
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
  matches WildIcon = True
  matches (SkillIcon s) = case skillTestType of
    SkillSkillTest sType -> s == sType
    AndSkillTest types -> s `elem` types
    ResourceSkillTest -> False

getAlternateSkill :: HasGame m => SkillTest -> SkillType -> m SkillType
getAlternateSkill st sType = do
  modifiers' <- getModifiers (skillTestInvestigator st)
  pure $ foldr applyModifier sType modifiers'
 where
  applyModifier (UseSkillInsteadOf original replacement) a | original == a = replacement
  applyModifier _ a = a
