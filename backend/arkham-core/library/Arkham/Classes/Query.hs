module Arkham.Classes.Query where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.Entity
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Projection
import Data.HashSet qualified as HashSet

type family QueryElement a where
  QueryElement AssetMatcher = AssetId
  QueryElement InvestigatorMatcher = InvestigatorId
  QueryElement PreyMatcher = InvestigatorId
  QueryElement LocationMatcher = LocationId
  QueryElement EnemyMatcher = EnemyId
  QueryElement VoidEnemyMatcher = EnemyId
  QueryElement TreacheryMatcher = TreacheryId
  QueryElement ExtendedCardMatcher = Card
  QueryElement DiscardedPlayerCardMatcher = PlayerCard
  QueryElement AbilityMatcher = Ability
  QueryElement SkillMatcher = SkillId
  QueryElement EventMatcher = EventId
  QueryElement EffectMatcher = EffectId
  QueryElement ActMatcher = ActId
  QueryElement AgendaMatcher = AgendaId
  QueryElement ScenarioMatcher = ScenarioId
  QueryElement CampaignMatcher = CampaignId
  QueryElement RemainingActMatcher = CardCode
  QueryElement CardMatcher = Card
  QueryElement (SetAsideMatcher a) = QueryElement a

selectCount :: (HasCallStack, Query a, HasGame m, Monad m) => a -> m Int
selectCount = fmap HashSet.size . select

selectAny :: (HasCallStack, Query a, HasGame m, Monad m) => a -> m Bool
selectAny = fmap notNull . selectListMap id

selectNone :: (HasCallStack, Query a, HasGame m, Monad m) => a -> m Bool
selectNone = fmap not . selectAny

selectList
  :: (HasCallStack, Query a, HasGame m, Monad m) => a -> m [QueryElement a]
selectList = selectListMap id

selectRandom
  :: (HasCallStack, Query a, HasGame m, MonadRandom m)
  => a
  -> m (Maybe (QueryElement a))
selectRandom matcher = do
  results <- selectList matcher
  maybe (pure Nothing) (fmap Just . sample) (nonEmpty results)

selectListMap
  :: (HasCallStack, Query a, HasGame m, Monad m)
  => (QueryElement a -> b)
  -> a
  -> m [b]
selectListMap f = fmap (map f . setToList) . select

selectJust
  :: (HasCallStack, Show a, Query a, HasGame m, Monad m)
  => a
  -> m (QueryElement a)
selectJust matcher = fromJustNote errorNote <$> selectOne matcher
  where errorNote = "Could not find any matches for: " <> show matcher

selectAgg
  :: ( Monoid monoid
     , Query a
     , QueryElement a ~ EntityId attrs
     , Projection attrs
     , HasGame m
     , Monad m
     )
  => (typ -> monoid)
  -> Field attrs typ
  -> a
  -> m monoid
selectAgg f p matcher = do
  results <- selectList matcher
  values <- traverse (fieldMap p f) results
  pure $ fold values

selectOne
  :: (HasCallStack, Query a, HasGame m, Monad m)
  => a
  -> m (Maybe (QueryElement a))
selectOne matcher = do
  result <- selectList matcher
  pure $ case result of
    [] -> Nothing
    x : _ -> Just x

isMatch
  :: (HasCallStack, Query matcher, HasGame m, Monad m)
  => QueryElement matcher
  -> matcher
  -> m Bool
isMatch a m = member a <$> select m

class (Hashable (QueryElement a), Eq (QueryElement a)) => Query a where
  select :: (HasCallStack, HasGame m, Monad m) => a -> m (HashSet (QueryElement a))
