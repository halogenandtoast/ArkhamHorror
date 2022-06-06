module Arkham.Classes.Query where

import Arkham.Prelude

import Arkham.Id
import Arkham.Matcher
import Arkham.Card
import Arkham.Projection
import Arkham.Ability
import {-# SOURCE #-} Arkham.GameEnv
import Data.HashSet qualified as HashSet
import Arkham.Classes.Entity

type family QueryElement a where
  QueryElement AssetMatcher = AssetId
  QueryElement InvestigatorMatcher = InvestigatorId
  QueryElement PreyMatcher = InvestigatorId
  QueryElement LocationMatcher = LocationId
  QueryElement EnemyMatcher = EnemyId
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

selectCount :: (HasCallStack, Query a) => a -> GameT Int
selectCount = fmap HashSet.size . select

selectAny :: (HasCallStack, Query a) => a -> GameT Bool
selectAny = fmap notNull . selectListMap id

selectNone :: (HasCallStack, Query a) => a -> GameT Bool
selectNone = fmap null . selectListMap id

selectList
  :: (HasCallStack, Query a) => a -> GameT [QueryElement a]
selectList = selectListMap id

selectRandom
  :: (HasCallStack, Query a)
  => a
  -> GameT (Maybe (QueryElement a))
selectRandom matcher = do
  results <- selectList matcher
  maybe (pure Nothing) (fmap Just . sample) (nonEmpty results)

selectListMap
  :: (HasCallStack, Query a)
  => (QueryElement a -> b)
  -> a
  -> GameT [b]
selectListMap f = fmap (map f . setToList) . select

selectJust
  :: (HasCallStack, Show a, Query a)
  => a
  -> GameT (QueryElement a)
selectJust matcher = fromJustNote errorNote <$> selectOne matcher
  where errorNote = "Could not find any matches for: " <> show matcher

selectAgg
  :: (Query a, Num typ, QueryElement a ~ EntityId attrs, Projection attrs)
  => (typ -> typ -> typ)
  -> Field attrs typ
  -> a
  -> GameT typ
selectAgg f p matcher = do
  results <- selectList matcher
  values <- traverse (field p) results
  pure $ foldl' f 0 values

selectOne
  :: (HasCallStack, Query a)
  => a
  -> GameT (Maybe (QueryElement a))
selectOne matcher = do
  result <- selectList matcher
  pure $ case result of
    [] -> Nothing
    x : _ -> Just x

class (Hashable (QueryElement a), Eq (QueryElement a)) => Query a where
  select :: HasCallStack => a -> GameT (HashSet (QueryElement a))
