module Arkham.Classes.Query where

import Arkham.Prelude

import Arkham.Id
import Arkham.Matcher
import Arkham.Card
import Arkham.Projection
import Arkham.Ability
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
  QueryElement ActMatcher = ActId
  QueryElement AgendaMatcher = AgendaId
  QueryElement RemainingActMatcher = CardCode

selectCount :: (HasCallStack, Query a m) => a -> m Int
selectCount = fmap HashSet.size . select

selectAny :: (HasCallStack, Query a m) => a -> m Bool
selectAny = fmap notNull . selectListMap id

selectNone :: (HasCallStack, Query a m) => a -> m Bool
selectNone = fmap null . selectListMap id

selectList
  :: (HasCallStack, Query a m) => a -> m [QueryElement a]
selectList = selectListMap id

selectRandom
  :: (HasCallStack, MonadRandom m, Query a m)
  => a
  -> m (Maybe (QueryElement a))
selectRandom matcher = do
  results <- selectList matcher
  maybe (pure Nothing) (fmap Just . sample) (nonEmpty results)

selectListMap
  :: (HasCallStack, Query a m)
  => (QueryElement a -> b)
  -> a
  -> m [b]
selectListMap f = fmap (map f . setToList) . select

selectJust
  :: (HasCallStack, Show a, Query a m)
  => a
  -> m (QueryElement a)
selectJust matcher = fromJustNote errorNote <$> selectOne matcher
  where errorNote = "Could not find any matches for: " <> show matcher

selectAgg
  :: (Query a m, Num typ, QueryElement a ~ EntityId attrs, Projection m attrs)
  => (typ -> typ -> typ)
  -> Field attrs typ
  -> a
  -> m typ
selectAgg f p matcher = do
  results <- selectList matcher
  values <- traverse (field p) results
  pure $ foldl' f 0 values

-- | Get a set aside card
--
-- Some cards may be double sided and completely different types
-- like Daniel Chesterfield. In these cases, we want to consider
-- the card a match, but "flip" it to the correct side.
--
-- This logic is a bit too generous and we may want to specify
-- on double sided cards which card code is on the other side.
getSetAsideCard
  :: (HasCallStack, Query ExtendedCardMatcher m)
  => CardDef
  -> m Card
getSetAsideCard def = do
  card <- selectJust . SetAsideCardMatch $ cardIs def
  pure $ if cardCodeExactEq (toCardCode card) (toCardCode def)
    then card
    else lookupCard (toCardCode def) (toCardId card)

getSetAsideEncounterCard
  :: (HasCallStack, Query ExtendedCardMatcher m)
  => CardDef
  -> m EncounterCard
getSetAsideEncounterCard =
  fmap (fromJustNote "must be encounter card" . preview _EncounterCard)
    . getSetAsideCard

getSetAsideCardsMatching
  :: (HasCallStack, Query ExtendedCardMatcher m)
  => CardMatcher
  -> m [Card]
getSetAsideCardsMatching = selectList . SetAsideCardMatch

selectOne
  :: (HasCallStack, Query a m)
  => a
  -> m (Maybe (QueryElement a))
selectOne matcher = do
  result <- selectList matcher
  pure $ case result of
    [] -> Nothing
    x : _ -> Just x

selectAssetController :: Query InvestigatorMatcher m => AssetId -> m (Maybe InvestigatorId)
selectAssetController = selectOne . HasMatchingAsset . AssetWithId

selectEventController :: Query InvestigatorMatcher m => EventId -> m (Maybe InvestigatorId)
selectEventController = selectOne . HasMatchingEvent . EventWithId

selectSkillController :: Query InvestigatorMatcher m => SkillId -> m (Maybe InvestigatorId)
selectSkillController = selectOne . HasMatchingSkill . SkillWithId

class (Monad m, Hashable (QueryElement a), Eq (QueryElement a)) => Query a m where
  select :: HasCallStack => a -> m (HashSet (QueryElement a))

