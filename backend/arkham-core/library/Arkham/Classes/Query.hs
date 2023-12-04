module Arkham.Classes.Query where

import Arkham.Prelude

import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Projection
import Arkham.Query
import Arkham.Target
import Data.Set qualified as Set
import Data.Typeable

selectCount :: (HasCallStack, Query a, HasGame m) => a -> m Int
selectCount = fmap Set.size . select

selectAny :: (HasCallStack, Query a, HasGame m) => a -> m Bool
selectAny = fmap notNull . selectListMap id

selectNone :: (HasCallStack, Query a, HasGame m) => a -> m Bool
selectNone = fmap not . selectAny

selectFilter :: (HasCallStack, Query a, HasGame m) => a -> [QueryElement a] -> m [QueryElement a]
selectFilter matcher ids = toList . (setFromList ids `intersection`) <$> select matcher

selectList
  :: (HasCallStack, Query a, HasGame m) => a -> m [QueryElement a]
selectList = selectListMap id

selectShuffled
  :: (HasCallStack, Query a, HasGame m, MonadRandom m)
  => a
  -> m [QueryElement a]
selectShuffled = shuffleM <=< selectList

selectWithField
  :: ( EntityId rec ~ QueryElement a
     , Projection rec
     , HasGame m
     , Query a
     )
  => Field rec typ
  -> a
  -> m [(QueryElement a, typ)]
selectWithField fld = traverse (traverseToSnd (field fld)) <=< selectList

selectField
  :: ( EntityId rec ~ QueryElement a
     , Projection rec
     , HasGame m
     , Query a
     )
  => Field rec typ
  -> a
  -> m [typ]
selectField fld = traverse (field fld) <=< selectList

selectRandom
  :: (HasCallStack, Query a, HasGame m, MonadRandom m)
  => a
  -> m (Maybe (QueryElement a))
selectRandom matcher = do
  results <- selectList matcher
  maybe (pure Nothing) (fmap Just . sample) (nonEmpty results)

selectRandomJust
  :: (HasCallStack, Query a, HasGame m, MonadRandom m)
  => String
  -> a
  -> m (QueryElement a)
selectRandomJust err matcher = do
  results <- selectList matcher
  maybe (error err) sample (nonEmpty results)

selectListMap
  :: (HasCallStack, Query a, HasGame m)
  => (QueryElement a -> b)
  -> a
  -> m [b]
selectListMap f = selectListMapM (pure . f)

selectTargets
  :: (HasCallStack, Query a, HasGame m, Targetable (QueryElement a))
  => a
  -> m [Target]
selectTargets = selectListMap toTarget

selectListMapM
  :: (HasCallStack, Query a, HasGame m)
  => (QueryElement a -> m b)
  -> a
  -> m [b]
selectListMapM f = traverse f . setToList <=< select

selectJust
  :: (HasCallStack, Show a, Query a, HasGame m)
  => a
  -> m (QueryElement a)
selectJust matcher = fromJustNote errorNote <$> selectOne matcher
 where
  errorNote = "Could not find any matches for: " <> show matcher

selectJustField
  :: ( HasCallStack
     , Show a
     , Query a
     , HasGame m
     , QueryElement a ~ EntityId entity
     , Projection entity
     )
  => Field entity typ
  -> a
  -> m typ
selectJustField fld matcher = field fld =<< selectJust matcher

selectFields
  :: ( Query a
     , QueryElement a ~ EntityId attrs
     , Projection attrs
     , HasGame m
     )
  => Field attrs typ
  -> a
  -> m [typ]
selectFields = selectAgg (: [])

selectAgg
  :: ( Monoid monoid
     , Query a
     , QueryElement a ~ EntityId attrs
     , Projection attrs
     , HasGame m
     )
  => (typ -> monoid)
  -> Field attrs typ
  -> a
  -> m monoid
selectAgg f p matcher = do
  results <- selectList matcher
  values <- traverse (fieldMap p f) results
  pure $ fold values

selectAgg'
  :: ( Monoid monoid
     , Query a
     , QueryElement a ~ EntityId attrs
     , Projection attrs
     , HasGame m
     , Coercible monoid b
     )
  => (typ -> monoid)
  -> Field attrs typ
  -> a
  -> m b
selectAgg' f p matcher = coerce <$> selectAgg f p matcher

selectSum
  :: ( QueryElement matcher ~ EntityId attrs
     , Num a
     , Query matcher
     , Projection attrs
     , HasGame m
     )
  => Field attrs a
  -> matcher
  -> m a
selectSum fld matcher = selectAgg' Sum fld matcher

fieldMax
  :: ( QueryElement matcher ~ EntityId attrs
     , Num a
     , Ord a
     , Query matcher
     , Projection attrs
     , HasGame m
     )
  => Field attrs a
  -> matcher
  -> m a
fieldMax fld matcher = selectAgg' Max0 fld matcher

maybeFieldMax
  :: ( QueryElement matcher ~ EntityId attrs
     , Num a
     , Ord a
     , Query matcher
     , Projection attrs
     , HasGame m
     )
  => Field attrs (Maybe a)
  -> matcher
  -> m a
maybeFieldMax fld matcher = do
  results <- catMaybes <$> selectField fld matcher
  pure $ getMax0 $ foldMap Max0 results

selectMax
  :: ( QueryElement matcher ~ EntityId attrs
     , Num a
     , Query matcher
     , Projection attrs
     , HasGame m
     , Ord a
     )
  => Field attrs a
  -> matcher
  -> m [QueryElement matcher]
selectMax fld matcher = do
  maxValue <- fieldMax fld matcher
  if maxValue > 0
    then do
      results <- selectList matcher
      filterM (fmap (== maxValue) . field fld) results
    else pure []

selectOne
  :: (HasCallStack, Query a, HasGame m)
  => a
  -> m (Maybe (QueryElement a))
selectOne matcher = do
  result <- selectList matcher
  pure $ case result of
    [] -> Nothing
    x : _ -> Just x

selectMaybe
  :: (HasCallStack, Query a, HasGame m)
  => b
  -> (QueryElement a -> b)
  -> a
  -> m b
selectMaybe def f matcher = maybe def f <$> selectOne matcher

selectOnlyOne
  :: forall a m
   . (HasCallStack, Show a, Query a, HasGame m, Typeable (QueryElement a))
  => a
  -> m (QueryElement a)
selectOnlyOne matcher =
  selectList matcher >>= \case
    [x] -> pure x
    xs ->
      error
        $ "Expected only one "
        <> show (typeRep (Proxy @(QueryElement a)))
        <> " result for: "
        <> show matcher
        <> ", got: "
        <> show (length xs)

isMatch
  :: (HasCallStack, Query matcher, HasGame m)
  => QueryElement matcher
  -> matcher
  -> m Bool
isMatch a m = member a <$> select m

class (Ord (QueryElement a), Eq (QueryElement a)) => Query a where
  select :: (HasCallStack, HasGame m) => a -> m (Set (QueryElement a))

matches :: (HasGame m, Query a) => QueryElement a -> a -> m Bool
matches a matcher = member a <$> select matcher

(<=~>) :: (HasGame m, Query a) => QueryElement a -> a -> m Bool
(<=~>) = matches

(<!=~>) :: (HasGame m, Query a) => QueryElement a -> a -> m Bool
(<!=~>) el q = not <$> matches el q
