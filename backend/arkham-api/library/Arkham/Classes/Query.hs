module Arkham.Classes.Query where

import Arkham.Classes.Entity
import Arkham.Classes.HasGame
import Arkham.Prelude
import Arkham.Projection
import Arkham.Query
import Arkham.Target
import Data.List qualified as List
import Data.Typeable

selectWhenNotNull :: (HasCallStack, Query a, HasGame m) => a -> ([QueryElement a] -> m ()) -> m ()
selectWhenNotNull q f = select q >>= \xs -> if null xs then pure () else f xs

selectCount :: (HasCallStack, Query a, HasGame m) => a -> m Int
selectCount = fmap length . select

selectAny :: (HasCallStack, Query a, HasGame m) => a -> m Bool
selectAny = fmap notNull . selectMap id

whenAny :: (HasCallStack, Query a, HasGame m) => a -> m () -> m ()
whenAny q f = whenM (selectAny q) f

selectNone :: (HasCallStack, Query a, HasGame m) => a -> m Bool
selectNone = fmap not . selectAny

whenNone :: (HasCallStack, Query a, HasGame m) => a -> m () -> m ()
whenNone q f = whenM (selectNone q) f

selectFilter :: (HasCallStack, Query a, HasGame m) => a -> [QueryElement a] -> m [QueryElement a]
selectFilter matcher ids = (ids `List.intersect`) <$> select matcher

selectShuffled
  :: (HasCallStack, Query a, HasGame m, MonadRandom m)
  => a
  -> m [QueryElement a]
selectShuffled = shuffleM <=< select

selectWithField
  :: ( EntityId rec ~ QueryElement a
     , Projection rec
     , HasGame m
     , Query a
     )
  => Field rec typ
  -> a
  -> m [(QueryElement a, typ)]
selectWithField fld = traverse (traverseToSnd (field fld)) <=< select

selectField
  :: ( EntityId rec ~ QueryElement a
     , Projection rec
     , HasGame m
     , Query a
     )
  => Field rec typ
  -> a
  -> m [typ]
selectField fld = traverse (field fld) <=< select

selectRandom
  :: (HasCallStack, Query a, HasGame m, MonadRandom m)
  => a
  -> m (Maybe (QueryElement a))
selectRandom matcher = do
  results <- select matcher
  maybe (pure Nothing) (fmap Just . sample) (nonEmpty results)

selectRandomJust
  :: (HasCallStack, Query a, HasGame m, MonadRandom m)
  => String
  -> a
  -> m (QueryElement a)
selectRandomJust err matcher = do
  results <- select matcher
  maybe (error err) sample (nonEmpty results)

selectMap
  :: (HasCallStack, Query a, HasGame m)
  => (QueryElement a -> b)
  -> a
  -> m [b]
selectMap f = selectMapM (pure . f)

selectTargets
  :: (HasCallStack, Query a, HasGame m, Targetable (QueryElement a))
  => a
  -> m [Target]
selectTargets = selectMap toTarget

selectMapM
  :: (HasCallStack, Query a, HasGame m)
  => (QueryElement a -> m b)
  -> a
  -> m [b]
selectMapM f = traverse f <=< select

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
  results <- select matcher
  values <- traverse (fieldMap p f) results
  pure $ fold values

selectAll
  :: ( Monoid typ
     , Query a
     , QueryElement a ~ EntityId attrs
     , Projection attrs
     , HasGame m
     )
  => Field attrs typ
  -> a
  -> m typ
selectAll p = select >=> fmap fold . traverse (field p)

selectAgg'
  :: forall attrs a b typ monoid m
   . ( Monoid monoid
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

fieldMaxBy
  :: ( QueryElement matcher ~ EntityId attrs
     , Num a
     , Ord a
     , Query matcher
     , Projection attrs
     , HasGame m
     )
  => Field attrs b
  -> (b -> a)
  -> matcher
  -> m a
fieldMaxBy fld f matcher = selectAgg' (Max0 . f) fld matcher

maybeFieldMax
  :: forall attrs a matcher m
   . ( QueryElement matcher ~ EntityId attrs
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
      results <- select matcher
      filterM (fmap (== maxValue) . field fld) results
    else pure []

selectMaybeMax
  :: ( QueryElement matcher ~ EntityId attrs
     , Num a
     , Query matcher
     , Projection attrs
     , HasGame m
     , Ord a
     )
  => Field attrs (Maybe a)
  -> matcher
  -> m [QueryElement matcher]
selectMaybeMax fld matcher = do
  maxValue <- maybeFieldMax fld matcher
  if maxValue > 0
    then do
      results <- select matcher
      filterM (fmap (== Just maxValue) . field fld) results
    else pure []

selectOne
  :: (HasCallStack, Query a, HasGame m)
  => a
  -> m (Maybe (QueryElement a))
selectOne matcher = do
  result <- select matcher
  pure $ case result of
    [] -> Nothing
    x : _ -> Just x

selectOrDefault :: (HasCallStack, Query a, HasGame m) => QueryElement a -> a -> m (QueryElement a)
selectOrDefault def matcher = selectMaybe def id matcher

selectMaybeT :: (HasCallStack, Query a, HasGame m) => a -> MaybeT m (QueryElement a)
selectMaybeT = MaybeT . selectOne

selectMaybe
  :: (HasCallStack, Query a, HasGame m)
  => b
  -> (QueryElement a -> b)
  -> a
  -> m b
selectMaybe def f matcher = maybe def f <$> selectOne matcher

selectMaybeM
  :: (HasCallStack, Query a, HasGame m)
  => b
  -> a
  -> (QueryElement a -> m b)
  -> m b
selectMaybeM def matcher f = maybe (pure def) f =<< selectOne matcher

selectWithFilterM
  :: (HasCallStack, Query a, HasGame m) => a -> (QueryElement a -> m Bool) -> m [QueryElement a]
selectWithFilterM matcher f = filterM f =<< select matcher

selectForMaybeM
  :: (HasCallStack, Query a, HasGame m)
  => a
  -> (QueryElement a -> m ())
  -> m ()
selectForMaybeM = selectMaybeM ()

selectOnlyOne
  :: forall a m
   . (HasCallStack, Show a, Query a, HasGame m, Typeable (QueryElement a))
  => a
  -> m (QueryElement a)
selectOnlyOne matcher =
  select matcher >>= \case
    [x] -> pure x
    xs ->
      error
        $ "Expected only one "
        <> show (typeRep (Proxy @(QueryElement a)))
        <> " result for: "
        <> show matcher
        <> ", got: "
        <> show (length xs)

selectSortedBy
  :: ( EntityId rec ~ QueryElement a
     , Projection rec
     , HasGame m
     , Query a
     , Ord typ
     )
  => Field rec typ
  -> a
  -> m [QueryElement a]
selectSortedBy fld matcher = do
  results <- selectWithField fld matcher
  pure $ map fst $ sortOn snd results

isMatch
  :: (HasCallStack, Query matcher, HasGame m)
  => QueryElement matcher
  -> matcher
  -> m Bool
isMatch a m = elem a <$> select m

class (Ord (QueryElement a), Eq (QueryElement a)) => Query a where
  select :: (HasCallStack, HasGame m) => a -> m [QueryElement a]

matches :: (HasGame m, Query a) => QueryElement a -> a -> m Bool
matches a matcher = elem a <$> select matcher

(<=~>) :: (HasGame m, Query a) => QueryElement a -> a -> m Bool
(<=~>) = matches

(<!=~>) :: (HasGame m, Query a) => QueryElement a -> a -> m Bool
(<!=~>) el q = not <$> matches el q
