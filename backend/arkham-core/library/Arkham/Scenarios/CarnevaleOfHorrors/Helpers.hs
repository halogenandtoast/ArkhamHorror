module Arkham.Scenarios.CarnevaleOfHorrors.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher
import Arkham.Store

getCnidathqua :: (HasGame m, Store m Card) => m (Maybe EnemyId)
getCnidathqua = selectOne $ enemyIs Cards.cnidathqua

-- | An across location will be 4 locations away
getAcrossLocation :: (HasCallStack, HasGame m, Store m Card) => LocationId -> m LocationId
getAcrossLocation lid = do
  clockwiseMap <- getClockwiseMap
  pure $
    foldl'
      (\lid' _ -> withMissingError $ lookup lid' clockwiseMap)
      lid
      range
 where
  withMissingError = fromJustNote "Could not traverse connected locations"
  range :: [Int]
  range = [1 .. 4]

getCounterClockwiseLocation :: (HasGame m, Store m Card) => LocationId -> m (Maybe LocationId)
getCounterClockwiseLocation lid = do
  counterClockwiseMap <- getCounterClockwiseMap
  pure $ lookup lid counterClockwiseMap

getCounterClockwiseLocations :: (HasGame m, Store m Card) => LocationId -> m [LocationId]
getCounterClockwiseLocations end = do
  counterClockwiseMap <- getCounterClockwiseMap
  pure $ buildList (lookup end counterClockwiseMap) counterClockwiseMap
 where
  buildList Nothing _ = []
  buildList (Just current) _ | current == end = [end]
  buildList (Just current) counterClockwiseMap =
    current : buildList (lookup current counterClockwiseMap) counterClockwiseMap

getClockwiseLocations :: (HasGame m, Store m Card) => LocationId -> m [LocationId]
getClockwiseLocations end = do
  clockwiseMap <- getClockwiseMap
  pure $ buildList (lookup end clockwiseMap) clockwiseMap
 where
  buildList Nothing _ = []
  buildList (Just current) _ | current == end = [end]
  buildList (Just current) clockwiseMap =
    current : buildList (lookup current clockwiseMap) clockwiseMap

getClockwiseMap :: (Store m Card, HasGame m) => m (Map LocationId LocationId)
getClockwiseMap = do
  lids <- selectList Anywhere
  mapFromList
    . concat
    <$> traverse
      ( \lid ->
          map (lid,) <$> selectList (AccessibleFrom $ LocationWithId lid)
      )
      lids

getCounterClockwiseMap :: (Store m Card, HasGame m) => m (Map LocationId LocationId)
getCounterClockwiseMap = do
  lids <- selectList Anywhere
  mapFromList
    . concat
    <$> traverse
      ( \lid ->
          map (,lid) <$> selectList (AccessibleFrom $ LocationWithId lid)
      )
      lids
