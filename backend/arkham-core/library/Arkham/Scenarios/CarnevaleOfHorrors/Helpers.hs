module Arkham.Scenarios.CarnevaleOfHorrors.Helpers where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Id
import Arkham.Matcher
import Arkham.GameEnv

getCnidathqua :: HasGame m => m (Maybe EnemyId)
getCnidathqua = selectOne $ enemyIs Cards.cnidathqua

-- | An across location will be 4 locations away
getAcrossLocation :: HasGame m => LocationId -> m LocationId
getAcrossLocation lid = do
  clockwiseMap <- getClockwiseMap
  pure $ foldl'
    (\lid' _ -> withMissingError $ lookup lid' clockwiseMap)
    lid
    range
 where
  withMissingError = fromJustNote "Could not traverse connected locations"
  range :: [Int]
  range = [1 .. 4]

getCounterClockwiseLocation :: HasGame m => LocationId -> m LocationId
getCounterClockwiseLocation lid = do
  counterClockwiseMap <- getCounterClockwiseMap
  case lookup lid counterClockwiseMap of
    Just x -> pure x
    Nothing -> error $ show lid <> "was not connected for some reason"

getCounterClockwiseLocations :: HasGame m => LocationId -> m [LocationId]
getCounterClockwiseLocations end = do
  counterClockwiseMap <- getCounterClockwiseMap
  pure $ buildList (lookup end counterClockwiseMap) counterClockwiseMap
 where
  buildList Nothing _ = []
  buildList (Just current) _ | current == end = [end]
  buildList (Just current) counterClockwiseMap =
    current : buildList (lookup current counterClockwiseMap) counterClockwiseMap

getClockwiseLocations :: HasGame m => LocationId -> m [LocationId]
getClockwiseLocations end = do
  clockwiseMap <- getClockwiseMap
  pure $ buildList (lookup end clockwiseMap) clockwiseMap
 where
  buildList Nothing _ = []
  buildList (Just current) _ | current == end = [end]
  buildList (Just current) clockwiseMap =
    current : buildList (lookup current clockwiseMap) clockwiseMap

getClockwiseMap :: HasGame m => m (Map LocationId LocationId)
getClockwiseMap = do
  lids <- selectList Anywhere
  mapFromList
    . concat
    <$> traverse
          (\lid ->
            map (lid, ) <$> selectList (AccessibleFrom $ LocationWithId lid)
          )
          lids

getCounterClockwiseMap :: HasGame m => m (Map LocationId LocationId)
getCounterClockwiseMap = do
  lids <- selectList Anywhere
  mapFromList
    . concat
    <$> traverse
          (\lid ->
            map (, lid) <$> selectList (AccessibleFrom $ LocationWithId lid)
          )
          lids
