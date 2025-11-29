module Arkham.Scenarios.CarnevaleOfHorrors.Helpers where

import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.ForMovement
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "carnevaleOfHorrors" a

getCnidathqua :: (HasGame m, Tracing m) => m (Maybe EnemyId)
getCnidathqua = selectOne $ enemyIs Cards.cnidathqua

-- | An across location will be 4 locations away
getAcrossLocation :: (HasGame m, Tracing m) => LocationId -> m (Maybe LocationId)
getAcrossLocation lid = do
  clockwiseMap <- getClockwiseMap
  pure $ foldl' (\mlid' _ -> (`lookup` clockwiseMap) =<< mlid') (Just lid) range
 where
  range :: [Int]
  range = [1 .. 4]

getCounterClockwiseLocation :: (HasGame m, Tracing m) => LocationId -> m (Maybe LocationId)
getCounterClockwiseLocation lid = lookup lid <$> getCounterClockwiseMap

getCounterClockwiseLocations :: (HasGame m, Tracing m) => LocationId -> m [LocationId]
getCounterClockwiseLocations end = do
  counterClockwiseMap <- getCounterClockwiseMap
  pure $ buildList (lookup end counterClockwiseMap) counterClockwiseMap
 where
  buildList Nothing _ = []
  buildList (Just current) _ | current == end = [end]
  buildList (Just current) counterClockwiseMap =
    current : buildList (lookup current counterClockwiseMap) counterClockwiseMap

getClockwiseLocations :: (HasGame m, Tracing m) => LocationId -> m [LocationId]
getClockwiseLocations end = do
  clockwiseMap <- getClockwiseMap
  pure $ buildList (lookup end clockwiseMap) clockwiseMap
 where
  buildList Nothing _ = []
  buildList (Just current) _ | current == end = [end]
  buildList (Just current) clockwiseMap =
    current : buildList (lookup current clockwiseMap) clockwiseMap

getClockwiseMap :: (HasGame m, Tracing m) => m (Map LocationId LocationId)
getClockwiseMap = do
  lids <- select Anywhere
  mapFromList
    . concat
    <$> traverse (\lid -> map (lid,) <$> select (AccessibleFrom NotForMovement $ LocationWithId lid)) lids

getCounterClockwiseMap :: (Tracing m, HasGame m) => m (Map LocationId LocationId)
getCounterClockwiseMap = do
  lids <- select Anywhere
  mapFromList
    . concat
    <$> traverse (\lid -> map (,lid) <$> select (AccessibleFrom NotForMovement $ LocationWithId lid)) lids
