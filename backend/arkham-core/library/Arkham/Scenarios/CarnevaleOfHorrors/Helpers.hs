module Arkham.Scenarios.CarnevaleOfHorrors.Helpers where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Id
import Arkham.Matcher
import {-# SOURCE #-} Arkham.GameEnv

getCnidathqua :: GameT (Maybe EnemyId)
getCnidathqua = selectOne $ enemyIs Cards.cnidathqua

-- | An across location will be 4 locations away
getAcrossLocation :: LocationId -> GameT LocationId
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

getCounterClockwiseLocation :: LocationId -> GameT LocationId
getCounterClockwiseLocation lid = do
  counterClockwiseMap <- getCounterClockwiseMap
  case lookup lid counterClockwiseMap of
    Just x -> pure x
    Nothing -> error $ show lid <> "was not connected for some reason"

getCounterClockwiseLocations :: LocationId -> GameT [LocationId]
getCounterClockwiseLocations end = do
  counterClockwiseMap <- getCounterClockwiseMap
  pure $ buildList (lookup end counterClockwiseMap) counterClockwiseMap
 where
  buildList Nothing _ = []
  buildList (Just current) _ | current == end = [end]
  buildList (Just current) counterClockwiseMap =
    current : buildList (lookup current counterClockwiseMap) counterClockwiseMap

getClockwiseLocations :: LocationId -> GameT [LocationId]
getClockwiseLocations end = do
  clockwiseMap <- getClockwiseMap
  pure $ buildList (lookup end clockwiseMap) clockwiseMap
 where
  buildList Nothing _ = []
  buildList (Just current) _ | current == end = [end]
  buildList (Just current) clockwiseMap =
    current : buildList (lookup current clockwiseMap) clockwiseMap

getClockwiseMap :: GameT (HashMap LocationId LocationId)
getClockwiseMap = do
  lids <- selectList Anywhere
  mapFromList
    . concat
    <$> traverse
          (\lid ->
            map (lid, ) <$> selectList (AccessibleFrom $ LocationWithId lid)
          )
          lids

getCounterClockwiseMap :: GameT (HashMap LocationId LocationId)
getCounterClockwiseMap = do
  lids <- selectList Anywhere
  mapFromList
    . concat
    <$> traverse
          (\lid ->
            map (, lid) <$> selectList (AccessibleFrom $ LocationWithId lid)
          )
          lids
