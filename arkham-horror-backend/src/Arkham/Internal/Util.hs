module Arkham.Internal.Util
  ( discardEnemy
  , getEnemy
  )
where

import Arkham.Internal.EncounterCard
import Arkham.Types
import Arkham.Types.Enemy
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.UUID
import Lens.Micro
import Safe (fromJustNote)

getEnemy :: HasEnemies a => a -> UUID -> ArkhamEnemy
getEnemy g enemyId =
  fromJustNote "could not find enemy" $ g ^? enemies . ix enemyId

discardEnemy
  :: MonadIO m
  => (HasEnemies a, HasEncounterDiscard a, HasPlayers a, HasLocations a)
  => a
  -> UUID
  -> m a
discardEnemy g enemyId = do
  encounterCard' <- lookupEncounterCard (_enemyCardCode enemy')
  pure
    $ g
    & enemies
    %~ HashMap.delete enemyId
    & locations
    . each
    . enemyIds
    %~ HashSet.delete enemyId
    & players
    . each
    . enemyIds
    %~ HashSet.delete enemyId
    & encounterDiscard
    %~ (encounterCard' :)
  where enemy' = getEnemy g enemyId
