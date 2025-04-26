{-# LANGUAGE TemplateHaskell #-}

module Arkham.Spawn where

import Arkham.Prelude
import Arkham.Matcher
import Arkham.Id
import Arkham.Placement
import Data.Aeson.TH
import GHC.Records

data SpawnDetails = SpawnDetails
  { spawnDetailsEnemy :: EnemyId
  , spawnDetailsInvestigator :: Maybe InvestigatorId
  , spawnDetailsSpawnAt :: SpawnAt
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "enemy" SpawnDetails EnemyId where
  getField = spawnDetailsEnemy

instance HasField "investigator" SpawnDetails (Maybe InvestigatorId) where
  getField = spawnDetailsInvestigator

instance HasField "location" SpawnDetails (Maybe LocationId) where
  getField sd = case sd.spawnAt of
    SpawnAtLocation lid -> Just lid
    _ -> Nothing

instance HasField "spawnAt" SpawnDetails SpawnAt where
  getField = spawnDetailsSpawnAt

data SpawnAt
  = SpawnAt LocationMatcher
  | SpawnAtLocation LocationId
  | SpawnPlaced Placement
  | SpawnAtRandomSetAsideLocation
  | SpawnAtFirst [SpawnAt]
  | SpawnEngagedWith InvestigatorMatcher
  | NoSpawn
  deriving stock (Show, Eq, Ord, Data)

instance IsString SpawnAt where
  fromString = SpawnAt . fromString

class IsSpawnAt a where
  toSpawnAt :: a -> SpawnAt

instance IsSpawnAt LocationMatcher where
  toSpawnAt = SpawnAt

instance IsSpawnAt Placement where
  toSpawnAt = SpawnPlaced

instance IsSpawnAt InvestigatorMatcher where
  toSpawnAt = SpawnEngagedWith

mconcat
  [ deriveJSON defaultOptions ''SpawnAt
  , deriveJSON defaultOptions ''SpawnDetails
  ]
