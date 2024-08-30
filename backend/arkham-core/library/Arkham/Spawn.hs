{-# LANGUAGE TemplateHaskell #-}

module Arkham.Spawn where

import Arkham.Prelude

import Arkham.Matcher
import Arkham.Placement
import Data.Aeson.TH

data SpawnAt
  = SpawnAt LocationMatcher
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

$(deriveJSON defaultOptions ''SpawnAt)
