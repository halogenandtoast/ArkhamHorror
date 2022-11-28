module Arkham.Spawn where

import Arkham.Prelude

import Arkham.Matcher
import Arkham.Placement

data SpawnAt
  = SpawnLocation LocationMatcher
  | SpawnPlaced Placement
  | SpawnAtRandomSetAsideLocation
  | SpawnAtFirst [SpawnAt]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

