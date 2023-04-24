{-# LANGUAGE TemplateHaskell #-}
module Arkham.Spawn where

import Arkham.Prelude

import Arkham.Matcher
import Arkham.Placement
import Data.Aeson.TH

data SpawnAt
  = SpawnLocation LocationMatcher
  | SpawnPlaced Placement
  | SpawnAtRandomSetAsideLocation
  | SpawnAtFirst [SpawnAt]
  deriving stock (Show, Eq)

$(deriveJSON defaultOptions ''SpawnAt)

