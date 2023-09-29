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
  deriving stock (Show, Eq)

instance IsString SpawnAt where
  fromString = SpawnAt . fromString

$(deriveJSON defaultOptions ''SpawnAt)
