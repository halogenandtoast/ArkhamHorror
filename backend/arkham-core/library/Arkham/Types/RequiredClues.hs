module Arkham.Types.RequiredClues where

import Arkham.Prelude

import Arkham.Types.GameValue
import Arkham.Types.LocationMatcher

data RequiredClues = RequiredClues (GameValue Int) (Maybe LocationMatcher)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
