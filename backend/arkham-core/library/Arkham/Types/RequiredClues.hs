module Arkham.Types.RequiredClues where

import Arkham.Prelude

import Arkham.Types.GameValue
import Arkham.Types.Matcher

data RequiredClues = RequiredClues (GameValue Int) (Maybe LocationMatcher)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
