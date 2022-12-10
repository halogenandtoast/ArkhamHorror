module Arkham.Timing where

import Arkham.Prelude

data Timing = When | AtIf | After
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
