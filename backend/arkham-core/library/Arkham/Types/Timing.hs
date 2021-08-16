module Arkham.Types.Timing where

import Arkham.Prelude

data Timing = When | After
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
