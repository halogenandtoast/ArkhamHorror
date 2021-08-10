module Arkham.Types.Timing where

import Arkham.Prelude

data Timing = Before | When | After
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
