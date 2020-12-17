module Arkham.Types.Difficulty where

import Arkham.Prelude

data Difficulty
  = Easy
  | Standard
  | Hard
  | Expert
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
