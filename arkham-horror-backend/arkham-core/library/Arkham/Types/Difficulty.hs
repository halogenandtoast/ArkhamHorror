module Arkham.Types.Difficulty where

import ClassyPrelude
import Data.Aeson

data Difficulty
  = Easy
  | Standard
  | Hard
  | Expert
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
