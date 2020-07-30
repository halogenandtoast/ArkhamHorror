module Arkham.Types.LocationSymbol where

import ClassyPrelude
import Data.Aeson

data LocationSymbol
  = Circle
  | Square
  | Triangle
  | Plus
  | Diamond
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
