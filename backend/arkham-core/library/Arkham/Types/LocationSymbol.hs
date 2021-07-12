module Arkham.Types.LocationSymbol where

import ClassyPrelude
import Data.Aeson

data LocationSymbol
  = Circle
  | Square
  | Triangle
  | Plus
  | Diamond
  | Squiggle
  | Moon
  | Hourglass
  | T
  | Equals
  | Heart
  | Star
  | Droplet
  | Trefoil
  | NoSymbol
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable)
