module Arkham.Types.Card.Cost
  ( CardCost(..)
  , toPrintedCost
  ) where

import ClassyPrelude

import Arkham.Json

toPrintedCost :: CardCost -> Int
toPrintedCost (StaticCost n) = n
toPrintedCost DynamicCost = 0

data CardCost = StaticCost Int | DynamicCost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
