module Arkham.Card.Cost
  ( CardCost(..)
  , toPrintedCost
  ) where

import Arkham.Prelude

toPrintedCost :: CardCost -> Int
toPrintedCost (StaticCost n) = n
toPrintedCost DynamicCost = 0

data CardCost = StaticCost Int | DynamicCost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
