module Arkham.Types.Card.Cost
  ( CardCost(..)
  )
where

import ClassyPrelude

import Arkham.Json

data CardCost = StaticCost Int | DynamicCost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
