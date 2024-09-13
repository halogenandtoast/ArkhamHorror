module Arkham.Location.FloodLevel where

import Arkham.Prelude

data FloodLevel = Unflooded | PartiallyFlooded | FullyFlooded
  deriving stock (Show, Eq, Generic, Ord, Data)
  deriving anyclass (ToJSON, FromJSON)

increaseFloodLevel :: FloodLevel -> FloodLevel
increaseFloodLevel = \case
  Unflooded -> PartiallyFlooded
  PartiallyFlooded -> FullyFlooded
  FullyFlooded -> FullyFlooded

decreaseFloodLevel :: FloodLevel -> FloodLevel
decreaseFloodLevel = \case
  Unflooded -> Unflooded
  PartiallyFlooded -> Unflooded
  FullyFlooded -> PartiallyFlooded
