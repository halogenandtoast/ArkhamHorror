module Arkham.Cost.Status where

import Arkham.Prelude

data CostStatus = UnpaidCost | PaidCost
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)
