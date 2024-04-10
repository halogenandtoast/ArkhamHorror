module Arkham.DeckBuilding.Adjustment where

import Arkham.Prelude

data DeckBuildingAdjustment = ReduceXpCostOfNextCardYouPurchaseBy Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
