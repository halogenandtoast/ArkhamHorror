module Arkham.DeckBuilding.Adjustment where

import Arkham.Prelude

data DeckBuildingAdjustment = ReduceXpCostOfNextCardYouPurchaseBy Int
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)
