module Arkham.Campaigns.TheForgottenAge.Supply where

import Arkham.Prelude

data Supply
  = Provisions
  | Medicine
  | Rope
  | Blanket
  | Canteen
  | Torches
  | Compass
  | Map
  | Binoculars
  | Chalk
  | Pendant
  deriving stock (Show, Eq, Bounded, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

allSupplies :: [Supply]
allSupplies = [minBound ..]
