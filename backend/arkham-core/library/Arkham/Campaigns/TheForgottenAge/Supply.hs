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
  | Gasoline
  | Pocketknife
  | Pickaxe
  deriving stock (Show, Eq, Bounded, Enum, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

prologueSupplies :: [Supply]
prologueSupplies =
  [ Provisions
  , Medicine
  , Rope
  , Blanket
  , Canteen
  , Torches
  , Compass
  , Map
  , Binoculars
  , Chalk
  , Pendant
  ]

resupplyPointSupplies :: [Supply]
resupplyPointSupplies =
  [ Provisions
  , Medicine
  , Gasoline
  , Blanket
  , Canteen
  , Compass
  , Binoculars
  , Chalk
  , Pocketknife
  , Pickaxe
  ]
