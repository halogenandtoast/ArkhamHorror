{-# LANGUAGE TemplateHaskell #-}
module Arkham.Campaigns.TheForgottenAge.Supply where

import Arkham.Prelude

import Data.Aeson.TH

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
  deriving stock (Show, Eq, Bounded, Enum, Ord)

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

$(deriveJSON defaultOptions ''Supply)
