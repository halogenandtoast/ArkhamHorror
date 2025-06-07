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
  | KeyOfEztli
  | MysteriousScepter
  deriving stock (Show, Eq, Bounded, Enum, Ord, Data)

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

supplyCost :: Supply -> Int
supplyCost = \case
  Provisions -> 1
  Medicine -> 2
  Gasoline -> 1
  Rope -> 3
  Blanket -> 2
  Canteen -> 2
  Torches -> 3
  Compass -> 2
  Map -> 3
  Binoculars -> 2
  Chalk -> 2
  Pocketknife -> 2
  Pickaxe -> 2
  Pendant -> 1
  KeyOfEztli -> 0
  MysteriousScepter -> 0
