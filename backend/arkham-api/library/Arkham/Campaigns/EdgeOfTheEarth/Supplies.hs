{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.EdgeOfTheEarth.Supplies where

import Arkham.Prelude
import Data.Aeson.TH

data Supply
  = SpareParts
  | GreenSoapstone
  | SmallRadio
  | MineralSpecimen
  | Dynamite
  | WoodenSledge
  | MiasmicCrystal

$(deriveJSON defaultOptions ''Supply)
