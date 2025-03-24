{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.EdgeOfTheEarth.Supplies where

import Arkham.Card.CardCode
import Arkham.Prelude
import Data.Aeson.TH

data Supply
  = GreenSoapstone
  | WoodenSledge
  | Dynamite
  | MiasmicCrystal
  | MineralSpecimen
  | SmallRadio
  | SpareParts
  deriving stock (Enum, Bounded)

instance HasCardCode Supply where
  toCardCode = \case
    GreenSoapstone -> "08614"
    WoodenSledge -> "08615"
    Dynamite -> "08616"
    MiasmicCrystal -> "08617"
    MineralSpecimen -> "08618"
    SmallRadio -> "08619"
    SpareParts -> "08620"

$(deriveJSON defaultOptions ''Supply)
