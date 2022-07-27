module Arkham.Location.Data where

import Arkham.Prelude

import Arkham.LocationSymbol
import Arkham.GameValue

data LocationData = LocationData
  { locationSymbol :: LocationSymbol
  , locationConnectedSymbols :: [LocationSymbol]
  , locationShroud :: Int
  , locationClues :: GameValue Int
  }

class HasLocationData a where
  toLocationData :: LocationData
