{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.GardenDistrict where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype GardenDistrict = GardenDistrict Attrs
  deriving newtype (Show, ToJSON, FromJSON)

gardenDistrict :: GardenDistrict
gardenDistrict = GardenDistrict $ baseAttrs
  "81008"
  "Garden District"
  1
  (Static 0)
  Plus
  [Square, Plus]
  [NewOrleans]

instance (IsInvestigator investigator) => HasActions env investigator GardenDistrict where
  getActions i window (GardenDistrict attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env GardenDistrict where
  runMessage msg (GardenDistrict attrs) =
    GardenDistrict <$> runMessage msg attrs
