{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.HumanitiesBuilding where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype HumanitiesBuilding = HumanitiesBuilding Attrs
  deriving newtype (Show, ToJSON, FromJSON)

humanitiesBuilding :: HumanitiesBuilding
humanitiesBuilding = HumanitiesBuilding $ baseAttrs
  "02049"
  "Humanities Building"
  EncounterSet.ExtracurricularActivity
  3
  (PerPlayer 2)
  Square
  [Plus, Triangle]
  [Miskatonic]

instance HasModifiersFor env HumanitiesBuilding where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HumanitiesBuilding where
  getActions i window (HumanitiesBuilding attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env HumanitiesBuilding where
  runMessage msg (HumanitiesBuilding attrs) =
    HumanitiesBuilding <$> runMessage msg attrs
