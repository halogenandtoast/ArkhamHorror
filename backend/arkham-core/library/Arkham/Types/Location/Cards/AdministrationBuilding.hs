{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.AdministrationBuilding where

import Arkham.Import

import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype AdministrationBuilding = AdministrationBuilding Attrs
  deriving newtype (Show, ToJSON, FromJSON)

administrationBuilding :: AdministrationBuilding
administrationBuilding = AdministrationBuilding $ baseAttrs
  "02053"
  "Administration Building"
  EncounterSet.ExtracurricularActivity
  4
  (PerPlayer 1)
  Circle
  [Plus, T]
  [Miskatonic]

instance HasModifiersFor env AdministrationBuilding where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env AdministrationBuilding where
  getActions i window (AdministrationBuilding attrs) =
    getActions i window attrs

instance (LocationRunner env) => RunMessage env AdministrationBuilding where
  runMessage msg (AdministrationBuilding attrs) =
    AdministrationBuilding <$> runMessage msg attrs
