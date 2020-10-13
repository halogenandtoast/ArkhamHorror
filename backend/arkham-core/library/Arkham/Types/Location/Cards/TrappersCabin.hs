{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.TrappersCabin where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype TrappersCabin = TrappersCabin Attrs
  deriving newtype (Show, ToJSON, FromJSON)

trappersCabin :: TrappersCabin
trappersCabin = TrappersCabin $ baseAttrs
  "81014"
  "Trapper's Cabin"
  3
  (Static 0)
  Moon
  [Diamond, Moon]
  [Wilderness]

instance (IsInvestigator investigator) => HasActions env investigator TrappersCabin where
  getActions i window (TrappersCabin attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env TrappersCabin where
  runMessage msg (TrappersCabin attrs) = TrappersCabin <$> runMessage msg attrs
