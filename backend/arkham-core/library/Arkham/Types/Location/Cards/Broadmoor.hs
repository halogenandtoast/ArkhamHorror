{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.Broadmoor where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype Broadmoor = Broadmoor Attrs
  deriving newtype (Show, ToJSON, FromJSON)

broadmoor :: Broadmoor
broadmoor = Broadmoor $ baseAttrs
  "81009"
  "Broadmoor"
  3
  (PerPlayer 1)
  Plus
  [Square, Plus]
  [NewOrleans]

instance (IsInvestigator investigator) => HasActions env investigator Broadmoor where
  getActions i window (Broadmoor attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env Broadmoor where
  runMessage msg (Broadmoor attrs) = Broadmoor <$> runMessage msg attrs
