{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.RitualGrounds where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype RitualGrounds = RitualGrounds Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ritualGrounds :: RitualGrounds
ritualGrounds = RitualGrounds $ baseAttrs
  "81017"
  "Ritual Grounds"
  2
  (PerPlayer 1)
  Equals
  [Hourglass, Equals]
  [Unhallowed]

instance (IsInvestigator investigator) => HasActions env investigator RitualGrounds where
  getActions i window (RitualGrounds attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env RitualGrounds where
  runMessage msg (RitualGrounds attrs) = RitualGrounds <$> runMessage msg attrs
