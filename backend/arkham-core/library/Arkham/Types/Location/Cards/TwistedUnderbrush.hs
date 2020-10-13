{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.TwistedUnderbrush where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype TwistedUnderbrush = TwistedUnderbrush Attrs
  deriving newtype (Show, ToJSON, FromJSON)

twistedUnderbrush :: TwistedUnderbrush
twistedUnderbrush = TwistedUnderbrush $ baseAttrs
  "81015"
  "Twisted Underbrush"
  3
  (PerPlayer 1)
  Moon
  [Diamond, Moon]
  [Wilderness]

instance (IsInvestigator investigator) => HasActions env investigator TwistedUnderbrush where
  getActions i window (TwistedUnderbrush attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env TwistedUnderbrush where
  runMessage msg (TwistedUnderbrush attrs) =
    TwistedUnderbrush <$> runMessage msg attrs
