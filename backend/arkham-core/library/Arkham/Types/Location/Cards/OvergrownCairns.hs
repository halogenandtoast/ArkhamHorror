{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.OvergrownCairns where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype OvergrownCairns = OvergrownCairns Attrs
  deriving newtype (Show, ToJSON, FromJSON)

overgrownCairns :: OvergrownCairns
overgrownCairns = OvergrownCairns $ baseAttrs
  "81018"
  "Overgrown Cairns"
  4
  (Static 0)
  Equals
  [Hourglass, Equals]
  [Unhallowed]

instance (IsInvestigator investigator) => HasActions env investigator OvergrownCairns where
  getActions i window (OvergrownCairns attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env OvergrownCairns where
  runMessage msg (OvergrownCairns attrs) =
    OvergrownCairns <$> runMessage msg attrs
