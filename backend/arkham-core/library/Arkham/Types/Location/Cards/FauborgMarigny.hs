{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.FauborgMarigny where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype FauborgMarigny = FauborgMarigny Attrs
  deriving newtype (Show, ToJSON, FromJSON)

fauborgMarigny :: FauborgMarigny
fauborgMarigny = FauborgMarigny $ baseAttrs
  "81012"
  "Faurborg Marigny"
  4
  (Static 0)
  Squiggle
  [Triangle, Squiggle]
  [Riverside]

instance (IsInvestigator investigator) => HasActions env investigator FauborgMarigny where
  getActions i window (FauborgMarigny attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env FauborgMarigny where
  runMessage msg (FauborgMarigny attrs) =
    FauborgMarigny <$> runMessage msg attrs
