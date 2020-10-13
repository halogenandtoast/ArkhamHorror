{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.AudubonPark where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype AudubonPark = AudubonPark Attrs
  deriving newtype (Show, ToJSON, FromJSON)

audubonPark :: AudubonPark
audubonPark = AudubonPark $ baseAttrs
  "81011"
  "Audubon Park"
  3
  (PerPlayer 1)
  Squiggle
  [Triangle, Squiggle]
  [Riverside]

instance (IsInvestigator investigator) => HasActions env investigator AudubonPark where
  getActions i window (AudubonPark attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env AudubonPark where
  runMessage msg (AudubonPark attrs) = AudubonPark <$> runMessage msg attrs
