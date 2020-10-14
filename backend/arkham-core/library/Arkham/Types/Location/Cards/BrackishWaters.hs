{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.BrackishWaters where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype BrackishWaters = BrackishWaters Attrs
  deriving newtype (Show, ToJSON, FromJSON)

brackishWaters :: BrackishWaters
brackishWaters = BrackishWaters $ baseAttrs
  "81010"
  "Brackish Waters"
  1
  (Static 0)
  Triangle
  [Squiggle, Square, Diamond, Hourglass]
  [Riverside, Bayou]

instance HasModifiersFor env investigator BrackishWaters where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator BrackishWaters where
  getActions i window (BrackishWaters attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env BrackishWaters where
  runMessage msg (BrackishWaters attrs) =
    BrackishWaters <$> runMessage msg attrs
