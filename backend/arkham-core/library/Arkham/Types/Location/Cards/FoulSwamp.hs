{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.FoulSwamp where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype FoulSwamp = FoulSwamp Attrs
  deriving newtype (Show, ToJSON, FromJSON)

foulSwamp :: FoulSwamp
foulSwamp = FoulSwamp $ baseAttrs
  "81016"
  "Foul Swamp"
  2
  (Static 0)
  Hourglass
  [Equals, Square, Triangle, Diamond]
  [Unhallowed, Bayou]

instance HasModifiersFor env investigator FoulSwamp where
  getModifiersFor _ _ _ = pure []

instance (IsInvestigator investigator) => HasActions env investigator FoulSwamp where
  getActions i window (FoulSwamp attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env FoulSwamp where
  runMessage msg (FoulSwamp attrs) = FoulSwamp <$> runMessage msg attrs
