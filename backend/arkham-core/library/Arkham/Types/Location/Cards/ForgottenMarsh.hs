{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Location.Cards.ForgottenMarsh where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Trait
import ClassyPrelude

newtype ForgottenMarsh = ForgottenMarsh Attrs
  deriving newtype (Show, ToJSON, FromJSON)

forgottenMarsh :: ForgottenMarsh
forgottenMarsh = ForgottenMarsh $ baseAttrs
  "81013"
  "Forgotten Marsh"
  2
  (Static 0)
  Diamond
  [Moon, Square, Triangle, Hourglass]
  [Wilderness, Bayou]

instance (IsInvestigator investigator) => HasActions env investigator ForgottenMarsh where
  getActions i window (ForgottenMarsh attrs) = getActions i window attrs

instance (LocationRunner env) => RunMessage env ForgottenMarsh where
  runMessage msg (ForgottenMarsh attrs) =
    ForgottenMarsh <$> runMessage msg attrs
