module Arkham.Location.Cards.Study where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (study)
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Location.Data

newtype Study = Study LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

study :: LocationCard Study
study = location Study Cards.study 2 (PerPlayer 2) Circle []

instance HasLocationData Study where
  toLocationData = LocationData
    { locationSymbol = Circle
    , locationConnectedSymbols = []
    , locationShroud = 2
    , locationClues = PerPlayer 2
    }

instance RunMessage Study where
  runMessage msg (Study attrs) = Study <$> runMessage msg attrs
