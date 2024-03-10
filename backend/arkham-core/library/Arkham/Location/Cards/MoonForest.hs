module Arkham.Location.Cards.MoonForest (
  moonForest,
  MoonForest (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MoonForest = MoonForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonForest :: LocationCard MoonForest
moonForest = location MoonForest Cards.moonForest 0 (Static 0)

instance HasAbilities MoonForest where
  getAbilities (MoonForest attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage MoonForest where
  runMessage msg (MoonForest attrs) =
    MoonForest <$> runMessage msg attrs
