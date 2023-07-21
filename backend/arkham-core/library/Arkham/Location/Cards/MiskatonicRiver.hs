module Arkham.Location.Cards.MiskatonicRiver (
  miskatonicRiver,
  MiskatonicRiver (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype MiskatonicRiver = MiskatonicRiver LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

miskatonicRiver :: LocationCard MiskatonicRiver
miskatonicRiver = location MiskatonicRiver Cards.miskatonicRiver 5 (Static 0)

instance HasAbilities MiskatonicRiver where
  getAbilities (MiskatonicRiver attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage MiskatonicRiver where
  runMessage msg (MiskatonicRiver attrs) =
    MiskatonicRiver <$> runMessage msg attrs
