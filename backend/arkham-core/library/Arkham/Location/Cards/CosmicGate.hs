module Arkham.Location.Cards.CosmicGate (
  cosmicGate,
  CosmicGate (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype CosmicGate = CosmicGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicGate :: LocationCard CosmicGate
cosmicGate =
  locationWith
    CosmicGate
    Cards.cosmicGate
    1
    (Static 1)
    (connectsToL .~ adjacentLocations)

instance HasAbilities CosmicGate where
  getAbilities (CosmicGate attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage CosmicGate where
  runMessage msg (CosmicGate attrs) =
    CosmicGate <$> runMessage msg attrs
