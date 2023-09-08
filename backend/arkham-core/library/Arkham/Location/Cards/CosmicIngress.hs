module Arkham.Location.Cards.CosmicIngress (
  cosmicIngress,
  CosmicIngress (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype CosmicIngress = CosmicIngress LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicIngress :: LocationCard CosmicIngress
cosmicIngress =
  locationWith
    CosmicIngress
    Cards.cosmicIngress
    2
    (Static 3)
    (connectsToL .~ adjacentLocations)

instance HasAbilities CosmicIngress where
  getAbilities (CosmicIngress attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage CosmicIngress where
  runMessage msg (CosmicIngress attrs) =
    CosmicIngress <$> runMessage msg attrs
