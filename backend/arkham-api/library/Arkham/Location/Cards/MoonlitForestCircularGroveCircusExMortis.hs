module Arkham.Location.Cards.MoonlitForestCircularGroveCircusExMortis (
  moonlitForestCircularGroveCircusExMortis,
) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MoonlitForestCircularGroveCircusExMortis = MoonlitForestCircularGroveCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- "This location loses its moon connection symbol." + an act-ability coupling that is
-- owned by the act, not this location.
-- TODO(homebrew): "loses its moon connection symbol" is implemented as cosmetic; the
-- intended effect is that this location has no adjacency (moon) connection. It is kept
-- connectsToAdjacent here to keep the grid traversable per the scenario author's ruling.
moonlitForestCircularGroveCircusExMortis :: LocationCard MoonlitForestCircularGroveCircusExMortis
moonlitForestCircularGroveCircusExMortis =
  locationWith
    MoonlitForestCircularGroveCircusExMortis
    Cards.moonlitForestCircularGroveCircusExMortis
    3
    (Static 2)
    connectsToAdjacent

instance RunMessage MoonlitForestCircularGroveCircusExMortis where
  runMessage msg (MoonlitForestCircularGroveCircusExMortis attrs) =
    MoonlitForestCircularGroveCircusExMortis <$> runMessage msg attrs
