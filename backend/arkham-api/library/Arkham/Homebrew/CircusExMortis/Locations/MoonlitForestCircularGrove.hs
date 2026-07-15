module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestCircularGrove (
  moonlitForestCircularGrove,
) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype MoonlitForestCircularGrove = MoonlitForestCircularGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- "This location loses its moon connection symbol." + an act-ability coupling that is
-- owned by the act, not this location.
-- TODO(homebrew): "loses its moon connection symbol" is implemented as cosmetic; the
-- intended effect is that this location has no adjacency (moon) connection. It is kept
-- connectsToAdjacent here to keep the grid traversable per the scenario author's ruling.
moonlitForestCircularGrove :: LocationCard MoonlitForestCircularGrove
moonlitForestCircularGrove =
  locationWith
    MoonlitForestCircularGrove
    Cards.moonlitForestCircularGrove
    3
    (Static 2)
    connectsToAdjacent

instance RunMessage MoonlitForestCircularGrove where
  runMessage msg (MoonlitForestCircularGrove attrs) =
    MoonlitForestCircularGrove <$> runMessage msg attrs
