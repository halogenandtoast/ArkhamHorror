module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestMistyMarsh (
  moonlitForestMistyMarsh,
) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype MoonlitForestMistyMarsh = MoonlitForestMistyMarsh LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- "This location loses its moon connection symbol." + an act-ability additional-cost
-- coupling that is owned by the act, not this location.
-- TODO(homebrew): "loses its moon connection symbol" is implemented as cosmetic; the
-- intended effect is that this location has no adjacency (moon) connection. It is kept
-- connectsToAdjacent here to keep the grid traversable per the scenario author's ruling.
moonlitForestMistyMarsh :: LocationCard MoonlitForestMistyMarsh
moonlitForestMistyMarsh =
  locationWith
    MoonlitForestMistyMarsh
    Cards.moonlitForestMistyMarsh
    3
    (Static 2)
    connectsToAdjacent

instance RunMessage MoonlitForestMistyMarsh where
  runMessage msg (MoonlitForestMistyMarsh attrs) =
    MoonlitForestMistyMarsh <$> runMessage msg attrs
