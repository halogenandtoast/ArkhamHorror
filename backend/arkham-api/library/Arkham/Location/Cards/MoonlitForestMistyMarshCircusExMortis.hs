module Arkham.Location.Cards.MoonlitForestMistyMarshCircusExMortis (
  moonlitForestMistyMarshCircusExMortis,
) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MoonlitForestMistyMarshCircusExMortis = MoonlitForestMistyMarshCircusExMortis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- "This location loses its moon connection symbol." + an act-ability additional-cost
-- coupling that is owned by the act, not this location.
-- TODO(homebrew): "loses its moon connection symbol" is implemented as cosmetic; the
-- intended effect is that this location has no adjacency (moon) connection. It is kept
-- connectsToAdjacent here to keep the grid traversable per the scenario author's ruling.
moonlitForestMistyMarshCircusExMortis :: LocationCard MoonlitForestMistyMarshCircusExMortis
moonlitForestMistyMarshCircusExMortis =
  locationWith
    MoonlitForestMistyMarshCircusExMortis
    Cards.moonlitForestMistyMarshCircusExMortis
    3
    (Static 2)
    connectsToAdjacent

instance RunMessage MoonlitForestMistyMarshCircusExMortis where
  runMessage msg (MoonlitForestMistyMarshCircusExMortis attrs) =
    MoonlitForestMistyMarshCircusExMortis <$> runMessage msg attrs
