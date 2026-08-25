module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestCircularGrove (
  moonlitForestCircularGrove,
) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype MoonlitForestCircularGrove = MoonlitForestCircularGrove LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- "This location loses its moon connection symbol." + an act-ability coupling that is
-- owned by the act, not this location. Losing the moon symbol only affects
-- moon-symbol-keyed effects, not grid adjacency — see local-faq
-- 2026-08-25_moonlit-forest-loses-moon-symbol-not-adjacency.
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
