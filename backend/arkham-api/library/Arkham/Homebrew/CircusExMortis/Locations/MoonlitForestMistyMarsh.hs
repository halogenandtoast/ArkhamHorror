module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestMistyMarsh (moonlitForestMistyMarsh) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MoonlitForestMistyMarsh = MoonlitForestMistyMarsh LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- "This location loses its moon connection symbol." Losing the moon symbol only
-- affects moon-symbol-keyed effects, not grid adjacency — see local-faq
-- 2026-08-25_moonlit-forest-loses-moon-symbol-not-adjacency.
moonlitForestMistyMarsh :: LocationCard MoonlitForestMistyMarsh
moonlitForestMistyMarsh =
  locationWith
    MoonlitForestMistyMarsh
    Cards.moonlitForestMistyMarsh
    3
    (Static 2)
    connectsToAdjacent

instance HasModifiersFor MoonlitForestMistyMarsh where
  getModifiersFor (MoonlitForestMistyMarsh a) = do
    investigators <- select $ InvestigatorAt (be a)
    unless (null investigators) do
      abilities <- select $ AbilityOnCard (cardIs Acts.forestOfIllusion) <> AbilityWithIndex 1
      modifyEach
        a
        [AbilityTarget iid ab.ref | iid <- investigators, ab <- abilities]
        [AdditionalCost $ SealOnInvestigatorCost moonToken]

instance RunMessage MoonlitForestMistyMarsh where
  runMessage msg (MoonlitForestMistyMarsh attrs) =
    MoonlitForestMistyMarsh <$> runMessage msg attrs
