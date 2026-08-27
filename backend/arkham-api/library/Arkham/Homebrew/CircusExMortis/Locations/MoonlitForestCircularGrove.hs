module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestCircularGrove (
  moonlitForestCircularGrove,
) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.LocationSymbol (LocationSymbol (Moon))

newtype MoonlitForestCircularGrove = MoonlitForestCircularGrove LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonlitForestCircularGrove :: LocationCard MoonlitForestCircularGrove
moonlitForestCircularGrove =
  location
    MoonlitForestCircularGrove
    Cards.moonlitForestCircularGrove
    3
    (Static 2)

instance HasModifiersFor MoonlitForestCircularGrove where
  getModifiersFor (MoonlitForestCircularGrove a) =
    -- "This location loses its {moon} connection symbol." Forest of Illusion's ability
    -- blanks this text, which drops this modifier and opens the printed {moon} connection
    -- to Circus Encampment.
    modifySelf a [LosesConnectionSymbol Moon]

instance RunMessage MoonlitForestCircularGrove where
  runMessage msg (MoonlitForestCircularGrove attrs) =
    MoonlitForestCircularGrove <$> runMessage msg attrs
