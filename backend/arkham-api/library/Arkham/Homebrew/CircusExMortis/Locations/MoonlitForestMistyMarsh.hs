module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestMistyMarsh (moonlitForestMistyMarsh) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken)
import Arkham.Location.Import.Lifted
import Arkham.LocationSymbol (LocationSymbol (Moon))
import Arkham.Matcher

newtype MoonlitForestMistyMarsh = MoonlitForestMistyMarsh LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonlitForestMistyMarsh :: LocationCard MoonlitForestMistyMarsh
moonlitForestMistyMarsh =
  location
    MoonlitForestMistyMarsh
    Cards.moonlitForestMistyMarsh
    3
    (Static 2)

instance HasModifiersFor MoonlitForestMistyMarsh where
  getModifiersFor (MoonlitForestMistyMarsh a) = do
    -- "This location loses its {moon} connection symbol." Forest of Illusion's ability
    -- blanks this text, which drops this modifier and opens the printed {moon} connection
    -- to Circus Encampment.
    modifySelf a [LosesConnectionSymbol Moon]

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
