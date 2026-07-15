module Arkham.Homebrew.CircusExMortis.Locations.MoonlitForestFogBank (
  moonlitForestFogBank,
) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MoonlitForestFogBank = MoonlitForestFogBank LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonlitForestFogBank :: LocationCard MoonlitForestFogBank
moonlitForestFogBank =
  locationWith
    MoonlitForestFogBank
    Cards.moonlitForestFogBank
    3
    (Static 2)
    connectsToAdjacent

instance HasModifiersFor MoonlitForestFogBank where
  getModifiersFor (MoonlitForestFogBank a) = do
    -- "This location gets +2 shroud value. Each adjacent copy of Moonlit Forest gets +1
    -- shroud value."
    modifySelf a [ShroudModifier 2]
    modifySelect a (LocationWithTitle "Moonlit Forest" <> connectedTo (be a)) [ShroudModifier 1]

instance RunMessage MoonlitForestFogBank where
  runMessage msg (MoonlitForestFogBank attrs) = runQueueT $ case msg of
    _ -> MoonlitForestFogBank <$> liftRunMessage msg attrs
