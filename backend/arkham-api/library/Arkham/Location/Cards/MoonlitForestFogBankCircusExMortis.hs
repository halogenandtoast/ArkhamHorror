module Arkham.Location.Cards.MoonlitForestFogBankCircusExMortis (
  moonlitForestFogBankCircusExMortis,
) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype MoonlitForestFogBankCircusExMortis = MoonlitForestFogBankCircusExMortis LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moonlitForestFogBankCircusExMortis :: LocationCard MoonlitForestFogBankCircusExMortis
moonlitForestFogBankCircusExMortis =
  locationWith
    MoonlitForestFogBankCircusExMortis
    Cards.moonlitForestFogBankCircusExMortis
    3
    (Static 2)
    connectsToAdjacent

instance HasModifiersFor MoonlitForestFogBankCircusExMortis where
  getModifiersFor (MoonlitForestFogBankCircusExMortis a) = do
    -- "This location gets +2 shroud value. Each adjacent copy of Moonlit Forest gets +1
    -- shroud value."
    modifySelf a [ShroudModifier 2]
    modifySelect a (LocationWithTitle "Moonlit Forest" <> connectedTo (be a)) [ShroudModifier 1]

instance RunMessage MoonlitForestFogBankCircusExMortis where
  runMessage msg (MoonlitForestFogBankCircusExMortis attrs) = runQueueT $ case msg of
    _ -> MoonlitForestFogBankCircusExMortis <$> liftRunMessage msg attrs
