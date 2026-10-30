-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaDelParcheLaDolceVita (trattoriaDelParcheLaDolceVita) where

import Arkham.Card (genCard)
import Arkham.Helpers.Location (swapLocation)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype TrattoriaDelParcheLaDolceVita = TrattoriaDelParcheLaDolceVita LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trattoriaDelParcheLaDolceVita :: LocationCard TrattoriaDelParcheLaDolceVita
trattoriaDelParcheLaDolceVita = location TrattoriaDelParcheLaDolceVita Cards.trattoriaDelParcheLaDolceVita 2 (Static 0)

instance RunMessage TrattoriaDelParcheLaDolceVita where
  runMessage msg l@(TrattoriaDelParcheLaDolceVita attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Cards.trattoriaDelParcheLaBruttaVita
      pure l
    _ -> TrattoriaDelParcheLaDolceVita <$> liftRunMessage msg attrs
