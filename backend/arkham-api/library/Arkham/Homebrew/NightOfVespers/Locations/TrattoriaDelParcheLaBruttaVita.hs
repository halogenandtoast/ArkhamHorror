-- Scaffold: printed card abilities are not implemented yet.
module Arkham.Homebrew.NightOfVespers.Locations.TrattoriaDelParcheLaBruttaVita (trattoriaDelParcheLaBruttaVita) where

import Arkham.Card (genCard)
import Arkham.Helpers.Location (swapLocation)
import Arkham.Homebrew.NightOfVespers.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted

newtype TrattoriaDelParcheLaBruttaVita = TrattoriaDelParcheLaBruttaVita LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

trattoriaDelParcheLaBruttaVita :: LocationCard TrattoriaDelParcheLaBruttaVita
trattoriaDelParcheLaBruttaVita =
  symbolLabel
    $ location TrattoriaDelParcheLaBruttaVita Cards.trattoriaDelParcheLaBruttaVita 2 (PerPlayer 1)

instance RunMessage TrattoriaDelParcheLaBruttaVita where
  runMessage msg l@(TrattoriaDelParcheLaBruttaVita attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      swapLocation attrs =<< genCard Cards.trattoriaDelParcheLaDolceVita
      pure l
    _ -> TrattoriaDelParcheLaBruttaVita <$> liftRunMessage msg attrs
