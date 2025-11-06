module Arkham.Location.Cards.PlazaHotel (plazaHotel) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype PlazaHotel = PlazaHotel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

plazaHotel :: LocationCard PlazaHotel
plazaHotel = symbolLabel $ location PlazaHotel Cards.plazaHotel 0 (Static 0)

instance HasAbilities PlazaHotel where
  getAbilities (PlazaHotel attrs) =
    extendRevealed attrs []

instance RunMessage PlazaHotel where
  runMessage msg (PlazaHotel attrs) = runQueueT $ case msg of
    _ -> PlazaHotel <$> liftRunMessage msg attrs
