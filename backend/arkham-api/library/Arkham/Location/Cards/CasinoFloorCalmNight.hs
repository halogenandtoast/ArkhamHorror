module Arkham.Location.Cards.CasinoFloorCalmNight (casinoFloorCalmNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CasinoFloorCalmNight = CasinoFloorCalmNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoFloorCalmNight :: LocationCard CasinoFloorCalmNight
casinoFloorCalmNight = symbolLabel $ location CasinoFloorCalmNight Cards.casinoFloorCalmNight 0 (Static 0)

instance HasAbilities CasinoFloorCalmNight where
  getAbilities (CasinoFloorCalmNight attrs) =
    extendRevealed attrs []

instance RunMessage CasinoFloorCalmNight where
  runMessage msg (CasinoFloorCalmNight attrs) = runQueueT $ case msg of
    _ -> CasinoFloorCalmNight <$> liftRunMessage msg attrs
