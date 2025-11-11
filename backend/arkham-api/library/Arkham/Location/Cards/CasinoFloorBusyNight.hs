module Arkham.Location.Cards.CasinoFloorBusyNight (casinoFloorBusyNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CasinoFloorBusyNight = CasinoFloorBusyNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoFloorBusyNight :: LocationCard CasinoFloorBusyNight
casinoFloorBusyNight = symbolLabel $ location CasinoFloorBusyNight Cards.casinoFloorBusyNight 0 (Static 0)

instance HasAbilities CasinoFloorBusyNight where
  getAbilities (CasinoFloorBusyNight attrs) =
    extendRevealed attrs []

instance RunMessage CasinoFloorBusyNight where
  runMessage msg (CasinoFloorBusyNight attrs) = runQueueT $ case msg of
    _ -> CasinoFloorBusyNight <$> liftRunMessage msg attrs
