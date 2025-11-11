module Arkham.Location.Cards.CasinoLoungeCalmNight (casinoLoungeCalmNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CasinoLoungeCalmNight = CasinoLoungeCalmNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoLoungeCalmNight :: LocationCard CasinoLoungeCalmNight
casinoLoungeCalmNight = symbolLabel $ location CasinoLoungeCalmNight Cards.casinoLoungeCalmNight 0 (Static 0)

instance HasAbilities CasinoLoungeCalmNight where
  getAbilities (CasinoLoungeCalmNight attrs) =
    extendRevealed attrs []

instance RunMessage CasinoLoungeCalmNight where
  runMessage msg (CasinoLoungeCalmNight attrs) = runQueueT $ case msg of
    _ -> CasinoLoungeCalmNight <$> liftRunMessage msg attrs
