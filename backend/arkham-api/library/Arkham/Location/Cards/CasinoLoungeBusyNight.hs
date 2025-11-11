module Arkham.Location.Cards.CasinoLoungeBusyNight (casinoLoungeBusyNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CasinoLoungeBusyNight = CasinoLoungeBusyNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casinoLoungeBusyNight :: LocationCard CasinoLoungeBusyNight
casinoLoungeBusyNight = symbolLabel $ location CasinoLoungeBusyNight Cards.casinoLoungeBusyNight 0 (Static 0)

instance HasAbilities CasinoLoungeBusyNight where
  getAbilities (CasinoLoungeBusyNight attrs) =
    extendRevealed attrs []

instance RunMessage CasinoLoungeBusyNight where
  runMessage msg (CasinoLoungeBusyNight attrs) = runQueueT $ case msg of
    _ -> CasinoLoungeBusyNight <$> liftRunMessage msg attrs
