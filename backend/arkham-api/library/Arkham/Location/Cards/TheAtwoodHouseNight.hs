module Arkham.Location.Cards.TheAtwoodHouseNight (theAtwoodHouseNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheAtwoodHouseNight = TheAtwoodHouseNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAtwoodHouseNight :: LocationCard TheAtwoodHouseNight
theAtwoodHouseNight = symbolLabel $ location TheAtwoodHouseNight Cards.theAtwoodHouseNight 0 (Static 0)

instance HasAbilities TheAtwoodHouseNight where
  getAbilities (TheAtwoodHouseNight a) =
    extendRevealed a []

instance RunMessage TheAtwoodHouseNight where
  runMessage msg (TheAtwoodHouseNight attrs) = runQueueT $ case msg of
    _ -> TheAtwoodHouseNight <$> liftRunMessage msg attrs
