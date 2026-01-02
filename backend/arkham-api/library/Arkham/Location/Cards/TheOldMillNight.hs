module Arkham.Location.Cards.TheOldMillNight (theOldMillNight) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheOldMillNight = TheOldMillNight LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldMillNight :: LocationCard TheOldMillNight
theOldMillNight = symbolLabel $ location TheOldMillNight Cards.theOldMillNight 0 (Static 0)

instance HasAbilities TheOldMillNight where
  getAbilities (TheOldMillNight a) =
    extendRevealed a []

instance RunMessage TheOldMillNight where
  runMessage msg (TheOldMillNight attrs) = runQueueT $ case msg of
    _ -> TheOldMillNight <$> liftRunMessage msg attrs
