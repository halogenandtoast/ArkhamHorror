module Arkham.Location.Cards.TheOldMillDay (theOldMillDay) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheOldMillDay = TheOldMillDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldMillDay :: LocationCard TheOldMillDay
theOldMillDay = symbolLabel $ location TheOldMillDay Cards.theOldMillDay 0 (Static 0)

instance HasAbilities TheOldMillDay where
  getAbilities (TheOldMillDay a) =
    extendRevealed a []

instance RunMessage TheOldMillDay where
  runMessage msg (TheOldMillDay attrs) = runQueueT $ case msg of
    _ -> TheOldMillDay <$> liftRunMessage msg attrs
