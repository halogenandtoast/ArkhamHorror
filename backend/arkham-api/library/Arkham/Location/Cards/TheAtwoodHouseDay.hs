module Arkham.Location.Cards.TheAtwoodHouseDay (theAtwoodHouseDay) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheAtwoodHouseDay = TheAtwoodHouseDay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theAtwoodHouseDay :: LocationCard TheAtwoodHouseDay
theAtwoodHouseDay = symbolLabel $ location TheAtwoodHouseDay Cards.theAtwoodHouseDay 0 (Static 0)

instance HasAbilities TheAtwoodHouseDay where
  getAbilities (TheAtwoodHouseDay a) =
    extendRevealed a []

instance RunMessage TheAtwoodHouseDay where
  runMessage msg (TheAtwoodHouseDay attrs) = runQueueT $ case msg of
    _ -> TheAtwoodHouseDay <$> liftRunMessage msg attrs
