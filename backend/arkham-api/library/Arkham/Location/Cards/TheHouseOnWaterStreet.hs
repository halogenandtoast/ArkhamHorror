module Arkham.Location.Cards.TheHouseOnWaterStreet
  ( theHouseOnWaterStreet
  , TheHouseOnWaterStreet(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype TheHouseOnWaterStreet = TheHouseOnWaterStreet LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHouseOnWaterStreet :: LocationCard TheHouseOnWaterStreet
theHouseOnWaterStreet = location TheHouseOnWaterStreet Cards.theHouseOnWaterStreet 1 (PerPlayer 2)

instance HasAbilities TheHouseOnWaterStreet where
  getAbilities (TheHouseOnWaterStreet attrs) =
    extendRevealed attrs []

instance RunMessage TheHouseOnWaterStreet where
  runMessage msg (TheHouseOnWaterStreet attrs) = runQueueT $ case msg of
    _ -> TheHouseOnWaterStreet <$> liftRunMessage msg attrs
