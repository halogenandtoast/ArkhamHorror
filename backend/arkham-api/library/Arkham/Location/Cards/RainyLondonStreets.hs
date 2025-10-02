module Arkham.Location.Cards.RainyLondonStreets (rainyLondonStreets) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RainyLondonStreets = RainyLondonStreets LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rainyLondonStreets :: LocationCard RainyLondonStreets
rainyLondonStreets = symbolLabel $ location RainyLondonStreets Cards.rainyLondonStreets 1 (PerPlayer 2)

instance HasAbilities RainyLondonStreets where
  getAbilities (RainyLondonStreets attrs) =
    extendRevealed attrs []

instance RunMessage RainyLondonStreets where
  runMessage msg (RainyLondonStreets attrs) = runQueueT $ case msg of
    _ -> RainyLondonStreets <$> liftRunMessage msg attrs
