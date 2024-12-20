module Arkham.Location.Cards.AlaskanWilds (alaskanWilds) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype AlaskanWilds = AlaskanWilds LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alaskanWilds :: LocationCard AlaskanWilds
alaskanWilds = location AlaskanWilds Cards.alaskanWilds 3 (PerPlayer 2)

instance HasAbilities AlaskanWilds where
  getAbilities (AlaskanWilds attrs) =
    extendRevealed attrs []

instance RunMessage AlaskanWilds where
  runMessage msg (AlaskanWilds attrs) = runQueueT $ case msg of
    _ -> AlaskanWilds <$> liftRunMessage msg attrs
