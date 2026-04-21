module Arkham.Location.Cards.DiningRoomHemlockHouse (diningRoomHemlockHouse) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype DiningRoomHemlockHouse = DiningRoomHemlockHouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

diningRoomHemlockHouse :: LocationCard DiningRoomHemlockHouse
diningRoomHemlockHouse = symbolLabel $ location DiningRoomHemlockHouse Cards.diningRoomHemlockHouse 2 (PerPlayer 3)

instance HasAbilities DiningRoomHemlockHouse where
  getAbilities (DiningRoomHemlockHouse a) =
    extendRevealed a []

instance RunMessage DiningRoomHemlockHouse where
  runMessage msg (DiningRoomHemlockHouse attrs) = runQueueT $ case msg of
    _ -> DiningRoomHemlockHouse <$> liftRunMessage msg attrs
