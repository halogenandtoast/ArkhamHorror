module Arkham.Location.Cards.CountingRoom (countingRoom) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype CountingRoom = CountingRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

countingRoom :: LocationCard CountingRoom
countingRoom = symbolLabel $ location CountingRoom Cards.countingRoom 0 (Static 0)

instance HasAbilities CountingRoom where
  getAbilities (CountingRoom attrs) =
    extendRevealed attrs []

instance RunMessage CountingRoom where
  runMessage msg (CountingRoom attrs) = runQueueT $ case msg of
    _ -> CountingRoom <$> liftRunMessage msg attrs
