module Arkham.Location.Cards.MapRoom (mapRoom) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MapRoom = MapRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mapRoom :: LocationCard MapRoom
mapRoom = location MapRoom Cards.mapRoom 2 (PerPlayer 1)

instance HasAbilities MapRoom where
  getAbilities (MapRoom attrs) =
    extendRevealed attrs []

instance RunMessage MapRoom where
  runMessage msg (MapRoom attrs) = runQueueT $ case msg of
    _ -> MapRoom <$> liftRunMessage msg attrs
