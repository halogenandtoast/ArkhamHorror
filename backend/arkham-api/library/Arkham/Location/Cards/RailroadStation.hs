module Arkham.Location.Cards.RailroadStation (
  railroadStation,
  RailroadStation (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers (connectsToAdjacent)
import Arkham.Location.Import.Lifted

newtype RailroadStation = RailroadStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

railroadStation :: LocationCard RailroadStation
railroadStation = locationWith RailroadStation Cards.railroadStation 1 (Static 0) connectsToAdjacent

instance HasAbilities RailroadStation where
  getAbilities (RailroadStation attrs) =
    extendRevealed attrs []

instance RunMessage RailroadStation where
  runMessage msg (RailroadStation attrs) = runQueueT $ case msg of
    _ -> RailroadStation <$> liftRunMessage msg attrs
