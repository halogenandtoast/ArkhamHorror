module Arkham.Location.Cards.LanternRoom (lanternRoom, LanternRoom (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype LanternRoom = LanternRoom LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lanternRoom :: LocationCard LanternRoom
lanternRoom = location LanternRoom Cards.lanternRoom 2 (PerPlayer 2)

instance HasModifiersFor LanternRoom where
  getModifiersFor target (LanternRoom attrs) = preventDrawConnections target attrs

instance HasAbilities LanternRoom where
  getAbilities (LanternRoom attrs) =
    extendRevealed attrs []

instance RunMessage LanternRoom where
  runMessage msg l@(LanternRoom attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> LanternRoom <$> liftRunMessage msg attrs
