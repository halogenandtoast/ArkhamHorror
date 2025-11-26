module Arkham.Location.Cards.MountainStream (mountainStream) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MountainStream = MountainStream LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mountainStream :: LocationCard MountainStream
mountainStream = location MountainStream Cards.mountainStream 0 (Static 0)

instance HasAbilities MountainStream where
  getAbilities (MountainStream attrs) =
    extendRevealed attrs []

instance RunMessage MountainStream where
  runMessage msg (MountainStream attrs) = runQueueT $ case msg of
    _ -> MountainStream <$> liftRunMessage msg attrs
