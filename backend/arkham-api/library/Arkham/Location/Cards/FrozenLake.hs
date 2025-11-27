module Arkham.Location.Cards.FrozenLake (frozenLake) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FrozenLake = FrozenLake LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenLake :: LocationCard FrozenLake
frozenLake = location FrozenLake Cards.frozenLake 3 (PerPlayer 2)

instance HasAbilities FrozenLake where
  getAbilities (FrozenLake attrs) =
    extendRevealed attrs []

instance RunMessage FrozenLake where
  runMessage msg (FrozenLake attrs) = runQueueT $ case msg of
    _ -> FrozenLake <$> liftRunMessage msg attrs
