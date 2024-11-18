module Arkham.Location.Cards.FrozenShores (frozenShores, FrozenShores (..)) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype FrozenShores = FrozenShores LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

frozenShores :: LocationCard FrozenShores
frozenShores = symbolLabel $ location FrozenShores Cards.frozenShores 0 (Static 0)

instance HasAbilities FrozenShores where
  getAbilities (FrozenShores attrs) =
    extendRevealed attrs []

instance RunMessage FrozenShores where
  runMessage msg (FrozenShores attrs) = runQueueT $ case msg of
    _ -> FrozenShores <$> liftRunMessage msg attrs
