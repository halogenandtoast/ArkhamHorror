module Arkham.Location.Cards.FrozenShores (frozenShores, FrozenShores (..)) where

import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype FrozenShores = FrozenShores LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

frozenShores :: LocationCard FrozenShores
frozenShores = symbolLabel $ location FrozenShores Cards.frozenShores 2 (PerPlayer 1)

instance HasModifiersFor FrozenShores where
  getModifiersFor (FrozenShores a) = modifySelfMaybe a do
    n <- selectCount $ TreacheryAttachedToLocation (be a)
    guard $ n > 0
    pure [ShroudModifier (n * 2)]

instance RunMessage FrozenShores where
  runMessage msg (FrozenShores attrs) = runQueueT $ case msg of
    _ -> FrozenShores <$> liftRunMessage msg attrs
