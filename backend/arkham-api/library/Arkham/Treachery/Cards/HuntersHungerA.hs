module Arkham.Treachery.Cards.HuntersHungerA (huntersHungerA) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HuntersHungerA = HuntersHungerA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntersHungerA :: TreacheryCard HuntersHungerA
huntersHungerA = treachery HuntersHungerA Cards.huntersHungerA

instance RunMessage HuntersHungerA where
  runMessage msg t@(HuntersHungerA attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> HuntersHungerA <$> liftRunMessage msg attrs
