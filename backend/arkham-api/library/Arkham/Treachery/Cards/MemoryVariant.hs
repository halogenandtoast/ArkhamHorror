module Arkham.Treachery.Cards.MemoryVariant (memoryVariant) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MemoryVariant = MemoryVariant TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryVariant :: TreacheryCard MemoryVariant
memoryVariant = treachery MemoryVariant Cards.memoryVariant

instance RunMessage MemoryVariant where
  runMessage msg t@(MemoryVariant attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> MemoryVariant <$> liftRunMessage msg attrs
