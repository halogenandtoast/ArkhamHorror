module Arkham.Treachery.Cards.MemoryOfOblivion
  ( memoryOfOblivion
  , MemoryOfOblivion(..)
  )
where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MemoryOfOblivion = MemoryOfOblivion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfOblivion :: TreacheryCard MemoryOfOblivion
memoryOfOblivion = treachery MemoryOfOblivion Cards.memoryOfOblivion

instance RunMessage MemoryOfOblivion where
  runMessage msg t@(MemoryOfOblivion attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> MemoryOfOblivion <$> liftRunMessage msg attrs
