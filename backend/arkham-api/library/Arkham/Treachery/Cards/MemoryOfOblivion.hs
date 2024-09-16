module Arkham.Treachery.Cards.MemoryOfOblivion (
  memoryOfOblivion,
  MemoryOfOblivion (..),
)
where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MemoryOfOblivion = MemoryOfOblivion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

memoryOfOblivion :: TreacheryCard MemoryOfOblivion
memoryOfOblivion = treachery MemoryOfOblivion Cards.memoryOfOblivion

instance RunMessage MemoryOfOblivion where
  runMessage msg t@(MemoryOfOblivion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #elderthing attrs attrs failSkillTest
      chooseOneM iid do
        for_ [#willpower, #intellect] \skill -> skillLabeled skill $ revelationSkillTest sid iid attrs skill (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      discardFromHand iid attrs DiscardChoose n
      pure t
    _ -> MemoryOfOblivion <$> liftRunMessage msg attrs
