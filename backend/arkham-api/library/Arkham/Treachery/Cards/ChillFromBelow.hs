module Arkham.Treachery.Cards.ChillFromBelow (chillFromBelow) where

import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ChillFromBelow = ChillFromBelow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillFromBelow :: TreacheryCard ChillFromBelow
chillFromBelow = treachery ChillFromBelow Cards.chillFromBelow

instance RunMessage ChillFromBelow where
  runMessage msg t@(ChillFromBelow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid source@(isSource attrs -> True) n -> do
      handCount <- fieldMap InvestigatorHand length iid
      randomDiscardN iid attrs (min n handCount)
      when (n - handCount > 0) $ assignDamage iid source (n - handCount)
      pure t
    _ -> ChillFromBelow <$> liftRunMessage msg attrs
