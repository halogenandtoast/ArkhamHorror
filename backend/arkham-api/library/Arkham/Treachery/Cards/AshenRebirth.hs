module Arkham.Treachery.Cards.AshenRebirth (ashenRebirth) where

import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype AshenRebirth = AshenRebirth TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ashenRebirth :: TreacheryCard AshenRebirth
ashenRebirth = treachery AshenRebirth Cards.ashenRebirth

instance RunMessage AshenRebirth where
  runMessage msg t@(AshenRebirth attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
        sid <- getRandom
        revelationSkillTest sid iid attrs #willpower (Fixed 4)
        pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid do
        labeled "Take 2 direct damage" $ push $ InvestigatorDirectDamage iid (toSource attrs) 2 0
        labeled "Take 2 direct horror" $ push $ InvestigatorDirectDamage iid (toSource attrs) 0 2
      pure t
    _ -> AshenRebirth <$> liftRunMessage msg attrs
