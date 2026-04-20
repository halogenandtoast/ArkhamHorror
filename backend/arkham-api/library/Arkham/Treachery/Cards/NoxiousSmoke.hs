module Arkham.Treachery.Cards.NoxiousSmoke (noxiousSmoke) where

import Arkham.Helpers.SkillTest.Lifted (beginSkillTest)
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (beginSkillTest)

newtype NoxiousSmoke = NoxiousSmoke TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noxiousSmoke :: TreacheryCard NoxiousSmoke
noxiousSmoke = treachery NoxiousSmoke Cards.noxiousSmoke

instance RunMessage NoxiousSmoke where
  runMessage msg t@(NoxiousSmoke attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseOneM iid do
        labeled "Test Intellect (3)" do
          beginSkillTest sid iid (toSource attrs) iid #intellect (Fixed 3)
        labeled "Test Agility (3)" do
          beginSkillTest sid iid (toSource attrs) iid #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      assignDamage iid attrs n
      pure t
    _ -> NoxiousSmoke <$> liftRunMessage msg attrs
