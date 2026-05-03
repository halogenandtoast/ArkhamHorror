module Arkham.Treachery.Cards.NoxiousSmoke (noxiousSmoke) where

import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype NoxiousSmoke = NoxiousSmoke TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

noxiousSmoke :: TreacheryCard NoxiousSmoke
noxiousSmoke = treachery NoxiousSmoke Cards.noxiousSmoke

instance RunMessage NoxiousSmoke where
  runMessage msg t@(NoxiousSmoke attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      chooseRevelationSkillTest sid iid attrs [#willpower, #agility] (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      assignDamage iid attrs n
      pure t
    _ -> NoxiousSmoke <$> liftRunMessage msg attrs
