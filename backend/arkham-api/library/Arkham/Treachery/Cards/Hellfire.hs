module Arkham.Treachery.Cards.Hellfire (hellfire) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Hellfire = Hellfire TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hellfire :: TreacheryCard Hellfire
hellfire = treachery Hellfire Cards.hellfire

instance RunMessage Hellfire where
  runMessage msg t@(Hellfire attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid1 <- getRandom
      revelationSkillTest sid1 iid attrs #agility (Fixed 1)
      sid2 <- getRandom
      revelationSkillTest sid2 iid attrs #agility (Fixed 1)
      sid3 <- getRandom
      revelationSkillTest sid3 iid attrs #agility (Fixed 1)
      doStep 2 msg
      pure t
    FailedThisSkillTest _iid (isSource attrs -> True) -> do
      let n :: Int = toResultDefault 0 attrs.meta
      pure $ Hellfire $ attrs & setMeta (n + 1)
    DoStep 2 (Revelation iid (isSource attrs -> True)) -> do
      let n :: Int = toResultDefault 0 attrs.meta
      when (n > 0) $ assignDamage iid attrs n
      pure t
    _ -> Hellfire <$> liftRunMessage msg attrs
