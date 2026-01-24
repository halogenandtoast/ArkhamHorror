module Arkham.Treachery.Cards.Swarm (swarm) where

import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Swarm = Swarm TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swarm :: TreacheryCard Swarm
swarm = treachery Swarm Cards.swarm

instance RunMessage Swarm where
  runMessage msg t@(Swarm attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTestBy _iid (isSource attrs -> True) n -> do
      doStep n msg
      pure t
    DoStep n msg'@(FailedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      resourceOk <- (> 0) <$> iid.resources
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeledValidate' resourceOk "loseResources" $ loseResources iid attrs 1
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid attrs 1
      doStep (n - 1) msg'
      pure t
    _ -> Swarm <$> liftRunMessage msg attrs
