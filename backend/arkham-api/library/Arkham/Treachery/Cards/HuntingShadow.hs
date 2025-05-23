module Arkham.Treachery.Cards.HuntingShadow (huntingShadow) where

import Arkham.Helpers.Investigator
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HuntingShadow = HuntingShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingShadow :: TreacheryCard HuntingShadow
huntingShadow = treachery HuntingShadow Cards.huntingShadow

instance RunMessage HuntingShadow where
  runMessage msg t@(HuntingShadow attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOrRunOneM iid $ withI18n do
        whenM (getCanSpendNClues iid 1) do
          countVar 1 $ labeled' "spendClues" $ spendClues iid 1
        countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
      pure t
    _ -> HuntingShadow <$> liftRunMessage msg attrs
