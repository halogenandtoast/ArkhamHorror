module Arkham.Treachery.Cards.ChildrenOfBlood.BloodMoon.BloodMoon (bloodMoon) where

import Arkham.Helpers.ChaosBag (getRemainingBloodTokens)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodMoon qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BloodMoon = BloodMoon TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodMoon :: TreacheryCard BloodMoon
bloodMoon = treachery BloodMoon Cards.bloodMoon

instance RunMessage BloodMoon where
  runMessage msg t@(BloodMoon attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasBlood <- (> 0) <$> getRemainingBloodTokens
      chooseOneM iid $ withI18n do
        when hasBlood $ labeled' "addBloodToken" $ addChaosToken #blood
        countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
      pure t
    _ -> BloodMoon <$> liftRunMessage msg attrs
