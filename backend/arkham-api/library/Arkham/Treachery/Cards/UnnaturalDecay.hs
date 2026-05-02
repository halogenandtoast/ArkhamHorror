module Arkham.Treachery.Cards.UnnaturalDecay (unnaturalDecay) where

import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnnaturalDecay = UnnaturalDecay TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unnaturalDecay :: TreacheryCard UnnaturalDecay
unnaturalDecay = treachery UnnaturalDecay Cards.unnaturalDecay

instance RunMessage UnnaturalDecay where
  runMessage msg t@(UnnaturalDecay attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assets <- select $ assetControlledBy iid
      if null assets
        then assignDamage iid attrs 2
        else chooseOneM iid $ withI18n do
          countVar 1 $ labeled' "placeDoom" do
            chooseTargetM iid assets $ placeDoomOn attrs 1
          countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
      pure t
    _ -> UnnaturalDecay <$> liftRunMessage msg attrs
