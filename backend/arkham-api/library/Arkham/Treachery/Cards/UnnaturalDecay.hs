module Arkham.Treachery.Cards.UnnaturalDecay (unnaturalDecay) where

import Arkham.Helpers.SkillTest.Lifted (beginSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted hiding (beginSkillTest)

newtype UnnaturalDecay = UnnaturalDecay TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unnaturalDecay :: TreacheryCard UnnaturalDecay
unnaturalDecay = treachery UnnaturalDecay Cards.unnaturalDecay

instance RunMessage UnnaturalDecay where
  runMessage msg t@(UnnaturalDecay attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      beginSkillTest sid iid (toSource attrs) iid #willpower (Fixed 4)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assets <- select $ AssetControlledBy (InvestigatorWithId iid)
      if notNull assets
        then chooseOneM iid do
          labeled "Place 1 doom on an asset you control" do
            chooseTargetM iid assets $ \aid -> placeDoom (toSource attrs) aid 1
          labeled "Take 2 damage" $ assignDamage iid attrs 2
        else assignDamage iid attrs 2
      pure t
    _ -> UnnaturalDecay <$> liftRunMessage msg attrs
