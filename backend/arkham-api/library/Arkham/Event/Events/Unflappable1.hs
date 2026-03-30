module Arkham.Event.Events.Unflappable1 (unflappable1) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher

newtype Unflappable1 = Unflappable1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unflappable1 :: EventCard Unflappable1
unflappable1 = event Unflappable1 Cards.unflappable1

instance RunMessage Unflappable1 where
  runMessage msg e@(Unflappable1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      modifySkill sid attrs iid #agility 3
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTest _iid (isSource attrs -> True) -> do
      doStep 2 msg
      pure e
    DoStep 0 (PassedThisSkillTest _iid (isSource attrs -> True)) -> do
      applyHealing attrs
      pure e
    DoStep n msg'@(PassedThisSkillTest iid (isSource attrs -> True)) | n > 0 -> do
      investigators <- select $ HealableInvestigator (toSource attrs) #horror (InvestigatorWithId iid)
      assets <- select $ HealableAsset (toSource attrs) #horror (assetControlledBy iid)
      chooseOrRunOneM iid do
        targets investigators $ healHorrorDelayedOn attrs 1
        targets assets $ healHorrorDelayedOn attrs 1
      doStep (n - 1) msg'
      pure e
    _ -> Unflappable1 <$> liftRunMessage msg attrs
