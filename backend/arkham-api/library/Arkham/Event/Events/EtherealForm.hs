module Arkham.Event.Events.EtherealForm (etherealForm, EtherealForm (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype EtherealForm = EtherealForm EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealForm :: EventCard EtherealForm
etherealForm = event EtherealForm Cards.etherealForm

instance RunMessage EtherealForm where
  runMessage msg e@(EtherealForm attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #willpower)
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      enemies <- select $ enemyEngagedWith iid
      roundModifiers attrs iid [Ethereal, CannotBeEngaged, CannotAttack, CannotDealDamage]
      pushAll $ map (DisengageEnemy iid) enemies
      pure e
    _ -> EtherealForm <$> liftRunMessage msg attrs
