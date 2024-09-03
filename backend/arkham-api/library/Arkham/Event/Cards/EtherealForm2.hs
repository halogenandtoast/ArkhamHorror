module Arkham.Event.Cards.EtherealForm2 (etherealForm2, EtherealForm2 (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Strategy

newtype EtherealForm2 = EtherealForm2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

etherealForm2 :: EventCard EtherealForm2
etherealForm2 = event EtherealForm2 Cards.etherealForm2

instance RunMessage EtherealForm2 where
  runMessage msg e@(EtherealForm2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (AddSkillValue #willpower)
      onRevealChaosTokenEffect sid IsSymbol attrs attrs do
        eventModifier attrs attrs (SetAfterPlay ReturnThisToHand)
      chooseEvadeEnemy sid iid attrs
      pure e
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      enemies <- select $ enemyEngagedWith iid
      roundModifiers attrs iid [Ethereal, CannotBeEngaged, CannotAttack, CannotDealDamage]
      pushAll $ map (DisengageEnemy iid) enemies
      pure e
    _ -> EtherealForm2 <$> liftRunMessage msg attrs
