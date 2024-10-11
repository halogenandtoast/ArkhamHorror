module Arkham.Event.Events.ImprovisedWeapon (improvisedWeapon, ImprovisedWeapon (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Zone

newtype ImprovisedWeapon = ImprovisedWeapon EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisedWeapon :: EventCard ImprovisedWeapon
improvisedWeapon = event ImprovisedWeapon Cards.improvisedWeapon

instance RunMessage ImprovisedWeapon where
  runMessage msg e@(ImprovisedWeapon attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ _ zone | eid == toId attrs -> do
      sid <- getRandom
      when (zone == FromDiscard) $ skillTestModifier sid attrs iid (DamageDealt 1)
      enemyIds <- select $ CanFightEnemy (toSource attrs)
      chooseTargetM iid enemyIds \enemyId -> do
        skillTestModifier sid attrs enemyId (EnemyFight (-1))
        push $ FightEnemy sid iid enemyId (toSource attrs) Nothing SkillCombat False
      when (zone == FromDiscard) $ shuffleIntoDeck iid attrs
      pure e
    _ -> ImprovisedWeapon <$> liftRunMessage msg attrs
