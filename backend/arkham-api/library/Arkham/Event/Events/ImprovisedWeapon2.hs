module Arkham.Event.Events.ImprovisedWeapon2 (improvisedWeapon2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Matcher
import Arkham.Fight
import Arkham.Modifier
import Arkham.Zone

newtype ImprovisedWeapon2 = ImprovisedWeapon2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisedWeapon2 :: EventCard ImprovisedWeapon2
improvisedWeapon2 = event ImprovisedWeapon2 Cards.improvisedWeapon2

instance RunMessage ImprovisedWeapon2 where
  runMessage msg e@(ImprovisedWeapon2 attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ _ zone | eid == toId attrs -> do
      sid <- getRandom
      when (zone == FromDiscard) $ skillTestModifier sid attrs iid (DamageDealt 1)
      enemyIds <- select $ CanFightEnemy (toSource attrs)
      chooseTargetM iid enemyIds \enemyId -> do
        skillTestModifier sid attrs enemyId (EnemyFight (-2))
        push $ FightEnemy enemyId $ mkChooseFightPure sid iid attrs
      when (zone == FromDiscard) do
        shuffleIntoDeck iid attrs
        drawCards iid attrs 1
      pure e
    _ -> ImprovisedWeapon2 <$> liftRunMessage msg attrs
