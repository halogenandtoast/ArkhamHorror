module Arkham.Event.Events.ImprovisedWeapon2 (improvisedWeapon2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype ImprovisedWeapon2 = ImprovisedWeapon2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisedWeapon2 :: EventCard ImprovisedWeapon2
improvisedWeapon2 = event ImprovisedWeapon2 Cards.improvisedWeapon2

instance RunMessage ImprovisedWeapon2 where
  runMessage msg e@(ImprovisedWeapon2 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      when attrs.playedFromDiscard $ skillTestModifier sid attrs iid (DamageDealt 1)
      chooseFightEnemy sid iid attrs
      when attrs.playedFromDiscard do
        shuffleIntoDeck iid attrs
        drawCards iid attrs 1
      pure e
    ChoseEnemy sid _ (isSource attrs -> True) enemyId -> do
      skillTestModifier sid attrs enemyId (EnemyFight (-2))
      pure e
    _ -> ImprovisedWeapon2 <$> liftRunMessage msg attrs
