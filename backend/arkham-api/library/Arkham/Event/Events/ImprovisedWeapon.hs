module Arkham.Event.Events.ImprovisedWeapon (improvisedWeapon) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Modifier

newtype ImprovisedWeapon = ImprovisedWeapon EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisedWeapon :: EventCard ImprovisedWeapon
improvisedWeapon = event ImprovisedWeapon Cards.improvisedWeapon

instance RunMessage ImprovisedWeapon where
  runMessage msg e@(ImprovisedWeapon attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      when attrs.playedFromDiscard $ skillTestModifier sid attrs iid (DamageDealt 1)
      chooseFightEnemy sid iid attrs
      when attrs.playedFromDiscard $ shuffleIntoDeck iid attrs
      pure e
    ChoseEnemy sid _ (isSource attrs -> True) enemyId -> do
      skillTestModifier sid attrs enemyId (EnemyFight (-1))
      pure e
    _ -> ImprovisedWeapon <$> liftRunMessage msg attrs
