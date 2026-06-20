module Arkham.Enemy.Cards.SadieSheldon (sadieSheldon) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers

newtype SadieSheldon = SadieSheldon EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sadieSheldon :: EnemyCard SadieSheldon
sadieSheldon = enemy SadieSheldon Cards.sadieSheldon

instance HasModifiersFor SadieSheldon where
  getModifiersFor (SadieSheldon attrs) = do
    n <- perPlayer 1
    modifySelf
      attrs
      [CannotMove, HealthModifier n, EnemyFight (min 3 attrs.damage)]

instance RunMessage SadieSheldon where
  runMessage msg (SadieSheldon attrs) = runQueueT $ SadieSheldon <$> liftRunMessage msg attrs
