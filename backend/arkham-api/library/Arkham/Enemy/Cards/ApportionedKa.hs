module Arkham.Enemy.Cards.ApportionedKa (apportionedKa) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ApportionedKa = ApportionedKa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

apportionedKa :: EnemyCard ApportionedKa
apportionedKa = enemyWith
  ApportionedKa
  Cards.apportionedKa
  (0, Static 1, 0)
  (1, 1)
  \e -> e {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing}

instance RunMessage ApportionedKa where
  runMessage msg (ApportionedKa attrs) = runQueueT $ case msg of
    _ -> ApportionedKa <$> liftRunMessage msg attrs
