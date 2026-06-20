module Arkham.Enemy.Cards.BeastOfAldebaran (beastOfAldebaran) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Strategy

newtype BeastOfAldebaran = BeastOfAldebaran EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

beastOfAldebaran :: EnemyCard BeastOfAldebaran
beastOfAldebaran =
  enemyWith
    BeastOfAldebaran
    Cards.beastOfAldebaran
    (damageStrategyL .~ SingleTarget)

instance RunMessage BeastOfAldebaran where
  runMessage msg (BeastOfAldebaran attrs) = BeastOfAldebaran <$> runMessage msg attrs
