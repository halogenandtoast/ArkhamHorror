module Arkham.Enemy.Cards.BeastOfAldebaran (beastOfAldebaran, BeastOfAldebaran (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype BeastOfAldebaran = BeastOfAldebaran EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

beastOfAldebaran :: EnemyCard BeastOfAldebaran
beastOfAldebaran =
  enemyWith
    BeastOfAldebaran
    Cards.beastOfAldebaran
    (3, Static 7, 5)
    (2, 1)
    (damageStrategyL .~ SingleTarget)

instance RunMessage BeastOfAldebaran where
  runMessage msg (BeastOfAldebaran attrs) =
    BeastOfAldebaran <$> runMessage msg attrs
