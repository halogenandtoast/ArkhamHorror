module Arkham.Enemy.Cards.MiGoDrone (miGoDrone) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype MiGoDrone = MiGoDrone EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

miGoDrone :: EnemyCard MiGoDrone
miGoDrone = enemy MiGoDrone Cards.miGoDrone (3, Static 1, 1) (0, 1)

instance RunMessage MiGoDrone where
  runMessage msg (MiGoDrone attrs) = MiGoDrone <$> runMessage msg attrs
