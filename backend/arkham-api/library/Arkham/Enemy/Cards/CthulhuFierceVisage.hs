module Arkham.Enemy.Cards.CthulhuFierceVisage (cthulhuFierceVisage) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CthulhuFierceVisage = CthulhuFierceVisage EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuFierceVisage :: EnemyCard CthulhuFierceVisage
cthulhuFierceVisage = enemy CthulhuFierceVisage Cards.cthulhuFierceVisage (0, Static 1, 0) (0, 1)

-- TODO: abilities
instance RunMessage CthulhuFierceVisage where
  runMessage msg (CthulhuFierceVisage attrs) = runQueueT $ CthulhuFierceVisage <$> liftRunMessage msg attrs
