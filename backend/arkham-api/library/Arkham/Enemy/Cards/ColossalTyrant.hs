module Arkham.Enemy.Cards.ColossalTyrant (colossalTyrant) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ColossalTyrant = ColossalTyrant EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

colossalTyrant :: EnemyCard ColossalTyrant
colossalTyrant = enemy ColossalTyrant Cards.colossalTyrant (3, Static 6, 3) (2, 0)

-- TODO: abilities
instance RunMessage ColossalTyrant where
  runMessage msg (ColossalTyrant attrs) = runQueueT $ ColossalTyrant <$> liftRunMessage msg attrs
