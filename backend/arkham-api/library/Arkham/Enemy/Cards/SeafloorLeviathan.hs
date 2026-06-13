module Arkham.Enemy.Cards.SeafloorLeviathan (seafloorLeviathan) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SeafloorLeviathan = SeafloorLeviathan EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seafloorLeviathan :: EnemyCard SeafloorLeviathan
seafloorLeviathan = enemy SeafloorLeviathan Cards.seafloorLeviathan (2, Static 4, 2) (1, 1)

-- TODO: abilities
instance RunMessage SeafloorLeviathan where
  runMessage msg (SeafloorLeviathan attrs) = runQueueT $ SeafloorLeviathan <$> liftRunMessage msg attrs
