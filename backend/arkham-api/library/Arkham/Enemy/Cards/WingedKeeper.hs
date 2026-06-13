module Arkham.Enemy.Cards.WingedKeeper (wingedKeeper) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype WingedKeeper = WingedKeeper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wingedKeeper :: EnemyCard WingedKeeper
wingedKeeper = enemy WingedKeeper Cards.wingedKeeper (3, Static 3, 4) (1, 1)

-- TODO: abilities
instance RunMessage WingedKeeper where
  runMessage msg (WingedKeeper attrs) = runQueueT $ WingedKeeper <$> liftRunMessage msg attrs
