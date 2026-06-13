module Arkham.Enemy.Cards.CthulhuHoaryWings (cthulhuHoaryWings) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CthulhuHoaryWings = CthulhuHoaryWings EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuHoaryWings :: EnemyCard CthulhuHoaryWings
cthulhuHoaryWings = enemy CthulhuHoaryWings Cards.cthulhuHoaryWings (0, Static 1, 0) (1, 1)

-- TODO: abilities
instance RunMessage CthulhuHoaryWings where
  runMessage msg (CthulhuHoaryWings attrs) = runQueueT $ CthulhuHoaryWings <$> liftRunMessage msg attrs
