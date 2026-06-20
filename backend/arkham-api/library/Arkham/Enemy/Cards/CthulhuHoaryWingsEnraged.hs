module Arkham.Enemy.Cards.CthulhuHoaryWingsEnraged (cthulhuHoaryWingsEnraged) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CthulhuHoaryWingsEnraged = CthulhuHoaryWingsEnraged EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cthulhuHoaryWingsEnraged :: EnemyCard CthulhuHoaryWingsEnraged
cthulhuHoaryWingsEnraged = enemy CthulhuHoaryWingsEnraged Cards.cthulhuHoaryWingsEnraged

-- TODO: abilities
instance RunMessage CthulhuHoaryWingsEnraged where
  runMessage msg (CthulhuHoaryWingsEnraged attrs) = runQueueT $ CthulhuHoaryWingsEnraged <$> liftRunMessage msg attrs
