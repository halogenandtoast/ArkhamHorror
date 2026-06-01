module Arkham.Enemy.Cards.CrystalMimic (crystalMimic) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CrystalMimic = CrystalMimic EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalMimic :: EnemyCard CrystalMimic
crystalMimic = enemy CrystalMimic Cards.crystalMimic (-2, Static 5, -2) (1, 1)

instance RunMessage CrystalMimic where
  runMessage msg (CrystalMimic attrs) =
    runQueueT $ CrystalMimic <$> liftRunMessage msg attrs
