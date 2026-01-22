module Arkham.Enemy.Cards.CrystalParasite (crystalParasite) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype CrystalParasite = CrystalParasite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

crystalParasite :: EnemyCard CrystalParasite
crystalParasite = enemy CrystalParasite Cards.crystalParasite (2, Static 6, 2) (1, 1)

instance RunMessage CrystalParasite where
  runMessage msg (CrystalParasite attrs) = runQueueT $ case msg of
    _ -> CrystalParasite <$> liftRunMessage msg attrs
