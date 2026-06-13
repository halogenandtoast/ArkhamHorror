module Arkham.Enemy.Cards.VaultAttendant (vaultAttendant) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype VaultAttendant = VaultAttendant EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultAttendant :: EnemyCard VaultAttendant
vaultAttendant = enemy VaultAttendant Cards.vaultAttendant (2, Static 2, 4) (2, 0)

-- TODO: abilities
instance RunMessage VaultAttendant where
  runMessage msg (VaultAttendant attrs) = runQueueT $ VaultAttendant <$> liftRunMessage msg attrs
