module Arkham.Enemy.Cards.GrapplingSpawn (grapplingSpawn) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Projection
import Arkham.Scenarios.HemlockHouse.Helpers (getFloorNumber)

newtype GrapplingSpawn = GrapplingSpawn EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

grapplingSpawn :: EnemyCard GrapplingSpawn
grapplingSpawn = enemy GrapplingSpawn Cards.grapplingSpawn (4, Static 2, 2) (1, 1)

instance HasModifiersFor GrapplingSpawn where
  getModifiersFor (GrapplingSpawn a) = do
    floorN <- field EnemyLocation a.id >>= maybe (pure 0) getFloorNumber
    modifySelf a [HealthModifier (max 1 floorN)]
