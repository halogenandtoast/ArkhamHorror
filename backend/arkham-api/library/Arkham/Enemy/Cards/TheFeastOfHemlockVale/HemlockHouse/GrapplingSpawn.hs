module Arkham.Enemy.Cards.TheFeastOfHemlockVale.HemlockHouse.GrapplingSpawn (grapplingSpawn) where

import Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.HemlockHouse qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Projection
import Arkham.Scenarios.TheFeastOfHemlockVale.HemlockHouse.Helpers (getFloorNumber)

newtype GrapplingSpawn = GrapplingSpawn EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

grapplingSpawn :: EnemyCard GrapplingSpawn
grapplingSpawn = enemy GrapplingSpawn Cards.grapplingSpawn

instance HasModifiersFor GrapplingSpawn where
  getModifiersFor (GrapplingSpawn a) = do
    floorN <- field EnemyLocation a.id >>= maybe (pure 0) getFloorNumber
    modifySelf a [HealthModifier (max 1 floorN)]
