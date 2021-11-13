module Arkham.Types.Enemy.Cards.RoachSwarm
  ( roachSwarm
  , RoachSwarm(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Query

newtype RoachSwarm = RoachSwarm EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

roachSwarm :: EnemyCard RoachSwarm
roachSwarm = enemy RoachSwarm Cards.roachSwarm (0, Static 2, 3) (1, 0)

instance HasCount Shroud env LocationId => HasModifiersFor env RoachSwarm where
  getModifiersFor _ target (RoachSwarm a) | isTarget a target = do
    x <- unShroud <$> getCount (enemyLocation a)
    pure $ toModifiers a [EnemyFight x]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env RoachSwarm where
  runMessage msg (RoachSwarm attrs) = RoachSwarm <$> runMessage msg attrs
