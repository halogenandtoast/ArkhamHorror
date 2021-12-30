module Arkham.Enemy.Cards.RoachSwarm
  ( roachSwarm
  , RoachSwarm(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Attrs
import Arkham.Id
import Arkham.Modifier
import Arkham.Query

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
