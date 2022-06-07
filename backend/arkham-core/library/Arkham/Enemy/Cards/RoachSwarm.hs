module Arkham.Enemy.Cards.RoachSwarm
  ( roachSwarm
  , RoachSwarm(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Location.Attrs ( Field (..) )
import Arkham.Modifier qualified as Modifier
import Arkham.Projection

newtype RoachSwarm = RoachSwarm EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

roachSwarm :: EnemyCard RoachSwarm
roachSwarm = enemy RoachSwarm Cards.roachSwarm (0, Static 2, 3) (1, 0)

instance HasModifiersFor RoachSwarm where
  getModifiersFor _ target (RoachSwarm a) | isTarget a target =
    case enemyLocation a of
      Nothing -> pure []
      Just loc -> do
        x <- field LocationShroud loc
        pure $ toModifiers a [Modifier.EnemyFight x]
  getModifiersFor _ _ _ = pure []

instance RunMessage RoachSwarm where
  runMessage msg (RoachSwarm attrs) = RoachSwarm <$> runMessage msg attrs
