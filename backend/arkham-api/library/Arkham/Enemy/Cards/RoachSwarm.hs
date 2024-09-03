module Arkham.Enemy.Cards.RoachSwarm (roachSwarm, RoachSwarm (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier qualified as Modifier
import Arkham.Prelude
import Arkham.Projection

newtype RoachSwarm = RoachSwarm EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

roachSwarm :: EnemyCard RoachSwarm
roachSwarm = enemy RoachSwarm Cards.roachSwarm (0, Static 2, 3) (1, 0)

instance HasModifiersFor RoachSwarm where
  getModifiersFor target (RoachSwarm a) | isTarget a target = do
    maybeModified a do
      lid <- MaybeT $ selectOne $ locationWithEnemy a.id
      shroud <- MaybeT $ field LocationShroud lid
      pure [Modifier.EnemyFight shroud]
  getModifiersFor _ _ = pure []

instance RunMessage RoachSwarm where
  runMessage msg (RoachSwarm attrs) = RoachSwarm <$> runMessage msg attrs
