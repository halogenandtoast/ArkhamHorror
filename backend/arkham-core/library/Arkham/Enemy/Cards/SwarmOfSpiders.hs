module Arkham.Enemy.Cards.SwarmOfSpiders (swarmOfSpiders, SwarmOfSpiders (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Modifier qualified as Mods
import Arkham.Prelude

newtype SwarmOfSpiders = SwarmOfSpiders EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

swarmOfSpiders :: EnemyCard SwarmOfSpiders
swarmOfSpiders = enemy SwarmOfSpiders Cards.swarmOfSpiders (1, Static 1, 0) (1, 0)

instance HasModifiersFor SwarmOfSpiders where
  getModifiersFor target (SwarmOfSpiders attrs) | attrs `is` target = do
    x <- selectCount $ enemyIs Cards.swarmOfSpiders <> at_ (locationWithEnemy attrs)
    pure $ toModifiers attrs [Mods.EnemyEvade x]
  getModifiersFor _ _ = pure []

instance RunMessage SwarmOfSpiders where
  runMessage msg (SwarmOfSpiders attrs) =
    SwarmOfSpiders <$> runMessage msg attrs
