module Arkham.Enemy.DefeatedProxy (toDefeatedEnemyProxy) where

import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities (HasAbilities (..))
import Arkham.Classes.HasModifiersFor (HasModifiersFor (..))
import Arkham.Classes.RunMessage.Internal (RunMessage (..))
import Arkham.Enemy.Types (Enemy (..), EnemyAttrs, IsEnemy)
import Arkham.Prelude

{- | An 'Enemy' carrier for the attrs recorded when an entity was defeated, used
where there is no card builder to rebuild the real card from.

Enemy-locations defeat through the same pipeline as enemies (see
'Arkham.Behavior.Defeat') and so get recorded in @ScenarioDefeatedEnemies@, but
their card codes aren't in @allEnemies@ — @lookupEnemy@ would error on one.
Defeated entities are only ever read back through field projection and matcher
filtering, never run, so the stored attrs are enough on their own;
@HasCardDef EnemyAttrs@ already resolves enemy-location codes.

This is deliberately a newtype rather than an @IsEnemy EnemyAttrs@ instance, so
that raw attrs can't be wrapped into an 'Enemy' by accident somewhere that does
need the real card's abilities and message handling.
-}
newtype DefeatedEnemyProxy = DefeatedEnemyProxy EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities DefeatedEnemyProxy where
  getAbilities _ = []

instance HasModifiersFor DefeatedEnemyProxy where
  getModifiersFor _ = pure ()

instance RunMessage DefeatedEnemyProxy where
  runMessage _ p = pure p

instance IsEnemy DefeatedEnemyProxy

toDefeatedEnemyProxy :: EnemyAttrs -> Enemy
toDefeatedEnemyProxy = Enemy . DefeatedEnemyProxy
