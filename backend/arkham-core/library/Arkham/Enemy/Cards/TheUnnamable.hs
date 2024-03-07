module Arkham.Enemy.Cards.TheUnnamable (
  theUnnamable,
  TheUnnamable (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Modifier qualified as Mod
import Arkham.ScenarioLogKey

newtype TheUnnamable = TheUnnamable EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theUnnamable :: EnemyCard TheUnnamable
theUnnamable =
  enemyWith TheUnnamable Cards.theUnnamable (5, Static 1, 5) (2, 2)
    $ (healthL .~ Nothing)
    . (spawnAtL ?~ SpawnAt (oneOf ["Attic", "Upstairs Hallway"]))

instance HasModifiersFor TheUnnamable where
  getModifiersFor target (TheUnnamable a) | isTarget a target = do
    n <- countM remembered [FoundACrackedMirror, StudiedADesecratedPortrait, NoticedTheMissingBones]
    pure $ toModifiers a $ CannotBeDefeated
      : (guard (n > 0) *> [Mod.EnemyFight (-n), Mod.EnemyEvade (-n)])
  getModifiersFor _ _ = pure []

instance RunMessage TheUnnamable where
  runMessage msg (TheUnnamable attrs) =
    TheUnnamable <$> runMessage msg attrs
