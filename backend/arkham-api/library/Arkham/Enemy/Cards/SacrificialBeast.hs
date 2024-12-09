module Arkham.Enemy.Cards.SacrificialBeast (sacrificialBeast, SacrificialBeast (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher

newtype SacrificialBeast = SacrificialBeast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sacrificialBeast :: EnemyCard SacrificialBeast
sacrificialBeast =
  enemyWith SacrificialBeast Cards.sacrificialBeast (4, Static 3, 2) (1, 1)
    $ spawnAtL
    ?~ SpawnAt (FarthestLocationFromYou Anywhere)

instance HasModifiersFor SacrificialBeast where
  getModifiersFor (SacrificialBeast a) = do
    modifySelect a (investigatorIs Investigators.jennyBarnes) [CannotGainResourcesFromPlayerCardEffects]

instance RunMessage SacrificialBeast where
  runMessage msg (SacrificialBeast attrs) =
    SacrificialBeast <$> runMessage msg attrs
