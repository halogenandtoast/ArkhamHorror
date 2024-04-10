module Arkham.Enemy.Cards.SacrificialBeast (sacrificialBeast, SacrificialBeast (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Matcher
import Arkham.Modifier

newtype SacrificialBeast = SacrificialBeast EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sacrificialBeast :: EnemyCard SacrificialBeast
sacrificialBeast =
  enemyWith SacrificialBeast Cards.sacrificialBeast (4, Static 3, 2) (1, 1)
    $ spawnAtL
    ?~ SpawnAt (FarthestLocationFromYou Anywhere)

instance HasModifiersFor SacrificialBeast where
  getModifiersFor (InvestigatorTarget iid) (SacrificialBeast attrs) = do
    affected <- iid <=~> investigatorIs Investigators.jennyBarnes
    pure $ toModifiers attrs [CannotGainResourcesFromPlayerCardEffects | affected]
  getModifiersFor _ _ = pure []

instance RunMessage SacrificialBeast where
  runMessage msg (SacrificialBeast attrs) =
    SacrificialBeast <$> runMessage msg attrs
