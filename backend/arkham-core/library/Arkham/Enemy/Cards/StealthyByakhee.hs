module Arkham.Enemy.Cards.StealthyByakhee (
  stealthyByakhee,
  StealthyByakhee (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Modifier qualified as Modifier

newtype StealthyByakhee = StealthyByakhee EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

stealthyByakhee :: EnemyCard StealthyByakhee
stealthyByakhee =
  enemy StealthyByakhee Cards.stealthyByakhee (5, Static 2, 3) (2, 1)

instance HasModifiersFor StealthyByakhee where
  getModifiersFor target (StealthyByakhee attrs)
    | isTarget attrs target =
        pure $ toModifiers attrs [Modifier.EnemyFight (-3) | enemyExhausted attrs]
  getModifiersFor _ _ = pure []

instance RunMessage StealthyByakhee where
  runMessage msg (StealthyByakhee attrs) =
    StealthyByakhee <$> runMessage msg attrs
