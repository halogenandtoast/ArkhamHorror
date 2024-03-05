module Arkham.Enemy.Cards.PackOfVooniths (packOfVooniths, PackOfVooniths (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Modifier qualified as Mod
import Arkham.Prelude

newtype PackOfVooniths = PackOfVooniths EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

packOfVooniths :: EnemyCard PackOfVooniths
packOfVooniths = enemy PackOfVooniths Cards.packOfVooniths (1, Static 2, 1) (1, 1)

instance HasModifiersFor PackOfVooniths where
  getModifiersFor target (PackOfVooniths a) | a `is` target = do
    isHost <- toId a <=~> IsHost
    noSwarm <- selectNone $ SwarmOf (toId a)
    pure $ toModifiers a $ guard (isHost && noSwarm) *> [Mod.EnemyFight 2, Mod.EnemyEvade 2]
  getModifiersFor _ _ = pure []

instance RunMessage PackOfVooniths where
  runMessage msg (PackOfVooniths attrs) =
    PackOfVooniths <$> runMessage msg attrs
