module Arkham.Enemy.Cards.PackOfVooniths (packOfVooniths, PackOfVooniths (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype PackOfVooniths = PackOfVooniths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

packOfVooniths :: EnemyCard PackOfVooniths
packOfVooniths = enemy PackOfVooniths Cards.packOfVooniths (1, Static 2, 1) (1, 1)

instance RunMessage PackOfVooniths where
  runMessage msg (PackOfVooniths attrs) =
    PackOfVooniths <$> runMessage msg attrs
