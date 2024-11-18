module Arkham.Enemy.Cards.ManifestationOfMadness (manifestationOfMadness, ManifestationOfMadness (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ManifestationOfMadness = ManifestationOfMadness EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

manifestationOfMadness :: EnemyCard ManifestationOfMadness
manifestationOfMadness = enemy ManifestationOfMadness Cards.manifestationOfMadness (0, Static 1, 0) (0, 0)

instance RunMessage ManifestationOfMadness where
  runMessage msg (ManifestationOfMadness attrs) = runQueueT $ case msg of
    _ -> ManifestationOfMadness <$> liftRunMessage msg attrs
