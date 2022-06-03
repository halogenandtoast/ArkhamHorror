module Arkham.Enemy.Cards.SebastienMoreau
  ( sebastienMoreau
  , SebastienMoreau(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Modifier

newtype SebastienMoreau = SebastienMoreau EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sebastienMoreau :: EnemyCard SebastienMoreau
sebastienMoreau =
  enemy SebastienMoreau Cards.sebastienMoreau (3, Static 5, 3) (2, 2)

instance HasModifiersFor env SebastienMoreau where
  getModifiersFor _ target (SebastienMoreau a) | isTarget a target =
    pure $ toModifiers a [AttacksCannotBeCancelled]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage SebastienMoreau where
  runMessage msg (SebastienMoreau attrs) =
    SebastienMoreau <$> runMessage msg attrs
