module Arkham.Enemy.Cards.SebastienMoreau (
  sebastienMoreau,
  SebastienMoreau (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SebastienMoreau = SebastienMoreau EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sebastienMoreau :: EnemyCard SebastienMoreau
sebastienMoreau = enemy SebastienMoreau Cards.sebastienMoreau (3, Static 5, 3) (2, 2)

instance HasModifiersFor SebastienMoreau where
  getModifiersFor (SebastienMoreau a) = modifySelf a [AttacksCannotBeCancelled]

instance RunMessage SebastienMoreau where
  runMessage msg (SebastienMoreau attrs) =
    SebastienMoreau <$> runMessage msg attrs
