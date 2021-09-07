module Arkham.Types.Enemy.Cards.SebastienMoreau
  ( sebastienMoreau
  , SebastienMoreau(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner

newtype SebastienMoreau = SebastienMoreau EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sebastienMoreau :: EnemyCard SebastienMoreau
sebastienMoreau =
  enemy SebastienMoreau Cards.sebastienMoreau (0, Static 1, 0) (0, 0)

instance EnemyRunner env => RunMessage env SebastienMoreau where
  runMessage msg (SebastienMoreau attrs) =
    SebastienMoreau <$> runMessage msg attrs
