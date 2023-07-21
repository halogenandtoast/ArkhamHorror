module Arkham.Enemy.Cards.PennyWhite (
  pennyWhite,
  PennyWhite (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype PennyWhite = PennyWhite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pennyWhite :: EnemyCard PennyWhite
pennyWhite = enemy PennyWhite Cards.pennyWhite (4, Static 5, 3) (0, 2)

instance RunMessage PennyWhite where
  runMessage msg (PennyWhite attrs) =
    PennyWhite <$> runMessage msg attrs
