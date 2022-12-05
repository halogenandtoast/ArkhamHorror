module Arkham.Enemy.Cards.AlejandroVela
  ( alejandroVela
  , AlejandroVela(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype AlejandroVela = AlejandroVela EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

alejandroVela :: EnemyCard AlejandroVela
alejandroVela =
  enemy AlejandroVela Cards.alejandroVela (6, PerPlayer 4, 3) (1, 2)

instance RunMessage AlejandroVela where
  runMessage msg (AlejandroVela attrs) = AlejandroVela <$> runMessage msg attrs
