module Arkham.Enemy.Cards.MindlessDancer
  ( mindlessDancer
  , MindlessDancer(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype MindlessDancer = MindlessDancer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mindlessDancer :: EnemyCard MindlessDancer
mindlessDancer = enemy MindlessDancer Cards.mindlessDancer (6, Static 5, 3) (2, 1)

instance RunMessage MindlessDancer where
  runMessage msg (MindlessDancer attrs) =
    MindlessDancer <$> runMessage msg attrs
