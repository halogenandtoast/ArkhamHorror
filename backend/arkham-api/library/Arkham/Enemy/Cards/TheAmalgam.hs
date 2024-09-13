module Arkham.Enemy.Cards.TheAmalgam
  ( theAmalgam
  , TheAmalgam(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype TheAmalgam = TheAmalgam EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theAmalgam :: EnemyCard TheAmalgam
theAmalgam = enemy TheAmalgam Cards.theAmalgam (3, Static 3, 2) (1, 1)

instance RunMessage TheAmalgam where
  runMessage msg (TheAmalgam attrs) =
    TheAmalgam <$> runMessage msg attrs
