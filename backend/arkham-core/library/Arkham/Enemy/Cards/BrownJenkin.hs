module Arkham.Enemy.Cards.BrownJenkin
  ( brownJenkin
  , BrownJenkin(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype BrownJenkin = BrownJenkin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

brownJenkin :: EnemyCard BrownJenkin
brownJenkin = enemy BrownJenkin Cards.brownJenkin (1, Static 1, 4) (1, 1)

instance RunMessage BrownJenkin where
  runMessage msg (BrownJenkin attrs) = BrownJenkin <$> runMessage msg attrs
