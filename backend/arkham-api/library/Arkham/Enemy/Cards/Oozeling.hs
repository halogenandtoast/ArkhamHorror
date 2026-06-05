module Arkham.Enemy.Cards.Oozeling (oozeling) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype Oozeling = Oozeling EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oozeling :: EnemyCard Oozeling
oozeling = enemy Oozeling Cards.oozeling (2, Static 3, 2) (1, 0)

instance RunMessage Oozeling where
  runMessage msg (Oozeling attrs) = Oozeling <$> runMessage msg attrs
