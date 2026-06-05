module Arkham.Enemy.Cards.Oozewraith (oozewraith) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype Oozewraith = Oozewraith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

oozewraith :: EnemyCard Oozewraith
oozewraith = enemy Oozewraith Cards.oozewraith (4, Static 7, 2) (1, 1)

instance RunMessage Oozewraith where
  runMessage msg (Oozewraith attrs) = Oozewraith <$> runMessage msg attrs
