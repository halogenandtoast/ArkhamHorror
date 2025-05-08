module Arkham.Enemy.Cards.StealthyByakhee (stealthyByakhee) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Modifier qualified as Modifier

newtype StealthyByakhee = StealthyByakhee EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

stealthyByakhee :: EnemyCard StealthyByakhee
stealthyByakhee = enemy StealthyByakhee Cards.stealthyByakhee (5, Static 2, 3) (2, 1)

instance HasModifiersFor StealthyByakhee where
  getModifiersFor (StealthyByakhee a) = modifySelfWhen a a.exhausted [Modifier.EnemyFight (-3)]

instance RunMessage StealthyByakhee where
  runMessage msg (StealthyByakhee attrs) = StealthyByakhee <$> runMessage msg attrs
