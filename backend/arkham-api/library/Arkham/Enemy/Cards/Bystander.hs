module Arkham.Enemy.Cards.Bystander (bystander) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype Bystander = Bystander EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

bystander :: EnemyCard Bystander
bystander =
  enemy Bystander Cards.bystander (1, Static 1, 1) (1, 0)

instance RunMessage Bystander where
  runMessage msg (Bystander attrs) = Bystander <$> runMessage msg attrs
