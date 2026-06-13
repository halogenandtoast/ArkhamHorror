module Arkham.Enemy.Cards.Medusa (medusa) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype Medusa = Medusa EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

medusa :: EnemyCard Medusa
medusa = enemy Medusa Cards.medusa (3, Static 2, 3) (0, 1)

-- TODO: abilities
instance RunMessage Medusa where
  runMessage msg (Medusa attrs) = runQueueT $ Medusa <$> liftRunMessage msg attrs
