module Arkham.Enemy.Cards.ThorneTheOneWithTheRedCravat (thorneTheOneWithTheRedCravat) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ThorneTheOneWithTheRedCravat = ThorneTheOneWithTheRedCravat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thorneTheOneWithTheRedCravat :: EnemyCard ThorneTheOneWithTheRedCravat
thorneTheOneWithTheRedCravat = enemy ThorneTheOneWithTheRedCravat Cards.thorneTheOneWithTheRedCravat (0, Static 1, 0) (0, 0)

instance RunMessage ThorneTheOneWithTheRedCravat where
  runMessage msg (ThorneTheOneWithTheRedCravat attrs) = runQueueT $ case msg of
    _ -> ThorneTheOneWithTheRedCravat <$> liftRunMessage msg attrs
