module Arkham.Enemy.Cards.TzuSanNiangTheLadyWithTheRedParasol (tzuSanNiangTheLadyWithTheRedParasol) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TzuSanNiangTheLadyWithTheRedParasol = TzuSanNiangTheLadyWithTheRedParasol EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tzuSanNiangTheLadyWithTheRedParasol :: EnemyCard TzuSanNiangTheLadyWithTheRedParasol
tzuSanNiangTheLadyWithTheRedParasol = enemy TzuSanNiangTheLadyWithTheRedParasol Cards.tzuSanNiangTheLadyWithTheRedParasol (0, Static 1, 0) (0, 0)

instance RunMessage TzuSanNiangTheLadyWithTheRedParasol where
  runMessage msg (TzuSanNiangTheLadyWithTheRedParasol attrs) = runQueueT $ case msg of
    _ -> TzuSanNiangTheLadyWithTheRedParasol <$> liftRunMessage msg attrs
