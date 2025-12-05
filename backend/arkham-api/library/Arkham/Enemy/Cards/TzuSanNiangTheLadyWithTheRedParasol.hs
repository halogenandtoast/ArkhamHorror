module Arkham.Enemy.Cards.TzuSanNiangTheLadyWithTheRedParasol (tzuSanNiangTheLadyWithTheRedParasol) where

import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Message (ReplaceStrategy (..))

newtype TzuSanNiangTheLadyWithTheRedParasol = TzuSanNiangTheLadyWithTheRedParasol EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tzuSanNiangTheLadyWithTheRedParasol :: EnemyCard TzuSanNiangTheLadyWithTheRedParasol
tzuSanNiangTheLadyWithTheRedParasol =
  enemy
    TzuSanNiangTheLadyWithTheRedParasol
    Cards.tzuSanNiangTheLadyWithTheRedParasol
    (0, Static 1, 0)
    (0, 0)

instance RunMessage TzuSanNiangTheLadyWithTheRedParasol where
  runMessage msg e@(TzuSanNiangTheLadyWithTheRedParasol attrs) = runQueueT $ case msg of
    Flip _ _ (isTarget attrs -> True) -> do
      outForBlood <- genCard Cards.tzuSanNiangOutForBlood
      push $ ReplaceEnemy attrs.id outForBlood Swap
      pure e
    _ -> TzuSanNiangTheLadyWithTheRedParasol <$> liftRunMessage msg attrs
