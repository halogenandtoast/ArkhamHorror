module Arkham.Enemy.Cards.TzuSanNiangOutForBlood (tzuSanNiangOutForBlood) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TzuSanNiangOutForBlood = TzuSanNiangOutForBlood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tzuSanNiangOutForBlood :: EnemyCard TzuSanNiangOutForBlood
tzuSanNiangOutForBlood = enemy TzuSanNiangOutForBlood Cards.tzuSanNiangOutForBlood (0, Static 1, 0) (0, 0)

instance RunMessage TzuSanNiangOutForBlood where
  runMessage msg (TzuSanNiangOutForBlood attrs) = runQueueT $ case msg of
    _ -> TzuSanNiangOutForBlood <$> liftRunMessage msg attrs
