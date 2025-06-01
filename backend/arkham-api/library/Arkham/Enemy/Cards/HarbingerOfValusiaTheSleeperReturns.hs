module Arkham.Enemy.Cards.HarbingerOfValusiaTheSleeperReturns (harbingerOfValusiaTheSleeperReturns) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HarbingerOfValusiaTheSleeperReturns = HarbingerOfValusiaTheSleeperReturns EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

harbingerOfValusiaTheSleeperReturns :: EnemyCard HarbingerOfValusiaTheSleeperReturns
harbingerOfValusiaTheSleeperReturns = enemy HarbingerOfValusiaTheSleeperReturns Cards.harbingerOfValusiaTheSleeperReturns (2, PerPlayer 10, 4) (2, 2)

instance RunMessage HarbingerOfValusiaTheSleeperReturns where
  runMessage msg (HarbingerOfValusiaTheSleeperReturns attrs) = runQueueT $ case msg of
    _ -> HarbingerOfValusiaTheSleeperReturns <$> liftRunMessage msg attrs
