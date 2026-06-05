module Arkham.Enemy.Cards.JudithPark (judithPark) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Message.Lifted.Placement

newtype JudithPark = JudithPark EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

judithPark :: EnemyCard JudithPark
judithPark = enemy JudithPark Cards.judithPark (5, Static 3, 2) (1, 0)

instance HasAbilities JudithPark where
  getAbilities (JudithPark a) =
    extend1 a $ restricted a 1 OnSameLocation (parleyAction (ResourceCost 5))

instance RunMessage JudithPark where
  runMessage msg e@(JudithPark attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      place attrs (OutOfPlay SetAsideZone)
      pure e
    _ -> JudithPark <$> liftRunMessage msg attrs
