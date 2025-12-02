module Arkham.Enemy.Cards.TheBeastInACowlOfCrimsonRavagerInRed (theBeastInACowlOfCrimsonRavagerInRed) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TheBeastInACowlOfCrimsonRavagerInRed = TheBeastInACowlOfCrimsonRavagerInRed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theBeastInACowlOfCrimsonRavagerInRed :: EnemyCard TheBeastInACowlOfCrimsonRavagerInRed
theBeastInACowlOfCrimsonRavagerInRed =
  enemy
    TheBeastInACowlOfCrimsonRavagerInRed
    Cards.theBeastInACowlOfCrimsonRavagerInRed
    (3, PerPlayer 3, 4)
    (2, 2)

instance RunMessage TheBeastInACowlOfCrimsonRavagerInRed where
  runMessage msg (TheBeastInACowlOfCrimsonRavagerInRed attrs) = runQueueT $ case msg of
    _ -> TheBeastInACowlOfCrimsonRavagerInRed <$> liftRunMessage msg attrs
