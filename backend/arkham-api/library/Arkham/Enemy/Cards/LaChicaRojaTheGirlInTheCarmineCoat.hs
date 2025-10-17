module Arkham.Enemy.Cards.LaChicaRojaTheGirlInTheCarmineCoat (laChicaRojaTheGirlInTheCarmineCoat) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype LaChicaRojaTheGirlInTheCarmineCoat = LaChicaRojaTheGirlInTheCarmineCoat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

laChicaRojaTheGirlInTheCarmineCoat :: EnemyCard LaChicaRojaTheGirlInTheCarmineCoat
laChicaRojaTheGirlInTheCarmineCoat = enemy LaChicaRojaTheGirlInTheCarmineCoat Cards.laChicaRojaTheGirlInTheCarmineCoat (3, Static 2, 5) (1, 1)

instance RunMessage LaChicaRojaTheGirlInTheCarmineCoat where
  runMessage msg (LaChicaRojaTheGirlInTheCarmineCoat attrs) = runQueueT $ case msg of
    _ -> LaChicaRojaTheGirlInTheCarmineCoat <$> liftRunMessage msg attrs
