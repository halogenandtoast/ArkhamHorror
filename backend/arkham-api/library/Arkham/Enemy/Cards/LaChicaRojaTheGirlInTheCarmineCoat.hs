module Arkham.Enemy.Cards.LaChicaRojaTheGirlInTheCarmineCoat (laChicaRojaTheGirlInTheCarmineCoat) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype LaChicaRojaTheGirlInTheCarmineCoat = LaChicaRojaTheGirlInTheCarmineCoat EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laChicaRojaTheGirlInTheCarmineCoat :: EnemyCard LaChicaRojaTheGirlInTheCarmineCoat
laChicaRojaTheGirlInTheCarmineCoat =
  enemy
    LaChicaRojaTheGirlInTheCarmineCoat
    Cards.laChicaRojaTheGirlInTheCarmineCoat
    (3, Static 2, 5)
    (1, 1)

instance HasAbilities LaChicaRojaTheGirlInTheCarmineCoat where
  getAbilities (LaChicaRojaTheGirlInTheCarmineCoat a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ Moves #when (You <> investigatorEngagedWith a) AnySource Anywhere Anywhere

instance RunMessage LaChicaRojaTheGirlInTheCarmineCoat where
  runMessage msg e@(LaChicaRojaTheGirlInTheCarmineCoat attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      disengageEnemy iid attrs
      pure e
    _ -> LaChicaRojaTheGirlInTheCarmineCoat <$> liftRunMessage msg attrs
