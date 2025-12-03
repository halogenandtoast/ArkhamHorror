module Arkham.Enemy.Cards.TheBeastInACowlOfCrimsonRavagerInRed (theBeastInACowlOfCrimsonRavagerInRed) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype TheBeastInACowlOfCrimsonRavagerInRed = TheBeastInACowlOfCrimsonRavagerInRed EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBeastInACowlOfCrimsonRavagerInRed :: EnemyCard TheBeastInACowlOfCrimsonRavagerInRed
theBeastInACowlOfCrimsonRavagerInRed =
  enemy
    TheBeastInACowlOfCrimsonRavagerInRed
    Cards.theBeastInACowlOfCrimsonRavagerInRed
    (3, PerPlayer 3, 4)
    (2, 2)

instance HasAbilities TheBeastInACowlOfCrimsonRavagerInRed where
  getAbilities (TheBeastInACowlOfCrimsonRavagerInRed a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyWouldBeDefeated #when (be a)

instance RunMessage TheBeastInACowlOfCrimsonRavagerInRed where
  runMessage msg e@(TheBeastInACowlOfCrimsonRavagerInRed attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      healAllDamage (attrs.ability 1) attrs
      exhaustThis attrs
      selectEach (location_ "Catacombs of Kom el Shoqafa") (enemyMoveTo (attrs.ability 1) attrs)
      pure e
    _ -> TheBeastInACowlOfCrimsonRavagerInRed <$> liftRunMessage msg attrs
