module Arkham.Enemy.Cards.StubbornDetective (stubbornDetective) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype StubbornDetective = StubbornDetective EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

stubbornDetective :: EnemyCard StubbornDetective
stubbornDetective =
  enemyWith StubbornDetective Cards.stubbornDetective (3, Static 2, 2) (1, 0)
    $ \a -> a & preyL .~ BearerOf (toId a)

instance HasModifiersFor StubbornDetective where
  getModifiersFor (StubbornDetective a) = modifySelect a (InvestigatorAt $ locationWithEnemy a) [Blank]

instance RunMessage StubbornDetective where
  runMessage msg (StubbornDetective attrs) = StubbornDetective <$> runMessage msg attrs
