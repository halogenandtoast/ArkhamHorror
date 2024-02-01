module Arkham.Enemy.Cards.StubbornDetective (
  StubbornDetective (..),
  stubbornDetective,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype StubbornDetective = StubbornDetective EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

stubbornDetective :: EnemyCard StubbornDetective
stubbornDetective =
  enemyWith StubbornDetective Cards.stubbornDetective (3, Static 2, 2) (1, 0)
    $ \a -> a & preyL .~ BearerOf (toId a)

instance HasModifiersFor StubbornDetective where
  getModifiersFor (InvestigatorTarget iid) (StubbornDetective a) = do
    sameLocation <- iid <=~> InvestigatorAt (locationWithEnemy $ toId a)
    pure $ toModifiers a [Blank | sameLocation]
  getModifiersFor _ _ = pure []

instance RunMessage StubbornDetective where
  runMessage msg (StubbornDetective attrs) = StubbornDetective <$> runMessage msg attrs
