module Arkham.Enemy.Cards.Hoods (hoods, Hoods (..)) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Hoods = Hoods EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hoods :: EnemyCard Hoods
hoods = enemy Hoods Cards.hoods (3, Static 3, 3) (1, 1)

instance HasAbilities Hoods where
  getAbilities (Hoods a) =
    withBaseAbilities a [mkAbility a 1 $ forced $ EnemyEvaded #after You AnyEnemy]

instance RunMessage Hoods where
  runMessage msg e@(Hoods attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ InitiateEnemyAttack $ enemyAttack (toId attrs) attrs iid
      pure e
    _ -> Hoods <$> runMessage msg attrs
