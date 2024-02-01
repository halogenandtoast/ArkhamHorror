module Arkham.Enemy.Cards.JordanPerry (
  jordanPerry,
  JordanPerry (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher

newtype JordanPerry = JordanPerry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

jordanPerry :: EnemyCard JordanPerry
jordanPerry = enemy JordanPerry Cards.jordanPerry (2, Static 8, 2) (1, 1)

instance HasAbilities JordanPerry where
  getAbilities (JordanPerry a) =
    withBaseAbilities a
      $ [ restrictedAbility a 1 (EnemyCriteria $ ThisEnemy EnemyWithAnyDamage)
            $ ForcedAbility
            $ PhaseBegins #when #enemy
        ]

instance RunMessage JordanPerry where
  runMessage msg e@(JordanPerry attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ HealDamage (toTarget attrs) (toAbilitySource attrs 1) 1
      pure e
    _ -> JordanPerry <$> runMessage msg attrs
