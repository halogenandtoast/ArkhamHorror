module Arkham.Enemy.Cards.JordanPerry
  ( jordanPerry
  , JordanPerry(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Phase
import Arkham.Timing qualified as Timing

newtype JordanPerry = JordanPerry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jordanPerry :: EnemyCard JordanPerry
jordanPerry = enemy JordanPerry Cards.jordanPerry (2, Static 8, 2) (1, 1)

instance HasAbilities JordanPerry where
  getAbilities (JordanPerry a) = withBaseAbilities
    a
    [ restrictedAbility a 1 (EnemyCriteria $ ThisEnemy EnemyWithAnyDamage)
      $ ForcedAbility
      $ PhaseBegins Timing.When
      $ PhaseIs EnemyPhase
    ]

instance RunMessage JordanPerry where
  runMessage msg e@(JordanPerry attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      push $ HealDamage (toTarget attrs) (toSource attrs) 1
      pure e
    _ -> JordanPerry <$> runMessage msg attrs
