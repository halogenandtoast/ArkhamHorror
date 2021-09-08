module Arkham.Types.Enemy.Cards.JordanPerry
  ( jordanPerry
  , JordanPerry(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Phase
import qualified Arkham.Types.Timing as Timing

newtype JordanPerry = JordanPerry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
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

instance EnemyRunner env => RunMessage env JordanPerry where
  runMessage msg e@(JordanPerry attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ push (HealDamage (toTarget attrs) 1)
    _ -> JordanPerry <$> runMessage msg attrs
