module Arkham.Enemy.Cards.YoungDeepOne
  ( YoungDeepOne(..)
  , youngDeepOne
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype YoungDeepOne = YoungDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youngDeepOne :: EnemyCard YoungDeepOne
youngDeepOne = enemyWith
  YoungDeepOne
  Cards.youngDeepOne
  (3, Static 3, 3)
  (1, 1)
  (preyL .~ Prey (InvestigatorWithLowestSkill SkillCombat))

instance HasAbilities YoungDeepOne where
  getAbilities (YoungDeepOne a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyEngaged Timing.After You
      $ EnemyWithId
      $ toId a
    ]

instance RunMessage YoungDeepOne where
  runMessage msg e@(YoungDeepOne attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      e <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> YoungDeepOne <$> runMessage msg attrs
