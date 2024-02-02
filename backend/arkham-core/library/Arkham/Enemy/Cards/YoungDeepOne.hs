module Arkham.Enemy.Cards.YoungDeepOne (
  YoungDeepOne (..),
  youngDeepOne,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype YoungDeepOne = YoungDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

youngDeepOne :: EnemyCard YoungDeepOne
youngDeepOne =
  enemyWith
    YoungDeepOne
    Cards.youngDeepOne
    (3, Static 3, 3)
    (1, 1)
    (preyL .~ Prey (InvestigatorWithLowestSkill SkillCombat))

instance HasAbilities YoungDeepOne where
  getAbilities (YoungDeepOne a) =
    withBaseAbilities a
      $ [ forcedAbility a 1
            $ EnemyEngaged Timing.After You
            $ EnemyWithId (toId a)
        ]

instance RunMessage YoungDeepOne where
  runMessage msg e@(YoungDeepOne attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ assignDamage iid attrs 1
      pure e
    _ -> YoungDeepOne <$> runMessage msg attrs
