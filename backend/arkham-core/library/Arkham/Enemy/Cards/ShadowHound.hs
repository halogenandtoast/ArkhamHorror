module Arkham.Enemy.Cards.ShadowHound
  ( shadowHound
  , ShadowHound(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding ( EnemyAttacks )
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype ShadowHound = ShadowHound EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shadowHound :: EnemyCard ShadowHound
shadowHound = enemyWith
  ShadowHound
  Cards.shadowHound
  (2, Static 3, 1)
  (1, 0)
  (preyL .~ Prey (InvestigatorWithLowestSkill SkillAgility))

instance HasAbilities ShadowHound where
  getAbilities (ShadowHound a) = withBaseAbilities
    a
    [ mkAbility a 1
      $ ForcedAbility
      $ EnemyAttacks Timing.When You AnyEnemyAttack
      $ EnemyWithId
      $ toId a
    ]

instance RunMessage ShadowHound where
  runMessage msg e@(ShadowHound attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      runHauntedAbilities iid
      pure e
    _ -> ShadowHound <$> runMessage msg attrs
