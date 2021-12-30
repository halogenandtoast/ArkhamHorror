module Arkham.Enemy.Cards.RoyalEmissary
  ( royalEmissary
  , RoyalEmissary(..)
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
import Arkham.Prey
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype RoyalEmissary = RoyalEmissary EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

royalEmissary :: EnemyCard RoyalEmissary
royalEmissary = enemyWith
  RoyalEmissary
  Cards.royalEmissary
  (4, Static 4, 2)
  (2, 0)
  (preyL .~ LowestSkill SkillWillpower)

investigatorMatcher :: EnemyAttrs -> InvestigatorMatcher
investigatorMatcher a = AnyInvestigator
  [ InvestigatorAt $ LocationWithId $ enemyLocation a
  , InvestigatorAt $ AccessibleFrom $ LocationWithId $ enemyLocation a
  ]

instance HasAbilities RoyalEmissary where
  getAbilities (RoyalEmissary a) = withBaseAbilities
    a
    [ restrictedAbility a 1 (InvestigatorExists $ investigatorMatcher a)
      $ ForcedAbility
      $ PhaseEnds Timing.When
      $ PhaseIs EnemyPhase
    ]

instance EnemyRunner env => RunMessage env RoyalEmissary where
  runMessage msg e@(RoyalEmissary attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      iids <- selectList (investigatorMatcher attrs)
      e <$ pushAll
        (map (\iid -> InvestigatorAssignDamage iid source DamageAny 0 1) iids)
    _ -> RoyalEmissary <$> runMessage msg attrs
