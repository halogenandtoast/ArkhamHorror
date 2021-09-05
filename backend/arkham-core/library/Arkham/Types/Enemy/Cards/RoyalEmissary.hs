module Arkham.Types.Enemy.Cards.RoyalEmissary
  ( royalEmissary
  , RoyalEmissary(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Phase
import Arkham.Types.Prey
import Arkham.Types.SkillType
import qualified Arkham.Types.Timing as Timing

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
  getAbilities (RoyalEmissary a) =
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
