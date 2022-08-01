module Arkham.Enemy.Cards.EztliGuardian
  ( eztliGuardian
  , EztliGuardian(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Classes
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Message hiding ( EnemyAttacks )
import Arkham.Phase
import Arkham.Timing qualified as Timing
import Arkham.Trait

newtype EztliGuardian = EztliGuardian EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eztliGuardian :: EnemyCard EztliGuardian
eztliGuardian = enemyWith
  EztliGuardian
  Cards.eztliGuardian
  (4, Static 2, 2)
  (1, 0)
  (spawnAtL ?~ FirstLocation
    [EmptyLocation <> LocationWithTrait Ancient, EmptyLocation]
  )

instance HasAbilities EztliGuardian where
  getAbilities (EztliGuardian a) = withBaseAbilities
    a
    [ limitedAbility (GroupLimit PerPhase 1)
      $ restrictedAbility a 1 (DuringPhase $ PhaseIs EnemyPhase)
      $ ForcedAbility
      $ EnemyAttacks Timing.When Anyone AnyEnemyAttack
      $ EnemyWithId
      $ toId a
    ]

instance RunMessage EztliGuardian where
  runMessage msg e@(EztliGuardian attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      leadInvestigatorId <- getLeadInvestigatorId
      adjacentInvestigators <-
        selectList $ InvestigatorAt $ ConnectedFrom $ LocationWithEnemy
          (EnemyWithId $ toId attrs)
      push $ chooseOneAtATime
        leadInvestigatorId
        [ targetLabel iid [InitiateEnemyAttack iid (toId attrs) RegularAttack]
        | iid <- adjacentInvestigators
        ]
      pure e
    _ -> EztliGuardian <$> runMessage msg attrs
