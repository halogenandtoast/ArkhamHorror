module Arkham.Enemy.Cards.EztliGuardian (
  eztliGuardian,
  EztliGuardian (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Id
import Arkham.Matcher
import Arkham.Trait

newtype EztliGuardian = EztliGuardian EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

eztliGuardian :: EnemyCard EztliGuardian
eztliGuardian =
  enemyWith EztliGuardian Cards.eztliGuardian (4, Static 2, 2) (1, 0)
    $ spawnAtL
    ?~ SpawnAt
      (FirstLocation [EmptyLocation <> LocationWithTrait Ancient, EmptyLocation])

investigatorMatcher :: EnemyId -> InvestigatorMatcher
investigatorMatcher eid = InvestigatorAt $ ConnectedFrom $ locationWithEnemy eid

instance HasAbilities EztliGuardian where
  getAbilities (EztliGuardian a) =
    withBaseAbilities a
      $ [ groupLimit PerPhase
            $ restrictedAbility a 1 (InvestigatorExists $ investigatorMatcher $ toId a)
            $ ForcedAbility
            $ PhaseStep #when EnemiesAttackStep
        ]

instance RunMessage EztliGuardian where
  runMessage msg e@(EztliGuardian attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      adjacentInvestigators <- selectList $ investigatorMatcher $ toId attrs
      insertAfterMatching
        [ EnemyWillAttack
          $ (enemyAttack (toId attrs) attrs iid)
            { attackDamageStrategy = enemyDamageStrategy attrs
            }
        | iid <- adjacentInvestigators
        ]
        (== EnemiesAttack)
      pure e
    _ -> EztliGuardian <$> runMessage msg attrs
