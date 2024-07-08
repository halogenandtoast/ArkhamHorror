module Arkham.Enemy.Cards.EztliGuardian (eztliGuardian, EztliGuardian (..)) where

import Arkham.Ability
import Arkham.Attack
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (PhaseStep)
import Arkham.Id
import Arkham.Matcher
import Arkham.Trait

newtype EztliGuardian = EztliGuardian EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eztliGuardian :: EnemyCard EztliGuardian
eztliGuardian =
  enemyWith EztliGuardian Cards.eztliGuardian (4, Static 2, 2) (1, 0)
    $ spawnAtL
    ?~ SpawnAt
      (FirstLocation [EmptyLocation <> LocationWithTrait Ancient, EmptyLocation])

atConnected :: EnemyId -> InvestigatorMatcher
atConnected eid = InvestigatorAt $ ConnectedFrom $ locationWithEnemy eid

instance HasAbilities EztliGuardian where
  getAbilities (EztliGuardian a) =
    extend
      a
      [ groupLimit PerPhase
          $ restrictedAbility a 1 (exists (atConnected a.id) <> exists (be a <> ReadyEnemy <> UnengagedEnemy))
          $ forced
          $ PhaseStep #when EnemiesAttackStep
      ]

instance RunMessage EztliGuardian where
  runMessage msg e@(EztliGuardian attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (atConnected attrs.id) \iid ->
        push
          $ EnemyWillAttack
          $ (enemyAttack attrs.id (attrs.ability 1) iid)
            { attackDamageStrategy = enemyDamageStrategy attrs
            }
      pure e
    _ -> EztliGuardian <$> liftRunMessage msg attrs
