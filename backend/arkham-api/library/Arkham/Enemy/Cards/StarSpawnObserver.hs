module Arkham.Enemy.Cards.StarSpawnObserver (starSpawnObserver) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (RevealChaosToken)
import Arkham.Matcher

newtype StarSpawnObserver = StarSpawnObserver EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starSpawnObserver :: EnemyCard StarSpawnObserver
starSpawnObserver =
  enemyWith
    StarSpawnObserver
    Cards.starSpawnObserver
    (preyL .~ Prey (InvestigatorWithHighestSkill #intellect UneliminatedInvestigator))

instance HasAbilities StarSpawnObserver where
  getAbilities (StarSpawnObserver a) =
    extend1 a
      $ restricted a 1 (DuringSkillTest $ WhileInvestigating $ locationWithEnemy a)
      $ forced
      $ RevealChaosToken #after You #elderthing

instance RunMessage StarSpawnObserver where
  runMessage msg e@(StarSpawnObserver attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      engageEnemy iid attrs.id
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> StarSpawnObserver <$> liftRunMessage msg attrs
