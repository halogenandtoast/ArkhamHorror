module Arkham.Enemy.Cards.DeclanPearce (
  declanPearce,
  DeclanPearce(..),
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype DeclanPearce = DeclanPearce EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

declanPearce :: EnemyCard DeclanPearce
declanPearce = enemy DeclanPearce Cards.declanPearce (2, Static 2, 2) (0, 2)

instance HasModifiersFor DeclanPearce where
  getModifiersFor (DeclanPearce a) = do
    healthModifier <- perPlayer 2
    modifySelf a [HealthModifier healthModifier]
    modifySelect a (investigatorEngagedWith a) [FewerActions 1]

instance HasAbilities DeclanPearce where
  getAbilities (DeclanPearce a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage DeclanPearce where
  runMessage msg e@(DeclanPearce attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      enemies <- selectList AnyEnemy
      for_ enemies \enemyId -> healDamage enemyId (attrs.ability 1) 1
      pure e
    _ -> DeclanPearce <$> liftRunMessage msg attrs
