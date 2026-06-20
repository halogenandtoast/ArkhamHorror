module Arkham.Enemy.Cards.GangEnforcer (gangEnforcer) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype GangEnforcer = GangEnforcer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gangEnforcer :: EnemyCard GangEnforcer
gangEnforcer = enemy GangEnforcer Cards.gangEnforcer

-- TODO: Prey - Most clues. Forced - the first time each round an investigator
-- at this enemy's location attacks another Criminal enemy, Gang Enforcer
-- readies and attacks that investigator.
instance RunMessage GangEnforcer where
  runMessage msg (GangEnforcer attrs) = runQueueT $ GangEnforcer <$> liftRunMessage msg attrs
