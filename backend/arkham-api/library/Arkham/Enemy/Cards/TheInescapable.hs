module Arkham.Enemy.Cards.TheInescapable (theInescapable) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TheInescapable = TheInescapable EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theInescapable :: EnemyCard TheInescapable
theInescapable = enemy TheInescapable Cards.theInescapable

-- TODO: abilities
instance RunMessage TheInescapable where
  runMessage msg (TheInescapable attrs) = runQueueT $ TheInescapable <$> liftRunMessage msg attrs
