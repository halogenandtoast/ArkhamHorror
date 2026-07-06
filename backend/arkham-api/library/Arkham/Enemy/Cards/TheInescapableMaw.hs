module Arkham.Enemy.Cards.TheInescapableMaw (theInescapableMaw) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TheInescapableMaw = TheInescapableMaw EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

-- | Anytime an investigator is forced to "decide" by an ability on a blue
-- encounter card, they must choose two options instead of one. This is
-- implemented by 'Arkham.Scenarios.WarOfTheOuterGods.Helpers.blueDecide' on
-- each blue card with such an ability.
theInescapableMaw :: EnemyCard TheInescapableMaw
theInescapableMaw = enemy TheInescapableMaw Cards.theInescapableMaw

instance RunMessage TheInescapableMaw where
  runMessage msg (TheInescapableMaw attrs) = TheInescapableMaw <$> runMessage msg attrs
