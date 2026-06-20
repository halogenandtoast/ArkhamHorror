module Arkham.Enemy.Cards.GangInformant (gangInformant) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype GangInformant = GangInformant EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gangInformant :: EnemyCard GangInformant
gangInformant = enemy GangInformant Cards.gangInformant

-- TODO: Spawn - nearest Arkham location with clues. Forced - after an
-- investigator at Gang Informant's location discovers 1 or more clues while
-- this enemy is ready, that investigator must either spend 3 resources or
-- place 1 doom on the current agenda.
instance RunMessage GangInformant where
  runMessage msg (GangInformant attrs) = runQueueT $ GangInformant <$> liftRunMessage msg attrs
