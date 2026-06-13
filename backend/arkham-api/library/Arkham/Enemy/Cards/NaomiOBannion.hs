module Arkham.Enemy.Cards.NaomiOBannion (naomiOBannion) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers

newtype NaomiOBannion = NaomiOBannion EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

naomiOBannion :: EnemyCard NaomiOBannion
naomiOBannion = enemy NaomiOBannion Cards.naomiOBannion (3, Static 4, 4) (2, 0)

instance HasModifiersFor NaomiOBannion where
  getModifiersFor (NaomiOBannion attrs) = do
    n <- perPlayer 1
    modifySelf attrs [CannotMove, HealthModifier n]

-- TODO: Forced - the first time Naomi is evaded each round, ready her; any
-- investigator at her location may take 1 damage to prevent this.
instance RunMessage NaomiOBannion where
  runMessage msg (NaomiOBannion attrs) = runQueueT $ NaomiOBannion <$> liftRunMessage msg attrs
