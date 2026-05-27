module Arkham.Enemy.Cards.MoltingHybridB (moltingHybridB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher.Damage
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreBarriers)

newtype MoltingHybridB = MoltingHybridB EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moltingHybridB :: EnemyCard MoltingHybridB
moltingHybridB = enemy MoltingHybridB Cards.moltingHybridB (2, Static 3, 4) (0, 1)

instance HasModifiersFor MoltingHybridB where
  getModifiersFor (MoltingHybridB a) =
    modifySelf a [IgnoreBarriers, MaxDamageTaken AnyDamageEffect 1]

instance RunMessage MoltingHybridB where
  runMessage msg (MoltingHybridB attrs) = runQueueT $ case msg of
    _ -> MoltingHybridB <$> liftRunMessage msg attrs
