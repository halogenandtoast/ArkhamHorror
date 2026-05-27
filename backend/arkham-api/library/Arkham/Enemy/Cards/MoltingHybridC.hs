module Arkham.Enemy.Cards.MoltingHybridC (moltingHybridC) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher.Damage
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreBarriers)

newtype MoltingHybridC = MoltingHybridC EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moltingHybridC :: EnemyCard MoltingHybridC
moltingHybridC = enemy MoltingHybridC Cards.moltingHybridC (2, Static 3, 4) (0, 1)

instance HasModifiersFor MoltingHybridC where
  getModifiersFor (MoltingHybridC a) =
    modifySelf a [IgnoreBarriers, MaxDamageTaken AnyDamageEffect 1]

instance RunMessage MoltingHybridC where
  runMessage msg (MoltingHybridC attrs) = runQueueT $ case msg of
    _ -> MoltingHybridC <$> liftRunMessage msg attrs
