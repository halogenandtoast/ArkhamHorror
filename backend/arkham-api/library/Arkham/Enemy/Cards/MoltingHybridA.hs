module Arkham.Enemy.Cards.MoltingHybridA (moltingHybridA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher.Damage
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreBarriers)

newtype MoltingHybridA = MoltingHybridA EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

moltingHybridA :: EnemyCard MoltingHybridA
moltingHybridA = enemy MoltingHybridA Cards.moltingHybridA (2, Static 3, 4) (0, 1)

instance HasModifiersFor MoltingHybridA where
  getModifiersFor (MoltingHybridA a) =
    modifySelf a [IgnoreBarriers, MaxDamageTaken AnyDamageEffect 1]

instance RunMessage MoltingHybridA where
  runMessage msg (MoltingHybridA attrs) = runQueueT $ case msg of
    _ -> MoltingHybridA <$> liftRunMessage msg attrs
