module Arkham.Enemy.Cards.EquineHybridB (equineHybridB) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher.Damage
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreTraps)

newtype EquineHybridB = EquineHybridB EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

equineHybridB :: EnemyCard EquineHybridB
equineHybridB = enemy EquineHybridB Cards.equineHybridB (2, Static 3, 2) (2, 0)

instance HasModifiersFor EquineHybridB where
  getModifiersFor (EquineHybridB a) =
    modifySelf a [IgnoreTraps, MaxDamageTaken AnyDamageEffect 1]

instance RunMessage EquineHybridB where
  runMessage msg (EquineHybridB attrs) = EquineHybridB <$> runMessage msg attrs
