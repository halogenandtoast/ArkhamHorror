module Arkham.Enemy.Cards.EquineHybridC (equineHybridC) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher.Damage
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreTraps)

newtype EquineHybridC = EquineHybridC EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

equineHybridC :: EnemyCard EquineHybridC
equineHybridC = enemy EquineHybridC Cards.equineHybridC (2, Static 3, 2) (2, 0)

instance HasModifiersFor EquineHybridC where
  getModifiersFor (EquineHybridC a) =
    modifySelf a [IgnoreTraps, MaxDamageTaken AnyDamageEffect 1]

instance RunMessage EquineHybridC where
  runMessage msg (EquineHybridC attrs) = EquineHybridC <$> runMessage msg attrs
