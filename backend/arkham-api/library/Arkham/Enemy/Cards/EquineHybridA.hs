module Arkham.Enemy.Cards.EquineHybridA (equineHybridA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher.Damage
import Arkham.Scenarios.TheLongestNight.Helpers (pattern IgnoreTraps)

newtype EquineHybridA = EquineHybridA EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

equineHybridA :: EnemyCard EquineHybridA
equineHybridA = enemy EquineHybridA Cards.equineHybridA (2, Static 3, 2) (2, 0)

instance HasModifiersFor EquineHybridA where
  getModifiersFor (EquineHybridA a) =
    modifySelf a [IgnoreTraps, MaxDamageTaken AnyDamageEffect 1]

instance RunMessage EquineHybridA where
  runMessage msg (EquineHybridA attrs) = EquineHybridA <$> runMessage msg attrs
