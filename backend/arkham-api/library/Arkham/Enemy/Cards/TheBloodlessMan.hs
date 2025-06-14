module Arkham.Enemy.Cards.TheBloodlessMan (theBloodlessMan, TheBloodlessMan(..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Keyword qualified as Keyword
import Arkham.Trait

newtype TheBloodlessMan = TheBloodlessMan EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theBloodlessMan :: EnemyCard TheBloodlessMan
theBloodlessMan =
  unique $ enemy TheBloodlessMan Cards.theBloodlessMan (4, Static 3, 2) (1, 1)

instance HasModifiersFor TheBloodlessMan where
  getModifiersFor (TheBloodlessMan a) =
    modifySelf a [AddKeyword Keyword.Aloof]

instance HasAbilities TheBloodlessMan where
  getAbilities = enemyAbilities

instance RunMessage TheBloodlessMan where
  runMessage msg (TheBloodlessMan attrs) = TheBloodlessMan <$> runMessage msg attrs
