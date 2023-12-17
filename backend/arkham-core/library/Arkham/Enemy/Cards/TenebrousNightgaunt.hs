module Arkham.Enemy.Cards.TenebrousNightgaunt (tenebrousNightgaunt, TenebrousNightgaunt (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype TenebrousNightgaunt = TenebrousNightgaunt EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

tenebrousNightgaunt :: EnemyCard TenebrousNightgaunt
tenebrousNightgaunt = enemy TenebrousNightgaunt Cards.tenebrousNightgaunt (4, Static 4, 1) (1, 1)

instance RunMessage TenebrousNightgaunt where
  runMessage msg (TenebrousNightgaunt attrs) =
    TenebrousNightgaunt <$> runMessage msg attrs
