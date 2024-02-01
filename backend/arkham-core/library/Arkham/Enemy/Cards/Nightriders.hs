module Arkham.Enemy.Cards.Nightriders (nightriders, Nightriders (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype Nightriders = Nightriders EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, HasAbilities)

nightriders :: EnemyCard Nightriders
nightriders = enemy Nightriders Cards.nightriders (2, Static 2, 5) (0, 1)

instance RunMessage Nightriders where
  runMessage msg (Nightriders attrs) =
    Nightriders <$> runMessage msg attrs
