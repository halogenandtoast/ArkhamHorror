module Arkham.Enemy.Cards.PriestOfAThousandMasks (priestOfAThousandMasks, PriestOfAThousandMasks (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype PriestOfAThousandMasks = PriestOfAThousandMasks EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

priestOfAThousandMasks :: EnemyCard PriestOfAThousandMasks
priestOfAThousandMasks = enemy PriestOfAThousandMasks Cards.priestOfAThousandMasks (2, Static 2, 2) (0, 1)

instance RunMessage PriestOfAThousandMasks where
  runMessage msg (PriestOfAThousandMasks attrs) =
    PriestOfAThousandMasks <$> runMessage msg attrs
