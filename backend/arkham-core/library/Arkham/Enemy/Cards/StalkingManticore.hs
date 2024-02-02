module Arkham.Enemy.Cards.StalkingManticore (stalkingManticore, StalkingManticore (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Prelude

newtype StalkingManticore = StalkingManticore EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

stalkingManticore :: EnemyCard StalkingManticore
stalkingManticore = enemy StalkingManticore Cards.stalkingManticore (4, PerPlayer 3, 2) (2, 1)

instance RunMessage StalkingManticore where
  runMessage msg (StalkingManticore attrs) =
    StalkingManticore <$> runMessage msg attrs
