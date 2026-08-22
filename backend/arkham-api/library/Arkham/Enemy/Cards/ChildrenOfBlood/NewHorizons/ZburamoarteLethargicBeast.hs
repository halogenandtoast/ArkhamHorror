module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.ZburamoarteLethargicBeast (zburamoarteLethargicBeast) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ZburamoarteLethargicBeast = ZburamoarteLethargicBeast EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

zburamoarteLethargicBeast :: EnemyCard ZburamoarteLethargicBeast
zburamoarteLethargicBeast = enemy ZburamoarteLethargicBeast Cards.zburamoarteLethargicBeast

instance RunMessage ZburamoarteLethargicBeast where
  runMessage msg (ZburamoarteLethargicBeast attrs) = ZburamoarteLethargicBeast <$> runMessage msg attrs
