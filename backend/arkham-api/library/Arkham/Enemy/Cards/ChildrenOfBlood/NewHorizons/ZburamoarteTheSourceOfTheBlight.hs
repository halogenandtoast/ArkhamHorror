module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.ZburamoarteTheSourceOfTheBlight (zburamoarteTheSourceOfTheBlight) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ZburamoarteTheSourceOfTheBlight = ZburamoarteTheSourceOfTheBlight EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

zburamoarteTheSourceOfTheBlight :: EnemyCard ZburamoarteTheSourceOfTheBlight
zburamoarteTheSourceOfTheBlight = enemy ZburamoarteTheSourceOfTheBlight Cards.zburamoarteTheSourceOfTheBlight

instance RunMessage ZburamoarteTheSourceOfTheBlight where
  runMessage msg (ZburamoarteTheSourceOfTheBlight attrs) = ZburamoarteTheSourceOfTheBlight <$> runMessage msg attrs
