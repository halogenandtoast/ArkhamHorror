module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.ZburamoarteProgenitorOfMonsters (zburamoarteProgenitorOfMonsters) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ZburamoarteProgenitorOfMonsters = ZburamoarteProgenitorOfMonsters EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

zburamoarteProgenitorOfMonsters :: EnemyCard ZburamoarteProgenitorOfMonsters
zburamoarteProgenitorOfMonsters = enemy ZburamoarteProgenitorOfMonsters Cards.zburamoarteProgenitorOfMonsters

instance RunMessage ZburamoarteProgenitorOfMonsters where
  runMessage msg (ZburamoarteProgenitorOfMonsters attrs) = ZburamoarteProgenitorOfMonsters <$> runMessage msg attrs
