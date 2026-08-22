module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.BlightedWorker (blightedWorker) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BlightedWorker = BlightedWorker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

blightedWorker :: EnemyCard BlightedWorker
blightedWorker = enemy BlightedWorker Cards.blightedWorker

instance RunMessage BlightedWorker where
  runMessage msg (BlightedWorker attrs) = BlightedWorker <$> runMessage msg attrs
