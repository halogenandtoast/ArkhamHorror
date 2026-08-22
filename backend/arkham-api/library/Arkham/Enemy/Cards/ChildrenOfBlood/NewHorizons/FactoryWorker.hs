module Arkham.Enemy.Cards.ChildrenOfBlood.NewHorizons.FactoryWorker (factoryWorker) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype FactoryWorker = FactoryWorker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

factoryWorker :: EnemyCard FactoryWorker
factoryWorker = enemy FactoryWorker Cards.factoryWorker

instance RunMessage FactoryWorker where
  runMessage msg (FactoryWorker attrs) = FactoryWorker <$> runMessage msg attrs
