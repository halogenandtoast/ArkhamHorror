module Arkham.Enemy.Cards.ChildrenOfBlood.ChildrenOfBlood.ChildOfBlood (childOfBlood) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.ChildrenOfBlood qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ChildOfBlood = ChildOfBlood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

childOfBlood :: EnemyCard ChildOfBlood
childOfBlood = enemy ChildOfBlood Cards.childOfBlood

instance RunMessage ChildOfBlood where
  runMessage msg (ChildOfBlood attrs) = runQueueT $ case msg of
    _ -> ChildOfBlood <$> liftRunMessage msg attrs
