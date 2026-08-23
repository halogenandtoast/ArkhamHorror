module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.ChildOfBlood (childOfBlood) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ChildOfBlood = ChildOfBlood EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

childOfBlood :: EnemyCard ChildOfBlood
childOfBlood = enemy ChildOfBlood Cards.childOfBlood

instance RunMessage ChildOfBlood where
  runMessage msg (ChildOfBlood attrs) = ChildOfBlood <$> runMessage msg attrs
