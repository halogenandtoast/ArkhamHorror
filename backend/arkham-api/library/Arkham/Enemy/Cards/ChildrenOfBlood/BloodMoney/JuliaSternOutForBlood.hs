module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.JuliaSternOutForBlood (juliaSternOutForBlood) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype JuliaSternOutForBlood = JuliaSternOutForBlood EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

juliaSternOutForBlood :: EnemyCard JuliaSternOutForBlood
juliaSternOutForBlood = enemy JuliaSternOutForBlood Cards.juliaSternOutForBlood

instance RunMessage JuliaSternOutForBlood where
  runMessage msg (JuliaSternOutForBlood attrs) = JuliaSternOutForBlood <$> runMessage msg attrs
