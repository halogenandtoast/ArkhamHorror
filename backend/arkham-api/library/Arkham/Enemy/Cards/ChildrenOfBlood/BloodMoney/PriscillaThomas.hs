module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.PriscillaThomas (priscillaThomas) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PriscillaThomas = PriscillaThomas EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

priscillaThomas :: EnemyCard PriscillaThomas
priscillaThomas = enemy PriscillaThomas Cards.priscillaThomas

instance RunMessage PriscillaThomas where
  runMessage msg (PriscillaThomas attrs) = PriscillaThomas <$> runMessage msg attrs
