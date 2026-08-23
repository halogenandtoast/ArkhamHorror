module Arkham.Enemy.Cards.ChildrenOfBlood.BloodMoney.JuliaSternFirstVictimOfNewHorizons (juliaSternFirstVictimOfNewHorizons) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype JuliaSternFirstVictimOfNewHorizons = JuliaSternFirstVictimOfNewHorizons EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

juliaSternFirstVictimOfNewHorizons :: EnemyCard JuliaSternFirstVictimOfNewHorizons
juliaSternFirstVictimOfNewHorizons = enemy JuliaSternFirstVictimOfNewHorizons Cards.juliaSternFirstVictimOfNewHorizons

instance RunMessage JuliaSternFirstVictimOfNewHorizons where
  runMessage msg (JuliaSternFirstVictimOfNewHorizons attrs) = JuliaSternFirstVictimOfNewHorizons <$> runMessage msg attrs
