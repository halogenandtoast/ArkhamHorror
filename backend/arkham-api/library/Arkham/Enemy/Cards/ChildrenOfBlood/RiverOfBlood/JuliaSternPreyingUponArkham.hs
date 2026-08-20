module Arkham.Enemy.Cards.ChildrenOfBlood.RiverOfBlood.JuliaSternPreyingUponArkham (juliaSternPreyingUponArkham) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype JuliaSternPreyingUponArkham = JuliaSternPreyingUponArkham EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

juliaSternPreyingUponArkham :: EnemyCard JuliaSternPreyingUponArkham
juliaSternPreyingUponArkham = enemy JuliaSternPreyingUponArkham Cards.juliaSternPreyingUponArkham

instance RunMessage JuliaSternPreyingUponArkham where
  runMessage msg (JuliaSternPreyingUponArkham attrs) = runQueueT $ case msg of
    _ -> JuliaSternPreyingUponArkham <$> liftRunMessage msg attrs
