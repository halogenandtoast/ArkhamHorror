module Arkham.Enemy.Cards.ChildrenOfBlood.RiverOfBlood.JuliaSternOnTheRun (juliaSternOnTheRun) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype JuliaSternOnTheRun = JuliaSternOnTheRun EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

juliaSternOnTheRun :: EnemyCard JuliaSternOnTheRun
juliaSternOnTheRun = enemy JuliaSternOnTheRun Cards.juliaSternOnTheRun

instance RunMessage JuliaSternOnTheRun where
  runMessage msg (JuliaSternOnTheRun attrs) = runQueueT $ case msg of
    _ -> JuliaSternOnTheRun <$> liftRunMessage msg attrs
