module Arkham.Enemy.Cards.ChildrenOfBlood.RiverOfBlood.JuliaSternStalkingTheStreets (juliaSternStalkingTheStreets) where

import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype JuliaSternStalkingTheStreets = JuliaSternStalkingTheStreets EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

juliaSternStalkingTheStreets :: EnemyCard JuliaSternStalkingTheStreets
juliaSternStalkingTheStreets = enemy JuliaSternStalkingTheStreets Cards.juliaSternStalkingTheStreets

instance RunMessage JuliaSternStalkingTheStreets where
  runMessage msg (JuliaSternStalkingTheStreets attrs) = runQueueT $ case msg of
    _ -> JuliaSternStalkingTheStreets <$> liftRunMessage msg attrs
