module Arkham.Enemy.Cards.TheBeastInACowlOfCrimsonWolfInSheepsClothing (theBeastInACowlOfCrimsonWolfInSheepsClothing) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TheBeastInACowlOfCrimsonWolfInSheepsClothing = TheBeastInACowlOfCrimsonWolfInSheepsClothing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theBeastInACowlOfCrimsonWolfInSheepsClothing :: EnemyCard TheBeastInACowlOfCrimsonWolfInSheepsClothing
theBeastInACowlOfCrimsonWolfInSheepsClothing = enemy TheBeastInACowlOfCrimsonWolfInSheepsClothing Cards.theBeastInACowlOfCrimsonWolfInSheepsClothing (0, Static 1, 0) (0, 0)

instance RunMessage TheBeastInACowlOfCrimsonWolfInSheepsClothing where
  runMessage msg (TheBeastInACowlOfCrimsonWolfInSheepsClothing attrs) = runQueueT $ case msg of
    _ -> TheBeastInACowlOfCrimsonWolfInSheepsClothing <$> liftRunMessage msg attrs
