module Arkham.Enemy.Cards.TerrorOfTheStarsBringerOfIceAndDeath
  ( terrorOfTheStarsBringerOfIceAndDeath
  , TerrorOfTheStarsBringerOfIceAndDeath(..)
  )
where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TerrorOfTheStarsBringerOfIceAndDeath = TerrorOfTheStarsBringerOfIceAndDeath EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

terrorOfTheStarsBringerOfIceAndDeath :: EnemyCard TerrorOfTheStarsBringerOfIceAndDeath
terrorOfTheStarsBringerOfIceAndDeath = enemy TerrorOfTheStarsBringerOfIceAndDeath Cards.terrorOfTheStarsBringerOfIceAndDeath (0, Static 1, 0) (0, 0)

instance RunMessage TerrorOfTheStarsBringerOfIceAndDeath where
  runMessage msg (TerrorOfTheStarsBringerOfIceAndDeath attrs) = runQueueT $ case msg of
    _ -> TerrorOfTheStarsBringerOfIceAndDeath <$> liftRunMessage msg attrs
