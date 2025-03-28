module Arkham.Enemy.Cards.TerrorOfTheStarsBaneOfTheElderThings (terrorOfTheStarsBaneOfTheElderThings) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype TerrorOfTheStarsBaneOfTheElderThings = TerrorOfTheStarsBaneOfTheElderThings EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

terrorOfTheStarsBaneOfTheElderThings :: EnemyCard TerrorOfTheStarsBaneOfTheElderThings
terrorOfTheStarsBaneOfTheElderThings = enemy TerrorOfTheStarsBaneOfTheElderThings Cards.terrorOfTheStarsBaneOfTheElderThings (4, Static 3, 3) (2, 2)

instance RunMessage TerrorOfTheStarsBaneOfTheElderThings where
  runMessage msg (TerrorOfTheStarsBaneOfTheElderThings attrs) = runQueueT $ case msg of
    _ -> TerrorOfTheStarsBaneOfTheElderThings <$> liftRunMessage msg attrs
