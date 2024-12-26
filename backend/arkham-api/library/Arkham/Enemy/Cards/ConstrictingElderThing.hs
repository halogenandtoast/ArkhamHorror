module Arkham.Enemy.Cards.ConstrictingElderThing (constrictingElderThing) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype ConstrictingElderThing = ConstrictingElderThing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

constrictingElderThing :: EnemyCard ConstrictingElderThing
constrictingElderThing = enemy ConstrictingElderThing Cards.constrictingElderThing (3, Static 1, 2) (1, 2)

instance RunMessage ConstrictingElderThing where
  runMessage msg (ConstrictingElderThing attrs) = runQueueT $ case msg of
    _ -> ConstrictingElderThing <$> liftRunMessage msg attrs
