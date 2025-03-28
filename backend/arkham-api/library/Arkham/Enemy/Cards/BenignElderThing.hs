module Arkham.Enemy.Cards.BenignElderThing (benignElderThing) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype BenignElderThing = BenignElderThing EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

benignElderThing :: EnemyCard BenignElderThing
benignElderThing = enemy BenignElderThing Cards.benignElderThing (1, Static 1, 1) (1, 1)

instance RunMessage BenignElderThing where
  runMessage msg (BenignElderThing attrs) = runQueueT $ case msg of
    _ -> BenignElderThing <$> liftRunMessage msg attrs
