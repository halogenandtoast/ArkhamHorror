module Arkham.Enemy.Cards.PersistentConstruct (persistentConstruct) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PersistentConstruct = PersistentConstruct EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

persistentConstruct :: EnemyCard PersistentConstruct
persistentConstruct = enemy PersistentConstruct Cards.persistentConstruct

-- TODO: abilities
instance RunMessage PersistentConstruct where
  runMessage msg (PersistentConstruct attrs) = runQueueT $ PersistentConstruct <$> liftRunMessage msg attrs
