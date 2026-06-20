module Arkham.Enemy.Cards.PersistentConstruct (persistentConstruct) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PersistentConstruct = PersistentConstruct EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

persistentConstruct :: EnemyCard PersistentConstruct
persistentConstruct = enemy PersistentConstruct Cards.persistentConstruct

-- TODO: abilities
instance RunMessage PersistentConstruct where
  runMessage msg (PersistentConstruct attrs) = runQueueT $ PersistentConstruct <$> liftRunMessage msg attrs
