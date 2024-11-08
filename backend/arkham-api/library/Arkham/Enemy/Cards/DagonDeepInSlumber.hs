module Arkham.Enemy.Cards.DagonDeepInSlumber (dagonDeepInSlumber, DagonDeepInSlumber (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers

newtype DagonDeepInSlumber = DagonDeepInSlumber EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dagonDeepInSlumber :: EnemyCard DagonDeepInSlumber
dagonDeepInSlumber =
  enemyWith DagonDeepInSlumber Cards.dagonDeepInSlumber (0, Static 1, 0) (0, 0)
    $ \a -> a {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing}

instance HasModifiersFor DagonDeepInSlumber where
  getModifiersFor target (DagonDeepInSlumber a) = maybeModified a do
    guard $ isTarget a target
    pure [Omnipotent]

instance RunMessage DagonDeepInSlumber where
  runMessage msg (DagonDeepInSlumber attrs) = runQueueT $ case msg of
    _ -> DagonDeepInSlumber <$> liftRunMessage msg attrs
