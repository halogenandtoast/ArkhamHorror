module Arkham.Enemy.Cards.SavioCorvi
  ( savioCorvi
  , SavioCorvi(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Classes
import Arkham.Enemy.Runner
import Arkham.Projection

newtype SavioCorvi = SavioCorvi EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

savioCorvi :: EnemyCard SavioCorvi
savioCorvi = enemy SavioCorvi Cards.savioCorvi (3, Static 5, 3) (1, 1)

instance HasModifiersFor SavioCorvi where
  getModifiersFor (EnemyTarget eid) (SavioCorvi attrs) | eid == toId attrs = do
    enemyLocation <- field EnemyLocation (toId attrs)
    case enemyLocation of
      Nothing -> pure []
      Just loc -> do
        acrossLocationId <- getAcrossLocation loc
        pure $ toModifiers attrs [HunterConnectedTo acrossLocationId]
  getModifiersFor _ _ = pure []

instance RunMessage SavioCorvi where
  runMessage msg (SavioCorvi attrs) = SavioCorvi <$> runMessage msg attrs
