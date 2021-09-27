module Arkham.Types.Enemy.Cards.SavioCorvi
  ( savioCorvi
  , SavioCorvi(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype SavioCorvi = SavioCorvi EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

savioCorvi :: EnemyCard SavioCorvi
savioCorvi = enemy SavioCorvi Cards.savioCorvi (3, Static 5, 3) (1, 1)

instance (HasSet ConnectedLocationId env LocationId, HasSet LocationId env ()) => HasModifiersFor env SavioCorvi where
  getModifiersFor _ (EnemyTarget eid) (SavioCorvi attrs) | eid == toId attrs =
    do
      acrossLocationId <- getAcrossLocation (enemyLocation attrs)
      pure $ toModifiers attrs [HunterConnectedTo acrossLocationId]
  getModifiersFor _ _ _ = pure []

instance EnemyRunner env => RunMessage env SavioCorvi where
  runMessage msg (SavioCorvi attrs) = SavioCorvi <$> runMessage msg attrs
