module Arkham.Types.Enemy.Cards.SavioCorvi
  ( savioCorvi
  , SavioCorvi(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Types.Classes
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Id
import Arkham.Types.Modifier
import Arkham.Types.Target

newtype SavioCorvi = SavioCorvi EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

savioCorvi :: EnemyCard SavioCorvi
savioCorvi = enemy SavioCorvi Cards.savioCorvi (3, Static 5, 3) (1, 1)

instance (HasSet ConnectedLocationId env LocationId, HasSet LocationId env ()) => HasModifiersFor env SavioCorvi where
  getModifiersFor _ (EnemyTarget eid) (SavioCorvi attrs) | eid == toId attrs =
    do
      acrossLocationId <- getAcrossLocation (enemyLocation attrs)
      pure $ toModifiers attrs [HunterConnectedTo acrossLocationId]
  getModifiersFor _ _ _ = pure []

instance EnemyAttrsHasActions env => HasActions env SavioCorvi where
  getActions i window (SavioCorvi attrs) = getActions i window attrs

instance EnemyAttrsRunMessage env => RunMessage env SavioCorvi where
  runMessage msg (SavioCorvi attrs) = SavioCorvi <$> runMessage msg attrs
