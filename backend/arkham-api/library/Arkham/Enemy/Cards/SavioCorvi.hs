module Arkham.Enemy.Cards.SavioCorvi (savioCorvi, SavioCorvi (..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfMaybe)
import Arkham.Projection
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers

newtype SavioCorvi = SavioCorvi EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

savioCorvi :: EnemyCard SavioCorvi
savioCorvi = enemy SavioCorvi Cards.savioCorvi (3, Static 5, 3) (1, 1)

instance HasModifiersFor SavioCorvi where
  getModifiersFor (SavioCorvi attrs) = modifySelfMaybe attrs do
    loc <- MaybeT $ field EnemyLocation attrs.id
    across <- MaybeT $ getAcrossLocation loc
    pure [HunterConnectedTo across]

instance RunMessage SavioCorvi where
  runMessage msg (SavioCorvi attrs) = SavioCorvi <$> runMessage msg attrs
