module Arkham.Enemy.Cards.SlithererInDarkness (slithererInDarkness) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype SlithererInDarkness = SlithererInDarkness EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slithererInDarkness :: EnemyCard SlithererInDarkness
slithererInDarkness = enemy SlithererInDarkness Cards.slithererInDarkness (3, Static 5, 4) (2, 1)

-- TODO: abilities
instance RunMessage SlithererInDarkness where
  runMessage msg (SlithererInDarkness attrs) = runQueueT $ SlithererInDarkness <$> liftRunMessage msg attrs
