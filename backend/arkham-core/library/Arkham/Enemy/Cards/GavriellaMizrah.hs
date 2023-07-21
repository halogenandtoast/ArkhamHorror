module Arkham.Enemy.Cards.GavriellaMizrah (
  gavriellaMizrah,
  GavriellaMizrah (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype GavriellaMizrah = GavriellaMizrah EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

gavriellaMizrah :: EnemyCard GavriellaMizrah
gavriellaMizrah = enemy GavriellaMizrah Cards.gavriellaMizrah (5, Static 4, 2) (2, 0)

instance RunMessage GavriellaMizrah where
  runMessage msg (GavriellaMizrah attrs) =
    GavriellaMizrah <$> runMessage msg attrs
