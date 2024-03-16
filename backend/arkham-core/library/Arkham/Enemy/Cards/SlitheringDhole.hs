module Arkham.Enemy.Cards.SlitheringDhole
  ( slitheringDhole
  , SlitheringDhole(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SlitheringDhole = SlitheringDhole EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

slitheringDhole :: EnemyCard SlitheringDhole
slitheringDhole = enemy SlitheringDhole Cards.slitheringDhole (3, Static 5, 3) (1, 1)

instance RunMessage SlitheringDhole where
  runMessage msg (SlitheringDhole attrs) =
    SlitheringDhole <$> runMessage msg attrs
