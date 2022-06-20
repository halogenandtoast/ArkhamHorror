module Arkham.Enemy.Cards.RiftSeeker
  ( riftSeeker
  , RiftSeeker(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Target

newtype RiftSeeker = RiftSeeker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riftSeeker :: EnemyCard RiftSeeker
riftSeeker = enemy RiftSeeker Cards.riftSeeker (3, Static 3, 3) (1, 1)

instance HasAbilities RiftSeeker where
  getAbilities (RiftSeeker a) = withBaseAbilities
    a
    [ restrictedAbility a 1 OnSameLocation
      $ ActionAbility (Just Action.Parley)
      $ HorrorCost (toSource a) YouTarget 2
    ]

instance RunMessage RiftSeeker where
  runMessage msg (RiftSeeker attrs) = RiftSeeker <$> runMessage msg attrs
