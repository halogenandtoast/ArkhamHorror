module Arkham.Enemy.Cards.RiftSeeker
  ( riftSeeker
  , RiftSeeker(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype RiftSeeker = RiftSeeker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

riftSeeker :: EnemyCard RiftSeeker
riftSeeker = enemy RiftSeeker Cards.riftSeeker (3, Static 3, 3) (1, 1)

instance RunMessage RiftSeeker where
  runMessage msg (RiftSeeker attrs) = RiftSeeker <$> runMessage msg attrs
