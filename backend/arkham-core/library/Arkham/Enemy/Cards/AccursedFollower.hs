module Arkham.Enemy.Cards.AccursedFollower
  ( accursedFollower
  , AccursedFollower(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype AccursedFollower = AccursedFollower EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

accursedFollower :: EnemyCard AccursedFollower
accursedFollower = enemy AccursedFollower Cards.accursedFollower (2, Static 2, 2) (1, 1)

instance RunMessage AccursedFollower where
  runMessage msg (AccursedFollower attrs) =
    AccursedFollower <$> runMessage msg attrs
