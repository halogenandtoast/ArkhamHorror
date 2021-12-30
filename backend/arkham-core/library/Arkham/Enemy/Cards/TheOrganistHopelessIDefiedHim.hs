module Arkham.Enemy.Cards.TheOrganistHopelessIDefiedHim
  ( theOrganistHopelessIDefiedHim
  , TheOrganistHopelessIDefiedHim(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Classes
import Arkham.Enemy.Attrs

newtype TheOrganistHopelessIDefiedHim = TheOrganistHopelessIDefiedHim EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theOrganistHopelessIDefiedHim :: EnemyCard TheOrganistHopelessIDefiedHim
theOrganistHopelessIDefiedHim = enemy
  TheOrganistHopelessIDefiedHim
  Cards.theOrganistHopelessIDefiedHim
  (5, Static 1, 3)
  (0, 3)

instance EnemyRunner env => RunMessage env TheOrganistHopelessIDefiedHim where
  runMessage msg (TheOrganistHopelessIDefiedHim attrs) =
    TheOrganistHopelessIDefiedHim <$> runMessage msg attrs
