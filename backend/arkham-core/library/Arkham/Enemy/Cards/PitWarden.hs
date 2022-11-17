module Arkham.Enemy.Cards.PitWarden
  ( pitWarden
  , PitWarden(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype PitWarden = PitWarden EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pitWarden :: EnemyCard PitWarden
pitWarden = enemy PitWarden Cards.pitWarden (4, Static 4, 1) (1, 1)

instance RunMessage PitWarden where
  runMessage msg (PitWarden attrs) = PitWarden <$> runMessage msg attrs
