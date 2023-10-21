module Arkham.Enemy.Cards.MrTrombly
  ( mrTrombly
  , MrTrombly(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype MrTrombly = MrTrombly EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mrTrombly :: EnemyCard MrTrombly
mrTrombly = enemy MrTrombly Cards.mrTrombly (4, Static 5, 4) (2, 1)

instance RunMessage MrTrombly where
  runMessage msg (MrTrombly attrs) =
    MrTrombly <$> runMessage msg attrs
