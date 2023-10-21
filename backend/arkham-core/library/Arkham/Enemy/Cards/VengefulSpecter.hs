module Arkham.Enemy.Cards.VengefulSpecter
  ( vengefulSpecter
  , VengefulSpecter(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype VengefulSpecter = VengefulSpecter EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

vengefulSpecter :: EnemyCard VengefulSpecter
vengefulSpecter = enemy VengefulSpecter Cards.vengefulSpecter (4, PerPlayer 4, 5) (0, 2)

instance RunMessage VengefulSpecter where
  runMessage msg (VengefulSpecter attrs) =
    VengefulSpecter <$> runMessage msg attrs
