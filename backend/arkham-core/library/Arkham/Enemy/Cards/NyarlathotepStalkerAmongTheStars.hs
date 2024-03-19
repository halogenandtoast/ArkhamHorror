module Arkham.Enemy.Cards.NyarlathotepStalkerAmongTheStars
  ( nyarlathotepStalkerAmongTheStars
  , NyarlathotepStalkerAmongTheStars(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype NyarlathotepStalkerAmongTheStars = NyarlathotepStalkerAmongTheStars EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepStalkerAmongTheStars :: EnemyCard NyarlathotepStalkerAmongTheStars
nyarlathotepStalkerAmongTheStars = enemy NyarlathotepStalkerAmongTheStars Cards.nyarlathotepStalkerAmongTheStars (3, Static 7, 3) (1, 1)

instance RunMessage NyarlathotepStalkerAmongTheStars where
  runMessage msg (NyarlathotepStalkerAmongTheStars attrs) =
    NyarlathotepStalkerAmongTheStars <$> runMessage msg attrs
