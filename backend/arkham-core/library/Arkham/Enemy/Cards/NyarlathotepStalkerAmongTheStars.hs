module Arkham.Enemy.Cards.NyarlathotepStalkerAmongTheStars (
  nyarlathotepStalkerAmongTheStars,
  NyarlathotepStalkerAmongTheStars (..),
)
where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Placement
import Arkham.Prelude

newtype NyarlathotepStalkerAmongTheStars = NyarlathotepStalkerAmongTheStars EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepStalkerAmongTheStars :: EnemyCard NyarlathotepStalkerAmongTheStars
nyarlathotepStalkerAmongTheStars =
  enemy
    NyarlathotepStalkerAmongTheStars
    Cards.nyarlathotepStalkerAmongTheStars
    (3, Static 7, 3)
    (1, 1)

instance RunMessage NyarlathotepStalkerAmongTheStars where
  runMessage msg e@(NyarlathotepStalkerAmongTheStars attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceEnemy attrs.id (StillInHand iid)
      pure e
    _ -> NyarlathotepStalkerAmongTheStars <$> runMessage msg attrs
