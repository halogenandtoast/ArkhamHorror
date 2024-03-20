module Arkham.Enemy.Cards.NyarlathotepTheCrawlingChaos (
  nyarlathotepTheCrawlingChaos,
  NyarlathotepTheCrawlingChaos (..),
)
where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Placement
import Arkham.Prelude

newtype NyarlathotepTheCrawlingChaos = NyarlathotepTheCrawlingChaos EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepTheCrawlingChaos :: EnemyCard NyarlathotepTheCrawlingChaos
nyarlathotepTheCrawlingChaos =
  enemy
    NyarlathotepTheCrawlingChaos
    Cards.nyarlathotepTheCrawlingChaos
    (5, Static 5, 2)
    (0, 1)

instance RunMessage NyarlathotepTheCrawlingChaos where
  runMessage msg e@(NyarlathotepTheCrawlingChaos attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ PlaceEnemy attrs.id (StillInHand iid)
      pure e
    _ -> NyarlathotepTheCrawlingChaos <$> runMessage msg attrs
