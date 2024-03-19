module Arkham.Enemy.Cards.NyarlathotepTheCrawlingChaos
  ( nyarlathotepTheCrawlingChaos
  , NyarlathotepTheCrawlingChaos(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype NyarlathotepTheCrawlingChaos = NyarlathotepTheCrawlingChaos EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

nyarlathotepTheCrawlingChaos :: EnemyCard NyarlathotepTheCrawlingChaos
nyarlathotepTheCrawlingChaos = enemy NyarlathotepTheCrawlingChaos Cards.nyarlathotepTheCrawlingChaos (5, Static 5, 2) (0, 1)

instance RunMessage NyarlathotepTheCrawlingChaos where
  runMessage msg (NyarlathotepTheCrawlingChaos attrs) =
    NyarlathotepTheCrawlingChaos <$> runMessage msg attrs
