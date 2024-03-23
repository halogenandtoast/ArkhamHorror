module Arkham.Enemy.Cards.LegsOfAtlachNacha_347 (
  legsOfAtlachNacha_347,
  LegsOfAtlachNacha_347 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype LegsOfAtlachNacha_347 = LegsOfAtlachNacha_347 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

legsOfAtlachNacha_347 :: EnemyCard LegsOfAtlachNacha_347
legsOfAtlachNacha_347 =
  enemyWith
    LegsOfAtlachNacha_347
    Cards.legsOfAtlachNacha_347
    (0, Static 1, 0)
    (0, 0)
    (asSelfLocationL ?~ "legs1")

instance RunMessage LegsOfAtlachNacha_347 where
  runMessage msg (LegsOfAtlachNacha_347 attrs) =
    LegsOfAtlachNacha_347 <$> runMessage msg attrs
