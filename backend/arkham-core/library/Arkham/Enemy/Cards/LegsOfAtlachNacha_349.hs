module Arkham.Enemy.Cards.LegsOfAtlachNacha_349 (
  legsOfAtlachNacha_349,
  LegsOfAtlachNacha_349 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype LegsOfAtlachNacha_349 = LegsOfAtlachNacha_349 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

legsOfAtlachNacha_349 :: EnemyCard LegsOfAtlachNacha_349
legsOfAtlachNacha_349 =
  enemyWith
    LegsOfAtlachNacha_349
    Cards.legsOfAtlachNacha_349
    (0, Static 1, 0)
    (0, 0)
    (asSelfLocationL ?~ "legs3")

instance RunMessage LegsOfAtlachNacha_349 where
  runMessage msg (LegsOfAtlachNacha_349 attrs) =
    LegsOfAtlachNacha_349 <$> runMessage msg attrs
