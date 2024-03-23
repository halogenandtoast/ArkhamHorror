module Arkham.Enemy.Cards.LegsOfAtlachNacha_348 (
  legsOfAtlachNacha_348,
  LegsOfAtlachNacha_348 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype LegsOfAtlachNacha_348 = LegsOfAtlachNacha_348 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

legsOfAtlachNacha_348 :: EnemyCard LegsOfAtlachNacha_348
legsOfAtlachNacha_348 =
  enemyWith
    LegsOfAtlachNacha_348
    Cards.legsOfAtlachNacha_348
    (0, Static 1, 0)
    (0, 0)
    (asSelfLocationL ?~ "legs2")

instance RunMessage LegsOfAtlachNacha_348 where
  runMessage msg (LegsOfAtlachNacha_348 attrs) =
    LegsOfAtlachNacha_348 <$> runMessage msg attrs
