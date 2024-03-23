module Arkham.Enemy.Cards.LegsOfAtlachNacha_350 (
  legsOfAtlachNacha_350,
  LegsOfAtlachNacha_350 (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype LegsOfAtlachNacha_350 = LegsOfAtlachNacha_350 EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

legsOfAtlachNacha_350 :: EnemyCard LegsOfAtlachNacha_350
legsOfAtlachNacha_350 =
  enemyWith
    LegsOfAtlachNacha_350
    Cards.legsOfAtlachNacha_350
    (0, Static 1, 0)
    (0, 0)
    (asSelfLocationL ?~ "legs4")

instance RunMessage LegsOfAtlachNacha_350 where
  runMessage msg (LegsOfAtlachNacha_350 attrs) =
    LegsOfAtlachNacha_350 <$> runMessage msg attrs
