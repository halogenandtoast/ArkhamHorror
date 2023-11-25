module Arkham.Enemy.Cards.SpiderOfLeng (
  spiderOfLeng,
  SpiderOfLeng (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype SpiderOfLeng = SpiderOfLeng EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

spiderOfLeng :: EnemyCard SpiderOfLeng
spiderOfLeng = enemy SpiderOfLeng Cards.spiderOfLeng (3, Static 4, 3) (1, 1)

instance RunMessage SpiderOfLeng where
  runMessage msg (SpiderOfLeng attrs) =
    SpiderOfLeng <$> runMessage msg attrs
