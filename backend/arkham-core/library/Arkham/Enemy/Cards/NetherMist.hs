module Arkham.Enemy.Cards.NetherMist
  ( netherMist
  , NetherMist(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype NetherMist = NetherMist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

netherMist :: EnemyCard NetherMist
netherMist = enemy NetherMist Cards.netherMist (3, Static 4, 3) (1, 1)

instance RunMessage NetherMist where
  runMessage msg (NetherMist attrs) =
    NetherMist <$> runMessage msg attrs
