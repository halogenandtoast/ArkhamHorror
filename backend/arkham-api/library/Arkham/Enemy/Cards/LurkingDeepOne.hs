module Arkham.Enemy.Cards.LurkingDeepOne
  ( lurkingDeepOne
  , LurkingDeepOne(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype LurkingDeepOne = LurkingDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

lurkingDeepOne :: EnemyCard LurkingDeepOne
lurkingDeepOne = enemy LurkingDeepOne Cards.lurkingDeepOne (2, Static 2, 4) (1, 1)

instance RunMessage LurkingDeepOne where
  runMessage msg (LurkingDeepOne attrs) =
    LurkingDeepOne <$> runMessage msg attrs
