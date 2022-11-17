module Arkham.Enemy.Cards.Yig
  ( yig
  , Yig(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype Yig = Yig EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

yig :: EnemyCard Yig
yig = enemy Yig Cards.yig (4, Static 6, 4) (3, 3)

instance RunMessage Yig where
  runMessage msg (Yig attrs) = Yig <$> runMessage msg attrs
