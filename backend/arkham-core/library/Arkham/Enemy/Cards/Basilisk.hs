module Arkham.Enemy.Cards.Basilisk
  ( basilisk
  , Basilisk(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype Basilisk = Basilisk EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

basilisk :: EnemyCard Basilisk
basilisk = enemy Basilisk Cards.basilisk (4, Static 4, 4) (2, 0)

instance RunMessage Basilisk where
  runMessage msg (Basilisk attrs) =
    Basilisk <$> runMessage msg attrs
