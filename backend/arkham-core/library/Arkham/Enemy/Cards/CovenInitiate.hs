module Arkham.Enemy.Cards.CovenInitiate
  ( covenInitiate
  , CovenInitiate(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype CovenInitiate = CovenInitiate EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

covenInitiate :: EnemyCard CovenInitiate
covenInitiate = enemy CovenInitiate Cards.covenInitiate (2, Static 2, 2) (0, 1)

instance RunMessage CovenInitiate where
  runMessage msg (CovenInitiate attrs) =
    CovenInitiate <$> runMessage msg attrs
